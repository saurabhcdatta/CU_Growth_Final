## =====================================================================
## 30_exit_hazard_models.R  --  Can a richer model predict exits better
##                              than the category rate in 26?
##
## WHY THIS SCRIPT EXISTS
##   26's category-only exit rates under-predicted exits by about 30% out
##   of sample on the full basis, worst in the $10M-$1B categories. Two
##   different things could be wrong, and they need different fixes:
##
##   (a) LEVEL drift -- mergers have run faster in the last decade than the
##       2005-2026 average. No institution-level covariate fixes this; it
##       is a period effect. The candidates below include a "merger
##       environment" adjustment that scales the rate by how the last
##       eight quarters compare with the long run.
##
##   (b) ALLOCATION -- which institutions exit. Unlike growth, exit
##       plausibly depends on things we can observe: slow or negative
##       growth, small and shrinking, low net worth, weak earnings, a
##       history of acquiring others (acquirers rarely get acquired). A
##       covariate model can move exits between categories and regions
##       even when the total is unchanged.
##
##   "Machine learning" helps with (b) if at all, and only if the signal is
##   there. This script measures that instead of assuming it. Candidates
##   run from the baseline up, each scored the same way, on blocked-origin
##   folds with an h-quarter embargo (22's folds), so nothing sees its own
##   future.
##
## THE SCORE THAT MATTERS
##   For counts, calibration beats discrimination: a model that ranks
##   institutions well but gets the number of exits wrong is useless here.
##   So the headline is predicted-vs-actual exits BY CATEGORY across
##   folds. Brier and concordance (AUC) are reported for completeness.
##
## WHAT IS PUBLISHED
##   Nothing institution-level. Whatever wins feeds 26 as P_EXIT, and 26
##   publishes expected counts only. The choice is made in the config:
##   EXIT_MODEL = "cat" | "cat_env" | "logit" | "logit_fin" | "tree".
##
## RUN AFTER 22 and 26 (needs feat, make_folds from 22, and the exit
## columns), and after 31 if peer groups are wanted. A few minutes; the
## tree model adds a few more if available.
## =====================================================================

## ---- config (production) --------------------------------------------
if (!exists("CONFIG_LOADED")) {
  for (.p in c("00_config.R", "../00_config.R",
               "S:/Projects/Credit_Union_Growth_Forecast/00_config.R"))
    if (file.exists(.p)) { source(.p); break }
  rm(.p)
}
if (!exists("cfg_get")) cfg_get <- function(name, default) default
setwd(cfg_get("DATA_DIR", "S:/Projects/Credit_Union_Growth_Forecast/Data"))

library(dplyr)
library(tidyr)
library(haven)
library(splines)     # base R; natural splines for the logit

## ---------------------------------------------------------------------
## [30.0] Objects
## ---------------------------------------------------------------------
SCRIPT30_VERSION <- "2026-09-20a"
cat("30_exit_hazard_models.R version", SCRIPT30_VERSION, "\n")
if (!exists("feat")) { fts <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv) }
if (!exists("make_folds")) { cvr <- readRDS("panel_cv.rds"); make_folds <- cvr$make_folds }
if (!exists("fc"))   { prb <- readRDS("panel_probs.rds"); list2env(prb, .GlobalEnv) }
stopifnot(exists("feat"), exists("make_folds"), exists("fc"), exists("H_SET"),
          exists("CAT_LABELS"), exists("N_CAT"), exists("N_Q"))
if (!exists("FOLD_WIDTH")) FOLD_WIDTH <- 8L
if (!exists("K_FOLDS"))    K_FOLDS    <- 4L
if (!exists("FOLD_GAP"))   FOLD_GAP   <- 0L

## Make every feature the trees use numeric at the source. region and
## cu_type arrive as character/factor; as.matrix() on a frame containing
## them turns the whole matrix to character, which xgboost rejects. Coding
## them here means the matrix is numeric no matter which function builds
## it. The codes are kept for reference.
for (v in c("region", "cu_type")) {
  if (!is.numeric(feat[[v]])) {
    lev <- sort(unique(as.character(feat[[v]])))
    feat[[paste0(v, "_lab")]] <- as.character(feat[[v]])
    feat[[v]] <- as.numeric(factor(as.character(feat[[v]]), levels = lev))
    cat(sprintf("feat$%s coded numeric: %s\n", v,
                paste(sprintf("%d=%s", seq_along(lev), lev), collapse = ", ")))
  }
}
if (exists("fc")) for (v in c("region", "cu_type"))
  if (!is.numeric(fc[[v]])) fc[[v]] <- as.numeric(factor(as.character(fc[[v]]),
    levels = sort(unique(as.character(feat[[paste0(v, "_lab")]])))))

## Peer groups and atypicality from 31, if it has run. The functions are
## what matter: clusters are refit INSIDE each training fold so the
## backtest stays honest.
if (!exists("cl_fit") && file.exists("panel_peers.rds")) {
  .pp <- readRDS("panel_peers.rds"); list2env(.pp, .GlobalEnv); rm(.pp)
}
HAVE_PEERS <- exists("cl_fit") && exists("atyp_fit")
cat("Peer groups from 31:", if (HAVE_PEERS) "available" else "not available (run 31 to add)", "\n")

## ---------------------------------------------------------------------
## [30.1] Settings
## ---------------------------------------------------------------------
EXIT_MODEL     <- cfg_get("EXIT_MODEL", "cat")      # what 26 will use; set after reading [30.6]
ENV_WINDOW_Q   <- cfg_get("EXIT_ENV_WINDOW_Q", 8L)  # quarters for the merger-environment factor
MIN_EXIT_POOL  <- cfg_get("MIN_EXIT_POOL", 200L)

## Financial covariates. The panel from 20 carries assets only; these are
## pulled from the .dta if the column names are known. Leave NULL to skip
## the "logit_fin" candidate. [30.2] prints candidate column names so the
## mapping can be filled in from the codebook.
FIN_VARS <- cfg_get("FIN_VARS", NULL)
##  e.g. FIN_VARS <- c(nw_ratio = "networth_ratio", roa = "roa",
##                     delinq = "delinq_ratio", members = "members",
##                     loans_shares = "loans_to_shares")

DTA <- cfg_get("DTA", NULL)

## Tree model: only if a package is already installed on this machine.
## IT blocks new installs, so this is a check, not an install.
TREE_PKG <- if (requireNamespace("ranger", quietly = TRUE)) "ranger" else
            if (requireNamespace("randomForest", quietly = TRUE)) "randomForest" else
            if (requireNamespace("xgboost", quietly = TRUE)) "xgboost" else NA
cat("Tree package available:", if (is.na(TREE_PKG)) "none (tree candidate skipped)" else TREE_PKG, "\n")

## ---------------------------------------------------------------------
## [30.2] Financial covariates from the .dta (optional)
## ---------------------------------------------------------------------
if (!is.null(DTA) && file.exists(DTA) && is.null(FIN_VARS)) {
  nms <- names(read_dta(DTA, n_max = 1))
  pat <- "networth|net_worth|nw_|roa|return|delinq|dq_|member|loan|share|capital|earn|expense|yield"
  cat("\nCandidate financial columns in the .dta (set FIN_VARS in the config to use them):\n")
  print(grep(pat, nms, value = TRUE, ignore.case = TRUE))
}

if (!is.null(FIN_VARS)) {
  fin <- read_dta(DTA, col_select = all_of(c("join_number", "year", "quarter", unname(FIN_VARS)))) %>%
    mutate(across(everything(), ~ as.numeric(zap_labels(.x)))) %>%
    rename(!!!setNames(unname(FIN_VARS), names(FIN_VARS))) %>%
    mutate(q_index = (year - START_YEAR) * 4 + quarter) %>%
    select(-year, -quarter)
  ## Two-year change in members (or the first variable named), the
  ## strongest single exit signal in most of the literature: shrinking
  ## membership precedes voluntary merger.
  key1 <- names(FIN_VARS)[1]
  fin <- fin %>% arrange(join_number, q_index) %>% group_by(join_number) %>%
    mutate(across(all_of(names(FIN_VARS)), ~ ifelse(is.finite(.x), .x, NA))) %>%
    mutate(mem_chg8 = if ("members" %in% names(FIN_VARS))
             log(pmax(members, 1)) - log(pmax(lag(members, 8), 1)) else NA_real_) %>%
    ungroup()
  feat <- feat %>% select(-any_of(c(names(FIN_VARS), "mem_chg8"))) %>%
    left_join(fin, by = c("join_number", "q_index"))
  cat("Financial covariates joined:", paste(names(FIN_VARS), collapse = ", "), "\n")
}

## ---------------------------------------------------------------------
## [30.3] Candidate models. Each is a function(train, test, h) that
## returns a predicted exit probability for every test row.
## ---------------------------------------------------------------------
cat_rate <- function(train, test) {
  r <- train %>% group_by(cat_k) %>% summarise(rate = mean(ex), n = n(), .groups = "drop")
  for (k in seq_len(N_CAT)) if (!(k %in% r$cat_k) || r$n[r$cat_k == k] < MIN_EXIT_POOL) {
    src <- r %>% filter(cat_k < k, n >= MIN_EXIT_POOL) %>% arrange(desc(cat_k))
    if (nrow(src)) r <- bind_rows(r %>% filter(cat_k != k),
                                  data.frame(cat_k = k, rate = src$rate[1], n = 0L))
  }
  r$rate[match(test$cat_k, r$cat_k)]
}

## Merger-environment factor: exits in the ENV_WINDOW_Q quarters before
## the test origin, relative to the long-run rate over the same window
## length. Uses the one-quarter-ahead exit flag so the window closes
## before the origin. A period effect, applied to every institution.
## Measured on ONE-YEAR exits (exit_h4), whatever the horizon being
## forecast, so the window is as close to the origin as the data allow:
## origins in the ENV_WINDOW_Q quarters ending four quarters before the
## test origin, against the long-run one-year rate over all earlier
## origins. Clamped to [0.5, 2]: it is an environment factor, not a
## forecast of its own.
env_factor <- function(origin) {
  us  <- feat$usable_h4 & feat$q_index <= origin - 4L
  e1  <- feat$exit_h4[us]; q1 <- feat$q_index[us]
  recent <- e1[q1 > origin - 4L - ENV_WINDOW_Q]
  if (length(recent) < 500 || length(e1) < 5000) return(1)
  min(max(mean(recent) / mean(e1), 0.5), 2)
}

m_cat      <- function(tr, te, h) cat_rate(tr, te)
m_cat_env  <- function(tr, te, h) cat_rate(tr, te) * env_factor(min(te$q_index))

## region and cu_type are numeric codes from [30.0]; the logit must still
## treat them as categories, hence factor() in the formula.
rhs_base <- "y + d_dn + g12 + g20 + vol + hist_len + cat_f + factor(region) + factor(cu_type) + acq_cum + shock_now + shock_trail"
m_logit <- function(tr, te, h) {
  m <- tryCatch(suppressWarnings(glm(as.formula(paste("ex ~", rhs_base)), data = tr,
                                     family = binomial())), error = function(e) NULL)
  if (is.null(m)) return(rep(NA_real_, nrow(te)))
  predict(m, newdata = te, type = "response")
}

rhs_fin <- paste(rhs_base, "+ ns(q_index, 3)",
                 if (!is.null(FIN_VARS)) paste("+", paste(c(names(FIN_VARS), "mem_chg8"), collapse = " + ")) else "")
m_logit_fin <- function(tr, te, h) {
  ok_tr <- complete.cases(tr[, c(names(FIN_VARS), "mem_chg8"), drop = FALSE])
  m <- tryCatch(suppressWarnings(glm(as.formula(paste("ex ~", rhs_fin)), data = tr[ok_tr, ],
                                     family = binomial())), error = function(e) NULL)
  if (is.null(m)) return(rep(NA_real_, nrow(te)))
  p <- rep(NA_real_, nrow(te))
  ok_te <- complete.cases(te[, c(names(FIN_VARS), "mem_chg8"), drop = FALSE])
  p[ok_te] <- predict(m, newdata = te[ok_te, ], type = "response")
  ## rows with missing financials fall back to the category rate
  p[!ok_te] <- cat_rate(tr, te[!ok_te, ])
  p
}

tree_vars <- c("y", "d_dn", "g12", "g20", "vol", "hist_len", "cat_k", "region",
               "cu_type", "acq_cum", "shock_now", "shock_trail",
               if (!is.null(FIN_VARS)) c(names(FIN_VARS), "mem_chg8"))

## ---- tuning ---------------------------------------------------------
## Small grid, tuned INSIDE the training fold on a temporal holdout (the
## last TUNE_HOLD_Q origins of the training data, with an h-quarter gap),
## scored on Brier -- the calibration score, since counts are the target.
## The test fold never sees the tuning. Grids are kept small on purpose:
## exits are rare events, trees overfit them readily, and a wide grid
## tuned on the same folds it is scored on would flatter the tree for the
## wrong reason. Widen only if the tuned setting sits at a grid edge.
TUNE_HOLD_Q <- cfg_get("TUNE_HOLD_Q", 8L)
GRID_RF  <- expand.grid(min.node.size = c(25, 100, 400), mtry_frac = c(0.33, 0.6))
GRID_XGB <- expand.grid(max_depth = c(2, 3, 4), eta = c(0.03, 0.1),
                        min_child_weight = c(20, 100))
XGB_MAX_ROUNDS <- 600L      # early stopping on the holdout decides the actual number

tune_split <- function(tr, h) {
  o_max <- max(tr$q_index)
  hold  <- tr$q_index > o_max - TUNE_HOLD_Q
  fit   <- tr$q_index <= o_max - TUNE_HOLD_Q - h      # embargo h quarters
  list(fit = fit, hold = hold)
}

## xgboost wants a numeric matrix; as.matrix() on a data frame with any
## character or factor column (region, cu_type) coerces everything to
## character. Code non-numeric columns as integers -- trees split on them
## as unordered categories anyway. Levels are fixed from the full feature
## table so train and test code identically.
LEVELS <- lapply(tree_vars, function(v) {
  x <- feat[[v]]
  if (is.numeric(x)) NULL else sort(unique(as.character(x)))
})
names(LEVELS) <- tree_vars
num_mat <- function(D) {
  M <- sapply(tree_vars, function(v) {
    x <- D[[v]]
    if (is.null(LEVELS[[v]])) as.numeric(x)
    else as.numeric(factor(as.character(x), levels = LEVELS[[v]]))
  })
  if (is.null(dim(M))) M <- matrix(M, nrow = 1, dimnames = list(NULL, tree_vars))
  storage.mode(M) <- "double"
  M
}

fit_tree <- function(X, yv, params) {
  if (TREE_PKG == "ranger") {
    ranger::ranger(x = X, y = factor(yv), probability = TRUE, num.trees = 400,
                   min.node.size = params$min.node.size,
                   mtry = max(1L, floor(params$mtry_frac * ncol(X))), seed = 1)
  } else if (TREE_PKG == "randomForest") {
    randomForest::randomForest(x = X, y = factor(yv), ntree = 400,
                               nodesize = params$min.node.size,
                               mtry = max(1L, floor(params$mtry_frac * ncol(X))))
  } else {
    ## xgb.train, not xgboost(): the high-level function in 3.x validates
    ## the objective against the label's type and refuses numeric 0/1 for
    ## binary:logistic. xgb.train takes the DMatrix as tuning did.
    xgboost::xgb.train(
      params = list(objective = "binary:logistic", eta = params$eta,
                    max_depth = params$max_depth,
                    min_child_weight = params$min_child_weight,
                    subsample = 0.8, colsample_bytree = 0.8),
      data = xgboost::xgb.DMatrix(num_mat(X), label = yv),
      nrounds = params$nrounds, verbose = 0)
  }
}
pred_tree <- function(m, Xt) {
  if (TREE_PKG == "ranger") predict(m, data = Xt)$predictions[, "1"]
  else if (TREE_PKG == "randomForest") predict(m, newdata = Xt, type = "prob")[, "1"]
  else predict(m, num_mat(Xt))
}

## xgboost's R API has moved the early-stopping result around between
## versions (m$best_iteration, 1-based; xgb.attr(m, "best_iteration"),
## 0-based; or absent). Read it tolerantly and fall back to the rounds
## actually trained. Off by one is immaterial here.
XGB_NEW_API <- !is.na(TREE_PKG) && TREE_PKG == "xgboost" &&
               packageVersion("xgboost") >= "2.1.0"
if (!is.na(TREE_PKG) && TREE_PKG == "xgboost")
  cat("xgboost version", as.character(packageVersion("xgboost")),
      if (XGB_NEW_API) "(new API: evals=)" else "(old API: watchlist=)", "\n")

xgb_train_es <- function(Xf, yf, Xh, yh, prm) {
  dtr <- xgboost::xgb.DMatrix(num_mat(Xf), label = yf)
  dho <- xgboost::xgb.DMatrix(num_mat(Xh), label = yh)
  if (XGB_NEW_API) {
    xgboost::xgb.train(params = prm, data = dtr, nrounds = XGB_MAX_ROUNDS,
                       evals = list(hold = dho), early_stopping_rounds = 30, verbose = 0)
  } else {
    xgboost::xgb.train(params = prm, data = dtr, nrounds = XGB_MAX_ROUNDS,
                       watchlist = list(hold = dho), early_stopping_rounds = 30, verbose = 0)
  }
}

xgb_best <- function(m) {
  b <- tryCatch(m$best_iteration, error = function(e) NULL)
  if (is.null(b) || !length(b) || is.na(b))
    b <- suppressWarnings(as.integer(xgboost::xgb.attr(m, "best_iteration"))) + 1L
  if (is.null(b) || !length(b) || is.na(b))
    b <- tryCatch(m$niter, error = function(e) NULL)
  if (is.null(b) || !length(b) || is.na(b)) b <- XGB_MAX_ROUNDS
  max(1L, as.integer(b))
}

tune_tree <- function(tr, h) {
  sp <- tune_split(tr, h)
  X  <- tr[, tree_vars]; ok <- complete.cases(X)
  fit <- ok & sp$fit; hold <- ok & sp$hold
  if (sum(fit) < 5000 || sum(hold) < 500) return(NULL)
  Xf <- X[fit, ]; yf <- tr$ex[fit]; Xh <- X[hold, ]; yh <- tr$ex[hold]
  if (TREE_PKG == "xgboost") {
    best <- NULL
    for (i in seq_len(nrow(GRID_XGB))) {
      g <- GRID_XGB[i, ]
      m <- xgb_train_es(Xf, yf, Xh, yh,
             list(objective = "binary:logistic", eta = g$eta, max_depth = g$max_depth,
                  min_child_weight = g$min_child_weight, subsample = 0.8,
                  colsample_bytree = 0.8, eval_metric = "logloss"))
      ## after early stopping, predict() uses the best iteration by default
      ## in every xgboost R version, so no iteration argument is passed
      br <- mean((predict(m, num_mat(Xh)) - yh)^2)
      if (is.null(best) || br < best$brier)
        best <- list(brier = br, params = list(eta = g$eta, max_depth = g$max_depth,
                                               min_child_weight = g$min_child_weight,
                                               nrounds = xgb_best(m)))
    }
  } else {
    best <- NULL
    for (i in seq_len(nrow(GRID_RF))) {
      g <- GRID_RF[i, ]
      m <- fit_tree(Xf, yf, list(min.node.size = g$min.node.size, mtry_frac = g$mtry_frac))
      br <- mean((pred_tree(m, Xh) - yh)^2)
      if (is.null(best) || br < best$brier)
        best <- list(brier = br, params = list(min.node.size = g$min.node.size,
                                               mtry_frac = g$mtry_frac))
    }
  }
  best
}

TUNE_LOG <- list()
TREE_ERRORS <- list()
m_tree <- function(tr, te, h) {
  if (is.na(TREE_PKG)) return(rep(NA_real_, nrow(te)))
  ## Fail soft: a tree problem must not take the other candidates down.
  ## The error is printed and logged so it can be fixed, and the fold is
  ## scored without the tree.
  out <- tryCatch(m_tree_core(tr, te, h), error = function(e) {
    msg <- conditionMessage(e)
    cat("  [tree skipped, h=", h, "] ", msg, "\n", sep = "")
    TREE_ERRORS[[length(TREE_ERRORS) + 1]] <<- data.frame(h = h, origin = min(te$q_index),
                                                          error = msg)
    rep(NA_real_, nrow(te))
  })
  out
}
m_tree_core <- function(tr, te, h) {
  tuned <- tune_tree(tr, h)
  if (is.null(tuned)) return(rep(NA_real_, nrow(te)))
  prm <- lapply(tuned$params, function(x) if (is.null(x) || !length(x)) NA else x[1])
  TUNE_LOG[[length(TUNE_LOG) + 1]] <<- data.frame(h = h, origin = min(te$q_index),
                                                   as.data.frame(prm),
                                                   hold_brier = tuned$brier)
  X  <- tr[, tree_vars]; Xt <- te[, tree_vars]
  ok <- complete.cases(X); okt <- complete.cases(Xt)
  m  <- fit_tree(X[ok, ], tr$ex[ok], tuned$params)
  p  <- rep(NA_real_, nrow(te))
  p[okt]  <- pred_tree(m, Xt[okt, ])
  p[!okt] <- cat_rate(tr, te[!okt, ])
  p
}

## Hybrid: the category rate sets HOW MANY exit in each category (which
## the rate gets right), the logit decides WHO (which the logit ranks
## well). Within each category, the rate is spread across institutions in
## proportion to their logit odds, then rescaled so the category total is
## unchanged. Calibrated by construction; better allocation if the
## ranking has any value.
m_cat_logit <- function(tr, te, h) {
  base <- cat_rate(tr, te)
  pl   <- m_logit(tr, te, h)
  if (all(is.na(pl))) return(base)
  out <- base
  for (k in unique(te$cat_k)) {
    i <- which(te$cat_k == k)
    w <- pl[i]; w[!is.finite(w)] <- mean(w, na.rm = TRUE)
    if (length(i) > 1 && sum(w) > 0)
      out[i] <- pmin(base[i] * w / mean(w), 1)   # mean over the category stays = rate
  }
  out
}

## ---- asset-based hazard ----------------------------------------------
## The category rate is a step function of size: every institution in a
## band gets the band's rate. Exit risk actually falls smoothly with size,
## so a smooth curve in log assets is the natural asset-based baseline. A
## natural spline with a few knots is flexible enough to bend at the small
## end and flat enough not to chase noise among the large institutions.
## No other covariate: this is the size-only model, the asset-based twin
## of "cat". Calibrated by construction (a logit with an intercept).
m_size <- function(tr, te, h) {
  m <- tryCatch(suppressWarnings(glm(ex ~ splines::ns(y, df = 5), data = tr,
                                     family = binomial())), error = function(e) NULL)
  if (is.null(m)) return(rep(NA_real_, nrow(te)))
  predict(m, newdata = te, type = "response")
}
m_size_env <- function(tr, te, h) m_size(tr, te, h) * env_factor(min(te$q_index))

## Hybrid on the asset-based level: the size curve sets the level within
## each category (summed over its members), the full logit ranks within it.
m_size_logit <- function(tr, te, h) {
  base <- m_size(tr, te, h)
  pl   <- m_logit(tr, te, h)
  if (all(is.na(base))) return(rep(NA_real_, nrow(te)))
  if (all(is.na(pl))) return(base)
  out <- base
  for (k in unique(te$cat_k)) {
    i <- which(te$cat_k == k)
    w <- pl[i]; w[!is.finite(w)] <- mean(w, na.rm = TRUE)
    if (length(i) > 1 && sum(w) > 0) {
      tot <- sum(base[i])
      out[i] <- pmin(tot * w / sum(w), 1)      # category total preserved
    }
  }
  out
}

## ---- peer-group candidates (from 31) --------------------------------
## "peer": clusters fit on the training fold, empirical exit rate per
## cluster (thin clusters borrow the overall rate), assigned to test rows
## by nearest centre. The category rate with better categories.
## "logit_cl": the full logit plus cluster dummies plus atypicality.
if (HAVE_PEERS) {
  peer_rate <- function(tr, te) {
    cf <- cl_fit(tr)
    ptr <- cl_assign(tr, cf); pte <- cl_assign(te, cf)
    r <- tapply(tr$ex, factor(ptr, levels = seq_len(cf$k)), mean)
    n <- tapply(tr$ex, factor(ptr, levels = seq_len(cf$k)), length)
    r[is.na(r) | n < CL_MIN_N] <- mean(tr$ex)
    list(p = as.numeric(r[pte]), fit = cf, ptr = ptr, pte = pte)
  }
  m_peer     <- function(tr, te, h) peer_rate(tr, te)$p
  m_peer_env <- function(tr, te, h) peer_rate(tr, te)$p * env_factor(min(te$q_index))
  m_logit_cl <- function(tr, te, h) {
    pr <- peer_rate(tr, te)
    af <- atyp_fit(tr)
    tr2 <- tr; te2 <- te
    tr2$peer <- factor(pr$ptr, levels = seq_len(pr$fit$k)); tr2$atyp <- atyp_score(tr, af)
    te2$peer <- factor(pr$pte, levels = seq_len(pr$fit$k)); te2$atyp <- atyp_score(te, af)
    m <- tryCatch(suppressWarnings(glm(as.formula(paste("ex ~", rhs_base, "+ peer + atyp")),
                                       data = tr2, family = binomial())), error = function(e) NULL)
    if (is.null(m)) return(rep(NA_real_, nrow(te)))
    predict(m, newdata = te2, type = "response")
  }
}

CANDS <- list(cat = m_cat, cat_env = m_cat_env,
              size = m_size, size_env = m_size_env,
              logit = m_logit, cat_logit = m_cat_logit, size_logit = m_size_logit)
if (HAVE_PEERS) { CANDS$peer <- m_peer; CANDS$peer_env <- m_peer_env; CANDS$logit_cl <- m_logit_cl }
if (!is.null(FIN_VARS)) CANDS$logit_fin <- m_logit_fin
if (!is.na(TREE_PKG))   CANDS$tree      <- m_tree

## ---------------------------------------------------------------------
## [30.4] Blocked-origin cross-validation, all candidates, all horizons
## ---------------------------------------------------------------------
auc <- function(p, y) {           # Mann-Whitney, base R
  if (length(unique(y)) < 2) return(NA_real_)
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

## Guard against stale function definitions left in the session by an
## earlier version of this script: the tuning code must not build its
## matrix with as.matrix().
if (!is.na(TREE_PKG) && any(grepl("as.matrix", deparse(tune_tree), fixed = TRUE)))
  stop("Stale tune_tree() in session. Run rm(tune_tree, fit_tree, pred_tree, m_tree) ",
       "and re-run this script from [30.3]. Loaded file version: ", SCRIPT30_VERSION)

cat("Running [30.4] with script version", SCRIPT30_VERSION, "| candidates:",
    paste(names(CANDS), collapse = ", "), "\n")
rows <- list()
for (h in H_SET) {
  us  <- feat[[paste0("usable_h", h)]]
  d_h <- feat[us, ] %>% mutate(ex = .data[[paste0("exit_h", h)]])
  for (f in make_folds(h)) {
    tr <- d_h[d_h$q_index <= f$train_max_origin, ]
    te <- d_h[d_h$q_index %in% f$test, ]
    if (nrow(tr) < 5000 || nrow(te) < 200) next
    for (nm in names(CANDS)) {
      p <- CANDS[[nm]](tr, te, h)
      if (all(is.na(p))) next
      p <- pmin(pmax(p, 0), 1)
      by_cat <- te %>% mutate(p = p) %>% group_by(cat_k) %>%
        summarise(n = n(), pred = sum(p), act = sum(ex), .groups = "drop")
      rows[[length(rows) + 1]] <- data.frame(
        h = h, fold = f$test[1], model = nm,
        n = nrow(te), brier = mean((p - te$ex)^2), auc = auc(p, te$ex),
        pred = sum(p), act = sum(te$ex),
        cat_mape = mean(abs(by_cat$pred - by_cat$act) / pmax(by_cat$act, 1)),
        cat_wape = sum(abs(by_cat$pred - by_cat$act)) / max(sum(by_cat$act), 1),
        stringsAsFactors = FALSE)
    }
    cat(sprintf("h=%2d fold@%d done\n", h, f$test[1]))
  }
}
cv_exit <- bind_rows(rows)

if (length(TREE_ERRORS)) {
  cat("\nTree candidate errors (tree skipped on these folds):\n")
  print(bind_rows(TREE_ERRORS), row.names = FALSE)
}
if (length(TUNE_LOG)) {
  tune_log <- bind_rows(TUNE_LOG)
  cat("\nTree settings chosen per fold (tuned on a holdout inside the training data):\n")
  print(tune_log, row.names = FALSE)
  cat("If a setting sits at the edge of its grid in most folds, widen the grid there.\n")
}

## ---------------------------------------------------------------------
## [30.5] Results
## ---------------------------------------------------------------------
summ <- cv_exit %>% group_by(h, model) %>%
  summarise(folds = n(),
            brier = round(mean(brier), 5),
            auc   = round(mean(auc, na.rm = TRUE), 3),
            ratio = round(sum(pred) / sum(act), 2),          # level: 1.00 is perfect
            cat_mape = round(100 * mean(cat_mape), 1),      # allocation, unweighted by category
            cat_wape = round(100 * mean(cat_wape), 1),      # allocation, weighted by exits -- the one to read
            .groups = "drop") %>%
  arrange(h, model)
cat("\n=== Exit-hazard candidates, blocked-origin CV ===\n",
    "ratio    = predicted / actual exits (level)\n",
    "cat_mape = mean abs % error of exits by category, each category equal\n",
    "           (dominated by the top categories, which have a handful of exits)\n",
    "cat_wape = abs errors by category summed / total exits -- weighted; READ THIS ONE\n",
    "auc      = ranking quality (0.5 = none)\n\n")
for (hh in H_SET) {
  cat("h =", hh, "\n")
  print(summ %>% filter(h == hh) %>% select(-h) %>% as.data.frame(), row.names = FALSE)
}

## ---------------------------------------------------------------------
## [30.6] Verdict
## ---------------------------------------------------------------------
## Pick on the count-relevant score: closest level ratio to 1 at five
## years, with allocation error as the tie-break. Discrimination (AUC) is
## reported but does not decide -- see the header.
## Score = weighted allocation error plus the level error, both in
## percent, so a model has to be better on the counts overall, not just on
## one of the two.
v5 <- summ %>% filter(h == 20) %>%
  mutate(level_err = 100 * abs(ratio - 1), score = cat_wape + level_err) %>%
  arrange(score)
best <- v5$model[1]
cat("\nFive-year scoreboard (lower is better):\n")
print(v5 %>% select(model, ratio, level_err, cat_wape, cat_mape, auc, score) %>%
        as.data.frame(), row.names = FALSE)
cat(sprintf("\nVerdict at five years: '%s' (level ratio %.2f, weighted allocation error %.1f%%).\n",
            best, v5$ratio[1], v5$cat_wape[1]))
base5 <- v5 %>% filter(model == "cat")
cat(sprintf("Baseline 'cat': level ratio %.2f, weighted allocation error %.1f%%.\n",
            base5$ratio, base5$cat_wape))
## The stakeholders want an asset-based method, so the reference model is
## "size" (smooth in assets), not "cat" (step by band). A richer model
## has to beat THAT by a margin worth its complexity.
size5 <- v5 %>% filter(model == "size")
if (nrow(size5)) cat(sprintf("Asset-based reference 'size': level ratio %.2f, weighted allocation error %.1f%%.\n",
                             size5$ratio, size5$cat_wape))
ref_score <- if (nrow(size5)) size5$score else base5$score
ref_name  <- if (nrow(size5)) "size" else "cat"
if (best != ref_name && (ref_score - v5$score[1]) < 3) {
  cat("Margin over '", ref_name, "' is under 3 points -- not worth the added complexity.\n", sep = "")
  best <- ref_name
}
if (best != "cat") {
  cat("A richer model beats the category rate on the scores that matter for counts.\n",
      "Set EXIT_MODEL <- \"", best, "\" in the config and re-run 26 -> 27.\n", sep = "")
} else {
  cat("The category rate holds up. The level miss in 26 is a period effect --\n",
      "use EXIT_BASIS = \"recent\" (already set) rather than a covariate model.\n")
}

## ---------------------------------------------------------------------
## [30.7] Cohort predictions for 26, whichever model is chosen
## ---------------------------------------------------------------------
## Fit each candidate on ALL usable history and score the cohort. 26
## reads P_EXIT_ALT[[EXIT_MODEL]] when EXIT_MODEL != "cat".
fc_rows_now <- feat %>% filter(q_index == N_Q) %>%
  semi_join(fc %>% select(join_number), by = "join_number")
P_EXIT_ALT <- list()
for (nm in names(CANDS)) {
  P_EXIT_ALT[[nm]] <- lapply(H_SET, function(h) {
    us <- feat[[paste0("usable_h", h)]]
    tr <- feat[us, ] %>% mutate(ex = .data[[paste0("exit_h", h)]])
    te <- fc_rows_now
    p  <- CANDS[[nm]](tr, te, h)
    p  <- pmin(pmax(p, 0), 1)
    p[match(fc$join_number, te$join_number)]
  })
  names(P_EXIT_ALT[[nm]]) <- as.character(H_SET)
}
cat("\nCohort exit probabilities computed for:", paste(names(P_EXIT_ALT), collapse = ", "), "\n")
cat("Expected five-year exits by model:",
    paste(sprintf("%s %.0f", names(P_EXIT_ALT),
                  sapply(P_EXIT_ALT, function(x) sum(x[["20"]], na.rm = TRUE))), collapse = " | "), "\n")

saveRDS(list(cv_exit = cv_exit, summ = summ, verdict = best,
             P_EXIT_ALT = P_EXIT_ALT, CANDS = names(CANDS),
             FIN_VARS = FIN_VARS, TREE_PKG = TREE_PKG,
             ENV_WINDOW_Q = ENV_WINDOW_Q, rhs_base = rhs_base, rhs_fin = rhs_fin,
             tune_log = if (length(TUNE_LOG)) bind_rows(TUNE_LOG) else NULL,
             GRID_RF = GRID_RF, GRID_XGB = GRID_XGB),
        file = "panel_exit_models.rds")
cat("Saved panel_exit_models.rds.\n")
