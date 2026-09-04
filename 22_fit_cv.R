## =====================================================================
## 22_fit_cv.R  --  Model selection for the bucket probabilities
##
## The quantity we want is P(institution i is in category k at t+h). The
## category edges are KNOWN CONSTANTS, so that probability is just the
## conditional distribution of h-step log growth evaluated at
## log(edge) - log(assets now). Estimate the distribution and the
## probabilities fall out; there is no cutpoint to estimate and no
## ordering to impose, because the edges already supply both.
##
## The distribution is estimated by DISTRIBUTION REGRESSION: a logit for
## P(dy <= c | x) at each of M thresholds c, rearranged to be monotone,
## then interpolated at whatever c each institution's edges imply. That is
## the whole method. It reduces to ordinary logits, so it runs on base R
## and does not need a package the firewall will block.
##
## Three specifications compete against three benchmarks, exactly the way
## 11 puts the flat and simple-growth benchmarks in the race rather than
## holding them as fallbacks. If the covariates do not beat the empirical
## distribution of the size cell, we should know that before publishing.
##
## Cross-validation is BLOCKED BY ORIGIN with an h-quarter embargo:
## training outcomes are fully realised before the test origin begins.
## Random k-fold would leak, because two origins from the same institution
## eight quarters apart share sixteen quarters of the same outcome window
## and would sit on both sides of the split.
##
## CLOSED COHORT. The published total is fixed at the 4,202 institutions
## active in 2026Q2. That needs no special handling here: the seven bucket
## probabilities on each row sum to one by construction, so summing them
## over 4,202 rows gives back exactly 4,202. The reconciliation is exact
## and is asserted at [22.7], not arranged after the fact.
##
## The exit hazard at [22.8] is therefore switched OFF by default. It is
## left in place because it costs nothing, and because it is the only
## measurement of the size of the assumption being made -- see [20.5].
##
## Runtime: roughly 20-40 minutes, not the 12 hours of script 11.
##
## Run block by block in RStudio.
## =====================================================================

library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

## prep <- readRDS("panel_prep.rds");     list2env(prep, .GlobalEnv)
## fts  <- readRDS("panel_features.rds"); list2env(fts,  .GlobalEnv)

## ---------------------------------------------------------------------
## [22.0] Input guard
##
## 22 reads `feat` and `fc_rows`, which must be the SCALED frames from
## [21.7]. Running 21 and 22 back to back in one session can leave the
## unscaled `feat` in the global environment, and the failure then surfaces
## deep inside model.matrix as "object 'cat_f' not found". Repair it here
## rather than making that error a puzzle every time.
## ---------------------------------------------------------------------
if (!"cat_f" %in% names(feat) || !"y_raw" %in% names(feat)) {
  if (exists("feat_s")) {
    message("[22.0] feat was unscaled -- using feat_s / fc_rows_s instead.")
    feat <- feat_s; fc_rows <- fc_rows_s
  } else if (exists("apply_scale")) {
    message("[22.0] feat was unscaled -- applying apply_scale().")
    feat <- apply_scale(feat); fc_rows <- apply_scale(fc_rows)
  } else {
    stop("[22.0] feat is unscaled and neither feat_s nor apply_scale exists. ",
         "Re-load panel_features.rds.")
  }
}

stopifnot(all(c("cat_f", "y_raw", "cat_k", "pos_in_cat") %in% names(feat)),
          all(c("cat_f", "y_raw", "cat_k", "pos_in_cat") %in% names(fc_rows)),
          all(paste0("usable_surv_h", H_SET) %in% names(feat)))

cat("[22.0] ok --", nrow(feat), "origin rows,", nrow(fc_rows), "forecast rows\n")

## Follows script 20. TRUE = fixed total, no exit column published.
CLOSED_COHORT <- if (exists("CLOSED_COHORT")) CLOSED_COHORT else TRUE
FIT_EXIT      <- !CLOSED_COHORT      # [22.8] runs only if exits are wanted
CLOSED_COHORT; FIT_EXIT

N_THRESH   <- 25      # distribution-regression grid points
K_FOLDS    <- 4
FOLD_WIDTH <- 4       # test origins per fold, in quarters
FOLD_GAP   <- 8       # spacing between fold test windows
P_FLOOR    <- 1e-6

## Threshold grid range. Pulled in from 0.02/0.98 because the extreme
## thresholds are where binomial separation lives: within a thin category
## cell, every observation can fall on one side, the likelihood has no
## interior maximum, and IRLS runs to its iteration cap for nothing.
THRESH_LO  <- 0.03
THRESH_HI  <- 0.97
GLM_MAXIT  <- 60

## Training-set thinning. With h=20 two origins one quarter apart share 19
## of 20 quarters of outcome window, so consecutive origins are very nearly
## duplicate observations: dropping every other one costs almost no
## information and halves the design matrix. MAX_TRAIN then caps what is
## left -- 40 parameters do not need 300,000 rows, and the estimates are
## indistinguishable well below that.
TRAIN_ORIGIN_STEP <- 2
MAX_TRAIN         <- 120000
set.seed(20260904)

## ---------------------------------------------------------------------
## [22.1] Specifications
##
## dr_base   size, position in the band, own growth history, volatility
## dr_full   adds acquisitions, peers, and the regime variables
## dr_int    adds the interactions that matter a priori: distance to each
##           edge by category, and growth by volatility. A fast institution
##           with erratic assets is not the same bet as a fast steady one.
##
## The benchmarks:
## emp_cell  empirical distribution of h-step growth in the same category,
##           from training origins only. No covariates at all.
## emp_all   pooled empirical distribution. The floor.
## norm_ls   linear model for the mean plus a fitted normal spread. This is
##           the closest analogue to script 16's soft counts, which take a
##           point forecast and hang a normal on it. It is the comparator
##           that decides whether modelling the whole distribution was
##           worth the trouble.
## ---------------------------------------------------------------------
rhs_base <- "y + d_up + d_dn + pos_in_cat + g4 + g12 + g20 + vol + hist_len + cat_f"
rhs_full <- paste(rhs_base,
                  "+ g_peer + acq_w + acq_cum + q_since_acq",
                  "+ region + cu_type + shock_now + shock_trail")
rhs_int  <- paste(rhs_full,
                  "+ cat_f:d_up + cat_f:d_dn + g12:vol + g12:cat_f")

spec_list <- list(
  dr_base = rhs_base,
  dr_full = rhs_full,
  dr_int  = rhs_int)

## Regime variables over the forward window are appended per horizon, since
## their names carry h. dr_base deliberately goes without them.
rhs_for <- function(spec, h) {
  if (spec == "dr_base") spec_list[[spec]]
  else paste(spec_list[[spec]], "+", paste(FEAT_FWD(h), collapse = " + "))
}

rhs_for("dr_full", 20)

## ---------------------------------------------------------------------
## [22.2] Folds
##
## Test windows walk backwards from the last origin whose outcome is
## observable. Training is everything whose outcome window CLOSED before
## the test window opens.
## ---------------------------------------------------------------------
make_folds <- function(h) {
  last_o <- N_Q - h
  starts <- last_o - FOLD_WIDTH + 1 - (seq_len(K_FOLDS) - 1) * (FOLD_WIDTH + FOLD_GAP)
  lapply(starts[starts > 30], function(s) {
    list(test = s:(s + FOLD_WIDTH - 1),
         train_max_origin = s - 1 - h)          # outcome closed before test
  })
}

for (h in H_SET) {
  fl <- make_folds(h)
  cat("h =", h, "\n")
  for (f in fl)
    cat(sprintf("   train origins <= %2d (%s)   test %2d-%2d (%s - %s)\n",
                f$train_max_origin, qgrid$q_label[f$train_max_origin],
                min(f$test), max(f$test),
                qgrid$q_label[min(f$test)], qgrid$q_label[max(f$test)]))
}

## ---------------------------------------------------------------------
## [22.3] Distribution regression: fit and predict
##
## Fitting is M binomial glms on the same design matrix, so build the
## matrix once and call glm.fit directly. Formula parsing per threshold is
## most of the cost otherwise.
##
## Rearrangement: fitted CDFs at adjacent thresholds can cross, because
## each logit is fitted separately. Taking the running maximum across
## thresholds restores monotonicity and is the standard fix; it also cannot
## make the fit worse in the L2 sense.
## ---------------------------------------------------------------------
## Build a model matrix for `d` aligned to a fitted model's columns.
## Used by BOTH the distribution regression and the norm_ls benchmark.
## predict() cannot be used for either: it refuses factor levels that were
## absent from training, which happens whenever a fold's training window
## contains no A7 institutions and its test window does. Aligning by hand
## gives an unseen level a zero column, which is the correct contribution
## for a level the fit never saw.
align_mm <- function(rhs, d, cols) {
  mf <- model.frame(as.formula(paste("~", rhs)), data = d, na.action = na.pass)
  X  <- model.matrix(as.formula(paste("~", rhs)), mf)
  Xa <- matrix(0, nrow = nrow(X), ncol = length(cols),
               dimnames = list(NULL, cols))
  sh <- intersect(colnames(X), cols)
  Xa[, sh] <- X[, sh]
  Xa
}

dr_fit <- function(dtr, rhs, thresholds, verbose = FALSE) {
  mf <- model.frame(as.formula(paste("~", rhs)), data = dtr,
                    na.action = na.pass)
  X  <- model.matrix(as.formula(paste("~", rhs)), mf)
  ok <- complete.cases(X)
  X  <- X[ok, , drop = FALSE]
  yv <- dtr$dy[ok]

  ## Drop columns with no variation in this fold, or glm.fit returns NAs
  keep <- apply(X, 2, function(z) length(unique(z)) > 1)
  keep[1] <- TRUE
  X <- X[, keep, drop = FALSE]

  B <- matrix(NA_real_, nrow = ncol(X), ncol = length(thresholds),
              dimnames = list(colnames(X), NULL))
  why <- character(length(thresholds))

  ## WARM STARTS. The thresholds are a rising sequence and their
  ## coefficient vectors move smoothly, so the previous solution is a good
  ## starting point for the next. This is the single largest speed win
  ## available here -- IRLS typically drops from twenty-odd iterations to a
  ## handful -- and it costs nothing in accuracy.
  start <- NULL

  for (m in seq_along(thresholds)) {
    zz <- as.numeric(yv <= thresholds[m])
    if (mean(zz) < 0.01 || mean(zz) > 0.99) { why[m] <- "degenerate"; next }

    ## WARNINGS ARE SUPPRESSED, NOT TREATED AS FAILURE.
    ## "fitted probabilities numerically 0 or 1 occurred" is routine for a
    ## binomial fit at an extreme threshold with category dummies in the
    ## design, and the fit it accompanies is fine. Discarding on warning --
    ## the pattern script 11 uses for Arima, where it IS right -- throws
    ## away most of the grid here, leaves an all-NA CDF, and surfaces much
    ## later as a NaN in the count reconciliation. Errors still fail.
    fit <- tryCatch(
      suppressWarnings(glm.fit(X, zz, family = binomial(), start = start,
                               control = glm.control(maxit = GLM_MAXIT))),
      error = function(e) NULL)

    ## A start that leads nowhere should not poison the rest of the grid
    if (is.null(fit) && !is.null(start)) {
      start <- NULL
      fit <- tryCatch(
        suppressWarnings(glm.fit(X, zz, family = binomial(),
                                 control = glm.control(maxit = GLM_MAXIT))),
        error = function(e) NULL)
    }
    if (is.null(fit)) { why[m] <- "error"; next }

    cf <- fit$coefficients
    ## Aliased columns come back NA from glm.fit. Zero them: the column
    ## contributes nothing, which is what aliasing means, rather than
    ## poisoning the whole threshold.
    cf[is.na(cf)] <- 0
    if (!all(is.finite(cf))) { why[m] <- "nonfinite"; next }

    ## A fit that hit the iteration cap is KEPT, and flagged. Under
    ## separation the likelihood has no interior maximum, so IRLS will
    ## never report convergence however long it runs -- but the fitted
    ## probabilities it produces at that threshold are pinned near 0 or 1,
    ## which is the right answer there. Rearrangement and interpolation
    ## downstream are indifferent to how the coefficients got large.
    ## Discarding these was what left 13 of 25 thresholds empty.
    if (!isTRUE(fit$converged)) why[m] <- "maxit (kept)"

    B[, m] <- cf
    start  <- cf
  }

  n_ok <- sum(!apply(is.na(B), 2, all))
  if (verbose || n_ok < 0.6 * length(thresholds))
    cat(sprintf("   dr_fit: %d/%d thresholds fitted   %s\n",
                n_ok, length(thresholds),
                paste(names(sort(table(why[why != ""]), decreasing = TRUE)),
                      collapse = ", ")))
  if (n_ok == 0)
    stop("dr_fit: no threshold could be fitted. Check the design matrix ",
         "for this fold before going further.")

  list(B = B, cols = colnames(X), rhs = rhs, thresholds = thresholds,
       n_ok = n_ok, why = why)
}

dr_cdf_matrix <- function(fit, dte) {
  Xa <- align_mm(fit$rhs, dte, fit$cols)

  ## A row with a missing covariate would produce an NA row here and the
  ## NA then travels all the way to the count reconciliation. Nothing
  ## should be missing after [21.7], so fail here where it is diagnosable.
  if (anyNA(Xa))
    stop("dr_cdf_matrix: ", sum(!complete.cases(Xa)),
         " test rows have missing covariates.")

  ## plogis rather than 1/(1+exp(-eta)): the separation-driven fits kept at
  ## [22.3] can produce very large |eta|, where the explicit form overflows
  ## to NaN and the implicit one does not.
  eta <- Xa %*% fit$B
  Fm  <- plogis(eta)

  ## Thresholds where the logit could not be fitted: carry the neighbour
  bad <- apply(is.na(Fm), 2, all)
  if (all(bad))
    stop("dr_cdf_matrix: every threshold is unfitted for this fold.")
  if (any(bad)) {
    good <- which(!bad)
    for (m in which(bad)) Fm[, m] <- Fm[, good[which.min(abs(good - m))]]
  }
  ## Monotone rearrangement across thresholds
  Fm <- t(apply(Fm, 1, cummax))
  Fm <- pmin(pmax(Fm, 0), 1)
  stopifnot(!anyNA(Fm))
  Fm
}

## Anchor the grid so interpolation never has to extrapolate
augment_grid <- function(Fm, thresholds, lo, hi) {
  list(C = c(lo, thresholds, hi),
       F = cbind(0, Fm, 1))
}

## CDF evaluated at row-specific points, vectorised over rows
cdf_at <- function(Fm, C, z) {
  n <- length(z)
  j  <- findInterval(z, C, rightmost.closed = TRUE, all.inside = TRUE)
  wl <- (C[j + 1] - z) / (C[j + 1] - C[j])
  wl <- pmin(pmax(wl, 0), 1)
  out <- wl * Fm[cbind(seq_len(n), j)] + (1 - wl) * Fm[cbind(seq_len(n), j + 1)]
  out[z <= C[1]] <- 0
  out[z >= C[length(C)]] <- 1
  out
}

## Bucket probabilities from a CDF grid. The edges are the known constants.
bucket_probs <- function(Fm, C, y_raw) {
  n <- length(y_raw)
  P <- matrix(0, n, N_CAT)
  Fprev <- rep(0, n)
  for (k in seq_len(N_CAT - 1)) {
    z <- LOG_EDGE[k + 1] - y_raw          # growth needed to reach the edge
    Fk <- cdf_at(Fm, C, z)
    Fk <- pmax(Fk, Fprev)                 # monotone across edges too
    P[, k] <- Fk - Fprev
    Fprev <- Fk
  }
  P[, N_CAT] <- 1 - Fprev

  ## Loud rather than silent: an NA here means the CDF grid had a hole, and
  ## it would otherwise show up as a NaN in the reconciliation assertion
  ## three functions later with nothing to point at.
  if (anyNA(P))
    stop("bucket_probs: ", sum(!complete.cases(P)),
         " rows produced NA probabilities.")

  P <- pmax(P, P_FLOOR)
  P <- P / rowSums(P)
  stopifnot(all(is.finite(P)))
  P
}

## ---------------------------------------------------------------------
## [22.4] Benchmarks, same interface: in a training frame, out a bucket
## probability matrix for the test frame.
## ---------------------------------------------------------------------
bench_probs <- function(kind, dtr, dte) {
  n <- nrow(dte)
  P <- matrix(0, n, N_CAT)

  if (kind == "emp_all") {
    Fh <- ecdf(dtr$dy)
    for (i in seq_len(n)) {
      z <- LOG_EDGE[-1] - dte$y_raw[i]
      cu <- c(Fh(z[-N_CAT]), 1)
      P[i, ] <- diff(c(0, cummax(cu)))
    }
  }

  if (kind == "emp_cell") {
    ## Empirical distribution of the institution's own category, which is
    ## the honest no-covariate benchmark: it already knows size.
    fns <- lapply(seq_len(N_CAT), function(k) {
      d <- dtr$dy[dtr$cat_k == k]
      if (length(d) < 50) d <- dtr$dy
      ecdf(d)
    })
    for (i in seq_len(n)) {
      Fh <- fns[[dte$cat_k[i]]]
      z <- LOG_EDGE[-1] - dte$y_raw[i]
      cu <- c(Fh(z[-N_CAT]), 1)
      P[i, ] <- diff(c(0, cummax(cu)))
    }
  }

  if (kind == "norm_ls") {
    ## Mean model plus fitted spread -- the soft-count analogue, and the
    ## comparator that decides whether modelling the whole distribution was
    ## worth it.
    ##
    ## Fitted with lm.fit and align_mm rather than lm/predict. predict.lm
    ## errors with "factor cat_f has new levels" whenever the test window
    ## contains a category the training window does not -- which happens
    ## for A7 in the early folds, where there are barely any $10B credit
    ## unions to train on. Manual alignment gives that column a zero
    ## coefficient instead of failing the fold.
    mf   <- model.frame(as.formula(paste("~", rhs_full)), data = dtr,
                        na.action = na.pass)
    Xtr  <- model.matrix(as.formula(paste("~", rhs_full)), mf)
    ok   <- complete.cases(Xtr)
    Xtr  <- Xtr[ok, , drop = FALSE]
    ytr  <- dtr$dy[ok]
    keep <- apply(Xtr, 2, function(z) length(unique(z)) > 1); keep[1] <- TRUE
    Xtr  <- Xtr[, keep, drop = FALSE]

    b_mu <- lm.fit(Xtr, ytr)$coefficients; b_mu[is.na(b_mu)] <- 0
    r    <- ytr - as.numeric(Xtr %*% b_mu)
    b_sd <- lm.fit(Xtr, log(pmax(abs(r), 1e-4)))$coefficients
    b_sd[is.na(b_sd)] <- 0

    Xte <- align_mm(rhs_full, dte, colnames(Xtr))
    mu  <- as.numeric(Xte %*% b_mu)
    sg  <- exp(as.numeric(Xte %*% b_sd)) * sqrt(pi / 2)      # E|N| -> sd
    sg  <- pmax(pmin(sg, 5), 0.02)

    for (k in seq_len(N_CAT)) {
      zl <- LOG_EDGE[k]     - dte$y_raw
      zh <- LOG_EDGE[k + 1] - dte$y_raw
      P[, k] <- pnorm(zh, mu, sg) - pnorm(zl, mu, sg)
    }
  }

  if (anyNA(P))
    stop("bench_probs (", kind, "): NA probabilities produced.")
  P <- pmax(P, P_FLOOR)
  P / rowSums(P)
}

## ---------------------------------------------------------------------
## [22.5] Scoring
##
## RPS is the headline. The categories are ORDERED, so putting an
## institution one category away should cost far less than three, and log
## loss does not know that. RPS is the ordered analogue of the Brier score
## and it is what a count deliverable should be selected on.
##
## Count error is reported alongside because it is the actual deliverable:
## a model can score well per institution and still be biased in aggregate.
## ---------------------------------------------------------------------
rps <- function(P, k_act) {
  CP <- t(apply(P, 1, cumsum))
  CA <- t(sapply(k_act, function(k) as.numeric(seq_len(N_CAT) >= k)))
  rowMeans((CP - CA)^2)
}

score_block <- function(P, dte) {
  k_act <- dte$cat_f_act
  ll <- -log(pmax(P[cbind(seq_along(k_act), k_act)], P_FLOOR))
  soft <- colSums(P)
  hard <- as.numeric(table(factor(k_act, levels = 1:N_CAT)))
  ## Closed-cohort reconciliation: rows sum to one, so the soft counts sum
  ## to the number of institutions. Report the actual discrepancy -- a bare
  ## stopifnot cannot distinguish a real mismatch from an NA, and the two
  ## have completely different causes.
  if (!is.finite(sum(soft)))
    stop("score_block: soft counts are not finite -- ",
         sum(!complete.cases(P)), " NA rows in P.")
  if (abs(sum(soft) - nrow(P)) > 1e-6)
    stop(sprintf("score_block: soft counts sum to %.6f, expected %d.",
                 sum(soft), nrow(P)))
  ## Direction: the frozen-cohort track's known weak point
  k_now <- dte$cat_k
  p_dn <- sum(rowSums(P * outer(k_now, seq_len(N_CAT), ">")))
  p_up <- sum(rowSums(P * outer(k_now, seq_len(N_CAT), "<")))
  data.frame(
    n = nrow(P),
    rps = mean(rps(P, k_act)),
    logloss = mean(ll),
    count_mae = mean(abs(soft - hard)),
    count_max = max(abs(soft - hard)),
    pred_down = p_dn, act_down = sum(k_act < k_now),
    pred_up   = p_up, act_up   = sum(k_act > k_now))
}

## ---------------------------------------------------------------------
## [22.6] The run
## ---------------------------------------------------------------------
t0 <- Sys.time()
cv_rows <- list(); oof <- list()

for (h in H_SET) {
  ## Bucket probabilities are CONDITIONAL ON SURVIVAL, which is exactly
  ## what a closed cohort wants: the question being answered is where these
  ## institutions land GIVEN they are still here. Merged-away rows are
  ## excluded from the growth sample rather than counted as an outcome.
  us <- feat[[paste0("usable_surv_h", h)]]

  d_h <- feat[us, ] %>%
    mutate(dy = .data[[paste0("dy_h", h)]],
           cat_f_act = .data[[paste0("cat_f_h", h)]])
  d_h <- d_h[!is.na(d_h$dy) & !is.na(d_h$cat_f_act), ]

  folds <- make_folds(h)

  for (fi in seq_along(folds)) {
    f <- folds[[fi]]
    dte <- d_h[d_h$q_index %in% f$test, ]

    ## Thin the training origins (see TRAIN_ORIGIN_STEP), keeping the most
    ## recent ones, then cap the row count. Both cuts are on the TRAINING
    ## side only -- the test fold is scored in full, so the fold sizes and
    ## the count reconciliation are unaffected.
    tr_or <- sort(unique(d_h$q_index[d_h$q_index <= f$train_max_origin]),
                  decreasing = TRUE)
    tr_or <- tr_or[seq(1, length(tr_or), by = TRAIN_ORIGIN_STEP)]
    dtr   <- d_h[d_h$q_index %in% tr_or, ]
    n_pre <- nrow(dtr)
    if (nrow(dtr) > MAX_TRAIN)
      dtr <- dtr[sort(sample.int(nrow(dtr), MAX_TRAIN)), ]

    if (nrow(dtr) < 5000 || nrow(dte) < 200) next

    thr <- unique(quantile(dtr$dy,
                           seq(THRESH_LO, THRESH_HI, length.out = N_THRESH)))
    lo  <- min(dtr$dy) - 0.5; hi <- max(dtr$dy) + 0.5

    for (sp in names(spec_list)) {
      fit <- dr_fit(dtr, rhs_for(sp, h), thr)
      Fm  <- dr_cdf_matrix(fit, dte)
      ag  <- augment_grid(Fm, thr, lo, hi)
      P   <- bucket_probs(ag$F, ag$C, dte$y_raw)
      s   <- score_block(P, dte)
      cv_rows[[length(cv_rows) + 1]] <-
        cbind(data.frame(h = h, fold = fi, spec = sp), s)
      if (sp == "dr_full")
        oof[[length(oof) + 1]] <- data.frame(
          h = h, fold = fi, join_number = dte$join_number,
          q_index = dte$q_index, cat_k = dte$cat_k,
          cat_act = dte$cat_f_act, p_assigned = P[cbind(1:nrow(P), dte$cat_f_act)],
          p_max = apply(P, 1, max), k_max = max.col(P))
    }

    for (bk in c("emp_cell", "emp_all", "norm_ls")) {
      P <- bench_probs(bk, dtr, dte)
      s <- score_block(P, dte)
      cv_rows[[length(cv_rows) + 1]] <-
        cbind(data.frame(h = h, fold = fi, spec = bk), s)
    }

    cat(sprintf("h=%2d fold %d  train %6d of %6d  test %5d   (%s elapsed)\n",
                h, fi, nrow(dtr), n_pre, nrow(dte),
                format(round(difftime(Sys.time(), t0, units = "mins"), 1))))
  }
}

difftime(Sys.time(), t0, units = "mins")

cv <- bind_rows(cv_rows)
nrow(cv)

## ---------------------------------------------------------------------
## [22.7] Read the result
## ---------------------------------------------------------------------
cv_summary <- cv %>%
  group_by(h, spec) %>%
  summarise(folds = n(),
            rps = round(mean(rps), 5),
            logloss = round(mean(logloss), 4),
            count_mae = round(mean(count_mae), 1),
            count_max = round(max(count_max), 1),
            down_ratio = round(sum(pred_down) / sum(act_down), 2),
            up_ratio   = round(sum(pred_up)   / sum(act_up), 2),
            .groups = "drop") %>%
  arrange(h, rps)

as.data.frame(cv_summary)

## The two questions this table has to answer:
##
## 1. Do the covariates beat emp_cell? If dr_* does not clear the
##    no-covariate empirical distribution of the size band, the covariates
##    are not earning their place and the honest deliverable is the
##    empirical transition matrix.
## 2. Does dr_* beat norm_ls? norm_ls is a mean model with a normal hung on
##    it, which is what script 16 already does. If the full distribution
##    does not beat it, the case for the rebuild is much weaker and it
##    should be said out loud rather than buried.
cv_summary %>% group_by(h) %>%
  summarise(best = spec[which.min(rps)],
            gain_vs_emp_cell = round(100 * (1 - min(rps) / rps[spec == "emp_cell"]), 1),
            gain_vs_norm_ls  = round(100 * (1 - min(rps) / rps[spec == "norm_ls"]), 1),
            .groups = "drop") %>% as.data.frame()

## Closed-cohort check on the fold totals: predicted and actual totals are
## identical by construction, so any gap is a bug, not a result.
cv %>% group_by(h, spec) %>%
  summarise(pred_total = round(sum(n)), act_total = sum(n), .groups = "drop") %>%
  filter(pred_total != act_total) %>% as.data.frame()   # expect zero rows

## Direction is the diagnostic that matters most. down_ratio near 1 means
## the method reproduces downward category movement, which the frozen-
## cohort track does not (46 predicted against 87 actual over five years).
cv %>% group_by(spec) %>%
  summarise(down_ratio = round(sum(pred_down) / sum(act_down), 2),
            up_ratio   = round(sum(pred_up)   / sum(act_up), 2),
            .groups = "drop") %>% as.data.frame()

BEST_SPEC <- cv_summary %>% filter(h == 20) %>% slice(1) %>% pull(spec)
BEST_SPEC

## ---------------------------------------------------------------------
## [22.8] Exit hazard -- OFF under CLOSED_COHORT
##
## Not published. Run it anyway once per refresh with FIT_EXIT <- TRUE: it
## is the only number that says how large the no-merger assumption is, and
## the Method tab should quote it rather than describing the assumption in
## words. Set CLOSED_COHORT <- FALSE in script 20 to publish exits.
## ---------------------------------------------------------------------
if (!FIT_EXIT) {
  cat("\n[22.8] skipped -- CLOSED_COHORT is TRUE, total held at",
      nrow(fc_rows), "institutions.\n")
  cat("        Set FIT_EXIT <- TRUE to measure the assumption.\n")
  exit_cv <- NULL
}

if (FIT_EXIT) {
rhs_exit <- paste("y + d_dn + g12 + g20 + vol + hist_len + cat_f",
                  "+ region + cu_type + acq_cum + shock_now + shock_trail")

ex_rows <- list()
for (h in H_SET) {
  us <- feat[[paste0("usable_h", h)]]
  d_h <- feat[us, ] %>% mutate(ex = .data[[paste0("exit_h", h)]])
  folds <- make_folds(h)
  for (fi in seq_along(folds)) {
    f <- folds[[fi]]
    dtr <- d_h[d_h$q_index <= f$train_max_origin, ]
    dte <- d_h[d_h$q_index %in% f$test, ]
    if (nrow(dtr) < 5000 || nrow(dte) < 200) next
    m <- tryCatch(glm(as.formula(paste("ex ~", rhs_exit)), data = dtr,
                      family = binomial()), error = function(e) NULL)
    if (is.null(m)) next
    p <- predict(m, newdata = dte, type = "response")
    ex_rows[[length(ex_rows) + 1]] <- data.frame(
      h = h, fold = fi, n = nrow(dte),
      brier = mean((p - dte$ex)^2),
      pred_exits = sum(p), act_exits = sum(dte$ex),
      base_rate = mean(dtr$ex))
  }
}

exit_cv <- bind_rows(ex_rows)
exit_cv %>% group_by(h) %>%
  summarise(brier = round(mean(brier), 5),
            pred = round(sum(pred_exits)), act = sum(act_exits),
            ratio = round(sum(pred_exits) / sum(act_exits), 2),
            .groups = "drop") %>% as.data.frame()

## Exit rate implied for the 2026Q2 cohort, as a first look. Compare it
## against the observed run rate at [20.5] before believing it.
cat("\nImplied 5-year exit share, training base rate:",
    round(100 * mean(exit_cv$base_rate[exit_cv$h == 20]), 1), "%\n")

}   # end if (FIT_EXIT)

## ---------------------------------------------------------------------
## [22.9] Calibration of the winner, out of fold
##
## Predicted probability against realised frequency. If this bends, the
## isotonic step in script 23 has something to fix; if it is flat on the
## diagonal, the soft counts can be published as they stand.
## ---------------------------------------------------------------------
oof_df <- bind_rows(oof)
nrow(oof_df)

## Bin on the probability the model gave its OWN modal category, and check
## how often that category was the one that happened. Binning on
## p_assigned instead would condition on the outcome and always look good.
oof_df %>%
  filter(h == 20) %>%
  mutate(bin = cut(p_max, seq(0, 1, 0.1), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(n = n(), mean_p = round(mean(p_max), 3),
            realised = round(mean(k_max == cat_act), 3),
            .groups = "drop") %>% as.data.frame()

## Hit rate of the modal category, by horizon -- comparable to the 95.2%
## two-year category hit rate in section 6 of the handoff.
oof_df %>% group_by(h) %>%
  summarise(hit_modal = round(100 * mean(k_max == cat_act), 1),
            mean_pmax = round(mean(p_max), 3), .groups = "drop") %>%
  as.data.frame()

saveRDS(list(cv = cv, cv_summary = cv_summary, exit_cv = exit_cv,
             CLOSED_COHORT = CLOSED_COHORT, FIT_EXIT = FIT_EXIT,
             oof = oof_df, BEST_SPEC = BEST_SPEC, spec_list = spec_list,
             rhs_for = rhs_for, rhs_full = rhs_full, rhs_exit = rhs_exit,
             dr_fit = dr_fit, dr_cdf_matrix = dr_cdf_matrix,
             augment_grid = augment_grid, cdf_at = cdf_at,
             bucket_probs = bucket_probs, bench_probs = bench_probs,
             rps = rps, make_folds = make_folds,
             N_THRESH = N_THRESH, P_FLOOR = P_FLOOR),
        file = "panel_cv.rds")
