## =====================================================================
## 22_fit_cv.R  --  Model selection for the bucket probabilities
##
## The quantity we want is P(institution i is in category k at t+h). The
## category edges are KNOWN CONSTANTS, so that probability is just the
## conditional distribution of h-step log growth evaluated at
## log(edge) - log(assets). Estimate the distribution and the
## probabilities fall out. There is no cutpoint to estimate and no
## ordering to impose, because the edges already supply both.
##
## Two families compete, plus one benchmark:
##
##   emp_*    the empirical conditional distribution of h-step growth,
##            within cells of increasing fineness. No parametric form, no
##            link function, exact tails, and separation is impossible.
##            emp_cell (category only) won the first two runs outright;
##            the finer variants test whether conditioning on position in
##            the band, momentum, volatility or region adds anything.
##
##   dr_*     distribution regression: a logit for P(dy <= c | x) at each
##            of M thresholds, rearranged monotone, interpolated at each
##            institution's own edges. Now RIDGE-PENALISED and rearranged
##            by SORTING rather than a running maximum -- see [22.4] for
##            why both were needed.
##
##   norm_ls  a mean model with a fitted normal spread. This is what
##            script 16 already does, so it is the comparator that decides
##            whether any of this was worth doing.
##
## Cross-validation is BLOCKED BY ORIGIN with an h-quarter embargo:
## training outcomes are fully realised before the test origin begins.
## Random k-fold would leak, because two origins from the same institution
## eight quarters apart share sixteen quarters of the same outcome window.
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
## unscaled `feat` in the global environment, and the failure then
## surfaces deep inside model.matrix as "object 'cat_f' not found".
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

cat("[22.0] ok --", nrow(feat), "origin rows,", nrow(fc_rows),
    "forecast rows\n")

## ---------------------------------------------------------------------
## [22.1] Settings
## ---------------------------------------------------------------------

## Follows script 20. TRUE = fixed total, no exit column published.
CLOSED_COHORT <- if (exists("CLOSED_COHORT")) CLOSED_COHORT else TRUE
FIT_EXIT      <- !CLOSED_COHORT

## The empirical family is cheap and has won twice. The distribution
## regression is the expensive part. Set RUN_DR <- FALSE on later refreshes
## once the comparison is settled and recorded on the Diagnostics tab.
RUN_DR <- TRUE

K_FOLDS    <- 4
FOLD_WIDTH <- 4       # test origins per fold, in quarters
FOLD_GAP   <- 8       # spacing between fold test windows
P_FLOOR    <- 1e-6

## Empirical cells: a cell needs this many training rows to be used,
## otherwise the row falls back to a coarser pool. See [22.3].
MIN_CELL   <- 400
MIN_WIN    <- 150

## Distribution regression grid and penalty
N_THRESH   <- 25      # grid points from the dy quantiles
N_ZGRID    <- 10      # extra points placed where the category edges are
THRESH_LO  <- 0.01
THRESH_HI  <- 0.99
IRLS_MAXIT <- 40
IRLS_TOL   <- 1e-7
RIDGE      <- 1e-4    # per observation; lambda = RIDGE * n
ETA_CAP    <- 25

## Training-set thinning. With h=20 two origins one quarter apart share 19
## of 20 quarters of outcome window, so consecutive origins are very nearly
## duplicate observations: dropping every other one costs almost no
## information and halves the design matrix.
TRAIN_ORIGIN_STEP <- 2
MAX_TRAIN         <- 80000
set.seed(20260904)

## ---------------------------------------------------------------------
## [22.2] Folds
##
## Test windows walk backwards from the last origin whose outcome is
## observable. Training is everything whose outcome window CLOSED before
## the test window opens.
## ---------------------------------------------------------------------
make_folds <- function(h) {
  last_o <- N_Q - h
  starts <- last_o - FOLD_WIDTH + 1 - (seq_len(K_FOLDS) - 1) *
            (FOLD_WIDTH + FOLD_GAP)
  lapply(starts[starts > 30], function(s) {
    list(test = s:(s + FOLD_WIDTH - 1),
         train_max_origin = s - 1 - h)
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
## [22.3] The empirical conditional distribution
##
## For a test institution, take the training credit unions that resemble
## it and read off the empirical CDF of their h-step growth at that
## institution's own category edges. That is the whole method.
##
## Its advantages over the regression are not incidental:
##   - the tails are EXACT, and the tails are where the edges are. [21.2]
##     shows median d_up running 0.36 to 1.71 by category while the middle
##     94% of five-year growth spans only about -0.20 to +0.65, so most
##     boundary crossings sit far out in the tail of the growth
##     distribution. A parametric link has to extrapolate there; an
##     empirical CDF does not.
##   - monotone by construction, so no rearrangement is needed
##   - separation cannot occur, because nothing is being maximised
##   - it runs in seconds, so the fold structure can be re-run freely
##
## Cells are hierarchical. A test row uses the finest pool with enough
## training rows, falling back: fine cell -> own category -> a window of
## three adjacent categories -> everything. The adjacent-category window
## matters for A7, which has only 229 institution-quarters in the whole
## panel: falling straight to the pooled distribution would hand a $10B
## credit union the growth distribution of a $5M one.
## ---------------------------------------------------------------------

## Tercile and median cutpoints come from TRAINING data and are applied to
## both sides, so the cell definition is identical across the split.
key_factory <- function(kind, dtr) {
  cut3 <- function(v) quantile(v, c(1/3, 2/3), na.rm = TRUE)
  cut2 <- function(v) quantile(v, 0.5, na.rm = TRUE)

  switch(kind,
    emp_cell = function(d) paste0("c", d$cat_k),

    emp_pos  = { q <- cut3(dtr$pos_in_cat)
                 function(d) paste0("c", d$cat_k, "_p",
                                    findInterval(d$pos_in_cat, q)) },

    emp_grow = { q <- cut3(dtr$g12)
                 function(d) paste0("c", d$cat_k, "_g",
                                    findInterval(d$g12, q)) },

    emp_gv   = { q <- cut3(dtr$g12); v <- cut2(dtr$vol)
                 function(d) paste0("c", d$cat_k, "_g",
                                    findInterval(d$g12, q), "_v",
                                    findInterval(d$vol, v)) },

    emp_reg  = function(d) paste0("c", d$cat_k, "_r",
                                  as.character(d$region), "_t",
                                  as.character(d$cu_type)),

    emp_all  = function(d) rep("_pool", nrow(d)),

    stop("unknown empirical cell kind: ", kind))
}

## Bucket probabilities from an empirical pool. Vectorised within each
## pool: findInterval on the sorted training growth vector IS the
## empirical CDF, evaluated at every edge at once.
emp_probs <- function(dtr, dte, keyf, min_cell = MIN_CELL) {
  ktr <- keyf(dtr); kte <- keyf(dte)

  pools <- list()
  tb <- table(ktr)
  for (k in names(tb)[tb >= min_cell]) pools[[k]] <- sort(dtr$dy[ktr == k])

  ## Coarser fallbacks, always built
  for (k in sort(unique(dtr$cat_k))) {
    pools[[paste0("_cat", k)]] <- sort(dtr$dy[dtr$cat_k == k])
    pools[[paste0("_win", k)]] <- sort(dtr$dy[abs(dtr$cat_k - k) <= 1])
  }
  pools[["_all"]] <- sort(dtr$dy)

  ## Finest available pool, then the fallback chain
  ref <- ifelse(kte %in% names(pools), kte, NA_character_)
  need <- is.na(ref)
  ref[need] <- paste0("_cat", dte$cat_k[need])
  ref[!ref %in% names(pools)] <-
    paste0("_win", dte$cat_k[!ref %in% names(pools)])

  thin <- lengths(pools[ref]) < MIN_WIN
  ref[thin] <- paste0("_win", dte$cat_k[thin])
  thin2 <- lengths(pools[ref]) < MIN_WIN
  ref[thin2] <- "_all"

  P <- matrix(0, nrow(dte), N_CAT)
  for (k in unique(ref)) {
    idx <- which(ref == k)
    V   <- pools[[k]]; nV <- length(V)
    Fprev <- rep(0, length(idx))
    for (j in seq_len(N_CAT - 1)) {
      z  <- LOG_EDGE[j + 1] - dte$y_raw[idx]
      Fk <- findInterval(z, V) / nV        # empirical CDF, exact
      Fk <- pmax(Fk, Fprev)
      P[idx, j] <- Fk - Fprev
      Fprev <- Fk
    }
    P[idx, N_CAT] <- 1 - Fprev
  }

  if (anyNA(P)) stop("emp_probs: NA probabilities produced.")
  P <- pmax(P, P_FLOOR)
  P / rowSums(P)
}

## ---------------------------------------------------------------------
## [22.4] Distribution regression -- ridge-penalised, sort-rearranged
##
## Two failures from the earlier runs are fixed here, and both are worth
## recording because both looked like model results and were not.
##
## RIDGE. At an extreme threshold, within a thin category cell, every
## training observation can fall on one side. The likelihood then has no
## interior maximum, plain IRLS runs its coefficients off toward infinity,
## and the fitted CDF is pinned at 0 or 1 for whole blocks of institutions.
## Keeping those fits (which one earlier version did) and discarding them
## (which the version before it did) are both wrong. A small ridge penalty
## gives a finite, sensible answer instead; the intercept is left
## unpenalised so the marginal rate is untouched.
##
## REARRANGEMENT BY SORTING, not by a running maximum. Each threshold is
## fitted separately, so adjacent fitted CDFs can cross and must be made
## monotone. cummax looks like the natural fix and is a trap: ONE
## spuriously high value at a LOW threshold propagates upward to every
## threshold above it, forcing all the probability mass into the bottom
## categories. That is exactly what produced up_ratio 0.00 alongside
## down_ratio 32. Sorting each row is the standard rearrangement operator,
## it cannot increase L2 error, and a single bad point stays a single bad
## point.
## ---------------------------------------------------------------------

## Model matrix for `d` aligned to a fitted model's columns. predict()
## cannot be used: it refuses factor levels absent from training, which
## happens whenever a fold's training window has no A7 institutions and
## its test window does. An unseen level gets a zero column, the correct
## contribution for a level the fit never saw.
align_mm <- function(rhs, d, cols) {
  mf <- model.frame(as.formula(paste("~", rhs)), data = d, na.action = na.pass)
  X  <- model.matrix(as.formula(paste("~", rhs)), mf)
  Xa <- matrix(0, nrow = nrow(X), ncol = length(cols),
               dimnames = list(NULL, cols))
  sh <- intersect(colnames(X), cols)
  Xa[, sh] <- X[, sh]
  Xa
}

## Ridge-penalised logistic IRLS. Base R only, no package needed.
ridge_logit <- function(X, y, lambda, start = NULL) {
  p   <- ncol(X)
  b   <- if (is.null(start)) rep(0, p) else start
  pen <- rep(lambda, p); pen[1] <- 0        # intercept unpenalised
  D   <- diag(pen, p)

  for (it in seq_len(IRLS_MAXIT)) {
    eta <- pmin(pmax(drop(X %*% b), -ETA_CAP), ETA_CAP)
    mu  <- plogis(eta)
    w   <- pmax(mu * (1 - mu), 1e-6)
    z   <- eta + (y - mu) / w

    A  <- crossprod(X * sqrt(w)) + D
    bb <- crossprod(X, w * z)
    bn <- tryCatch(drop(solve(A, bb)), error = function(e) NULL)
    if (is.null(bn) || !all(is.finite(bn))) return(NULL)

    delta <- max(abs(bn - b))
    b <- bn
    if (delta < IRLS_TOL) break
  }
  b
}

## Threshold grid. The obvious choice -- quantiles of dy -- is the wrong
## one. The model is never asked for the CDF at a typical growth rate; it
## is asked for it at each institution's own boundary-crossing point,
## z = log(edge) - log(assets), and those sit far out in the tail. So:
## quantiles of dy for the body, plus points at the quantiles of the
## crossing distances that will actually be evaluated, clipped to the
## support of dy.
make_grid <- function(dtr, n_thresh = N_THRESH, n_z = N_ZGRID) {
  thr_q <- quantile(dtr$dy, seq(THRESH_LO, THRESH_HI, length.out = n_thresh))

  z_need <- as.vector(outer(LOG_EDGE[2:N_CAT], dtr$y_raw, "-"))
  z_need <- z_need[is.finite(z_need)]
  zq     <- quantile(z_need, seq(0.05, 0.95, length.out = n_z))

  supp <- quantile(dtr$dy, c(0.004, 0.996))
  zq   <- zq[zq > supp[1] & zq < supp[2]]

  sort(unique(c(thr_q, zq)))
}

dr_fit <- function(dtr, rhs, thresholds, verbose = FALSE) {
  mf <- model.frame(as.formula(paste("~", rhs)), data = dtr,
                    na.action = na.pass)
  X  <- model.matrix(as.formula(paste("~", rhs)), mf)
  ok <- complete.cases(X)
  X  <- X[ok, , drop = FALSE]
  yv <- dtr$dy[ok]

  keep <- apply(X, 2, function(z) length(unique(z)) > 1)
  keep[1] <- TRUE
  X <- X[, keep, drop = FALSE]

  lambda <- RIDGE * nrow(X)
  B   <- matrix(NA_real_, ncol(X), length(thresholds),
                dimnames = list(colnames(X), NULL))
  why <- character(length(thresholds))
  start <- NULL           # warm start: adjacent thresholds are close

  for (m in seq_along(thresholds)) {
    zz <- as.numeric(yv <= thresholds[m])
    if (mean(zz) < 0.005 || mean(zz) > 0.995) { why[m] <- "degenerate"; next }

    b <- ridge_logit(X, zz, lambda, start)
    if (is.null(b) && !is.null(start)) b <- ridge_logit(X, zz, lambda, NULL)
    if (is.null(b)) { why[m] <- "solve failed"; next }

    B[, m] <- b
    start  <- b
  }

  n_ok <- sum(!apply(is.na(B), 2, all))
  if (verbose || n_ok < 0.6 * length(thresholds))
    cat(sprintf("   dr_fit: %d/%d thresholds fitted   %s\n",
                n_ok, length(thresholds),
                paste(names(sort(table(why[why != ""]), decreasing = TRUE)),
                      collapse = ", ")))
  if (n_ok == 0)
    stop("dr_fit: no threshold could be fitted for this fold.")

  list(B = B, cols = colnames(X), rhs = rhs, thresholds = thresholds,
       n_ok = n_ok, why = why, lambda = lambda)
}

dr_cdf_matrix <- function(fit, dte) {
  Xa <- align_mm(fit$rhs, dte, fit$cols)
  if (anyNA(Xa))
    stop("dr_cdf_matrix: ", sum(!complete.cases(Xa)),
         " test rows have missing covariates.")

  eta <- Xa %*% fit$B
  eta <- pmin(pmax(eta, -ETA_CAP), ETA_CAP)
  Fm  <- plogis(eta)

  ## Unfitted thresholds: carry the nearest fitted neighbour
  bad <- apply(is.na(Fm), 2, all)
  if (all(bad)) stop("dr_cdf_matrix: every threshold unfitted for this fold.")
  if (any(bad)) {
    good <- which(!bad)
    for (m in which(bad)) Fm[, m] <- Fm[, good[which.min(abs(good - m))]]
  }

  ## Rearrangement by SORTING each row -- see the note at the head of
  ## [22.4]. Never cummax.
  Fm <- t(apply(Fm, 1, sort))
  Fm <- pmin(pmax(Fm, 0), 1)
  stopifnot(!anyNA(Fm))
  Fm
}

## CDF at row-specific points. Interpolated on the LOGIT scale and
## extrapolated beyond the grid from the slope of the two nearest points.
## Anchoring the ends at F=0 and F=1 and interpolating linearly in
## probability -- an earlier version -- ran a straight line across the
## whole tail and grossly overstated the chance of a crossing.
cdf_at <- function(Fm, C, z) {
  n <- length(z); M <- length(C)
  stopifnot(M >= 2)
  L <- qlogis(pmin(pmax(Fm, 1e-6), 1 - 1e-6))

  j  <- findInterval(z, C)
  jl <- pmin(pmax(j, 1), M - 1)
  jh <- jl + 1
  w  <- (z - C[jl]) / (C[jh] - C[jl])       # outside the grid: w<0 or w>1

  out <- (1 - w) * L[cbind(seq_len(n), jl)] + w * L[cbind(seq_len(n), jh)]
  plogis(pmin(pmax(out, -ETA_CAP), ETA_CAP))
}

## Bucket probabilities from a CDF grid. The edges are known constants.
bucket_probs <- function(Fm, C, y_raw) {
  n <- length(y_raw)
  P <- matrix(0, n, N_CAT)
  Fprev <- rep(0, n)
  for (k in seq_len(N_CAT - 1)) {
    z  <- LOG_EDGE[k + 1] - y_raw
    Fk <- cdf_at(Fm, C, z)
    Fk <- pmax(Fk, Fprev)
    P[, k] <- Fk - Fprev
    Fprev <- Fk
  }
  P[, N_CAT] <- 1 - Fprev

  if (anyNA(P))
    stop("bucket_probs: ", sum(!complete.cases(P)), " rows produced NA.")
  P <- pmax(P, P_FLOOR)
  P <- P / rowSums(P)
  stopifnot(all(is.finite(P)))
  P
}

## ---------------------------------------------------------------------
## [22.5] Specifications
## ---------------------------------------------------------------------
EMP_SPECS <- c("emp_cell", "emp_pos", "emp_grow", "emp_gv", "emp_reg",
               "emp_all")

rhs_base <- paste("y + d_up + d_dn + pos_in_cat + g4 + g12 + g20 + vol",
                  "+ hist_len + cat_f")
rhs_full <- paste(rhs_base,
                  "+ g_peer + acq_w + acq_cum + q_since_acq",
                  "+ region + cu_type + shock_now + shock_trail")
rhs_int  <- paste(rhs_full,
                  "+ cat_f:d_up + cat_f:d_dn + g12:vol + g12:cat_f")

spec_list <- list(dr_base = rhs_base, dr_full = rhs_full, dr_int = rhs_int)

rhs_for <- function(spec, h) {
  if (spec == "dr_base") spec_list[[spec]]
  else paste(spec_list[[spec]], "+", paste(FEAT_FWD(h), collapse = " + "))
}

## norm_ls: mean model plus fitted spread. Fitted with lm.fit and align_mm
## rather than lm/predict, which errors on factor levels the training
## window did not contain -- A7 in the early folds.
norm_ls_probs <- function(dtr, dte) {
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
  sg  <- pmax(pmin(exp(as.numeric(Xte %*% b_sd)) * sqrt(pi / 2), 5), 0.02)

  P <- matrix(0, nrow(dte), N_CAT)
  for (k in seq_len(N_CAT)) {
    zl <- LOG_EDGE[k]     - dte$y_raw
    zh <- LOG_EDGE[k + 1] - dte$y_raw
    P[, k] <- pnorm(zh, mu, sg) - pnorm(zl, mu, sg)
  }
  if (anyNA(P)) stop("norm_ls_probs: NA probabilities produced.")
  P <- pmax(P, P_FLOOR)
  P / rowSums(P)
}

## ---------------------------------------------------------------------
## [22.6] Scoring
##
## RPS is the headline. The categories are ORDERED, so putting an
## institution one category away should cost far less than three, and log
## loss does not know that. Count error sits alongside because the count
## is the actual deliverable: a model can score well per institution and
## still be biased in aggregate.
## ---------------------------------------------------------------------
rps <- function(P, k_act) {
  CP <- t(apply(P, 1, cumsum))
  ## CA[i, j] = 1 once column j has reached the realised category.
  ## outer() rather than sapply(): this is called once per spec per fold,
  ## on ~20,000 rows, and the loop version dominated the scoring time.
  CA <- outer(k_act, seq_len(N_CAT), "<=") * 1
  rowMeans((CP - CA)^2)
}

score_block <- function(P, dte) {
  k_act <- dte$cat_f_act
  ll   <- -log(pmax(P[cbind(seq_along(k_act), k_act)], P_FLOOR))
  soft <- colSums(P)
  hard <- as.numeric(table(factor(k_act, levels = 1:N_CAT)))

  ## Closed-cohort reconciliation. Rows sum to one, so the soft counts sum
  ## to the number of institutions. Report the discrepancy rather than a
  ## bare stopifnot, which cannot distinguish a real mismatch from an NA.
  if (!is.finite(sum(soft)))
    stop("score_block: soft counts not finite -- ",
         sum(!complete.cases(P)), " NA rows in P.")
  if (abs(sum(soft) - nrow(P)) > 1e-6)
    stop(sprintf("score_block: soft counts sum to %.6f, expected %d.",
                 sum(soft), nrow(P)))

  k_now <- dte$cat_k
  list(
    row = data.frame(
      n = nrow(P),
      rps = mean(rps(P, k_act)),
      logloss = mean(ll),
      count_mae = mean(abs(soft - hard)),
      count_max = max(abs(soft - hard)),
      pred_down = sum(rowSums(P * outer(k_now, seq_len(N_CAT), ">"))),
      act_down  = sum(k_act < k_now),
      pred_up   = sum(rowSums(P * outer(k_now, seq_len(N_CAT), "<"))),
      act_up    = sum(k_act > k_now)),
    buckets = data.frame(cat = CAT_LABELS, soft = soft, hard = hard))
}

## ---------------------------------------------------------------------
## [22.7] The run
## ---------------------------------------------------------------------
t0 <- Sys.time()
cv_rows <- list(); oof <- list(); buck <- list()

add_result <- function(P, dte, h, fi, sp) {
  s <- score_block(P, dte)
  cv_rows[[length(cv_rows) + 1]] <<-
    cbind(data.frame(h = h, fold = fi, spec = sp), s$row)
  buck[[length(buck) + 1]] <<-
    cbind(data.frame(h = h, fold = fi, spec = sp), s$buckets)
  ## Out-of-fold rows for EVERY spec. The winner is not known until [22.8],
  ## and storing only one leaves the calibration table describing a model
  ## that was not selected.
  oof[[length(oof) + 1]] <<- data.frame(
    h = h, fold = fi, spec = sp, join_number = dte$join_number,
    q_index = dte$q_index, cat_k = dte$cat_k, cat_act = dte$cat_f_act,
    p_assigned = P[cbind(seq_len(nrow(P)), dte$cat_f_act)],
    p_max = apply(P, 1, max), k_max = max.col(P))
  invisible(NULL)
}

for (h in H_SET) {
  ## Bucket probabilities are CONDITIONAL ON SURVIVAL, which is what a
  ## closed cohort wants: where do these institutions land GIVEN they are
  ## still here. Merged-away rows are excluded, not counted as an outcome.
  us <- feat[[paste0("usable_surv_h", h)]]

  d_h <- feat[us, ] %>%
    mutate(dy = .data[[paste0("dy_h", h)]],
           cat_f_act = .data[[paste0("cat_f_h", h)]])
  d_h <- d_h[!is.na(d_h$dy) & !is.na(d_h$cat_f_act), ]

  folds <- make_folds(h)

  for (fi in seq_along(folds)) {
    f   <- folds[[fi]]
    dte <- d_h[d_h$q_index %in% f$test, ]

    ## Thin the training origins, keeping the most recent, then cap rows.
    ## Both cuts are TRAINING-side only; the test fold is scored in full,
    ## so fold sizes and the count reconciliation are unaffected.
    tr_or <- sort(unique(d_h$q_index[d_h$q_index <= f$train_max_origin]),
                  decreasing = TRUE)
    tr_or <- tr_or[seq(1, length(tr_or), by = TRAIN_ORIGIN_STEP)]
    dtr   <- d_h[d_h$q_index %in% tr_or, ]
    n_pre <- nrow(dtr)
    if (nrow(dtr) > MAX_TRAIN)
      dtr <- dtr[sort(sample.int(nrow(dtr), MAX_TRAIN)), ]
    if (nrow(dtr) < 5000 || nrow(dte) < 200) next

    ## --- empirical family (seconds) ---
    for (sp in EMP_SPECS) {
      keyf <- key_factory(sp, dtr)
      add_result(emp_probs(dtr, dte, keyf), dte, h, fi, sp)
    }

    ## --- parametric benchmark ---
    add_result(norm_ls_probs(dtr, dte), dte, h, fi, "norm_ls")

    ## --- distribution regression (the expensive part) ---
    n_thr <- NA
    if (RUN_DR) {
      thr <- make_grid(dtr); n_thr <- length(thr)
      for (sp in names(spec_list)) {
        fit <- dr_fit(dtr, rhs_for(sp, h), thr)
        Fm  <- dr_cdf_matrix(fit, dte)
        add_result(bucket_probs(Fm, thr, dte$y_raw), dte, h, fi, sp)
      }
    }

    cat(sprintf("h=%2d fold %d  train %6d of %6d  test %5d  thr %s  (%s min)\n",
                h, fi, nrow(dtr), n_pre, nrow(dte),
                ifelse(is.na(n_thr), "--", n_thr),
                format(round(difftime(Sys.time(), t0, units = "mins"), 1))))
  }
}

difftime(Sys.time(), t0, units = "mins")

cv      <- bind_rows(cv_rows)
oof_df  <- bind_rows(oof)
buck_df <- bind_rows(buck)
nrow(cv); nrow(oof_df)

## ---------------------------------------------------------------------
## [22.8] Read the result
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

## The question that decides the rebuild. norm_ls is a mean model with a
## normal hung on it, which is what script 16 already does. Everything
## else is a candidate replacement for it.
cv_summary %>% group_by(h) %>%
  summarise(best = spec[which.min(rps)],
            best_rps = min(rps),
            gain_vs_norm_ls  = round(100 * (1 - min(rps) /
                                            rps[spec == "norm_ls"]), 1),
            gain_vs_emp_cell = round(100 * (1 - min(rps) /
                                            rps[spec == "emp_cell"]), 1),
            .groups = "drop") %>% as.data.frame()

## Closed-cohort check on the fold totals: predicted and actual totals are
## identical by construction, so any gap is a bug, not a result.
cv %>% group_by(h, spec) %>%
  summarise(pred_total = round(sum(n)), act_total = sum(n),
            .groups = "drop") %>%
  filter(pred_total != act_total) %>% as.data.frame()   # expect zero rows

## Direction. down_ratio near 1 means the method reproduces downward
## category movement, which the frozen-cohort track does not: 46 predicted
## against 87 actual over five years, a ratio near 0.53.
cv %>% group_by(spec) %>%
  summarise(down_ratio = round(sum(pred_down) / sum(act_down), 2),
            up_ratio   = round(sum(pred_up)   / sum(act_up), 2),
            .groups = "drop") %>% arrange(spec) %>% as.data.frame()

## Winner per horizon. 23 fits each horizon separately so they are allowed
## to differ; in practice they should not.
BEST_BY_H <- cv_summary %>% group_by(h) %>%
  summarise(spec = spec[which.min(rps)], .groups = "drop")
as.data.frame(BEST_BY_H)

BEST_SPEC <- BEST_BY_H$spec[BEST_BY_H$h == max(H_SET)]
BEST_SPEC

## ---------------------------------------------------------------------
## [22.9] Diagnostics for the winner
## ---------------------------------------------------------------------

## Calibration, out of fold. Bin on the probability the model gave its OWN
## modal category and check how often that category happened. Binning on
## p_assigned would condition on the outcome and always look good.
## If this sits on the diagonal, the soft counts can be published as they
## stand and the isotonic step in 23 has nothing to fix.
for (hh in H_SET) {
  sp <- BEST_BY_H$spec[BEST_BY_H$h == hh]
  cat("\nh =", hh, " spec =", sp, "\n")
  print(oof_df %>%
    filter(h == hh, spec == sp) %>%
    mutate(bin = cut(p_max, seq(0, 1, 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(n = n(), mean_p = round(mean(p_max), 3),
              realised = round(mean(k_max == cat_act), 3),
              .groups = "drop") %>% as.data.frame())
}

## Hit rate of the modal category -- comparable to the 95.2% two-year
## category hit rate in section 6 of the handoff.
oof_df %>% group_by(h, spec) %>%
  summarise(hit_modal = round(100 * mean(k_max == cat_act), 1),
            mean_pmax = round(mean(p_max), 3), .groups = "drop") %>%
  arrange(h, desc(hit_modal)) %>% as.data.frame()

## Where the count error sits, by category. This is what 24 needs before
## setting the ranking cuts, and what the Diagnostics tab should show: a
## bucket with a systematic bias here produces a systematically wrong
## named list.
buck_df %>%
  inner_join(BEST_BY_H, by = c("h", "spec")) %>%
  group_by(h, cat) %>%
  summarise(soft = round(sum(soft), 1), hard = sum(hard),
            err = round(sum(soft) - sum(hard), 1),
            pct = round(100 * (sum(soft) - sum(hard)) / pmax(sum(hard), 1), 1),
            .groups = "drop") %>% as.data.frame()

## Movement and hit rate by starting category for the winner. A1 and A7
## are where the frozen-cohort track failed, so check them specifically.
oof_df %>%
  inner_join(BEST_BY_H, by = c("h", "spec")) %>%
  group_by(h, cat_k) %>%
  summarise(n = n(),
            act_down = sum(cat_act < cat_k),
            act_up   = sum(cat_act > cat_k),
            hit = round(100 * mean(k_max == cat_act), 1),
            .groups = "drop") %>% as.data.frame()

## ---------------------------------------------------------------------
## [22.10] Exit hazard -- OFF under CLOSED_COHORT
##
## Not published. Run it once per refresh with FIT_EXIT <- TRUE anyway: it
## is the only number that says how large the no-merger assumption is, and
## the Method tab should quote it rather than describing the assumption in
## words. Set CLOSED_COHORT <- FALSE in script 20 to publish exits.
## ---------------------------------------------------------------------

## Defined outside the gate: the saveRDS below stores it either way, and a
## name that exists on only one branch fails the save after the full run.
rhs_exit <- paste("y + d_dn + g12 + g20 + vol + hist_len + cat_f",
                  "+ region + cu_type + acq_cum + shock_now + shock_trail")
exit_cv  <- NULL

if (!FIT_EXIT)
  cat("\n[22.10] skipped -- CLOSED_COHORT is TRUE, total held at",
      nrow(fc_rows), "institutions.\n",
      "        Set FIT_EXIT <- TRUE to measure the assumption.\n")

if (FIT_EXIT) {
  ex_rows <- list()
  for (h in H_SET) {
    us  <- feat[[paste0("usable_h", h)]]
    d_h <- feat[us, ] %>% mutate(ex = .data[[paste0("exit_h", h)]])
    for (f in make_folds(h)) {
      dtr <- d_h[d_h$q_index <= f$train_max_origin, ]
      dte <- d_h[d_h$q_index %in% f$test, ]
      if (nrow(dtr) < 5000 || nrow(dte) < 200) next
      m <- tryCatch(suppressWarnings(
             glm(as.formula(paste("ex ~", rhs_exit)), data = dtr,
                 family = binomial())), error = function(e) NULL)
      if (is.null(m)) next
      p <- predict(m, newdata = dte, type = "response")
      ex_rows[[length(ex_rows) + 1]] <- data.frame(
        h = h, n = nrow(dte), brier = mean((p - dte$ex)^2),
        pred_exits = sum(p), act_exits = sum(dte$ex),
        base_rate = mean(dtr$ex))
    }
  }
  exit_cv <- bind_rows(ex_rows)
  print(exit_cv %>% group_by(h) %>%
          summarise(brier = round(mean(brier), 5),
                    pred = round(sum(pred_exits)), act = sum(act_exits),
                    ratio = round(sum(pred_exits) / sum(act_exits), 2),
                    .groups = "drop") %>% as.data.frame())
}

saveRDS(list(cv = cv, cv_summary = cv_summary, buck = buck_df, oof = oof_df,
             exit_cv = exit_cv, BEST_SPEC = BEST_SPEC, BEST_BY_H = BEST_BY_H,
             CLOSED_COHORT = CLOSED_COHORT, FIT_EXIT = FIT_EXIT,
             RUN_DR = RUN_DR, EMP_SPECS = EMP_SPECS, spec_list = spec_list,
             rhs_for = rhs_for, rhs_full = rhs_full, rhs_exit = rhs_exit,
             key_factory = key_factory, emp_probs = emp_probs,
             norm_ls_probs = norm_ls_probs,
             dr_fit = dr_fit, dr_cdf_matrix = dr_cdf_matrix,
             make_grid = make_grid, cdf_at = cdf_at,
             bucket_probs = bucket_probs, ridge_logit = ridge_logit,
             align_mm = align_mm, rps = rps, make_folds = make_folds,
             MIN_CELL = MIN_CELL, MIN_WIN = MIN_WIN, P_FLOOR = P_FLOOR,
             N_THRESH = N_THRESH, N_ZGRID = N_ZGRID,
             THRESH_LO = THRESH_LO, THRESH_HI = THRESH_HI,
             RIDGE = RIDGE, ETA_CAP = ETA_CAP),
        file = "panel_cv.rds")
