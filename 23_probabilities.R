## =====================================================================
## 23_probabilities.R  --  Final bucket probabilities for the 2026Q2 cohort
##
## Script 22 chose the method. This applies it: fit on the full usable
## history rather than a training fold, and produce P(category k at t+h)
## for every one of the 4,202 institutions, at h = 4, 12, 20.
##
## The winner is emp_cell -- the empirical conditional distribution of
## h-step log growth within the institution's own asset category, read off
## at that institution's own category edges. Written out in full:
##
##     P(cat = j at t+h)  =  F_k( log(edge_j+1) - log(A) )
##                         - F_k( log(edge_j)   - log(A) )
##
## where F_k is the empirical CDF of h-step growth among training credit
## unions in category k. No parameters, no link function, exact tails.
##
## One consequence worth noticing: because the method conditions only on
## category and current assets, the 10 cohort members too young for the
## MIN_HIST feature window are NOT a special case. They have a category
## and they have assets, so they get the same treatment as everyone else.
## The peer-cell fallback I expected to need here is unnecessary.
##
## Run block by block in RStudio.
## =====================================================================

library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

## prep <- readRDS("panel_prep.rds");     list2env(prep, .GlobalEnv)
## fts  <- readRDS("panel_features.rds"); list2env(fts,  .GlobalEnv)
## cvr  <- readRDS("panel_cv.rds");       list2env(cvr,  .GlobalEnv)

## ---------------------------------------------------------------------
## [23.1] Settings
## ---------------------------------------------------------------------

## The winner from [22.8]. emp_cell at every horizon, deliberately, even
## though emp_grow edged it on RPS at h=20 (0.01119 vs 0.01135, a 1.4%
## gap across three folds). emp_grow was WORSE on the two things that get
## published -- count_mae 103.1 against 92.8, down_ratio 0.74 against
## 0.78 -- and one method across all three horizons is a great deal
## easier to put on a Method tab than a split rule. Momentum conditioning
## was tested and did not earn its place; that belongs on Diagnostics.
SPEC <- "emp_cell"

## Recency weighting on the training origins. OFF by default so this
## reproduces exactly what 22 cross-validated.
##
## Why it is here: down_ratio came in at 0.72-0.78 across the empirical
## specs. Much better than the frozen-cohort track's 0.53, but short of 1.
## The pooled 2005-2020 history contains less downward category movement
## than the last five years did. Half-life weighting tilts the pool toward
## recent origins. It is NOT cross-validated -- if you turn it on, re-run
## 22 with the same setting before publishing.
WEIGHTED  <- FALSE
HALFLIFE  <- 24        # quarters

## Scenario. The regime covariates built in [21.4] are unused by emp_cell,
## which conditions only on category. The empirical analogue of a scenario
## is to restrict the training origins: "shock" fits the distribution using
## only origins whose forward window contained a recession or rate shock,
## which is what a 2008- or 2020-style path actually looked like.
## "baseline" uses everything.
SCENARIO <- "baseline"          # "baseline" | "shock" | "calm"

## Optional bucket calibration -- see [23.6]. Off by default.
BUCKET_CALIB <- FALSE

MIN_POOL <- 150        # matches MIN_WIN in 22; below this, widen the pool
P_FLOOR  <- 1e-6

cat("Spec:", SPEC, " weighted:", WEIGHTED, " scenario:", SCENARIO, "\n")

## ---------------------------------------------------------------------
## [23.2] The forecast set -- all 4,202
##
## fc_rows holds the 4,192 cohort members with a full feature window.
## short_cu holds the 10 that are too young. emp_cell needs only category
## and log assets, both of which come straight off the cohort table, so
## the two are simply stacked.
## ---------------------------------------------------------------------
fc <- bind_rows(
  fc_rows %>% select(join_number, cu_name, region, cu_type, state,
                     cat_k, y_raw) %>%
    mutate(region = as.character(region), cu_type = as.character(cu_type),
           short_history = FALSE),
  short_cu %>% transmute(join_number, cu_name,
                         region = as.character(region),
                         cu_type = as.character(cu_type), state,
                         cat_k = cat_k_now, y_raw = log(assets_now),
                         short_history = TRUE))

fc <- fc %>% mutate(assets_now = exp(y_raw))

nrow(fc)                                    # must be 4,202
stopifnot(nrow(fc) == nrow(cohort),
          !any(duplicated(fc$join_number)),
          !anyNA(fc$cat_k), !anyNA(fc$y_raw))

fc %>% count(cat_k) %>% as.data.frame()
sum(fc$short_history)

## ---------------------------------------------------------------------
## [23.3] Training pools
##
## One sorted growth vector per category per horizon, built from every
## origin whose outcome window is fully observed. The CV in 22 thinned and
## capped the training set for speed; there is no reason to here, because
## sorting a vector is free. The final pools are therefore built on MORE
## data than any CV fold saw, which can only help.
## ---------------------------------------------------------------------

## A pool is a sorted growth vector plus its cumulative weight. With no
## weights the cumulative weight is just the ECDF.
make_pool <- function(dy, w = NULL) {
  o  <- order(dy)
  dy <- dy[o]
  cw <- if (is.null(w)) seq_along(dy) / length(dy) else cumsum(w[o]) / sum(w)
  list(dy = dy, cw = c(0, cw), n = length(dy))
}

pool_cdf <- function(pl, z) pl$cw[findInterval(z, pl$dy) + 1]

pool_q <- function(pl, p) {
  j <- findInterval(p, pl$cw)
  pl$dy[pmin(pmax(j, 1), pl$n)]
}

build_pools <- function(h) {
  us  <- feat[[paste0("usable_surv_h", h)]]
  d_h <- feat[us, ]
  d_h$dy <- d_h[[paste0("dy_h", h)]]
  d_h <- d_h[!is.na(d_h$dy), ]

  ## Scenario: restrict the origins the distribution is learned from
  if (SCENARIO != "baseline") {
    sh <- d_h[[paste0("shock_fwd_h", h)]]
    d_h <- if (SCENARIO == "shock") d_h[sh > 0, ] else d_h[sh == 0, ]
  }

  w <- NULL
  if (WEIGHTED) {
    age <- (N_Q - h) - d_h$q_index          # quarters before the last origin
    w   <- 0.5 ^ (pmax(age, 0) / HALFLIFE)
  }

  pools <- list()
  for (k in seq_len(N_CAT)) {
    idx <- which(d_h$cat_k == k)
    ## Thin category (A7 always, sometimes A5): widen to the three-category
    ## window rather than falling to the pooled distribution, which would
    ## hand a $10B credit union the growth distribution of a $5M one.
    if (length(idx) < MIN_POOL) idx <- which(abs(d_h$cat_k - k) <= 1)
    if (length(idx) < MIN_POOL) idx <- seq_len(nrow(d_h))
    pools[[as.character(k)]] <- make_pool(d_h$dy[idx], w[idx])
  }
  attr(pools, "n_origins") <- nrow(d_h)
  pools
}

POOLS <- lapply(H_SET, build_pools)
names(POOLS) <- as.character(H_SET)

## Pool sizes and shape. Check A7 -- it is the category that falls back.
for (h in H_SET) {
  cat("\nh =", h, "  training rows:", attr(POOLS[[as.character(h)]],
                                           "n_origins"), "\n")
  print(data.frame(
    cat = CAT_LABELS,
    n   = sapply(POOLS[[as.character(h)]], function(p) p$n),
    p10 = round(100 * (exp(sapply(POOLS[[as.character(h)]], pool_q, 0.10) *
                           4 / h) - 1), 1),
    med = round(100 * (exp(sapply(POOLS[[as.character(h)]], pool_q, 0.50) *
                           4 / h) - 1), 1),
    p90 = round(100 * (exp(sapply(POOLS[[as.character(h)]], pool_q, 0.90) *
                           4 / h) - 1), 1)))
}

## ---------------------------------------------------------------------
## [23.4] Probabilities
## ---------------------------------------------------------------------
emp_bucket_probs <- function(pools, cat_k, y_raw) {
  P <- matrix(0, length(y_raw), N_CAT,
              dimnames = list(NULL, CAT_LABELS))
  for (k in sort(unique(cat_k))) {
    idx <- which(cat_k == k)
    pl  <- pools[[as.character(k)]]
    Fprev <- rep(0, length(idx))
    for (j in seq_len(N_CAT - 1)) {
      z  <- LOG_EDGE[j + 1] - y_raw[idx]
      Fk <- pmax(pool_cdf(pl, z), Fprev)     # monotone across edges
      P[idx, j] <- Fk - Fprev
      Fprev <- Fk
    }
    P[idx, N_CAT] <- 1 - Fprev
  }
  if (anyNA(P)) stop("emp_bucket_probs: NA produced.")
  P <- pmax(P, P_FLOOR)
  P / rowSums(P)
}

PROB <- lapply(H_SET, function(h)
  emp_bucket_probs(POOLS[[as.character(h)]], fc$cat_k, fc$y_raw))
names(PROB) <- as.character(H_SET)

## ---------------------------------------------------------------------
## [23.5] Guardrails
##
## The analogue of the guardrail block in script 12, and the reason
## script 13's top-down correction is not needed: the reconciliation is
## structural, not something arranged afterwards.
## ---------------------------------------------------------------------
for (h in H_SET) {
  P <- PROB[[as.character(h)]]
  stopifnot(nrow(P) == nrow(fc),
            all(is.finite(P)),
            all(abs(rowSums(P) - 1) < 1e-9),      # every row a distribution
            abs(sum(P) - nrow(fc)) < 1e-6)        # closed cohort, exactly
  cat(sprintf("h=%2d  rows %d  total %.4f  min p %.2e  max p %.4f\n",
              h, nrow(P), sum(P), min(P), max(P)))
}

## Monotone CDF check: cumulative probability must be non-decreasing across
## categories for every institution.
for (h in H_SET) {
  CP <- t(apply(PROB[[as.character(h)]], 1, cumsum))
  stopifnot(all(apply(CP, 1, function(z) all(diff(z) >= -1e-12))))
}
cat("Monotonicity ok.\n")

## ---------------------------------------------------------------------
## [23.6] OPTIONAL bucket calibration
##
## Out of fold, A7 soft counts ran +42%, +55% and +28% against actual at
## the three horizons. That is the SAME failure the frozen-cohort track
## has (+54%), and it is a small-n problem rather than a method problem:
## A7 has 229 institution-quarters in the whole panel, and no category
## above it to absorb upward drift.
##
## This block rescales each category by its out-of-fold bias and then
## rakes the rows back to sum to one, so the closed-cohort total survives.
## It is OFF by default because it is a correction estimated on three
## folds; turn it on only if 25's backtest confirms the same bias.
## ---------------------------------------------------------------------
calib_factors <- buck %>%
  inner_join(BEST_BY_H, by = c("h", "spec")) %>%
  group_by(h, cat) %>%
  summarise(f = sum(hard) / pmax(sum(soft), 1e-9), .groups = "drop")

calib_factors %>%
  mutate(f = round(f, 3)) %>%
  pivot_wider(names_from = h, values_from = f) %>% as.data.frame()

rake <- function(P, f, iters = 50) {
  for (i in seq_len(iters)) {
    P <- sweep(P, 2, f, "*")
    P <- P / rowSums(P)
  }
  P
}

if (BUCKET_CALIB) {
  for (h in H_SET) {
    f <- calib_factors$f[calib_factors$h == h]
    names(f) <- calib_factors$cat[calib_factors$h == h]
    PROB[[as.character(h)]] <- rake(PROB[[as.character(h)]], f[CAT_LABELS])
  }
  cat("Bucket calibration APPLIED.\n")
} else {
  cat("Bucket calibration available but NOT applied (BUCKET_CALIB = FALSE).\n")
}

## ---------------------------------------------------------------------
## [23.7] Institution-level output
##
## What the regional tabs need. The median forecast asset level is the
## institution's own pool median applied to its own assets, and it is what
## 24 ranks on. p10 and p90 give the band; publishing a point without them
## would imply a precision this does not have.
## ---------------------------------------------------------------------
inst <- fc %>% select(join_number, cu_name, region, cu_type, state,
                      assets_now, cat_k, short_history) %>%
  mutate(asset_cat_now = CAT_LABELS[cat_k])

for (h in H_SET) {
  P  <- PROB[[as.character(h)]]
  pl <- POOLS[[as.character(h)]]

  ## One quantile per category, then mapped onto rows. The pool is the
  ## same for every institution in a category, so there is nothing to
  ## compute per row.
  qk <- function(p) vapply(seq_len(N_CAT),
                           function(k) pool_q(pl[[as.character(k)]], p), 0)
  q50 <- qk(0.50)[fc$cat_k]
  q10 <- qk(0.10)[fc$cat_k]
  q90 <- qk(0.90)[fc$cat_k]

  inst[[paste0("assets_med_h", h)]] <- exp(fc$y_raw + q50)
  inst[[paste0("assets_p10_h", h)]] <- exp(fc$y_raw + q10)
  inst[[paste0("assets_p90_h", h)]] <- exp(fc$y_raw + q90)

  inst[[paste0("p_down_h", h)]] <-
    rowSums(P * outer(fc$cat_k, seq_len(N_CAT), ">"))
  inst[[paste0("p_same_h", h)]] <-
    P[cbind(seq_len(nrow(P)), fc$cat_k)]
  inst[[paste0("p_up_h", h)]] <-
    rowSums(P * outer(fc$cat_k, seq_len(N_CAT), "<"))

  inst[[paste0("k_modal_h", h)]] <- max.col(P)
  inst[[paste0("p_modal_h", h)]] <- apply(P, 1, max)
  inst[[paste0("cat_modal_h", h)]] <- CAT_LABELS[max.col(P)]

  ## Confidence band for the institution tabs. Publish this NEXT TO the
  ## assignment, not in a footnote: the aggregate is far more reliable
  ## than any single row, and a supervisor reading one row needs to see
  ## that.
  inst[[paste0("conf_h", h)]] <- cut(
    apply(P, 1, max), c(0, 0.6, 0.8, 1),
    labels = c("weak", "moderate", "strong"), include.lowest = TRUE)
}

## How confident are the institution-level calls, by horizon
for (h in H_SET) {
  cat("\nh =", h, "\n")
  print(inst %>% count(.data[[paste0("conf_h", h)]]) %>%
          mutate(pct = round(100 * n / sum(n), 1)) %>% as.data.frame())
}

## Movement probabilities, cohort-wide
for (h in H_SET)
  cat(sprintf("h=%2d  E[down] %6.1f   E[same] %7.1f   E[up] %6.1f\n", h,
              sum(inst[[paste0("p_down_h", h)]]),
              sum(inst[[paste0("p_same_h", h)]]),
              sum(inst[[paste0("p_up_h", h)]])))

## ---------------------------------------------------------------------
## [23.8] Aggregate counts and transition matrices
##
## The count is the sum of probabilities, not a count of modal winners.
## Summing 4,202 probabilities is what makes the total tie exactly and
## what stops the middle categories being over-counted.
## ---------------------------------------------------------------------
counts <- data.frame(cat = CAT_LABELS, pretty = CAT_PRETTY[CAT_LABELS],
                     now = as.numeric(table(factor(fc$cat_k,
                                                   levels = 1:N_CAT))))
for (h in H_SET)
  counts[[paste0("h", h)]] <- round(colSums(PROB[[as.character(h)]]), 1)

counts %>%
  mutate(chg_5y = round(h20 - now, 1),
         pct_5y = round(100 * (h20 - now) / pmax(now, 1), 1)) %>%
  as.data.frame()

colSums(counts[, paste0("h", H_SET)])       # each must be 4,202

## Transition matrices. NOT a fitted object -- the average probability
## vector of the institutions starting in each row. That is why it can be
## cut by region and charter without fitting anything separately, and why
## T20 is not T4 cubed.
trans <- function(h) {
  P <- PROB[[as.character(h)]]
  m <- t(sapply(seq_len(N_CAT), function(k) {
    idx <- which(fc$cat_k == k)
    if (!length(idx)) return(rep(NA_real_, N_CAT))
    colMeans(P[idx, , drop = FALSE])
  }))
  dimnames(m) <- list(CAT_LABELS, CAT_LABELS)
  m
}

TRANS <- lapply(H_SET, trans); names(TRANS) <- as.character(H_SET)

for (h in H_SET) {
  cat("\n--- Transition matrix, h =", h, "(%) ---\n")
  print(round(100 * TRANS[[as.character(h)]], 1))
}

## Region x charter counts, for the six regional tabs. A plain loop:
## six cells, three horizons, seven categories.
cell_rows <- list()
cells <- fc %>% distinct(region, cu_type) %>% arrange(region, cu_type)

for (h in H_SET) {
  P <- PROB[[as.character(h)]]
  for (i in seq_len(nrow(cells))) {
    sel <- fc$region == cells$region[i] & fc$cu_type == cells$cu_type[i]
    cell_rows[[length(cell_rows) + 1]] <- data.frame(
      h = h, region = cells$region[i], cu_type = cells$cu_type[i],
      n_now = sum(sel), cat = CAT_LABELS,
      now = as.numeric(table(factor(fc$cat_k[sel], levels = 1:N_CAT))),
      fcst = round(colSums(P[sel, , drop = FALSE]), 1))
  }
}
cell_counts <- bind_rows(cell_rows)

## Each cell must still tie to its own institution count
cell_counts %>% group_by(h, region, cu_type) %>%
  summarise(n_now = first(n_now), fcst_total = round(sum(fcst), 1),
            .groups = "drop") %>%
  filter(abs(n_now - fcst_total) > 0.05) %>% as.data.frame()   # expect none

cell_counts %>% filter(h == 20) %>%
  select(region, cu_type, cat, now, fcst) %>%
  pivot_wider(names_from = cat, values_from = c(now, fcst)) %>%
  as.data.frame()

## ---------------------------------------------------------------------
## [23.9] Sanity against the frozen-cohort track
##
## Not a validation -- 25 does that -- but the first thing anyone will
## ask is how the two sets of numbers differ, so look now.
## ---------------------------------------------------------------------
cat("\nFive-year change by category, this method:\n")
print(counts %>% transmute(cat, now, h20, chg = round(h20 - now, 1),
                           pct = round(100 * (h20 - now) / pmax(now, 1), 1)) %>%
        as.data.frame())

cat("\nA7 check. Out of fold this category ran +28% to +55% high, and the\n",
    "frozen-cohort track runs +54%. It is 13 institutions and there is no\n",
    "category above it to absorb drift -- 24 handles it explicitly.\n")
print(inst %>% filter(cat_k == 7) %>%
        transmute(cu_name, assets_now = round(assets_now / 1e9, 2),
                  p_down_h20 = round(p_down_h20, 3),
                  p_same_h20 = round(p_same_h20, 3)) %>% as.data.frame())

## Institutions most likely to change category over five years -- the
## review queue, and a useful spot-check that the probabilities behave
## sensibly for institutions sitting near an edge.
inst %>%
  mutate(p_move = 1 - p_same_h20) %>%
  arrange(desc(p_move)) %>%
  transmute(cu_name, asset_cat_now,
            assets_now = round(assets_now / 1e6, 1),
            cat_modal_h20, p_move = round(p_move, 3)) %>%
  head(20) %>% as.data.frame()

saveRDS(list(fc = fc, inst = inst, PROB = PROB, POOLS = POOLS,
             TRANS = TRANS, counts = counts, cell_counts = cell_counts,
             calib_factors = calib_factors,
             SPEC = SPEC, WEIGHTED = WEIGHTED, HALFLIFE = HALFLIFE,
             SCENARIO = SCENARIO, BUCKET_CALIB = BUCKET_CALIB,
             MIN_POOL = MIN_POOL, P_FLOOR = P_FLOOR,
             make_pool = make_pool, pool_cdf = pool_cdf, pool_q = pool_q,
             build_pools = build_pools,
             emp_bucket_probs = emp_bucket_probs, rake = rake),
        file = "panel_probs.rds")
