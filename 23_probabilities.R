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

## ---------------------------------------------------------------------
## GROWTH BASIS -- which years the growth distribution is learned from
##
## This is the single most consequential choice in the script and it must
## be stated on the Method tab, with the alternatives shown. It is a
## judgement about which period represents the next five years, not a
## finding.
##
##   "full"       every usable origin. Includes the 2020-21 deposit surge,
##                which raises measured growth in every category.
##   "pre2015"    origins through 2014 only. A full five-year window from
##                a period containing neither the 2008 aftermath nor the
##                pandemic surge. The least tunable of the three -- a date
##                restriction anyone can check.
##   "post_surge" shifts each category's distribution by the gap between
##                its post-2022 growth (measured at h=12, where clean
##                windows exist) and its full-history growth. Reflects the
##                deposit runoff and rate shock, which is not a neutral
##                period either -- expect this to under-state growth as
##                much as "full" over-states it.
##
## APPLIED TO EVERY CATEGORY, not just A6. Adjusting one category because
## its answer is inconvenient and leaving the rest alone is not a growth
## assumption, it is a thumb on one number. If post-surge growth is the
## right basis it is the right basis for A1 as well, and the counts below
## will move accordingly.
##
## A6 median annual growth under each, for reference:
##   full 7.91%   pre2015 6.67%   post_surge 4.21%
GROWTH_BASIS <- "full"          # "full" | "pre2015" | "post_surge"
PRE_CUTOFF   <- 2014             # last origin year for "pre2015"
RECENT_FROM  <- 2022             # first origin year counted as post-surge

## Scenario. The regime covariates built in [21.4] are unused by emp_cell,
## which conditions only on category. The empirical analogue of a scenario
## is to restrict the training origins: "shock" fits the distribution using
## only origins whose forward window contained a recession or rate shock,
## which is what a 2008- or 2020-style path actually looked like.
## "baseline" uses everything.
SCENARIO <- "baseline"          # "baseline" | "shock" | "calm"

## ---------------------------------------------------------------------
## REAL TERMS -- the published basis
##
## The category edges are fixed nominal dollars that have never been
## indexed, so a forecast on nominal edges mixes two things: credit unions
## growing, and the definition of each size band eroding. Holding the edges
## constant in REAL terms separates them and is the more meaningful
## economic statement.
##
## What this does: the edges are inflated at CPI_ASSUMPTION a year, which
## is arithmetically the same as deflating each institution's forecast
## assets to 2026 dollars. Category membership is then "large relative to
## the 2026 economy" rather than "above a number set at some past date".
##
## TWO THINGS FOR THE METHOD TAB.
##
## 1. The $10B line is STATUTORY and not indexed -- CFPB supervisory
##    authority and stress-testing tiers attach to the nominal figure. For
##    a supervisory planning question the nominal count is the relevant
##    one. Both are produced below; which one leads is an editorial
##    decision and should be stated as one.
##
## 2. The deflation is approximate. The growth pools are NOMINAL log
##    growth drawn from a period averaging roughly 2.5% inflation, so
##    deflating at 2.5% recovers real growth on average. Under
##    GROWTH_BASIS = "post_surge" the pools come from 2022-24 windows,
##    where inflation ran well above that, so this UNDER-corrects. The
##    clean version deflates each historical window by its realised CPI
##    before the pools are built, which needs a CPI series this pipeline
##    does not currently load.
## THREE INTERNALLY CONSISTENT CONFIGURATIONS. Pick one.
##
##   "nominal"     nominal growth pools, fixed statutory edges.
##                 Answers: how many cross the $10B line as written.
##
##   "real_approx" nominal growth pools, edges indexed forward at
##                 CPI_ASSUMPTION. Approximate, because the pools embed
##                 whatever inflation their own windows contained. Fine
##                 when the pools span a long period averaging near the
##                 assumption; WRONG under GROWTH_BASIS = "post_surge",
##                 whose windows carry 4-8% inflation.
##
##   "real_exact"  each historical window's growth is deflated by its OWN
##                 realised CPI, then compared against unindexed edges.
##                 Both sides in the same dollars. This is the correct
##                 real-terms calculation and the one to publish.
##
## DO NOT deflate history and index the edges as well. They are the same
## correction and applying both double-counts it. The guard below enforces
## this rather than trusting anyone to remember.
PRICE_BASIS    <- "nominal"       # "nominal" | "real_approx" | "real_exact"
CPI_ASSUMPTION <- 0.025           # forward rate, used by "real_approx" only

REAL_TERMS <- PRICE_BASIS != "nominal"   # kept for downstream references

## ---------------------------------------------------------------------
## CPI-U, annual averages, 1982-84 = 100.
##
## VERIFY 2025 AND 2026 AGAINST BLS BEFORE PUBLISHING. Those two are
## estimates; every year through 2024 is the published annual average.
## The 2026 value in particular sets the base year for every deflated
## figure in the workbook.
## ---------------------------------------------------------------------
CPI <- c(`2005` = 195.300, `2006` = 201.600, `2007` = 207.342,
         `2008` = 215.303, `2009` = 214.537, `2010` = 218.056,
         `2011` = 224.939, `2012` = 229.594, `2013` = 232.957,
         `2014` = 236.736, `2015` = 237.017, `2016` = 240.007,
         `2017` = 245.120, `2018` = 251.107, `2019` = 255.657,
         `2020` = 258.811, `2021` = 270.970, `2022` = 292.655,
         `2023` = 304.702, `2024` = 313.689,
         `2025` = 321.000,                    # ESTIMATE -- verify
         `2026` = 328.000)                    # ESTIMATE -- verify

## Quarterly log CPI on the panel's own index. Annual averages are centred
## at mid-year, so they are placed at year + 0.5 and interpolated to
## quarter midpoints.
q_time <- START_YEAR + (seq_len(N_Q) - 1) %/% 4 +
          ((seq_len(N_Q) - 1) %% 4 + 0.5) / 4
LOGCPI <- approx(as.numeric(names(CPI)) + 0.5, log(CPI),
                 xout = q_time, rule = 2)$y

## Realised inflation over the last five years, as a sanity check on the
## series and as the number the Method tab should quote.
cat(sprintf("CPI check: %.1f%% a year over the last 5 years\n",
            100 * (exp((LOGCPI[N_Q] - LOGCPI[N_Q - 20]) / 5) - 1)))

## Optional bucket calibration -- see [23.6]. Off by default.
BUCKET_CALIB <- FALSE

## Minimum pool size before falling back to a wider one. Lowered from 150
## after the first run of [23.3]: at 150, A7 fell back to the
## three-category window at h=12 and h=20 and drew on 15,515 and 12,250
## rows -- more than A6 has of its own. A7's forecast was therefore A6's
## growth distribution, and it showed: p_down at five years came out at
## 0.8% when 5 of 19 A7 origins in the panel actually moved down.
##
## 60 was still not low enough: A7 kept falling back at h=20, where its own
## usable count is under 60 because $10B credit unions barely existed
## before 2015 and each origin needs five years of forward data. At 20 it
## uses its own data at every horizon. That is thin for an empirical CDF
## and the probabilities visibly quantise at roughly 5% -- note that on the
## Diagnostics tab -- but the alternative is not "a noisier A7 estimate",
## it is "an A7 estimate of something else". A thin pool of the right
## institutions beats a thick pool of the wrong ones. The printout below
## shows which categories used their own data.
MIN_POOL <- 20

## Below this many observations, institution-level probabilities are
## flagged as indicative on the tabs. This does NOT change any number --
## the counts are unaffected -- it only labels rows whose precision the
## data cannot support. A7 at h=20 sits here by construction.
THIN_POOL <- 100
P_FLOOR  <- 1e-6

cat("Spec:", SPEC, " growth:", GROWTH_BASIS, " prices:", PRICE_BASIS,
    " weighted:", WEIGHTED, " scenario:", SCENARIO, "\n")

stopifnot(PRICE_BASIS %in% c("nominal", "real_approx", "real_exact"))

## Combining a post-surge growth basis with a real price basis strips the
## same price effect twice: the 2022-24 windows are low-growth largely
## BECAUSE inflation was high, and deflating them removes that again. The
## run will complete, but the resulting forecast is of sustained real
## contraction across the industry and should not be published without
## that being the headline.
if (PRICE_BASIS != "nominal" && GROWTH_BASIS == "post_surge")
  warning("post_surge growth with a real price basis double-corrects for ",
          "inflation. Use GROWTH_BASIS = 'full' or 'pre2015' with real ",
          "prices, or keep post_surge and PRICE_BASIS = 'nominal'.",
          call. = FALSE, immediate. = TRUE)

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

  ## REAL GROWTH: subtract the inflation each window actually experienced,
  ## not an average. A 2018Q1 origin's five-year window covers 2018-2023
  ## and carries roughly 20% cumulative CPI; a 2010Q1 origin's carries
  ## about 8%. Using a flat deflator would leave the difference in the
  ## "real" figures, which is exactly the error the approximate version
  ## makes under a post-surge growth basis.
  if (PRICE_BASIS == "real_exact")
    d_h$dy <- d_h$dy - (LOGCPI[pmin(d_h$q_index + h, N_Q)] -
                        LOGCPI[d_h$q_index])

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
  src   <- character(N_CAT)
  for (k in seq_len(N_CAT)) {
    idx <- which(d_h$cat_k == k)
    src[k] <- "own"
    ## Widen to the three-category window rather than falling to the pooled
    ## distribution, which would hand a $10B credit union the growth
    ## distribution of a $5M one. Which categories take this branch is
    ## printed below -- it changes what the forecast means for them.
    if (length(idx) < MIN_POOL) {
      idx <- which(abs(d_h$cat_k - k) <= 1); src[k] <- "window"
    }
    if (length(idx) < MIN_POOL) {
      idx <- seq_len(nrow(d_h)); src[k] <- "pooled"
    }
    pools[[as.character(k)]] <- make_pool(d_h$dy[idx], w[idx])
  }
  attr(pools, "n_origins") <- nrow(d_h)
  attr(pools, "source")    <- src
  pools
}

POOLS <- lapply(H_SET, build_pools)
names(POOLS) <- as.character(H_SET)

## ---------------------------------------------------------------------
## [23.3b] Apply the growth basis
##
## "pre2015" rebuilds the pools from the restricted origin set. Nothing
## else changes, so the fallback logic and MIN_POOL still apply.
##
## "post_surge" cannot rebuild: at h=20 the last usable origin is 2021Q2,
## so no five-year window lies entirely after the surge -- five years have
## not passed. The distribution is therefore SHIFTED rather than re-fitted,
## by a per-category delta measured at h=12 where clean windows do exist.
## The shape of each category's growth distribution is kept; only its
## location moves. That is an assumption and it is stated as one.
## ---------------------------------------------------------------------
origin_year <- function(qi) START_YEAR + (qi - 1) %/% 4

growth_delta <- function() {
  ## per-quarter log-growth gap, by category, measured at h = 12
  us  <- feat[["usable_surv_h12"]]
  d   <- feat[us, ]; d$dy <- d[["dy_h12"]]
  if (PRICE_BASIS == "real_exact")
    d$dy <- d$dy - (LOGCPI[pmin(d$q_index + 12, N_Q)] - LOGCPI[d$q_index])
  d   <- d[!is.na(d$dy), ]
  d$yr <- origin_year(d$q_index)

  vapply(seq_len(N_CAT), function(k) {
    all_k <- d$dy[d$cat_k == k]
    rec_k <- d$dy[d$cat_k == k & d$yr >= RECENT_FROM]
    if (length(rec_k) < 100) return(0)          # too thin to shift on
    (median(rec_k) - median(all_k)) / 12        # per quarter
  }, 0)
}

if (GROWTH_BASIS == "pre2015") {
  build_pools_pre <- function(h) {
    keep <- origin_year(feat$q_index) <= PRE_CUTOFF
    old_feat <- feat
    assign("feat", feat[keep, ], envir = .GlobalEnv)
    out <- build_pools(h)
    assign("feat", old_feat, envir = .GlobalEnv)
    out
  }
  POOLS <- lapply(H_SET, build_pools_pre)
  names(POOLS) <- as.character(H_SET)
  DELTA <- rep(0, N_CAT)
}

if (GROWTH_BASIS == "post_surge") {
  DELTA <- growth_delta()
  for (h in H_SET)
    for (k in seq_len(N_CAT))
      POOLS[[as.character(h)]][[as.character(k)]]$dy <-
        POOLS[[as.character(h)]][[as.character(k)]]$dy + DELTA[k] * h
}

if (GROWTH_BASIS == "full") DELTA <- rep(0, N_CAT)

## What the basis did, in annual growth terms. Every category should move,
## and if one of them does not, say why on the Method tab.
cat("
Growth basis:", GROWTH_BASIS, "
")
print(data.frame(
  cat = CAT_LABELS,
  shift_ann_pp = round(100 * (exp(DELTA * 4) - 1), 2),
  med_ann_h20  = round(100 * (exp(sapply(POOLS[["20"]], pool_q, 0.50) *
                                  4 / 20) - 1), 2)))

## Pool sizes, source and shape. `src` is the thing to read first: any
## category showing "window" is being forecast with its neighbours' growth
## distribution, not its own, and its numbers should be read accordingly.
## p_neg is the share of the pool with negative growth -- the raw material
## for every down-move the method can produce.
for (h in H_SET) {
  pl <- POOLS[[as.character(h)]]
  cat("\nh =", h, "  training rows:", attr(pl, "n_origins"), "\n")
  print(data.frame(
    cat   = CAT_LABELS,
    src   = attr(pl, "source"),
    n     = sapply(pl, function(p) p$n),
    ## Coarsest probability the pool can express. An empirical CDF built on
    ## n observations can only take values in multiples of 1/n, so a pool
    ## of 21 produces probabilities in steps of 4.8% -- two institutions
    ## get identical figures and several get exactly zero. The count is
    ## still right; the institution-level precision is not.
    grain = round(100 / sapply(pl, function(p) p$n), 1),
    p_neg = round(100 * sapply(pl, function(p) mean(p$dy < 0)), 1),
    p10   = round(100 * (exp(sapply(pl, pool_q, 0.10) * 4 / h) - 1), 1),
    med   = round(100 * (exp(sapply(pl, pool_q, 0.50) * 4 / h) - 1), 1),
    p90   = round(100 * (exp(sapply(pl, pool_q, 0.90) * 4 / h) - 1), 1)))
}

## A7 specifically: its own data against the window it would otherwise
## borrow. If these two rows look alike the fallback was harmless; if they
## do not, MIN_POOL is doing real work.
for (h in H_SET) {
  us  <- feat[[paste0("usable_surv_h", h)]]
  d_h <- feat[us, ]; d_h$dy <- d_h[[paste0("dy_h", h)]]
  d_h <- d_h[!is.na(d_h$dy), ]
  own <- d_h$dy[d_h$cat_k == 7]
  win <- d_h$dy[abs(d_h$cat_k - 7) <= 1]
  cat(sprintf("h=%2d  A7 own  n=%5d  p_neg %4.1f%%  med %5.1f%%\n",
              h, length(own), 100 * mean(own < 0),
              100 * (exp(median(own) * 4 / h) - 1)))
  cat(sprintf("      A6+A7   n=%5d  p_neg %4.1f%%  med %5.1f%%\n",
              length(win), 100 * mean(win < 0),
              100 * (exp(median(win) * 4 / h) - 1)))
}

## ---------------------------------------------------------------------
## [23.4] Probabilities
## ---------------------------------------------------------------------
## `edges` defaults to the nominal boundaries. Passing inflated edges is
## how the real-terms version is produced -- an explicit argument rather
## than reassigning LOG_EDGE in the global environment, which the earlier
## sensitivity block did and which is far too easy to leave switched on.
emp_bucket_probs <- function(pools, cat_k, y_raw, edges = LOG_EDGE) {
  P <- matrix(0, length(y_raw), N_CAT,
              dimnames = list(NULL, CAT_LABELS))
  for (k in sort(unique(cat_k))) {
    idx <- which(cat_k == k)
    pl  <- pools[[as.character(k)]]
    Fprev <- rep(0, length(idx))
    for (j in seq_len(N_CAT - 1)) {
      z  <- edges[j + 1] - y_raw[idx]
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

## Edges for each horizon. Real terms inflates them by CPI over the
## horizon; nominal leaves them alone.
real_edges <- function(h) {
  e <- LOG_EDGE + log((1 + CPI_ASSUMPTION) ^ (h / 4))
  e[1] <- -Inf; e[N_CAT + 1] <- Inf
  e
}
## Edges are indexed only under "real_approx". Under "real_exact" the
## deflation has already happened on the growth side, so the edges stay
## where the statute put them.
EDGES <- lapply(H_SET, function(h)
  if (PRICE_BASIS == "real_approx") real_edges(h) else LOG_EDGE)
names(EDGES) <- as.character(H_SET)

## The alternate basis for the sensitivity row: whichever of nominal /
## real the published run is not.
EDGES_ALT <- lapply(H_SET, function(h)
  if (PRICE_BASIS == "real_approx") LOG_EDGE else real_edges(h))
names(EDGES_ALT) <- as.character(H_SET)

## Both bases are always computed. PROB is the published one; PROB_ALT is
## the other, kept for the sensitivity row on the Method tab.
PROB <- lapply(H_SET, function(h)
  emp_bucket_probs(POOLS[[as.character(h)]], fc$cat_k, fc$y_raw,
                   EDGES[[as.character(h)]]))
names(PROB) <- as.character(H_SET)

PROB_ALT <- lapply(H_SET, function(h)
  emp_bucket_probs(POOLS[[as.character(h)]], fc$cat_k, fc$y_raw,
                   EDGES_ALT[[as.character(h)]]))
names(PROB_ALT) <- as.character(H_SET)

cat("\nPublished basis:", PRICE_BASIS, " growth basis:", GROWTH_BASIS, "\n")

## Real median annual growth by category, five-year horizon. Under
## "real_exact" these are REAL rates and should be read as such: roughly
## 3-5% for the larger categories on full history. If any of them is
## negative, the run is projecting sustained real contraction for that
## category and that needs saying out loud, not burying in a count.
cat("\nMedian annual growth by category (h=20), on the published basis:\n")
print(data.frame(
  cat = CAT_LABELS,
  med_ann = round(100 * (exp(sapply(POOLS[["20"]], pool_q, 0.50) * 4/20) - 1), 2)))

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
## Guard. In a session that has just run 22, `buck` may still be the list
## accumulator rather than the bound data frame. Same class of collision as
## feat / feat_s at [22.0]; repair it rather than making the error a puzzle.
if (!is.data.frame(buck)) {
  if (exists("buck_df") && is.data.frame(buck_df)) {
    message("[23.6] buck was a list -- using buck_df instead.")
    buck <- buck_df
  } else {
    buck <- dplyr::bind_rows(buck)
  }
}
stopifnot(is.data.frame(buck), is.data.frame(BEST_BY_H))

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

  ## Under REAL_TERMS these are stated in 2026 dollars, so they are
  ## directly comparable with assets_now and with the (unindexed) edges.
  ## "real_exact" already deflated the growth, so the level is in 2026
  ## dollars with no further adjustment. "real_approx" deflates here.
  defl <- if (PRICE_BASIS == "real_approx") (1 + CPI_ASSUMPTION)^(h/4) else 1
  inst[[paste0("assets_med_h", h)]] <- exp(fc$y_raw + q50) / defl
  inst[[paste0("assets_p10_h", h)]] <- exp(fc$y_raw + q10) / defl
  inst[[paste0("assets_p90_h", h)]] <- exp(fc$y_raw + q90) / defl

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

  ## How many observations sit behind THIS institution's probabilities, and
  ## the resulting granularity. A7 at h=20 has about 20, so its rows carry
  ## probabilities in ~5% steps. The tab must say so: a supervisor reading
  ## "0.000" for their credit union should understand that means "none of
  ## the twenty comparable historical cases went that way", not "we have
  ## established this cannot happen".
  n_by_cat <- sapply(pl, function(p) p$n)
  inst[[paste0("pool_n_h", h)]]  <- n_by_cat[fc$cat_k]
  inst[[paste0("grain_h", h)]]   <- round(1 / n_by_cat[fc$cat_k], 4)
  inst[[paste0("thin_h", h)]]    <- n_by_cat[fc$cat_k] < THIN_POOL
}

## Which institutions carry thin-pool probabilities, and how coarse
cat("\nThin-pool rows (fewer than", THIN_POOL, "historical observations):\n")
for (h in H_SET) {
  th <- inst[[paste0("thin_h", h)]]
  if (!any(th)) { cat(sprintf("h=%2d  none\n", h)); next }
  cat(sprintf("h=%2d  %d rows in %s   pool n = %d   steps of %.1f%%\n",
              h, sum(th),
              paste(unique(inst$asset_cat_now[th]), collapse = ", "),
              min(inst[[paste0("pool_n_h", h)]][th]),
              100 * max(inst[[paste0("grain_h", h)]][th])))
}
cat("Counts are unaffected. Flag these rows as indicative on the tabs.\n")

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
for (h in H_SET) {
  counts[[paste0("h", h)]] <- round(colSums(PROB[[as.character(h)]]), 1)
  counts[[paste0("alt_h", h)]] <- round(colSums(PROB_ALT[[as.character(h)]]), 1)
}

## THE PUBLISHED TABLE -- real terms at 1, 3 and 5 years.
lab_pub <- if (REAL_TERMS) "real" else "nominal"
lab_alt <- if (REAL_TERMS) "nominal" else "real"

pub_table <- counts %>%
  transmute(Category = pretty, Today = now,
            `1 year` = h4, `3 years` = h12, `5 years` = h20,
            `Change` = round(h20 - now, 1),
            `Pct` = round(100 * (h20 - now) / pmax(now, 1), 1))
cat("\n=== PUBLISHED (", lab_pub, " terms) ===\n", sep = "")
print(as.data.frame(pub_table))

alt_table <- counts %>%
  transmute(Category = pretty, Today = now,
            `1 year` = alt_h4, `3 years` = alt_h12, `5 years` = alt_h20,
            `Change` = round(alt_h20 - now, 1))
cat("\n=== SENSITIVITY (", lab_alt, " terms) ===\n", sep = "")
print(as.data.frame(alt_table))

## Side by side at five years, with the split
cat("\n=== how much of the five-year change is threshold drift ===\n")
print(counts %>%
  transmute(Category = pretty, Today = now,
            real  = if (REAL_TERMS) h20 else alt_h20,
            nominal = if (REAL_TERMS) alt_h20 else h20) %>%
  mutate(real_chg = round(real - Today, 1),
         nominal_chg = round(nominal - Today, 1),
         threshold_drift = round(nominal - real, 1)) %>%
  as.data.frame())

## Both bases must still tie to the cohort at every horizon
for (h in H_SET)
  stopifnot(abs(sum(PROB[[as.character(h)]]) - nrow(fc)) < 1e-6,
            abs(sum(PROB_ALT[[as.character(h)]]) - nrow(fc)) < 1e-6)
cat("\nBoth bases tie to", nrow(fc), "at every horizon.\n")

counts %>%
  mutate(chg_5y = round(h20 - now, 1),
         pct_5y = round(100 * (h20 - now) / pmax(now, 1), 1)) %>%
  as.data.frame()

colSums(counts[, paste0("h", H_SET)])       # each must be 4,202

## ---------------------------------------------------------------------
## [23.8b] Supplementary thresholds
##
## Examination staff work to cut-offs that are not category edges. The
## probability of clearing any dollar level falls straight out of the same
## fitted CDF -- no extra model, no re-fitting -- because the method
## produces a distribution per institution rather than a category label.
##
## These are counts ABOVE a level, not a new category, so they overlap the
## table above: every $15B institution is also counted in A7. Present them
## as a supplementary line, never as another row of the main table, or the
## total stops adding to 4,202.
##
## THE CAVEAT IS SHARPER HERE THAN ANYWHERE ELSE. A $15B threshold sits in
## the far right tail of a pool of about 21 observations, so the estimate
## rests on a handful of historical cases and moves in coarse steps. It is
## a reasonable order of magnitude and not a precise figure.
## ---------------------------------------------------------------------
EXTRA_THRESHOLDS <- c(15e9, 20e9)

p_above <- function(h, level) {
  pl <- POOLS[[as.character(h)]]
  z  <- log(level) - fc$y_raw
  vapply(seq_len(nrow(fc)), function(i)
    1 - pool_cdf(pl[[as.character(fc$cat_k[i])]], z[i]), 0)
}

extra <- lapply(EXTRA_THRESHOLDS, function(L) {
  data.frame(threshold = L,
             label = paste0("$", format(L / 1e9, trim = TRUE), "B and over"),
             now = sum(fc$assets_now >= L),
             h4  = round(sum(p_above(4,  L)), 1),
             h12 = round(sum(p_above(12, L)), 1),
             h20 = round(sum(p_above(20, L)), 1))
})
extra <- bind_rows(extra)

cat("\n=== SUPPLEMENTARY: counts above additional thresholds ===\n")
cat("Overlaps the table above -- these institutions are also counted in\n")
cat("A7. Not a partition; do not add to the total.\n\n")
print(as.data.frame(extra))

## Must be nested: $20B+ <= $15B+ <= A7 count, at every horizon
for (h in H_SET) {
  col <- paste0("h", h)
  stopifnot(all(diff(rev(extra[[col]])) >= -1e-9),
            max(extra[[col]]) <= counts[[col]][N_CAT] + 1e-9)
}
cat("\nNesting check passed: $20B+ <= $15B+ <= $10B+ at every horizon.\n")

## Who they are, at five years. The named list matters more than the count
## for examination planning.
above15 <- fc %>%
  mutate(p15 = p_above(20, 15e9),
         med20 = inst$assets_med_h20) %>%
  filter(p15 > 0.05) %>%
  arrange(desc(p15)) %>%
  transmute(cu_name, region, cu_type,
            assets_now_b = round(assets_now / 1e9, 2),
            med_h20_b = round(med20 / 1e9, 2),
            p_above_15B = round(p15, 3),
            pool_n = inst$pool_n_h20[match(join_number, inst$join_number)])

cat("\nInstitutions with a 5% or better chance of exceeding $15B by",
    qgrid$q_label[N_Q + 20], ":\n")
print(as.data.frame(above15))

cat("\nRead p_above_15B as indicative. It comes from the right tail of a\n",
    "pool of", POOLS[["20"]][["7"]]$n, "to", POOLS[["20"]][["6"]]$n,
    "observations depending on the institution's category.\n")

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

## A5 at five years is close to a coin flip -- roughly half stay, half move
## up to A6. That is arithmetic rather than instability, and it should be
## explained on the Method tab before someone reads it as a model failure:
## the $500M-$1B band is 0.69 log units wide, median A5 growth compounds to
## about half that over twenty quarters, so about half the band crosses.
cat(sprintf("\nA5 band width %.2f log units; median A5 5y growth %.2f log units\n",
            LOG_EDGE[6] - LOG_EDGE[5],
            pool_q(POOLS[["20"]][["5"]], 0.50)))

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
      ## Keep the unrounded value for the tie-out; round only for display.
      ## Summing seven values each rounded to 0.1 can drift by up to 0.35,
      ## which is what made the first run report six false failures here.
      fcst_exact = colSums(P[sel, , drop = FALSE]))
  }
}
cell_counts <- bind_rows(cell_rows) %>% mutate(fcst = round(fcst_exact, 1))

## Each cell must still tie to its own institution count, EXACTLY, on the
## unrounded probabilities.
cell_counts %>% group_by(h, region, cu_type) %>%
  summarise(n_now = first(n_now), fcst_total = sum(fcst_exact),
            .groups = "drop") %>%
  filter(abs(n_now - fcst_total) > 1e-6) %>% as.data.frame()   # expect none

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

## ---- nominal drift: the single biggest driver of these numbers --------
##
## Every category below $100M shrinks and everything above grows. The
## mechanism is not a model result, it is arithmetic: the pool medians
## above run roughly 1% to 8% a year, the category edges are fixed nominal
## dollars, and the industry climbs the ladder past them. The frozen-cohort
## track has exactly the same property.
##
## This has to go on the Method tab. "$10B credit unions grow 263% in five
## years" is the sentence the field team will react to, and most of it is
## the denominator rather than the numerator.
cat("\nNominal drift check\n")
cat(sprintf("  E[up]   %7.1f\n  E[down] %7.1f\n  ratio   %7.1f : 1\n",
            sum(inst$p_up_h20), sum(inst$p_down_h20),
            sum(inst$p_up_h20) / pmax(sum(inst$p_down_h20), 1e-9)))

## Real-dollar sensitivity. Reruns the same method with the edges inflated
## at CPI_ASSUMPTION a year, which is what the counts would look like if
## the thresholds kept pace with prices. The gap between the two columns is
## how much of the published movement is nominal.
CPI_ASSUMPTION <- 0.025

## Superseded by [23.4], which computes both bases with an explicit edges
## argument. Kept as a named object because 27 and the Method tab refer to
## it, but it is now just a view on what is already computed.
real_counts <- lapply(H_SET, function(h)
  colSums(if (REAL_TERMS) PROB[[as.character(h)]]
          else PROB_ALT[[as.character(h)]]))
names(real_counts) <- paste0("real_h", H_SET)

## Built from the two bases computed at [23.4], never from real_counts,
## which under a real PRICE_BASIS is the same object as counts$h20 and
## produced a table comparing the run against itself.
nominal_vs_real <- counts %>%
  transmute(cat = pretty, now,
            nominal_h20 = if (PRICE_BASIS == "nominal") h20 else alt_h20,
            real_h20    = if (PRICE_BASIS == "nominal") alt_h20 else h20) %>%
  mutate(nominal_chg = round(nominal_h20 - now, 1),
         real_chg    = round(real_h20 - now, 1),
         threshold_drift = round(nominal_h20 - real_h20, 1))

cat("\nNominal vs inflation-indexed edges at", 100 * CPI_ASSUMPTION,
    "% a year:\n")
print(as.data.frame(nominal_vs_real))

cat("\nA7 check. Out of fold this category ran +28% to +55% high, and the\n",
    "frozen-cohort track runs +54%. It is 13 institutions and there is no\n",
    "category above it to absorb drift -- 24 handles it explicitly.\n")
cat("A7 probabilities come from",
    POOLS[["20"]][["7"]]$n, "historical observations, so they move in steps\n",
    "of about", round(100 / POOLS[["20"]][["7"]]$n, 1),
    "percentage points. Repeated values and exact zeros in the\n",
    "column below are that granularity, not a finding. Label the tab.\n")
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
             extra = extra, above15 = above15,
             EXTRA_THRESHOLDS = EXTRA_THRESHOLDS, p_above = p_above,
             nominal_vs_real = nominal_vs_real, real_counts = real_counts,
             CPI_ASSUMPTION = CPI_ASSUMPTION, REAL_TERMS = REAL_TERMS,
             PRICE_BASIS = PRICE_BASIS, CPI = CPI, LOGCPI = LOGCPI,
             EDGES_ALT = EDGES_ALT,
             PROB_ALT = PROB_ALT, EDGES = EDGES, pub_table = pub_table,
             alt_table = alt_table,
             SPEC = SPEC, WEIGHTED = WEIGHTED, HALFLIFE = HALFLIFE,
             SCENARIO = SCENARIO, BUCKET_CALIB = BUCKET_CALIB,
             GROWTH_BASIS = GROWTH_BASIS, DELTA = DELTA,
             PRE_CUTOFF = PRE_CUTOFF, RECENT_FROM = RECENT_FROM,
             MIN_POOL = MIN_POOL, THIN_POOL = THIN_POOL, P_FLOOR = P_FLOOR,
             make_pool = make_pool, pool_cdf = pool_cdf, pool_q = pool_q,
             build_pools = build_pools,
             emp_bucket_probs = emp_bucket_probs, rake = rake),
        file = "panel_probs.rds")
