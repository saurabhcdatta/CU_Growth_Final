## =====================================================================
## 26_exit_adjustment.R  --  Merger/liquidation adjustment: from the
##                           surviving-cohort forecast to expected
##                           POPULATION counts
##
## WHAT THIS DOES
##   Scripts 23-24 hold the total at 4,214: every institution is assumed to
##   survive, and the published counts describe where today's credit
##   unions would sit if none merged or closed. Field supervisors asked for
##   the other number -- how many credit unions there will actually be in
##   each category. This script supplies it.
##
##   For each institution the seven category probabilities from 23 are
##   conditional on survival. Multiply them by the probability of surviving
##   to the horizon and the remainder is the probability of having exited:
##
##     P(cat j at t+h)  =  (1 - P(exit by t+h)) * P(cat j | survive)
##     P(exit by t+h)   =  historical exit rate for the institution's
##                         asset category over h quarters
##
##   The exit rate is estimated the same way the growth distributions are:
##   from every institution-quarter since 2005 in that category whose
##   h-quarter window closed inside the sample (usable_h in script 21),
##   counting the share that merged away or closed (exit_h). Category is
##   the only conditioning variable, for the same reason it is the only one
##   in the growth model: it is what cross-validation supports, and it is
##   what an examiner can check.
##
## WHAT IS PUBLISHED AND WHAT IS NOT
##   Published: expected population counts by category, nationally and by
##   region x charter, and the expected number of exits by category of
##   origin. These are sums of probabilities rounded to whole numbers.
##   NOT published: any institution-level exit probability, flag or
##   assignment. A category-level rate says nothing about a particular
##   credit union, and naming institutions as "projected to merge" is not
##   something this office should put in front of the field. The
##   institution lists on the regional and Institutions tabs stay exactly
##   as they are -- conditional on survival, which is the honest reading of
##   a named row.
##
## WHERE IT SITS
##   After 24 (needs PROB, fc, counts_int, apportion) and before 27, which
##   reads panel_exit.rds if it exists and adds a "With Mergers" tab plus a
##   population block on each regional tab. Runtime: seconds.
##
## New charters are ignored: a handful a year against ~150 exits.
##
## WHICH RATES ARE APPLIED. The category rates estimated in [26.2] are
## applied only when EXIT_MODEL = "cat". Under a model chosen in 30
## (cat_env2 since 21 Sep 2026) the probabilities come from
## panel_exit_models.rds; [26.4b] then tabulates the rates AS APPLIED
## (exit_rates_applied), and that table -- not exit_rates -- is what 27
## prints as "Exit rates used".
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

## The config is loaded once per session (the CONFIG_LOADED guard above),
## but EXIT_MODEL is edited MID-session -- right after reading 30's verdict,
## just before this script is run. Check the file on disk against what the
## session holds and re-load it if it has changed, so 26 cannot quietly
## apply the model that was set before the edit.
## If copies of the config in Data/ and in the project root DISAGREE, nothing
## is re-loaded: which one is current is not for a script to guess.
.cf <- c("00_config.R", "../00_config.R", "S:/Projects/Credit_Union_Growth_Forecast/00_config.R")
.cf <- unique(normalizePath(.cf[file.exists(.cf)]))
if (length(.cf) && exists("CFG")) {
  .cfgs <- lapply(.cf, function(f) {
    e <- new.env(); invisible(capture.output(sys.source(f, envir = e))); e$CFG })
  if (length(.cf) > 1 && !all(vapply(.cfgs[-1], identical, NA, .cfgs[[1]]))) {
    warning("Different 00_config.R files on the search path (", paste(.cf, collapse = "; "),
            "). The settings already loaded were kept; remove the stale copy.")
  } else {
    .chg <- names(.cfgs[[1]])[!mapply(identical, .cfgs[[1]], CFG[names(.cfgs[[1]])])]
    if (length(.chg)) {
      cat("00_config.R has changed on disk since this session loaded it (",
          paste(.chg, collapse = ", "), "): re-loading it.\n", sep = "")
      source(.cf[1])
    }
    rm(.chg)
  }
  rm(.cfgs)
}
rm(.cf)

library(dplyr)
library(tidyr)

## ---------------------------------------------------------------------
## [26.0] Objects. Everything below is in the session after 20 -> 24. If
## starting cold, the rds files carry what is needed.
## ---------------------------------------------------------------------
SCRIPT26_VERSION <- "2026-09-21a"
cat("26_exit_adjustment.R version", SCRIPT26_VERSION, "\n")
if (!exists("CAT_LABELS") || !exists("N_Q") || !exists("qgrid") || !exists("START_YEAR")) {   # 20's constants live in panel_prep.rds
  .pp <- readRDS("panel_prep.rds")
  for (.k in c("CAT_LABELS", "CAT_PRETTY", "N_CAT", "N_Q", "START_YEAR", "BREAKS", "LOG_EDGE",
               "qgrid", "REGIONS", "ASSET_SCALE")) if (!exists(.k) && !is.null(.pp[[.k]])) assign(.k, .pp[[.k]])
  rm(.pp, .k)
}
if (!exists("feat"))  { fts <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv) }
if (!exists("PROB"))  { prb <- readRDS("panel_probs.rds");    list2env(prb, .GlobalEnv) }
if (!exists("inst_out")) { asg <- readRDS("panel_assign.rds"); list2env(asg, .GlobalEnv) }
stopifnot(exists("feat"), exists("PROB"), exists("fc"), exists("H_SET"),
          exists("CAT_LABELS"), exists("N_CAT"), exists("N_Q"),
          exists("START_YEAR"), exists("qgrid"))
## 27 indexes P_EXIT (fc's row order) by inst_out's rows without a join,
## so the two must list the same institutions in the same order.
if (exists("inst_out"))
  stopifnot(nrow(inst_out) == nrow(fc),
            all(as.character(inst_out$join_number) == as.character(fc$join_number)))

## ---------------------------------------------------------------------
## [26.1] Settings
## ---------------------------------------------------------------------
## EXIT_BASIS: which origins the exit rate is estimated from.
##   "full"   -- every origin since 2005 with a closed window (default;
##               matches GROWTH_BASIS = "full")
##   "recent" -- origins in the last EXIT_RECENT_Q quarters only. Use if
##               the by-year table in [26.2] shows a clear trend.
## Sept 10 2026 backtest on the full basis: actual exits ran 1.35 / 1.33 /
## 1.27 times predicted at 1 / 3 / 5 years, with the gap concentrated in
## the $10M-$1B categories (ratios 1.4-3.2) and the under-$10M category
## nearly right (1.08). Mergers have shifted toward mid-sized institutions
## over the last decade; a 2005-2026 average understates that. "recent"
## with a 15-year window keeps enough closed five-year windows to backtest.
EXIT_BASIS    <- cfg_get("EXIT_BASIS", "recent")
EXIT_RECENT_Q <- cfg_get("EXIT_RECENT_Q", 60L)
MIN_EXIT_POOL <- cfg_get("MIN_EXIT_POOL", 200L)   # fall back to neighbour cats below this
H_LAB_X <- setNames(c("1yr", "3yr", "5yr"), as.character(H_SET))

## EXIT_MODEL is settled HERE, before the backtest, so that [26.3] tests
## the rates [26.4] applies -- in a clean session and under run_all.R as
## well as mid-session. (Until 2026-09-21 it was read in [26.4]: a clean
## run back-tested "cat" and 27 printed those ratios under cat_env2
## counts.) panel_exit_models.rds is read from disk every time: it is
## what 30 last saved, which a P_EXIT_ALT left in the session may not be.
EXIT_MODEL <- cfg_get("EXIT_MODEL", "cat")
ENV_NOW_30 <- NULL
if (EXIT_MODEL != "cat") {
  if (file.exists("panel_exit_models.rds")) {
    .m <- readRDS("panel_exit_models.rds")
    P_EXIT_ALT <- .m$P_EXIT_ALT; ENV_NOW_30 <- .m$env_now; rm(.m)
  }
  if (!exists("P_EXIT_ALT") || is.null(P_EXIT_ALT[[EXIT_MODEL]])) {
    warning("EXIT_MODEL = '", EXIT_MODEL, "' requested but 30 has not produced it; using category rates.")
    EXIT_MODEL <- "cat"
  } else if (any(lengths(P_EXIT_ALT[[EXIT_MODEL]]) != nrow(fc))) {
    stop("panel_exit_models.rds holds ", length(P_EXIT_ALT[[EXIT_MODEL]][[1]]),
         " institutions but this cohort has ", nrow(fc),
         ": it was written for another cohort. Run 31 -> 30 for this cohort, then 26.")
  }
}
cat("Exit model in force:", EXIT_MODEL, "\n")

## Largest-remainder rounding to a fixed total (same rule as 24)
lr_round <- function(x, total) {
  fl <- floor(x); rem <- x - fl
  k <- as.integer(round(total - sum(fl)))
  if (k > 0) { o <- order(rem, decreasing = TRUE)[seq_len(k)]; fl[o] <- fl[o] + 1 }
  as.integer(fl)
}

## ---------------------------------------------------------------------
## [26.2] Exit rates by category and horizon
## ---------------------------------------------------------------------
## Rows usable for the exit model: window closed inside the sample and the
## outcome either observed or an exit (usable_h from 21). exit_h is 1 for
## merged-away or closed; "filter" departures are NA by construction and
## never counted.
exit_long_all <- bind_rows(lapply(H_SET, function(h) {
  us <- feat[[paste0("usable_h", h)]]
  feat[us, ] %>%
    transmute(h = h, q_index, cat_k,
              ex = .data[[paste0("exit_h", h)]],
              year = START_YEAR + (q_index - 1) %/% 4)
}))
## exit_long_all: every origin since START_YEAR -- what 30 trains on, and
## so the base for the backtest and for the long-run column under a model
## from 30. exit_long: the same table on EXIT_BASIS, 26's own rates.
exit_long <- exit_long_all

## By origin year, five-year horizon -- the trend check
exit_by_year <- exit_long %>%
  filter(h == 20) %>%
  group_by(year) %>%
  summarise(n = n(), exits = sum(ex), rate_5y = round(100 * mean(ex), 1),
            .groups = "drop")
cat("Five-year exit rate by origin year (all categories):\n")
print(as.data.frame(exit_by_year), row.names = FALSE)

if (EXIT_BASIS == "recent") {
  exit_long <- exit_long %>% filter(q_index > N_Q - EXIT_RECENT_Q)
  cat("EXIT_BASIS = recent: origins after q_index", N_Q - EXIT_RECENT_Q, "\n")
}

exit_rates <- exit_long %>%
  group_by(h, cat_k) %>%
  summarise(n = n(), exits = sum(ex), rate = mean(ex), .groups = "drop") %>%
  arrange(h, cat_k)
exit_rates$borrowed <- FALSE          # set below where a thin cell borrows

## Thin cells (the $10B category at every horizon) borrow the neighbour
## below -- a $10B institution's exit risk is at least as low as a $1-10B
## one's, so this is conservative in the direction of MORE exits.
for (i in seq_len(nrow(exit_rates))) {
  if (exit_rates$n[i] < MIN_EXIT_POOL && exit_rates$cat_k[i] > 1) {
    j <- which(exit_rates$h == exit_rates$h[i] & exit_rates$cat_k == exit_rates$cat_k[i] - 1)
    exit_rates$rate[i] <- exit_rates$rate[j]
    exit_rates$borrowed <- if (is.null(exit_rates$borrowed)) FALSE else exit_rates$borrowed
    exit_rates$borrowed[i] <- TRUE
  }
}
if (is.null(exit_rates$borrowed)) exit_rates$borrowed <- FALSE

exit_rates <- exit_rates %>%
  mutate(cat = CAT_LABELS[cat_k],
         rate_pct = round(100 * rate, 1),
         annual_pct = round(100 * (1 - (1 - rate)^(4 / h)), 2))

cat("\nExit rates by category (share merged or closed within the horizon):\n")
print(exit_rates %>% select(h, cat, n, exits, rate_pct, annual_pct, borrowed) %>%
        as.data.frame(), row.names = FALSE)

exit_wide <- exit_rates %>%
  select(cat, h, rate_pct) %>%
  pivot_wider(names_from = h, values_from = rate_pct, names_prefix = "h")
print(as.data.frame(exit_wide), row.names = FALSE)

## ---------------------------------------------------------------------
## [26.3] Backtest: predicted vs actual exits from past origins
## ---------------------------------------------------------------------
## For each horizon, take the cohort at origin N_Q - h (so the window ends
## at the cohort date), estimate rates from origins whose window closed
## BEFORE that origin, predict exits by category, compare to what happened.
## The backtest must test the rates AS APPLIED. When EXIT_MODEL is one of
## the environment-adjusted models from 30, the published counts use the
## long-run category rate times a merger-environment factor measured at
## the cohort date; so the backtest applies the same factor measured at
## each past origin (from one-year exits closed before that origin), the
## way 30's cross-validation does. Without this the block shows the
## un-adjusted rates' 20-25% under-prediction under counts that no longer
## use them. Same maths as env_factor() / env_factor_cat() in 30.
ENV_WINDOW_Q <- cfg_get("EXIT_ENV_WINDOW_Q", 8L)
ENV_SHRINK_N <- cfg_get("EXIT_ENV_SHRINK_N", 2000)
env_at <- function(origin) {           # aggregate factor and shrunk category factors at an origin
  us  <- feat$usable_h4 & feat$q_index <= origin - 4L
  e1  <- feat$exit_h4[us]; q1 <- feat$q_index[us]; k1 <- feat$cat_k[us]
  rec <- q1 > origin - 4L - ENV_WINDOW_Q
  if (sum(rec) < 500 || length(e1) < 5000) return(list(all = 1, cat = rep(1, N_CAT)))
  f_all <- mean(e1[rec]) / mean(e1); out <- rep(f_all, N_CAT)
  for (k in seq_len(N_CAT)) {
    lr <- mean(e1[k1 == k]); n_r <- sum(rec & k1 == k)
    if (n_r > 0 && is.finite(lr) && lr > 0)
      out[k] <- (n_r * mean(e1[rec & k1 == k]) / lr + ENV_SHRINK_N * f_all) / (n_r + ENV_SHRINK_N)
  }
  list(all = min(max(f_all, 0.5), 2), cat = pmin(pmax(out, 0.5), 3))
}
BT_ENV <- EXIT_MODEL                  # settled in [26.1]
BT_MODEL <- BT_ENV                    # under this name in the session and in panel_exit.rds; 27 checks it
## Base rates as applied: 26's own basis under "cat"; every origin since
## START_YEAR under a model from 30, because that is what [30.7] fits on
## and what the factor is measured against. (Until 2026-09-21 the recent
## basis was used for both, counting part of the uplift twice.)
bt_src <- if (BT_ENV == "cat") exit_long else exit_long_all
exit_bt <- bind_rows(lapply(H_SET, function(h) {
  o <- N_Q - h
  cohort <- feat %>% filter(q_index == o, .data[[paste0("usable_h", h)]]) %>%
    transmute(cat_k, ex = .data[[paste0("exit_h", h)]])
  train <- bt_src %>% filter(h == !!h, q_index + h <= o)
  rates_o <- train %>% group_by(cat_k) %>%
    summarise(rate = mean(ex), n = n(), .groups = "drop")
  for (k in seq_len(N_CAT)) if (!(k %in% rates_o$cat_k) ||
                                rates_o$n[rates_o$cat_k == k] < MIN_EXIT_POOL) {
    src <- rates_o %>% filter(cat_k < k, n >= MIN_EXIT_POOL) %>% arrange(desc(cat_k))
    if (nrow(src)) rates_o <- bind_rows(rates_o %>% filter(cat_k != k),
                                        data.frame(cat_k = k, rate = src$rate[1], n = 0L))
  }
  if (BT_ENV %in% c("cat_env", "cat_env2", "size_env", "size_env2")) {
    f <- env_at(o)
    fk <- if (BT_ENV %in% c("cat_env2", "size_env2")) f$cat else rep(f$all, N_CAT)
    rates_o <- rates_o %>% mutate(rate = pmin(rate * fk[cat_k], 1))
  }
  cohort %>% left_join(rates_o, by = "cat_k") %>%
    group_by(cat_k) %>%
    summarise(n = n(), predicted = sum(rate), actual = sum(ex), .groups = "drop") %>%
    mutate(h = h, origin = qgrid$q_label[o], cat = CAT_LABELS[cat_k],
           ratio = ifelse(predicted >= 0.5, round(actual / predicted, 2), NA))
}))
cat("\nBacktest -- exits predicted vs actual by category of origin:\n")
print(exit_bt %>% select(h, origin, cat, n, predicted = predicted, actual, ratio) %>%
        mutate(predicted = round(predicted, 1)) %>% as.data.frame(), row.names = FALSE)
exit_bt_tot <- exit_bt %>% group_by(h, origin) %>%
  summarise(n = sum(n), predicted = round(sum(predicted), 1), actual = sum(actual),
            ratio = round(sum(actual) / sum(predicted), 2), .groups = "drop")
cat("\nBacktest totals (rates as applied under EXIT_MODEL =", BT_ENV, "):\n")
print(as.data.frame(exit_bt_tot), row.names = FALSE)
if (BT_ENV %in% c("cat_env2", "size_env2")) for (h in H_SET)
  cat(sprintf("  environment factors at %s: %s\n", qgrid$q_label[N_Q - h],
              paste(sprintf("%.2f", env_at(N_Q - h)$cat), collapse = " / ")))
## What the backtest tested, in words; 27 prints it over the block.
BT_NOTE <- switch(BT_ENV,
  cat       = sprintf("category rates on the '%s' basis, as applied", EXIT_BASIS),
  cat_env   = "long-run category rates times the system-wide merger-environment factor measured at each past date, as applied",
  cat_env2  = "long-run category rates times each category's merger-environment factor measured at each past date, as applied",
  size_env  = "a category-rate approximation of the size curve, times the merger-environment factor measured at each past date",
  size_env2 = "a category-rate approximation of the size curve, times the category merger-environment factors measured at each past date",
  sprintf("long-run category rates only; the '%s' model itself is back-tested in script 30", BT_ENV))
cat("\nThe backtest tested:", BT_NOTE, "\n")
cat("A ratio near 1 means the rates as applied were unbiased in aggregate from\n",
    "that date. Above 1: more exits happened than predicted. The category\n",
    "table above shows where; 30's cross-validation is the fuller test.\n")

## ---------------------------------------------------------------------
## [26.4] Apply to the cohort: population probabilities and counts
## ---------------------------------------------------------------------
## P_exit for each institution is its category's rate at each horizon.
## Category probabilities from 23 (already A7-corrected) are conditional on
## survival; scale them by (1 - P_exit). The row now has eight states and
## still sums to one.
P_EXIT <- lapply(H_SET, function(h) {
  r <- exit_rates %>% filter(h == !!h) %>% arrange(cat_k) %>% pull(rate)
  r[fc$cat_k]
})
names(P_EXIT) <- as.character(H_SET)

## A richer exit model from 30, if one was chosen there (settled in
## [26.1]). Institutions the model could not score -- no feature row at
## the cohort date, about ten of them -- take the average of the model's
## own probabilities in their category, which for the category-rate
## models IS the rate as applied; 26's own rate only if nobody in the
## category was scored. (Until 2026-09-21 they took 26's un-adjusted
## rate.) The publication rule is unchanged: expected counts only,
## nothing institution-level.
N_UNSCORED <- 0L
if (EXIT_MODEL != "cat") {
  for (hh in as.character(H_SET)) {
    alt  <- P_EXIT_ALT[[EXIT_MODEL]][[hh]]
    ok   <- is.finite(alt)
    fill <- as.numeric(tapply(alt[ok], factor(fc$cat_k[ok], levels = seq_len(N_CAT)), mean))[fc$cat_k]
    fill <- ifelse(is.finite(fill), fill, P_EXIT[[hh]])
    P_EXIT[[hh]] <- ifelse(ok, alt, fill)
    N_UNSCORED <- max(N_UNSCORED, sum(!ok))
  }
  cat("Exit probabilities from 30's '", EXIT_MODEL, "' model; ", N_UNSCORED,
      " institutions it could not score take their category's average.\n", sep = "")
} else cat("Exit probabilities: category rates (EXIT_MODEL = cat).\n")

PROB_POP <- lapply(as.character(H_SET), function(hh) {
  P  <- PROB[[hh]] * (1 - P_EXIT[[hh]])
  cbind(P, exit = P_EXIT[[hh]])
})
names(PROB_POP) <- as.character(H_SET)
stopifnot(all(sapply(PROB_POP, function(P) max(abs(rowSums(P) - 1)) < 1e-9)))

## Population counts, national. Eight states rounded to the cohort total
## by largest remainder; the exit row is what leaves.
STATE_LAB <- c(CAT_LABELS, "exit")
pop_soft <- sapply(as.character(H_SET), function(hh) colSums(PROB_POP[[hh]]))
pop_counts <- data.frame(state = STATE_LAB,
                         now = c(as.integer(table(factor(fc$cat_k, levels = seq_len(N_CAT)))), 0L),
                         stringsAsFactors = FALSE)
for (hh in as.character(H_SET))
  pop_counts[[paste0("h", hh)]] <- lr_round(pop_soft[, hh], nrow(fc))
stopifnot(all(colSums(pop_counts[, -1]) == nrow(fc)))

cat("\nPOPULATION counts (surviving institutions by category; 'exit' = merged or closed):\n")
print(pop_counts, row.names = FALSE)
cat(sprintf("Surviving total: %d -> %d / %d / %d\n", nrow(fc),
            nrow(fc) - pop_counts$h4[N_CAT + 1], nrow(fc) - pop_counts$h12[N_CAT + 1],
            nrow(fc) - pop_counts$h20[N_CAT + 1]))

## Exits by category of ORIGIN -- "how many of today's under-$10M credit
## unions will be gone" -- which is the question the field asks.
exits_by_origin <- data.frame(cat = CAT_LABELS, now = pop_counts$now[seq_len(N_CAT)])
for (hh in as.character(H_SET))
  exits_by_origin[[paste0("h", hh)]] <-
    lr_round(tapply(P_EXIT[[hh]], factor(fc$cat_k, levels = seq_len(N_CAT)), sum),
             pop_counts[[paste0("h", hh)]][N_CAT + 1])
cat("\nExpected exits by 2026Q2 category:\n")
print(exits_by_origin, row.names = FALSE)

## Side by side with the survivor-only forecast (counts_int from 27's
## tally, or the soft counts from 23 if 27 has not run)
surv <- if (exists("counts_int")) counts_int else
  data.frame(cat = CAT_LABELS, now = pop_counts$now[1:N_CAT],
             h4 = round(colSums(PROB[["4"]])), h12 = round(colSums(PROB[["12"]])),
             h20 = round(colSums(PROB[["20"]])))
compare_pop <- data.frame(
  cat = CAT_LABELS, now = surv$now,
  no_mergers_5y = surv$h20, with_mergers_5y = pop_counts$h20[1:N_CAT],
  difference = pop_counts$h20[1:N_CAT] - surv$h20)
cat("\nFive years out, survivor-only vs population:\n")
print(compare_pop, row.names = FALSE)

## ---------------------------------------------------------------------
## [26.4b] The rates AS APPLIED -- what 27 prints as "Exit rates used"
## ---------------------------------------------------------------------
## exit_rates ([26.2]) is 26's own table and is applied only under "cat".
## This is the table that ties to the counts whatever the model: the
## average exit probability over today's institutions in each category,
## next to the long-run category rate (every origin since START_YEAR, the
## same thin-pool borrowing as cat_rate() in 30) and the ratio of the two.
## Expected exits from a category = institutions today x the applied rate.
cat_rate_tbl <- function(d) {
  r <- d %>% group_by(cat_k) %>% summarise(rate = mean(ex), n = n(), .groups = "drop")
  for (k in seq_len(N_CAT)) if (!(k %in% r$cat_k) || r$n[r$cat_k == k] < MIN_EXIT_POOL) {
    src <- r %>% filter(cat_k < k, n >= MIN_EXIT_POOL) %>% arrange(desc(cat_k))
    if (nrow(src)) r <- bind_rows(r %>% filter(cat_k != k),
                                  data.frame(cat_k = k, rate = src$rate[1], n = 0L))
  }
  r %>% arrange(cat_k)
}
base_long <- bind_rows(lapply(H_SET, function(h)
  cat_rate_tbl(exit_long_all %>% filter(h == !!h)) %>% mutate(h = h)))
exit_rates_applied <- bind_rows(lapply(H_SET, function(h) {
  f <- factor(fc$cat_k, levels = seq_len(N_CAT))
  data.frame(h = h, cat_k = seq_len(N_CAT), n_inst = as.integer(table(f)),
             rate = as.numeric(tapply(P_EXIT[[as.character(h)]], f, mean)))
})) %>%
  left_join(base_long %>% select(h, cat_k, base = rate), by = c("h", "cat_k")) %>%
  mutate(cat = CAT_LABELS[cat_k],
         rate_pct   = round(100 * rate, 1),
         annual_pct = round(100 * (1 - (1 - rate)^(4 / h)), 2),
         base_pct   = round(100 * base, 1),
         uplift     = ifelse(is.finite(base) & base > 0, round(rate / base, 2), NA_real_))
cat("\nExit rates AS APPLIED under EXIT_MODEL =", EXIT_MODEL,
    "(base = long-run category rate; uplift = applied / base):\n")
print(exit_rates_applied %>% select(h, cat, n_inst, base_pct, uplift, rate_pct, annual_pct) %>%
        as.data.frame(), row.names = FALSE)

## Tie-out for the category-factor models: the uplift in 30's probabilities
## must equal the factor 26 measures itself at the cohort date (the same
## env_at() the backtest uses) and the one 30 saved. A gap means
## panel_exit_models.rds and this session are not from the same run.
ENV_NOW_26 <- env_at(N_Q)
if (EXIT_MODEL %in% c("cat_env", "cat_env2")) {
  f26 <- if (EXIT_MODEL == "cat_env2") ENV_NOW_26$cat else rep(ENV_NOW_26$all, N_CAT)
  f30 <- if (is.null(ENV_NOW_30)) rep(NA_real_, N_CAT) else
         if (EXIT_MODEL == "cat_env2") ENV_NOW_30$factor_cat else rep(ENV_NOW_30$factor_all, N_CAT)
  upl <- with(exit_rates_applied[exit_rates_applied$h == 20, ], ifelse(is.finite(base) & base > 0, rate / base, NA_real_))
  cat(sprintf("\nEnvironment factor at %s, three ways (they should agree):\n", qgrid$q_label[N_Q]))
  cat("  26 env_at()            :", paste(sprintf("%.2f", f26), collapse = " / "), "\n")
  cat("  30 env_now (saved)     :", paste(sprintf("%.2f", f30), collapse = " / "), "\n")
  cat("  applied / long-run, 5yr:", paste(sprintf("%.2f", upl), collapse = " / "), "\n")
  gap <- max(abs(upl - f26), abs(f30 - f26), na.rm = TRUE)
  if (is.finite(gap) && gap > 0.02)
    warning("Environment factors disagree by ", round(gap, 3),
            ": panel_exit_models.rds may not be from this panel. Re-run 30, then 26.")
}

## ---------------------------------------------------------------------
## [26.5] Region x charter population counts
## ---------------------------------------------------------------------
## Rounded within each cell (eight states to the cell's own total) so each
## regional block reconciles to its own institution count. The national
## table above is rounded separately; the two can differ by one or two in
## a category, which 27 reports rather than hides.
pop_cells <- bind_rows(lapply(split(seq_len(nrow(fc)), list(fc$region, fc$cu_type), drop = TRUE),
  function(idx) {
    out <- data.frame(region = fc$region[idx[1]], cu_type = fc$cu_type[idx[1]],
                      state = STATE_LAB,
                      now = c(as.integer(table(factor(fc$cat_k[idx], levels = seq_len(N_CAT)))), 0L),
                      stringsAsFactors = FALSE)
    for (hh in as.character(H_SET))
      out[[paste0("h", hh)]] <- lr_round(colSums(PROB_POP[[hh]][idx, , drop = FALSE]), length(idx))
    out
  }))
stopifnot(all((pop_cells %>% group_by(region, cu_type) %>%
                 summarise(across(c(now, h4, h12, h20), sum), .groups = "drop") %>%
                 mutate(ok = now == h4 & now == h12 & now == h20))$ok))
cell_vs_nat <- pop_cells %>% group_by(state) %>%
  summarise(across(c(h4, h12, h20), sum), .groups = "drop") %>%
  arrange(match(state, STATE_LAB))
cat("\nSum of cells minus national rounding (should be 0 or within +/-2 (rounding across many state cells)):\n")
print(data.frame(state = STATE_LAB,
                 d4 = cell_vs_nat$h4 - pop_counts$h4,
                 d12 = cell_vs_nat$h12 - pop_counts$h12,
                 d20 = cell_vs_nat$h20 - pop_counts$h20), row.names = FALSE)

## ---------------------------------------------------------------------
## [26.6] Save
## ---------------------------------------------------------------------
saveRDS(list(exit_rates = exit_rates, exit_wide = exit_wide,
             exit_by_year = exit_by_year, exit_bt = exit_bt,
             exit_bt_tot = exit_bt_tot, pop_counts = pop_counts,
             exits_by_origin = exits_by_origin, compare_pop = compare_pop,
             pop_cells = pop_cells, P_EXIT = P_EXIT, STATE_LAB = STATE_LAB,
             EXIT_BASIS = EXIT_BASIS, EXIT_RECENT_Q = EXIT_RECENT_Q,
             EXIT_MODEL = EXIT_MODEL,
             exit_rates_applied = exit_rates_applied, BT_MODEL = BT_MODEL,
             BT_NOTE = BT_NOTE, N_UNSCORED = N_UNSCORED,
             ENV_NOW_26 = ENV_NOW_26, P_EXIT_JOIN = fc$join_number,
             SCRIPT26_VERSION = SCRIPT26_VERSION,
             MIN_EXIT_POOL = MIN_EXIT_POOL),
        file = "panel_exit.rds")
cat(sprintf("\nOverall: %.1f%% of the cohort exits within five years (%.2f%%/yr).\n",
            100 * pop_counts$h20[N_CAT + 1] / nrow(fc),
            100 * (1 - (1 - pop_counts$h20[N_CAT + 1] / nrow(fc))^(1/5))))
cat("Saved panel_exit.rds. Run 27 to add the With Mergers tab.\n")
