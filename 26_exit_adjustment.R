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

## ---------------------------------------------------------------------
## [26.0] Objects. Everything below is in the session after 20 -> 24. If
## starting cold, the rds files carry what is needed.
## ---------------------------------------------------------------------
if (!exists("feat"))  { fts <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv) }
if (!exists("PROB"))  { prb <- readRDS("panel_probs.rds");    list2env(prb, .GlobalEnv) }
if (!exists("inst_out")) { asg <- readRDS("panel_assign.rds"); list2env(asg, .GlobalEnv) }
stopifnot(exists("feat"), exists("PROB"), exists("fc"), exists("H_SET"),
          exists("CAT_LABELS"), exists("N_CAT"), exists("N_Q"))

## ---------------------------------------------------------------------
## [26.1] Settings
## ---------------------------------------------------------------------
## EXIT_BASIS: which origins the exit rate is estimated from.
##   "full"   -- every origin since 2005 with a closed window (default;
##               matches GROWTH_BASIS = "full")
##   "recent" -- origins in the last EXIT_RECENT_Q quarters only. Use if
##               the by-year table in [26.2] shows a clear trend.
EXIT_BASIS    <- cfg_get("EXIT_BASIS", "full")
EXIT_RECENT_Q <- cfg_get("EXIT_RECENT_Q", 40L)
MIN_EXIT_POOL <- cfg_get("MIN_EXIT_POOL", 200L)   # fall back to neighbour cats below this
H_LAB_X <- setNames(c("1yr", "3yr", "5yr"), as.character(H_SET))

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
exit_long <- bind_rows(lapply(H_SET, function(h) {
  us <- feat[[paste0("usable_h", h)]]
  feat[us, ] %>%
    transmute(h = h, q_index, cat_k,
              ex = .data[[paste0("exit_h", h)]],
              year = START_YEAR + (q_index - 1) %/% 4)
}))

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
exit_bt <- bind_rows(lapply(H_SET, function(h) {
  o <- N_Q - h
  cohort <- feat %>% filter(q_index == o, .data[[paste0("usable_h", h)]]) %>%
    transmute(cat_k, ex = .data[[paste0("exit_h", h)]])
  train <- exit_long %>% filter(h == !!h, q_index + h <= o)
  rates_o <- train %>% group_by(cat_k) %>%
    summarise(rate = mean(ex), n = n(), .groups = "drop")
  for (k in seq_len(N_CAT)) if (!(k %in% rates_o$cat_k) ||
                                rates_o$n[rates_o$cat_k == k] < MIN_EXIT_POOL) {
    src <- rates_o %>% filter(cat_k < k, n >= MIN_EXIT_POOL) %>% arrange(desc(cat_k))
    if (nrow(src)) rates_o <- bind_rows(rates_o %>% filter(cat_k != k),
                                        data.frame(cat_k = k, rate = src$rate[1], n = 0L))
  }
  cohort %>% left_join(rates_o, by = "cat_k") %>%
    group_by(cat_k) %>%
    summarise(n = n(), predicted = sum(rate), actual = sum(ex), .groups = "drop") %>%
    mutate(h = h, origin = qgrid$q_label[o], cat = CAT_LABELS[cat_k],
           ratio = round(actual / pmax(predicted, 1e-9), 2))
}))
cat("\nBacktest -- exits predicted vs actual by category of origin:\n")
print(exit_bt %>% select(h, origin, cat, n, predicted = predicted, actual, ratio) %>%
        mutate(predicted = round(predicted, 1)) %>% as.data.frame(), row.names = FALSE)
exit_bt_tot <- exit_bt %>% group_by(h, origin) %>%
  summarise(n = sum(n), predicted = round(sum(predicted), 1), actual = sum(actual),
            ratio = round(sum(actual) / sum(predicted), 2), .groups = "drop")
cat("\nBacktest totals:\n"); print(as.data.frame(exit_bt_tot), row.names = FALSE)
cat("\nA ratio near 1 at every horizon means the category-level rate is\n",
    "unbiased in aggregate. Ratios well below 1 in every category would say\n",
    "exits are slowing; set EXIT_BASIS <- \"recent\" and compare.\n")

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
cat("\nSum of cells minus national rounding (should be 0 or +/-1):\n")
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
             MIN_EXIT_POOL = MIN_EXIT_POOL),
        file = "panel_exit.rds")
cat("\nSaved panel_exit.rds. Run 27 to add the With Mergers tab.\n")
