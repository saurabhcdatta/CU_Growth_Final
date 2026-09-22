## =====================================================================
## 33_env_window_check.R  --  How long a memory should the merger-
##                            environment factor have, horizon by horizon?
##
## WHY THIS SCRIPT EXISTS
##   26's backtest of the rates AS APPLIED (cat_env2, 21 Sep 2026) read
##   actual / predicted = 1.15 one year out (from 2025Q2), 1.12 three
##   years out (from 2023Q2) and 1.48 five years out (from 2021Q2). With
##   NO factor the same three read 1.35 / 1.33 / 1.26: the factor helps at
##   one and three years and HURTS at five. The factors measured at 2021Q2
##   were 0.90 / 0.82 / 0.65 / 0.91 / 0.88 / 1.12 / 0.76 -- the pandemic
##   lull in mergers -- so the model would have cut every rate just before
##   activity rebounded, and held the cut for five years.
##
##   30's scoreboard (five-year level ratio 1.01) did not show this, for
##   two reasons that have nothing to do with the data:
##     (a) 30 measures the factor ONCE per fold, at the fold's first
##         origin, and applies it to all eight origins in the fold. The
##         last five-year fold (origins 2019Q3-2021Q2) therefore used the
##         2019Q3 factor throughout; the lull factors that production
##         would have used from 2020Q3 on were never applied.
##     (b) 1.01 pools four folds (origins 2013Q3-2021Q2). Five-year exit
##         rates fell from 18% to 14.7% and rose again over that span, so
##         over- and under-prediction can cancel in the pool.
##
## WHAT IT DOES
##   A rolling-origin test of the CATEGORY-RATE family exactly as
##   production applies it: at every origin the long-run category rate
##   comes from windows closed by that origin, and the factor is measured
##   AT that origin from one-year exits closed before it. The candidates
##   differ only in the factor:
##     none       no factor (EXIT_MODEL = "cat" on the full basis)
##     agg8       one system-wide factor, 8-quarter window   (cat_env)
##     prod       shrunk category factors, window from the config by horizon (PRODUCTION)
##     cat8       shrunk category factors, 8-quarter window  (production until 21 Sep 2026)
##     cat12      the same, 12-quarter window
##     cat20      the same, 20-quarter window
##     cat8_half  cat8 applied at half strength: 1 + (f - 1) / 2
##     cat8_cv30  cat8 the way 30's CV measured it (factor and training
##                frozen at the first origin of each 8-origin block)
##   Same 32 origins per horizon that 30's four folds cover. Category
##   means only -- no regression -- so it runs in seconds.
##
## WHAT TO READ
##   [33.4] the table by horizon. 'level' is predicted / actual pooled
##   over origins (30's convention; 1.00 is perfect). 'act_over_pred' is
##   26's convention for the same thing. 'cat_wape' is the weighted
##   allocation error, 'score' is 30's score (cat_wape + level error).
##   'worst' and 'last' are actual / predicted at the worst origin and at
##   the most recent one -- 'last' is the number the With Mergers tab
##   prints. cat8 vs cat8_cv30 shows how much of 30's 1.01 was the way the
##   factor was measured. cat8 vs cat12 vs cat20, down each horizon, says
##   whether a longer memory is safer at longer horizons.
##
## OUTCOME, 21 SEP 2026. Window length did not change accuracy (scores
## within 2.2 points at every horizon) but it changed the tail and the
## stability: worst five-year miss 1.48 (8q) / 1.35 (12q) / 1.26 (20q), and
## the $100M-$500M factor moved ~0.16 a year on 8 quarters against ~0.06 on
## 20. Production now matches the window to the horizon -- max(8, h), i.e.
## 8 / 12 / 20 quarters -- set in the config as EXIT_ENV_WINDOW_Q. The row
## 'prod' below is whatever the config says; cat8 is the old production.
##
## NOTHING HERE CHANGES A PUBLISHED NUMBER. If a horizon-matched window
## wins by 30's own margin (3 points) without giving ground at one and
## three years, the change belongs in 30 (measure the factor at each
## origin; window by horizon), then 26 and 32 -- not here.
##
## RUN AFTER 21 (needs feat). Block by block in RStudio.
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

## The config is loaded once per session (CONFIG_LOADED). If 00_config.R has
## been edited or replaced since -- as it is whenever EXIT_MODEL or the
## environment window changes -- re-load it, so this script cannot run on
## the settings from before the edit. If copies in Data/ and the project
## root DISAGREE nothing is re-loaded: which is current is not for a script
## to guess. (Same block as in 26.)
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
## [33.0] Objects
## ---------------------------------------------------------------------
SCRIPT33_VERSION <- "2026-09-21b"
cat("33_env_window_check.R version", SCRIPT33_VERSION, "\n")
if (!exists("CAT_LABELS") || !exists("N_Q") || !exists("qgrid")) {   # 20's constants live in panel_prep.rds
  .pp <- readRDS("panel_prep.rds")
  for (.k in c("CAT_LABELS", "CAT_PRETTY", "N_CAT", "N_Q", "START_YEAR", "BREAKS", "qgrid"))
    if (!exists(.k) && !is.null(.pp[[.k]])) assign(.k, .pp[[.k]])
  rm(.pp, .k)
}
if (!exists("feat")) { fts <- readRDS("panel_features.rds"); invisible(list2env(fts, .GlobalEnv)) }
stopifnot(exists("feat"), exists("H_SET"), exists("N_CAT"), exists("N_Q"), exists("qgrid"))

## ---------------------------------------------------------------------
## [33.1] Settings
## ---------------------------------------------------------------------
WINDOWS       <- c(8L, 12L, 20L)                         # factor windows tried, quarters
ENV_WINDOW_Q  <- cfg_get("EXIT_ENV_WINDOW_Q", c("4" = 8L, "12" = 12L, "20" = 20L))   # production, by horizon
env_window <- function(h) {
  w <- ENV_WINDOW_Q
  if (length(w) == 1L) return(as.integer(w))
  as.integer(w[[as.character(h)]])
}
SHRINK_N      <- cfg_get("EXIT_ENV_SHRINK_N", 2000)      # as 26 and 30
MIN_EXIT_POOL <- cfg_get("MIN_EXIT_POOL", 200L)
N_ORIGINS     <- 32L                                     # 4 folds x 8 origins, as 30
FOLD_WIDTH    <- 8L

## ---------------------------------------------------------------------
## [33.2] Counts by origin and category, once
## ---------------------------------------------------------------------
## Everything below is sums of these two kinds of table, so the rolling
## test is arithmetic on cumulative sums rather than 96 passes over feat.
##   tab_n[[h]][q, k], tab_e[[h]][q, k] : institutions at origin q in category k
##                                with a closed h-quarter window, and how
##                                many of them exited within it
tab_qk <- function(q, k, w = NULL) {
  f <- list(factor(q, levels = seq_len(N_Q)), factor(k, levels = seq_len(N_CAT)))
  m <- if (is.null(w)) table(f[[1]], f[[2]]) else tapply(w, f, sum)
  m <- matrix(as.numeric(m), N_Q, N_CAT); m[is.na(m)] <- 0; m
}
tab_n <- tab_e <- list()
for (h in H_SET) {
  us <- feat[[paste0("usable_h", h)]]
  tab_n[[as.character(h)]] <- tab_qk(feat$q_index[us], feat$cat_k[us])
  tab_e[[as.character(h)]] <- tab_qk(feat$q_index[us], feat$cat_k[us], feat[[paste0("exit_h", h)]][us])
}
cum <- function(m) apply(m, 2, cumsum)                   # running totals down the origins
cum_n <- lapply(tab_n, cum); cum_e <- lapply(tab_e, cum)
upto <- function(cm, q) if (q < 1) rep(0, ncol(cm)) else cm[min(q, nrow(cm)), ]
sapply(tab_n, sum)                                           # usable institution-quarters by horizon

## ---------------------------------------------------------------------
## [33.3] The pieces: category rate and environment factor at an origin
## ---------------------------------------------------------------------
## Long-run category rate from every window closed by 'last_train' (the
## last training origin), thin categories borrowing from the one below --
## the rule in 30's cat_rate() and 26's backtest.
cat_rate_at <- function(h, last_train) {
  n <- upto(cum_n[[as.character(h)]], last_train); e <- upto(cum_e[[as.character(h)]], last_train)
  r <- ifelse(n > 0, e / n, NA_real_)
  for (k in seq_len(N_CAT)) if (n[k] < MIN_EXIT_POOL) {
    src <- which(n[seq_len(k - 1)] >= MIN_EXIT_POOL)
    if (length(src)) r[k] <- r[max(src)]
  }
  r
}
## Environment factor at 'origin' over a window of w quarters: one-year
## exits from origins closed by then, recent window against everything
## earlier. Same maths as env_factor() / env_factor_cat() in 30 and
## env_at() in 26, with the window as an argument.
env_at_w <- function(origin, w) {
  hi <- origin - 4L; lo <- hi - w
  n_lr <- upto(cum_n[["4"]], hi); e_lr <- upto(cum_e[["4"]], hi)
  n_re <- n_lr - upto(cum_n[["4"]], lo); e_re <- e_lr - upto(cum_e[["4"]], lo)
  if (sum(n_re) < 500 || sum(n_lr) < 5000) return(list(all = 1, cat = rep(1, N_CAT)))
  f_all <- (sum(e_re) / sum(n_re)) / (sum(e_lr) / sum(n_lr))
  out <- rep(f_all, N_CAT)
  for (k in seq_len(N_CAT)) {
    lr <- if (n_lr[k] > 0) e_lr[k] / n_lr[k] else NA_real_
    if (n_re[k] > 0 && is.finite(lr) && lr > 0)
      out[k] <- (n_re[k] * (e_re[k] / n_re[k]) / lr + SHRINK_N * f_all) / (n_re[k] + SHRINK_N)
  }
  list(all = min(max(f_all, 0.5), 2), cat = pmin(pmax(out, 0.5), 3))
}
## Check against what 26 printed at the cohort date (1.09 / 1.31 / ...):
round(env_at_w(N_Q, 8L)$cat, 2)

## ---------------------------------------------------------------------
## [33.4] Rolling-origin test
## ---------------------------------------------------------------------
rows <- list()
for (h in H_SET) {
  hh <- as.character(h); last_o <- N_Q - h
  origins <- seq(last_o - N_ORIGINS + 1L, last_o); origins <- origins[origins > 30]
  for (o in origins) {
    n_k <- tab_n[[hh]][o, ]; act_k <- tab_e[[hh]][o, ]
    rate <- cat_rate_at(h, o - h)                        # windows closed by the origin
    ## 30's CV: factor and training frozen at the first origin of the 8-origin block
    s <- last_o - FOLD_WIDTH + 1L - FOLD_WIDTH * ((last_o - o) %/% FOLD_WIDTH)
    f <- list(none      = rep(1, N_CAT),
              agg8      = rep(env_at_w(o, 8L)$all, N_CAT),
              prod      = env_at_w(o, env_window(h))$cat,
              cat8      = env_at_w(o, 8L)$cat,
              cat12     = env_at_w(o, 12L)$cat,
              cat20     = env_at_w(o, 20L)$cat,
              cat8_half = 1 + (env_at_w(o, 8L)$cat - 1) / 2,
              cat8_cv30 = env_at_w(s, 8L)$cat)
    for (nm in names(f)) {
      r <- if (nm == "cat8_cv30") cat_rate_at(h, s - 1L - h) else rate
      pred_k <- n_k * pmin(r * f[[nm]], 1); pred_k[!is.finite(pred_k)] <- 0
      rows[[length(rows) + 1]] <- data.frame(
        h = h, origin = o, q_label = qgrid$q_label[o], cand = nm,
        pred = sum(pred_k), act = sum(act_k),
        cat_wape = sum(abs(pred_k - act_k)) / max(sum(act_k), 1),
        stringsAsFactors = FALSE)
    }
  }
}
env_res <- bind_rows(rows)

CAND_ORDER <- c("none", "agg8", "prod", "cat8", "cat12", "cat20", "cat8_half", "cat8_cv30")
env_summ <- env_res %>% group_by(h, cand) %>%
  summarise(origins = n(),
            level = round(sum(pred) / sum(act), 2),              # 30's convention: predicted / actual
            act_over_pred = round(sum(act) / sum(pred), 2),      # 26's convention
            cat_wape = round(100 * mean(cat_wape), 1),
            worst = round(max(act / pred), 2),                   # worst single origin, actual / predicted
            best  = round(min(act / pred), 2),
            last  = round((act / pred)[which.max(origin)], 2),   # most recent origin: what the tab prints
            .groups = "drop") %>%
  mutate(score = round(cat_wape + 100 * abs(level - 1), 1)) %>%
  arrange(h, match(cand, CAND_ORDER))
cat("\n=== Category-rate family, factor measured AT each origin (prod = the config's window by horizon:",
    paste(sapply(H_SET, env_window), collapse = " / "), "quarters) ===\n",
    "level = predicted/actual pooled (1.00 perfect) | act_over_pred = the same, 26's way round\n",
    "worst / best / last = actual/predicted at single origins ('last' is what the With Mergers tab prints)\n",
    "score = cat_wape + level error, as in 30 -- lower is better\n\n")
for (h in H_SET) {
  cat("h =", h, " origins", paste(range(env_res$q_label[env_res$h == h]), collapse = " to "), "\n")
  print(env_summ %>% filter(h == !!h) %>% select(-h) %>% as.data.frame(), row.names = FALSE)
  cat("\n")
}

## Does the last origin reproduce 26's backtest? (it must, for prod, if 26 ran under cat_env2 with the same config)
if (file.exists("panel_exit.rds")) {
  .ex <- readRDS("panel_exit.rds")
  if (identical(.ex$BT_MODEL, "cat_env2")) {
    chk <- env_summ %>% filter(cand == "prod") %>% select(h, here = last) %>%
      left_join(.ex$exit_bt_tot %>% select(h, script26 = ratio), by = "h")
    cat("Most recent origin, prod, against 26's backtest (should match):\n")
    print(as.data.frame(chk), row.names = FALSE)
  }
  rm(.ex)
}

## ---------------------------------------------------------------------
## [33.5] Five years, origin by origin -- where each version goes wrong
## ---------------------------------------------------------------------
## Actual / predicted at every second-quarter origin. Read across: the
## pandemic-lull origins (2020-21) are where a short window does damage.
by_origin <- env_res %>% filter(h == max(H_SET), grepl("Q2$", q_label)) %>%
  mutate(ap = round(act / pred, 2)) %>% select(q_label, act, cand, ap) %>%
  pivot_wider(names_from = cand, values_from = ap) %>% select(q_label, act, all_of(CAND_ORDER))
cat("\nFive-year horizon, actual / predicted by origin:\n")
print(as.data.frame(by_origin), row.names = FALSE)

## The factors themselves at those origins, for the $100M-$500M category
## (the one the forecast leans on most): how far apart the windows are.
fac_tbl <- bind_rows(lapply(which(grepl("Q2$", qgrid$q_label) & seq_len(N_Q) > 30), function(o)
  data.frame(q_label = qgrid$q_label[o], w8 = round(env_at_w(o, 8L)$cat[4], 2),
             w12 = round(env_at_w(o, 12L)$cat[4], 2), w20 = round(env_at_w(o, 20L)$cat[4], 2),
             aggregate_w8 = round(env_at_w(o, 8L)$all, 2))))
cat("\nEnvironment factor for", CAT_LABELS[4], "by measurement date and window:\n")
print(fac_tbl, row.names = FALSE)

## What the cohort date would read under each window (production: EXIT_ENV_WINDOW_Q by horizon)
cat("\nFactors at", qgrid$q_label[N_Q], "by window:\n")
for (w in WINDOWS) cat(sprintf("  w = %2d : %s\n", w, paste(sprintf("%.2f", env_at_w(N_Q, w)$cat), collapse = " / ")))

## ---------------------------------------------------------------------
## [33.6] Save
## ---------------------------------------------------------------------
saveRDS(list(env_res = env_res, env_summ = env_summ, by_origin = by_origin, fac_tbl = fac_tbl,
             WINDOWS = WINDOWS, SHRINK_N = SHRINK_N, SCRIPT33_VERSION = SCRIPT33_VERSION),
        file = "panel_env_window.rds")
cat("\nSaved panel_env_window.rds. Nothing published reads it.\n")
