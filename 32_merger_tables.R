## =====================================================================
## 32_merger_tables.R  --  Reportable merger tables for the field
##
## Everything here is derived from 26/30/31 output; nothing new is
## estimated except a one-origin backtest of the risk tiers. Produces:
##
##   T1  Merger-rate curve by asset size (1/3/5 yr, with and without the
##       merger-environment factor) -- THE asset-based rate table
##   T2  Historical one-year merger rate by year, and the environment factor
##   T3  Expected exits 2026Q2 -> horizons, by category, region x charter,
##       and state (counts, from the chosen EXIT_MODEL)
##   T4  Who absorbs whom, by asset category (targets vs acquirers)
##   T5  Consolidation-risk tiers: definition, size of each tier in the
##       cohort, and the tier's REALISED five-year exit rate in a backtest
##       from 2021Q2 -- so "High" is a measured frequency, not a prediction
##   T6  (restricted, off by default) the named list behind T5
##
## Output: CU_Merger_Tables_<cohort>.xlsx if 27's sheet helpers are in the
## session, otherwise CSVs in Data/merger_tables/. The restricted list is
## written only when PUBLISH_WATCHLIST is TRUE, to its own file.
##
## RUN AFTER 30 (and 31 for T4). Seconds.
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
library(dplyr); library(tidyr); library(splines)
SCRIPT32_VERSION <- "2026-09-25e"
cat("32_merger_tables.R version", SCRIPT32_VERSION, "\n")

## ---------------------------------------------------------------------
## [32.0] Objects
## ---------------------------------------------------------------------
if (!exists("feat"))      { fts <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv) }
if (!exists("CAT_LABELS") || !exists("N_Q")) {   # 20's constants live in panel_prep.rds
  .pp <- readRDS("panel_prep.rds")
  for (.k in c("CAT_LABELS", "CAT_PRETTY", "N_CAT", "N_Q", "START_YEAR", "BREAKS", "LOG_EDGE",
               "qgrid", "REGIONS", "ASSET_SCALE")) if (!exists(.k) && !is.null(.pp[[.k]])) assign(.k, .pp[[.k]])
  rm(.pp, .k)
}
if (!exists("fc"))        { prb <- readRDS("panel_probs.rds");    list2env(prb, .GlobalEnv) }
if (!exists("inst_out"))  { asg <- readRDS("panel_assign.rds");   list2env(asg, .GlobalEnv) }
if ((!exists("P_EXIT_ALT") || !exists("env_now")) && file.exists("panel_exit_models.rds")) {
  .m <- readRDS("panel_exit_models.rds"); P_EXIT_ALT <- .m$P_EXIT_ALT; env_now <- .m$env_now; rm(.m)
}
if (!exists("env_now")) env_now <- NULL
if (!exists("flow_tbl") && file.exists("panel_peers.rds")) {
  .pp <- readRDS("panel_peers.rds"); list2env(.pp, .GlobalEnv); rm(.pp)
}
if (!exists("panel")) { .pp <- readRDS("panel_prep.rds"); panel <- .pp$panel; rm(.pp) }
stopifnot(exists("feat"), exists("fc"), exists("inst_out"), exists("P_EXIT_ALT"))

## 20's session constants, derived if 20 has not run in this session
if (!exists("START_YEAR")) START_YEAR <- cfg_get("START_YEAR", 2005L)
if (!exists("END_Y"))      END_Y      <- cfg_get("END_Y", START_YEAR + (N_Q - 1L) %/% 4L)
if (!exists("REG_LAB"))    REG_LAB    <- cfg_get("REG_LAB", c("1" = "Region 1", "2" = "Region 2", "3" = "Region 3", "8" = "ONES"))
if (!exists("CT_LAB"))     CT_LAB     <- cfg_get("CT_LAB", c("1" = "FCU", "2" = "FISCU"))
if (!exists("CAT_PRETTY")) { .pp <- readRDS("panel_prep.rds"); CAT_PRETTY <- .pp$CAT_PRETTY; rm(.pp) }

EXIT_MODEL <- cfg_get("EXIT_MODEL", "size_env")
stopifnot(!is.null(P_EXIT_ALT[[EXIT_MODEL]]))
P5 <- P_EXIT_ALT[[EXIT_MODEL]]
## Use the probabilities 26 actually applied when they are on disk for the
## same model. They are 30's, except that the ~10 institutions 30 could
## not score (no feature row at the cohort date) carry their category's
## rate instead of NA -- so the expected exits here are the With Mergers
## tab's, not three short of it.
P5_FROM_26 <- FALSE
P5_SOURCE <- "30 (panel_exit_models.rds); institutions 30 could not score count as zero"
if (file.exists("panel_exit.rds")) {
  .ex <- readRDS("panel_exit.rds")
  if (identical(.ex$EXIT_MODEL, EXIT_MODEL) && !is.null(.ex$P_EXIT) &&
      all(lengths(.ex$P_EXIT) == nrow(fc)) && all(is.finite(unlist(.ex$P_EXIT)))) {
    P5 <- .ex$P_EXIT; P5_FROM_26 <- TRUE
    P5_SOURCE <- "26 (panel_exit.rds) -- the probabilities behind the With Mergers tab"
  } else cat("panel_exit.rds was not written under EXIT_MODEL =", EXIT_MODEL,
             "for this cohort: run 26 first if these tables are to tie to the growth workbook.\n")
  rm(.ex)
}
cat("Using exit model:", EXIT_MODEL, "| probabilities from", P5_SOURCE, "\n")
if (!exists("BREAKS"))      BREAKS      <- readRDS("panel_prep.rds")$BREAKS
if (!exists("ASSET_SCALE")) ASSET_SCALE <- 1
MIN_EXIT_POOL_32 <- cfg_get("MIN_EXIT_POOL", 200L)
## Models whose rate is a category rate (so the category table, not the size
## curve, is what the counts use), and models that apply an environment factor.
CAT_FAMILY <- EXIT_MODEL %in% c("cat", "cat_env", "cat_env2", "cat_logit")
ENV_MODELS <- c("cat_env", "cat_env2", "size_env", "size_env2", "peer_env")

PUBLISH_WATCHLIST <- cfg_get("PUBLISH_WATCHLIST", FALSE)
## The factor's window is BY HORIZON since 21 Sep 2026 (max(8, h) quarters; see 30 [30.1]).
## One number in the config still means that window at every horizon.
ENV_WINDOW_Q      <- cfg_get("EXIT_ENV_WINDOW_Q", c("4" = 8L, "12" = 12L, "20" = 20L))
env_window <- function(h) {
  w <- ENV_WINDOW_Q
  if (length(w) == 1L) return(as.integer(w))
  stopifnot(as.character(h) %in% names(w))
  as.integer(w[[as.character(h)]])
}
RAW_WINDOW_Q      <- 8L      # the descriptive "last two years" comparison on the History tab
H_LAB <- setNames(c("1yr", "3yr", "5yr"), as.character(H_SET))
cohort_lab <- qgrid$q_label[N_Q]

## ---------------------------------------------------------------------
## [32.1] T1 -- merger-rate curve by asset size
## ---------------------------------------------------------------------
## Refit the size curve on all usable history (as 30 did) and read it at
## a grid of asset levels. The environment factor is the same one 30 used.
env_all_at <- function(w) {            # system-wide factor over a window of w quarters
  us <- feat$usable_h4 & feat$q_index <= N_Q - 4L
  e1 <- feat$exit_h4[us]; q1 <- feat$q_index[us]
  recent <- e1[q1 > N_Q - 4L - w]
  min(max(mean(recent) / mean(e1), 0.5), 2)
}
env_factor_now <- env_all_at(RAW_WINDOW_Q)                       # the two-year figure quoted on History
env_all_h <- setNames(sapply(H_SET, function(h) env_all_at(env_window(h))), as.character(H_SET))
cat(sprintf("System-wide merger-environment factor at %s: %.2f over two years; by horizon window %s\n",
            cohort_lab, env_factor_now, paste(sprintf("%.2f", env_all_h), collapse = " / ")))
## 30 saves the cohort-date environment BY HORIZON since 2026-09-25a (lists
## named "4", "12", "20"); before that, one set. Read either.
env_pick <- function(what, h) {
  x <- env_now[[what]]
  if (is.null(x)) return(NULL)
  if (is.data.frame(x) || !is.list(x)) {
    if (what == "factor_all" && length(x) > 1) x[[as.character(h)]] else x
  } else x[[as.character(h)]]
}
WINS <- sapply(H_SET, env_window)
WIN_WORDS <- if (length(unique(WINS)) == 1L) sprintf("the last %g years", WINS[1] / 4) else
  paste0("a window as long as the forecast reaches -- ",
         paste(sprintf("the last %g years for the %s counts", WINS / 4, H_LAB), collapse = ", "))

## The factor the CHOSEN model applies, as a function of y (standardised
## log assets) and of category, so the 'Current' columns below reproduce
## the published counts. size_env2 bends the factor with size; cat_env2
## uses a shrunk factor per category; everything else one aggregate number.
env_fn <- function(y, cat_k, h) {
  if (!(EXIT_MODEL %in% ENV_MODELS)) return(rep(1, length(y)))
  cv <- env_pick("curve", h); fk <- env_pick("factor_cat", h)
  if (EXIT_MODEL == "size_env2" && !is.null(cv)) approx(cv$y, cv$factor, xout = y, rule = 2)$y
  else if (EXIT_MODEL == "cat_env2" && !is.null(fk)) fk[cat_k]
  else rep(env_all_h[[as.character(h)]], length(y))
}
ENV_DESC <- if (!(EXIT_MODEL %in% ENV_MODELS)) "1.00 (the chosen exit model applies no merger-environment factor)" else switch(EXIT_MODEL,
  size_env2 = "a merger-environment factor that varies with size (recent one-year rate over long-run, as a smooth curve in assets)",
  cat_env2  = sprintf("a merger-environment factor specific to each asset category: its recent one-year rate over its long-run rate, pulled toward the system-wide figure, 'recent' being %s", WIN_WORDS),
  sprintf("the system-wide merger-environment factor (%s by horizon): the recent one-year rate divided by the long-run average, 'recent' being %s",
          paste(sprintf("%.2f", env_all_h), collapse = " / "), WIN_WORDS))

grid_usd <- c(1e6, 2e6, 5e6, 10e6, 25e6, 50e6, 100e6, 250e6, 500e6, 1e9, 2.5e9, 5e9, 10e9)
## The asset category each grid size falls in (24's rule), so a category
## factor can be looked up for it. Until 2026-09-25c the grid passed
## cat_k = NA, and under cat_env2 every 'Current' cell came out NA.
grid_cat <- pmin(pmax(findInterval(grid_usd / ASSET_SCALE, BREAKS[-1]) + 1L, 1L), N_CAT)
y_scale  <- function(a) {   # feat$y is standardised log assets; recover the mapping from y_raw
  fit <- lm(y ~ y_raw, data = feat[sample(nrow(feat), min(50000L, nrow(feat))), ])
  predict(fit, newdata = data.frame(y_raw = log(a)))
}
T1 <- data.frame(`Assets` = paste0("$", format(grid_usd / 1e6, big.mark = ",", trim = TRUE), "M"),
                 check.names = FALSE)
for (h in H_SET) {
  us <- feat[[paste0("usable_h", h)]]
  d  <- feat[us, ]; d$ex <- d[[paste0("exit_h", h)]]
  m  <- glm(ex ~ ns(y, df = 5), data = d, family = binomial())
  p  <- predict(m, newdata = data.frame(y = y_scale(grid_usd)), type = "response")
  T1[[paste0("Long-run ", H_LAB[as.character(h)], " (%)")]] <- round(100 * p, 1)
  T1[[paste0("Current ", H_LAB[as.character(h)], " (%)")]]  <-
    round(100 * pmin(p * env_fn(y_scale(grid_usd), grid_cat, h), 1), 1)
}
T1[["Current, per year (%)"]] <- round(100 * (1 - (1 - T1[["Current 5yr (%)"]] / 100)^(1/5)), 2)
cat("\nT1 -- merger rate by asset size (share exiting within the horizon):\n")
print(T1, row.names = FALSE)

## ---- T1b: the same curve summarised by the seven asset categories ----
## The category rate is the average of the size curve over the institutions
## in that category today, so it is consistent with the grid above: a
## category is a mix of sizes, and its rate is the mean of its members'.
fc_now <- feat %>% filter(q_index == N_Q) %>%
  semi_join(fc %>% select(join_number), by = "join_number") %>%
  select(join_number, y, cat_k)
T1b <- data.frame(Category = CAT_PRETTY[CAT_LABELS], Institutions = 0L,
                  check.names = FALSE, stringsAsFactors = FALSE)
## asset_cat_now holds the LABELS (A1_LT10M ...), so the levels are
## CAT_LABELS. (Until 2026-09-25c they were 1..7 and every count was 0.)
T1b$Institutions <- as.integer(table(factor(inst_out$asset_cat_now[inst_out$join_number %in% fc$join_number],
                                            levels = CAT_LABELS)))
fc_k <- factor(fc$cat_k, levels = seq_len(N_CAT))
stopifnot(identical(T1b$Institutions, as.integer(table(fc_k))))
## Category rate with thin-pool borrowing -- the same rule as cat_rate() in 30.
cat_rate_k <- function(d) {
  f <- factor(d$cat_k, levels = seq_len(N_CAT))
  r <- tapply(d$ex, f, mean); n <- tapply(d$ex, f, length); n[is.na(n)] <- 0
  for (k in seq_len(N_CAT)) if (n[k] < MIN_EXIT_POOL_32) {
    src <- which(n[seq_len(k - 1)] >= MIN_EXIT_POOL_32)
    if (length(src)) r[k] <- r[max(src)]
  }
  as.numeric(r)
}
## 'Current' is the rate AS APPLIED, whatever the model: the average of the
## cohort's exit probabilities in each category, so institutions x rate
## reproduces the expected exits below and in the growth workbook.
## 'Long-run' is what that rate starts from: the category rate for the
## category models, the size curve averaged over today's members otherwise.
for (h in H_SET) {
  us <- feat[[paste0("usable_h", h)]]
  d  <- feat[us, ]; d$ex <- d[[paste0("exit_h", h)]]
  if (CAT_FAMILY) {
    lr <- cat_rate_k(d)
  } else {
    m  <- glm(ex ~ ns(y, df = 5), data = d, family = binomial())
    p  <- predict(m, newdata = fc_now, type = "response")
    lr <- tapply(p, factor(fc_now$cat_k, levels = seq_len(N_CAT)), mean)
  }
  cu <- tapply(P5[[as.character(h)]], fc_k, mean, na.rm = TRUE)
  if (!exists("RATE_KEEP")) RATE_KEEP <- list()
  RATE_KEEP[[as.character(h)]] <- list(long_run = as.numeric(lr), current = as.numeric(cu))   # unrounded, for [32.2b]
  T1b[[paste0("Long-run ", H_LAB[as.character(h)], " (%)")]] <- round(100 * as.numeric(lr), 1)
  T1b[[paste0("Current ", H_LAB[as.character(h)], " (%)")]]  <- round(100 * as.numeric(cu), 1)
}
T1b[["Current, per year (%)"]] <- round(100 * (1 - (1 - T1b[["Current 5yr (%)"]] / 100)^(1/5)), 2)
for (h in H_SET)                       # the factor applied in each category, horizon by horizon
  T1b[[paste0("Factor ", H_LAB[as.character(h)])]] <-
    round(as.numeric(tapply(env_fn(fc_now$y, fc_now$cat_k, h),
                            factor(fc_now$cat_k, levels = seq_len(N_CAT)), mean)), 2)
## Institutions still operating after five years (whatever their size by
## then), from today's count and each five-year rate.
T1b[["Still operating in 5 yrs (long-run)"]] <- round(T1b$Institutions * (1 - T1b[["Long-run 5yr (%)"]] / 100))
## 'current': today's count less the expected exits from that category,
## the exits rounded to whole institutions so they add to the total (the
## same largest-remainder rule 27 uses for its exits-by-category block).
.ex5 <- as.numeric(tapply(P5[["20"]], fc_k, sum, na.rm = TRUE)); .ex5[is.na(.ex5)] <- 0
.tot <- as.integer(round(sum(.ex5))); .fl <- floor(.ex5); .k <- .tot - sum(.fl)
if (.k > 0) { .o <- order(.ex5 - .fl, decreasing = TRUE)[seq_len(.k)]; .fl[.o] <- .fl[.o] + 1 }
T1b[["Still operating in 5 yrs (current)"]]  <- T1b$Institutions - as.integer(.fl)
rm(.ex5, .tot, .fl, .k)
## Realised history alongside, for the reader who wants the raw record:
## the share of institutions in each category since 2005 that exited
## within five years. For the category models that IS the long-run
## five-year column, so it is shown only for the others.
if (!CAT_FAMILY) {
  hist5 <- feat %>% filter(usable_h20) %>% group_by(cat_k) %>%
    summarise(r = 100 * mean(exit_h20), .groups = "drop")
  T1b[["Realised 5yr since 2005 (%)"]] <- round(hist5$r[match(seq_len(N_CAT), hist5$cat_k)], 1)
}
tot <- T1b[1, ]; tot[] <- NA; tot$Category <- "All institutions"
tot$Institutions <- sum(T1b$Institutions)
for (v in c("Still operating in 5 yrs (long-run)", "Still operating in 5 yrs (current)")) tot[[v]] <- sum(T1b[[v]])
tot[["Long-run 5yr (%)"]] <- round(100 * (1 - tot[["Still operating in 5 yrs (long-run)"]] / tot$Institutions), 1)
tot[["Current 5yr (%)"]]  <- round(100 * (1 - tot[["Still operating in 5 yrs (current)"]]  / tot$Institutions), 1)
tot[["Current, per year (%)"]] <- round(100 * (1 - (1 - tot[["Current 5yr (%)"]] / 100)^(1/5)), 2)
T1b <- rbind(T1b, tot)
cat("\nT1b -- merger rate by asset category:\n")
print(T1b, row.names = FALSE)

## ---------------------------------------------------------------------
## [32.2] T2 -- the historical record and the environment factor
## ---------------------------------------------------------------------
## One-, three- and five-year rates by origin year. Longer horizons are
## only available for origins whose window has closed: three-year to
## END_Y - 3, five-year to END_Y - 5. Institutions counted are those with
## a usable one-year window (the widest set).
rate_by_year <- function(h) {
  feat %>% filter(.data[[paste0("usable_h", h)]]) %>%
    mutate(year = START_YEAR + (q_index - 1) %/% 4) %>%
    group_by(year) %>%
    summarise(r = round(100 * mean(.data[[paste0("exit_h", h)]]), 2), .groups = "drop")
}
T2 <- feat %>% filter(usable_h4) %>%
  mutate(year = START_YEAR + (q_index - 1) %/% 4) %>%
  group_by(year) %>%
  summarise(institutions = n_distinct(join_number), .groups = "drop") %>%
  filter(year < END_Y) %>%
  left_join(rate_by_year(4)  %>% rename(`1-yr rate (%)` = r), by = "year") %>%
  left_join(rate_by_year(12) %>% rename(`3-yr rate (%)` = r), by = "year") %>%
  left_join(rate_by_year(20) %>% rename(`5-yr rate (%)` = r), by = "year") %>%
  mutate(year = as.character(year)) %>%   # text, so Excel does not show 2,007
  rename(Year = year, Institutions = institutions)
longrun    <- round(100 * mean(feat$exit_h4[feat$usable_h4]), 2)
longrun_3  <- round(100 * mean(feat$exit_h12[feat$usable_h12]), 2)
longrun_5  <- round(100 * mean(feat$exit_h20[feat$usable_h20]), 2)
cat(sprintf("\nT2 -- exit rate by origin year (long-run 1/3/5 yr: %.2f / %.2f / %.2f %%):\n",
            longrun, longrun_3, longrun_5))
print(as.data.frame(T2), row.names = FALSE)

## ---- T2b: is the stable aggregate hiding movement within categories? ----
## The environment factor compares the AGGREGATE one-year rate, recent vs
## long-run. The population has shifted toward larger institutions (which
## merge less) while mid-sized rates rose; those can cancel in the total.
## Here the same comparison is made within each category. Factors near 1
## everywhere mean the aggregate factor is adequate; a spread means the
## correction belongs at the category level.
us1   <- feat$usable_h4 & feat$q_index <= N_Q - 4L
rec1  <- us1 & feat$q_index > N_Q - 4L - RAW_WINDOW_Q
T2b <- data.frame(Category = CAT_PRETTY[CAT_LABELS], stringsAsFactors = FALSE, check.names = FALSE)
lr_c  <- tapply(feat$exit_h4[us1],  factor(feat$cat_k[us1],  levels = seq_len(N_CAT)), mean)
re_c  <- tapply(feat$exit_h4[rec1], factor(feat$cat_k[rec1], levels = seq_len(N_CAT)), mean)
n_re  <- tapply(feat$exit_h4[rec1], factor(feat$cat_k[rec1], levels = seq_len(N_CAT)), length)
T2b[["Long-run 1-yr rate (%)"]]    <- round(100 * as.numeric(lr_c), 2)
T2b[["Last 2 years 1-yr rate (%)"]] <- round(100 * as.numeric(re_c), 2)
T2b[["Institution-quarters, last 2 years"]] <- as.integer(n_re)
T2b[["Factor (recent / long-run)"]] <- round(as.numeric(re_c) / as.numeric(lr_c), 2)
T2b <- rbind(T2b, data.frame(Category = "All institutions",
                             `Long-run 1-yr rate (%)` = longrun,
                             `Last 2 years 1-yr rate (%)` = round(100 * mean(feat$exit_h4[rec1]), 2),
                             `Institution-quarters, last 2 years` = sum(rec1),
                             `Factor (recent / long-run)` = round(env_factor_now, 2),
                             check.names = FALSE))
## Under cat_env2 the forecast applies these category factors SHRUNK toward
## the system-wide figure; show them next to the raw ones so the reader can
## get from 2.5 (raw, thin category) to 1.8 (applied).
if (EXIT_MODEL == "cat_env2" && !is.null(env_now$factor_cat)) {
  if (length(unique(WINS)) > 1L) {
    for (h in H_SET)
      T2b[[sprintf("Applied to %s counts (last %g yrs, pulled toward the total)", H_LAB[as.character(h)], env_window(h) / 4)]] <-
        c(round(env_pick("factor_cat", h), 2), NA)
  } else T2b[["Factor as applied (pulled toward the total)"]] <- c(round(env_pick("factor_cat", H_SET[1]), 2), NA)
}
cat("\nT2b -- merger pace by category, last two years vs long-run:\n")
print(T2b, row.names = FALSE)
print(as.data.frame(T2), row.names = FALSE)


## ---------------------------------------------------------------------
## [32.2b] The five-year rate six ways; "normal years" and how much the
##         dates matter
## ---------------------------------------------------------------------
## Asked for by the field (21 Sep 2026): (1) a long-run rate without the
## abnormal episodes -- the 2008-09 recession and the pandemic; (2) a rate
## "matched to today's economy".
##
## (1) CANNOT be done by dropping five-year windows: of the 66 in the panel
## only about two dozen touch neither episode, and nearly all of those start
## in 2009-2014 -- the post-crisis consolidation wave, the busiest stretch on
## record. One-year windows do not have the problem (59 of 82 are clean under
## the configured dates, spread over 2005-06, 2010-18 and 2021-25). So the
## adjustment is measured on one-year windows and carried to the long-run
## rate exactly as 'Current' is: long-run x (one-year rate in normal years /
## one-year rate in all years), by category, thin categories pulled toward
## the system-wide ratio with the same prior weight as the environment
## factor. Dates come from the config (EXIT_NORMAL_EXCLUDE), fixed from
## outside sources; T2c shows the answer under three other sets of dates.
##
## (2) is NOT built, on purpose: the record holds two or three stretches
## resembling any given mix of rates, inflation and unemployment; a five-
## year rate depends on what happens AFTER the start date; and the merger
## pace has barely moved with the economy. What carries information about
## the present is the recent merger pace by size class -- the Current
## column. In its place, three columns from real history: what happened
## over the latest five years, and the busiest and quietest five-year
## stretches on record (bookends, composition-neutral: each is judged by the
## exits its category rates would produce among TODAY's institutions).
NORMAL_EXCLUDE <- cfg_get("EXIT_NORMAL_EXCLUDE",
                          list(recession_2008 = c("2007Q4", "2010Q2"), pandemic = c("2020Q1", "2021Q2")))
SHRINK_N_32 <- cfg_get("EXIT_ENV_SHRINK_N", 2000)
q_of <- function(lab) {
  i <- match(lab, qgrid$q_label)
  if (is.na(i)) stop("EXIT_NORMAL_EXCLUDE: quarter '", lab, "' is not in the panel (",
                     qgrid$q_label[1], " to ", qgrid$q_label[N_Q], ")")
  i
}
flagged_q <- function(spans) sort(unique(unlist(lapply(spans, function(s) seq(q_of(s[1]), q_of(s[2]))))))
clean_origins <- function(spans, h = 4L) {     # TRUE where the window q+1 .. q+h touches no flagged quarter
  fl <- flagged_q(spans)
  vapply(seq_len(N_Q), function(q) !any((q + 1L):(q + h) %in% fl), NA)
}
span_words <- function(spans) paste(vapply(spans, function(s) paste(s[1], "to", s[2]), ""), collapse = " and ")

## institutions and exits by origin quarter and category, one-year and five-year windows
tab_qk32 <- function(q, k, w = NULL) {
  f <- list(factor(q, levels = seq_len(N_Q)), factor(k, levels = seq_len(N_CAT)))
  m <- if (is.null(w)) table(f[[1]], f[[2]]) else tapply(w, f, sum)
  m <- matrix(as.numeric(m), N_Q, N_CAT); m[is.na(m)] <- 0; m
}
.u4 <- feat$usable_h4; .u20 <- feat$usable_h20
n1 <- tab_qk32(feat$q_index[.u4],  feat$cat_k[.u4]);  e1 <- tab_qk32(feat$q_index[.u4],  feat$cat_k[.u4],  feat$exit_h4[.u4])
n5 <- tab_qk32(feat$q_index[.u20], feat$cat_k[.u20]); e5 <- tab_qk32(feat$q_index[.u20], feat$cat_k[.u20], feat$exit_h20[.u20])
rm(.u4, .u20)

normal_factor <- function(spans) {
  ok    <- clean_origins(spans) & seq_len(N_Q) <= N_Q - 4L
  n_all <- colSums(n1); e_all <- colSums(e1)
  n_cl  <- colSums(n1[ok, , drop = FALSE]); e_cl <- colSums(e1[ok, , drop = FALSE])
  f_all <- (sum(e_cl) / sum(n_cl)) / (sum(e_all) / sum(n_all))
  f_k   <- ifelse(n_cl > 0 & e_all > 0, (e_cl / n_cl) / (e_all / n_all), NA_real_)
  f     <- ifelse(is.finite(f_k), (n_cl * f_k + SHRINK_N_32 * f_all) / (n_cl + SHRINK_N_32), f_all)
  list(kept = sum(ok), of = N_Q - 4L, ok = ok,
       rate_all = 100 * sum(e_all) / sum(n_all), rate_clean = 100 * sum(e_cl) / sum(n_cl),
       f_all = f_all, f_cat = pmin(pmax(f, 0.5), 3), raw_cat = f_k)
}
NF <- normal_factor(NORMAL_EXCLUDE)
cat(sprintf("\nNormal years: leaving out %s keeps %d of %d one-year windows.\n", span_words(NORMAL_EXCLUDE), NF$kept, NF$of))
cat(sprintf("  one-year rate, all years %.2f%%; normal years %.2f%%; ratio %.3f; by category (as applied): %s\n",
            NF$rate_all, NF$rate_clean, NF$f_all, paste(sprintf("%.2f", NF$f_cat), collapse = " / ")))

## ---- the six five-year rates, by category ----
n_today <- T1b$Institutions[seq_len(N_CAT)]
lr5 <- RATE_KEEP[["20"]]$long_run; cu5 <- RATE_KEEP[["20"]]$current
normal5 <- pmin(lr5 * NF$f_cat, 1)
o_last  <- N_Q - 20L                                       # latest start date whose five years are complete
latest5 <- ifelse(n5[o_last, ] > 0, e5[o_last, ] / n5[o_last, ], NA_real_)
## Bookends on the SAME footing as the latest-five-years column: one starting date each, so the
## latest stretch can never read busier than the 'busiest' (it is one of the candidates).
EPISODE_WIDTH <- 1L                                        # quarterly starting dates pooled per episode
blk_rate <- function(o, width = EPISODE_WIDTH) {
  i <- o:(o + width - 1L); n <- colSums(n5[i, , drop = FALSE]); e <- colSums(e5[i, , drop = FALSE])
  ifelse(n > 0, e / n, NA_real_)
}
blk_lab <- function(o) paste0(
  if (EPISODE_WIDTH == 1L) sprintf("the credit unions active at %s, five years on", qgrid$q_label[o])
  else sprintf("starting dates %s to %s", qgrid$q_label[o], qgrid$q_label[o + EPISODE_WIDTH - 1L]),
  if (o + EPISODE_WIDTH - 1L == o_last) " -- the latest five years" else "")
.starts <- seq_len(o_last - EPISODE_WIDTH + 1L); .starts <- .starts[rowSums(n5[.starts, , drop = FALSE]) > 0]
.implied <- vapply(.starts, function(o) sum(n_today * blk_rate(o), na.rm = TRUE), 0)   # exits among TODAY's institutions
o_hi <- .starts[which.max(.implied)]; o_lo <- .starts[which.min(.implied)]
rm(.starts, .implied)

FIVE <- list(lr5, normal5, cu5, latest5, blk_rate(o_hi), blk_rate(o_lo))
names(FIVE) <- c(
  "Long-run, all years (%)",
  sprintf("Long-run, normal years (%%): leaves out %s", span_words(NORMAL_EXCLUDE)),
  "Current, as applied in the growth forecast (%)",
  sprintf("Latest five years, realised: credit unions active at %s, by %s (%%)", qgrid$q_label[o_last], cohort_lab),
  sprintf("Busiest five years on record: %s (%%)", blk_lab(o_hi)),
  sprintf("Quietest five years on record: %s (%%)", blk_lab(o_lo)))
lr_int <- function(x) {                                    # whole numbers that add to the rounded total
  x[!is.finite(x)] <- 0; tot <- as.integer(round(sum(x))); fl <- floor(x); k <- tot - sum(fl)
  if (k > 0) { o <- order(x - fl, decreasing = TRUE)[seq_len(k)]; fl[o] <- fl[o] + 1 }
  as.integer(fl)
}
T7  <- data.frame(Category = c(CAT_PRETTY[CAT_LABELS], "All institutions (today's mix)"),
                  `Institutions today` = c(n_today, sum(n_today)), check.names = FALSE, stringsAsFactors = FALSE)
T7b <- data.frame(Category = c(CAT_PRETTY[CAT_LABELS], "Total"),
                  `Institutions today` = c(n_today, sum(n_today)), check.names = FALSE, stringsAsFactors = FALSE)
for (nm in names(FIVE)) {
  r <- FIVE[[nm]]
  T7[[nm]] <- round(100 * c(r, sum(n_today * r, na.rm = TRUE) / sum(n_today)), 1)
  ex <- if (nm == names(FIVE)[3]) n_today - T1b[["Still operating in 5 yrs (current)"]][seq_len(N_CAT)]   # ties to the With Mergers tab
        else lr_int(n_today * r)
  T7b[[sub(" \\(%\\)", "", nm)]] <- c(as.integer(ex), as.integer(sum(ex)))
}
T7c <- data.frame(Category = c(CAT_PRETTY[CAT_LABELS], "All institutions"), check.names = FALSE, stringsAsFactors = FALSE)
T7c[[sprintf("Active at %s", qgrid$q_label[o_last])]]        <- as.integer(c(n5[o_last, ], sum(n5[o_last, ])))
T7c[[sprintf("Merged or closed by %s", cohort_lab)]]        <- as.integer(c(e5[o_last, ], sum(e5[o_last, ])))
T7c[["Share (%)"]] <- round(100 * c(e5[o_last, ] / pmax(n5[o_last, ], 1), sum(e5[o_last, ]) / sum(n5[o_last, ])), 1)
cat("\nT7 -- the five-year rate six ways (%):\n");  print(setNames(T7,  c("Category", "n", "long_run", "normal_yrs", "current", "latest_5y", "busiest", "quietest")), row.names = FALSE)
cat(sprintf("   busiest: %s | quietest: %s\n", blk_lab(o_hi), blk_lab(o_lo)))
cat("\nT7b -- exits among today's institutions over five years under each:\n"); print(setNames(T7b, c("Category", "n", "long_run", "normal_yrs", "current", "latest_5y", "busiest", "quietest")), row.names = FALSE)

## ---- T2c: does the normal-years rate hinge on the dates? ----
.rules <- c(list(NORMAL_EXCLUDE),
            list(list(c("2007Q4", "2009Q2"), c("2020Q1", "2020Q2"))),
            list(list(c("2007Q4", "2011Q2"), c("2020Q1", "2022Q2"))),
            list(c(NORMAL_EXCLUDE, list(c("2022Q2", "2023Q4")))))
names(.rules) <- c(paste0("As configured: ", span_words(NORMAL_EXCLUDE)),
                   "Recession quarters only: 2007Q4 to 2009Q2 and 2020Q1 to 2020Q2",
                   "Recessions and the eight quarters after: 2007Q4 to 2011Q2 and 2020Q1 to 2022Q2",
                   "As configured, and the 2022Q2 to 2023Q4 rate shock as well")
T2c <- bind_rows(lapply(names(.rules), function(nm) {
  nf <- normal_factor(.rules[[nm]])
  data.frame(`Periods left out` = nm, `One-year windows kept` = nf$kept, `of` = nf$of,
             `One-year rate, all years (%)` = round(nf$rate_all, 2),
             `One-year rate, normal years (%)` = round(nf$rate_clean, 2),
             `Normal / all` = round(nf$f_all, 3),
             `Five-year exits implied for today's institutions` = as.integer(round(sum(n_today * pmin(lr5 * nf$f_cat, 1), na.rm = TRUE))),
             check.names = FALSE, stringsAsFactors = FALSE)
}))
T2c <- rbind(data.frame(`Periods left out` = "None (long-run, all years)", `One-year windows kept` = NF$of, `of` = NF$of,
                        `One-year rate, all years (%)` = round(NF$rate_all, 2), `One-year rate, normal years (%)` = round(NF$rate_all, 2),
                        `Normal / all` = 1, `Five-year exits implied for today's institutions` = as.integer(round(sum(n_today * lr5, na.rm = TRUE))),
                        check.names = FALSE, stringsAsFactors = FALSE), T2c)
rm(.rules)
cat("\nT2c -- normal-years rate under other sets of dates:\n"); print(T2c, row.names = FALSE)

## which starting years enter the normal-years average, and the factor by category, on the History tables
.yr <- START_YEAR + (seq_len(N_Q) - 1L) %/% 4L
T2[["In the normal-years average?"]] <- vapply(as.integer(T2$Year), function(y) {
  i <- which(.yr == y & seq_len(N_Q) <= N_Q - 4L); k <- sum(NF$ok[i])
  if (!length(i)) "" else if (k == length(i)) "yes" else if (k == 0) "no" else "partly" }, "")
T2b[["Normal years / all years (one-year rate, as applied)"]] <- c(round(NF$f_cat, 2), round(NF$f_all, 2))
rm(.yr)

## ---------------------------------------------------------------------
## [32.3] T3 -- expected exits from the cohort, by category / region / state
## ---------------------------------------------------------------------
coh <- inst_out %>%
  select(join_number, cu_name, region, cu_type, state, assets_now, asset_cat_now)
for (hh in as.character(H_SET)) coh[[paste0("p", hh)]] <- P5[[hh]][match(coh$join_number, fc$join_number)]

exp_tbl <- function(g) {
  coh %>% group_by(across(all_of(g))) %>%
    summarise(Institutions = n(),
              `Expected exits 1yr` = round(sum(p4, na.rm = TRUE), 1),
              `Expected exits 3yr` = round(sum(p12, na.rm = TRUE), 1),
              `Expected exits 5yr` = round(sum(p20, na.rm = TRUE), 1),
              `5yr rate (%)` = round(100 * mean(p20, na.rm = TRUE), 1),
              .groups = "drop")
}
T3_cat <- exp_tbl("asset_cat_now") %>%
  mutate(asset_cat_now = CAT_PRETTY[as.character(asset_cat_now)]) %>% rename(Category = asset_cat_now)
T3_cell <- exp_tbl(c("region", "cu_type")) %>%
  mutate(region = REG_LAB[as.character(region)], cu_type = CT_LAB[as.character(cu_type)]) %>%
  rename(Region = region, Charter = cu_type)
T3_state <- exp_tbl("state") %>% rename(State = state) %>% arrange(desc(`Expected exits 5yr`))
cat("\nT3 -- expected exits by category:\n"); print(as.data.frame(T3_cat), row.names = FALSE)
cat(sprintf("Total expected exits by %s: %.0f of %d (%.1f%%)\n", H_LAB["20"],
            sum(coh$p20, na.rm = TRUE), nrow(coh), 100 * mean(coh$p20, na.rm = TRUE)))

## ---------------------------------------------------------------------
## [32.4] T4 -- who absorbs whom, by asset category
## ---------------------------------------------------------------------
T4 <- NULL
if (exists("flow") || exists("flow_tbl")) {
  acq_events <- panel %>%
    filter(!is.na(join_number_acquired), join_number_acquired > 0) %>%
    transmute(acquirer = join_number, target = join_number_acquired, q_acq = q_index)
  cat_at <- panel %>% select(join_number, q_index, cat_k)
  tgt <- panel %>% group_by(join_number) %>% filter(q_index == max(q_index)) %>% ungroup() %>%
    select(target = join_number, target_cat = cat_k)
  fl <- acq_events %>% inner_join(tgt, by = "target") %>%
    mutate(q_pre = q_acq - 4L) %>%
    left_join(cat_at %>% rename(acquirer = join_number, q_pre = q_index, acquirer_cat = cat_k),
              by = c("acquirer", "q_pre")) %>% filter(!is.na(acquirer_cat))
  T4 <- as.data.frame.matrix(table(CAT_PRETTY[CAT_LABELS[fl$target_cat]],
                                   CAT_PRETTY[CAT_LABELS[fl$acquirer_cat]]))
  T4 <- T4[CAT_PRETTY[CAT_LABELS][CAT_PRETTY[CAT_LABELS] %in% rownames(T4)],
           CAT_PRETTY[CAT_LABELS][CAT_PRETTY[CAT_LABELS] %in% colnames(T4)], drop = FALSE]
  ## Percentages: rows to 100 (who absorbs targets of this size) and
  ## columns to 100 (what acquirers of this size take on). Largest-
  ## remainder rounding so each row / column adds to exactly 100.
  lr100 <- function(x) {
    if (sum(x) == 0) return(rep(0L, length(x)))
    p <- 100 * x / sum(x); fl <- floor(p); k <- 100L - sum(fl)
    if (k > 0) { o <- order(p - fl, decreasing = TRUE)[seq_len(k)]; fl[o] <- fl[o] + 1 }
    as.integer(fl)
  }
  M4 <- as.matrix(T4)
  T4_row <- as.data.frame(t(apply(M4, 1, lr100))); names(T4_row) <- colnames(M4)
  T4_col <- as.data.frame(apply(M4, 2, lr100));    names(T4_col) <- colnames(M4)
  T4_row <- cbind(`Target category` = rownames(M4), T4_row, `Row total` = rowSums(T4_row))
  T4_col <- cbind(`Target category` = rownames(M4), T4_col)
  T4_col <- rbind(T4_col, data.frame(`Target category` = "Column total",
                                     as.list(colSums(T4_col[, -1])), check.names = FALSE))
  rownames(T4_row) <- NULL; rownames(T4_col) <- NULL
  T4 <- cbind(`Target category` = rownames(T4), T4, Total = rowSums(T4))
  rownames(T4) <- NULL
  cat("\nT4 -- mergers since", START_YEAR, ": target category (rows) by acquirer category (cols):\n")
  print(T4, row.names = FALSE)
}

## ---------------------------------------------------------------------
## [32.5] T5 -- consolidation-risk tiers, with their measured hit rates
## ---------------------------------------------------------------------
## Tier = rank of the covariate logit's five-year exit odds WITHIN the
## institution's asset category (so a tier compares like with like):
##   High     top 10%      Elevated  next 15%
##   Typical  middle 50%   Low       bottom 25%
## The logit ranks well (AUC ~0.74) but counts badly, so it is used ONLY
## for ordering. The tier's meaning is its realised exit rate, measured
## by forming the same tiers at 2021Q2 and counting who had exited by
## the cohort date.
rhs_tier <- "y + d_dn + g12 + g20 + vol + hist_len + cat_f + factor(region) + factor(cu_type) + acq_cum + shock_now + shock_trail"
tier_of <- function(p, k) {
  out <- character(length(p))
  for (kk in unique(k)) {
    i <- which(k == kk); r <- rank(-p[i], ties.method = "first") / length(i)
    out[i] <- ifelse(r <= 0.10, "High", ifelse(r <= 0.25, "Elevated", ifelse(r <= 0.75, "Typical", "Low")))
  }
  factor(out, levels = c("High", "Elevated", "Typical", "Low"))
}
for (v in c("region", "cu_type")) if (!is.numeric(feat[[v]]))
  feat[[v]] <- as.numeric(factor(as.character(feat[[v]])))

## backtest: tiers formed at origin N_Q - 20 from data closed before it
o_bt <- N_Q - 20L
tr_bt <- feat[feat$usable_h20 & (feat$q_index + 20L) <= o_bt, ]; tr_bt$ex <- tr_bt$exit_h20
te_bt <- feat[feat$q_index == o_bt & feat$usable_h20, ];       te_bt$ex <- te_bt$exit_h20
m_bt  <- glm(as.formula(paste("ex ~", rhs_tier)), data = tr_bt, family = binomial())
te_bt$tier <- tier_of(predict(m_bt, newdata = te_bt, type = "response"), te_bt$cat_k)
T5_hit <- te_bt %>% group_by(tier) %>%
  summarise(Institutions = n(), `Exited by cohort date` = sum(ex),
            `Realised 5yr exit rate (%)` = round(100 * mean(ex), 1), .groups = "drop") %>%
  rename(Tier = tier)
cat(sprintf("\nT5 -- tiers formed at %s, outcomes by %s:\n", qgrid$q_label[o_bt], cohort_lab))
print(as.data.frame(T5_hit), row.names = FALSE)

## the cohort's tiers, from a model fit on all history
tr_all <- feat[feat$usable_h20, ]; tr_all$ex <- tr_all$exit_h20
m_all  <- glm(as.formula(paste("ex ~", rhs_tier)), data = tr_all, family = binomial())
fc_now <- feat %>% filter(q_index == N_Q) %>% semi_join(fc %>% select(join_number), by = "join_number")
fc_now$tier <- tier_of(predict(m_all, newdata = fc_now, type = "response"), fc_now$cat_k)
coh <- coh %>% left_join(fc_now %>% select(join_number, tier), by = "join_number")
T5_now <- coh %>% group_by(Category = CAT_PRETTY[as.character(asset_cat_now)], tier) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = tier, values_from = n, values_fill = 0)
cat("\nT5 -- cohort institutions by category and tier:\n"); print(as.data.frame(T5_now), row.names = FALSE)

## ---------------------------------------------------------------------
## [32.6] T6 -- restricted named list (off by default)
## ---------------------------------------------------------------------
if (PUBLISH_WATCHLIST) {
  T6 <- coh %>% filter(tier %in% c("High", "Elevated")) %>%
    arrange(tier, region, cu_type, desc(assets_now)) %>%
    transmute(Tier = tier, `Join number` = join_number, `Credit union` = cu_name,
              Region = REG_LAB[as.character(region)], Charter = CT_LAB[as.character(cu_type)],
              State = state, `Assets ($M)` = round(assets_now / 1e6, 4),
              Category = CAT_PRETTY[as.character(asset_cat_now)])
  fn6 <- sprintf("RESTRICTED_consolidation_risk_%s.csv", cohort_lab)
  write.csv(T6, fn6, row.names = FALSE)
  cat("\nRESTRICTED list written:", fn6, "--", nrow(T6), "institutions. Do not circulate without approval.\n")
} else {
  cat("\nNamed list not written (PUBLISH_WATCHLIST = FALSE).\n")
}

## ---------------------------------------------------------------------
## [32.7] Write -- a standalone, formatted workbook
##
## Sources the project's own base-R Excel writer (the only one that works
## on the locked-down machine) rather than borrowing 27's session, so this
## script writes the workbook whether or not 27 has run.
## ---------------------------------------------------------------------
find_src <- function(fn) {
  cand <- c(fn, file.path("..", fn), file.path("S:/Projects/Credit_Union_Growth_Forecast", fn))
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop(fn, " not found. Searched: ", paste(cand, collapse = ", "))
  source(hit)
}
if (!exists("xl_block")) find_src("0_xlsx_helpers.R")
if (!exists("zip_base")) find_src("0b_zip_base.R")      # must follow the helpers

sheet32 <- function(name, title, subtitle = NULL, notes = NULL, blocks = list(),
                    cols = NULL, freeze = NULL) {
  rows <- character(0); r <- 1
  rows <- c(rows, xl_line(title, r, S_TITLE)); r <- r + 1
  if (!is.null(subtitle)) { rows <- c(rows, xl_line(subtitle, r, S_SUB)); r <- r + 1 }
  r <- r + 1
  for (n in notes) { rows <- c(rows, xl_line(n, r, S_NORM)); r <- r + 1 }
  if (length(notes)) r <- r + 1
  for (b in blocks) {
    if (!is.null(b$head)) { rows <- c(rows, xl_line(b$head, r, S_BOLD)); r <- r + 1 }
    bl <- xl_block(b$df, r, col_styles = b$styles)
    rows <- c(rows, bl$xml); r <- bl$next_row + 1
  }
  list(name = name, rows = rows, cols = cols, freeze = freeze, autofilter = NULL)
}
chr <- function(x) { x[] <- lapply(x, function(v) if (is.factor(v)) as.character(v) else v); x }

## ---- Read Me ----------------------------------------------------------
readme <- data.frame(
  Tab = c("Merger rate by size", "History", "Expected exits", "Who absorbs whom", "Risk tiers"),
  `What it shows` = c(
    "The chance that a credit union merges or closes within one, three or five years: by asset category, and by asset size from $1M to $10B. Read your institution's category or size down the first column.",
    "The one-year merger rate for every year since 2007, and the long-run average. This is where the 'current' adjustment on the first tab comes from.",
    "How many of today's institutions are expected to merge or close by each date, by asset category, by region and charter, and by state. Counts, not names.",
    sprintf("Every merger since %d: the size category of the credit union absorbed (rows) against the size category of the acquirer (columns).", START_YEAR),
    "Four consolidation-risk tiers, ranked within each asset category, with the share of each tier that actually merged when the same tiers were formed five years ago."),
  `How to use it` = c(
    "For planning: the 'Current, per year' column is the annual merger rate for institutions of that size.",
    "For context: whether the merger pace right now is above or below normal.",
    "For resource planning by region and state.",
    "For understanding consolidation: who the typical acquirer of a small credit union is.",
    "For prioritisation only: a tier is a frequency, not a prediction about any single institution. No institution is named."),
  check.names = FALSE, stringsAsFactors = FALSE)

## Column styles for the category table BY NAME. (The positional vector
## used until 2026-09-25c formatted 'Realised 5yr' as a whole number and
## 'Still operating (long-run)' with decimals.)
sty_T1b <- ifelse(names(T1b) == "Category", S_NORM,
           ifelse(names(T1b) == "Institutions" | grepl("^Still operating", names(T1b)), S_INT, S_DEC))

readme <- rbind(readme[1, ],
  data.frame(Tab = "Five-year rates",
             `What it shows` = "The five-year merger rate by asset category read six ways: long-run; long-run without the 2008-09 recession and the pandemic; current; what actually happened over the latest five years; and the busiest and quietest five-year stretches on record -- with the number of exits each would mean among today's institutions.",
             `How to use it` = "For judging how much the five-year outlook depends on which period is taken as the guide. The last three columns are real history, not forecasts.",
             check.names = FALSE, stringsAsFactors = FALSE),
  readme[-1, ])

SHm <- list(
  sheet32("Read Me", "Credit union merger tables",
          sprintf("Cohort %s, %s federally insured credit unions. Office of the Chief Economist.",
                  cohort_lab, format(nrow(coh), big.mark = ",")),
          notes = c("These tables describe mergers and closures: how often they happen by size of institution, how many to expect over the next five years and where, who absorbs whom, and which kinds of institutions carry elevated risk.",
                    "Every rate is a frequency from the record of all federally insured credit unions since 2005. 'Exit' means merged into another credit union or closed; closures are under 1% of exits above $10M and about 5% below.",
                    if (CAT_FAMILY) paste0("Companion to the growth forecast workbook: the 'Current' category rates on the first tab are the rates its With Mergers tab applies", if (P5_FROM_26) ", and the expected exits here are the same numbers." else ".")
                    else "Companion to the growth forecast workbook; the population counts there use the rates on the first tab."),
          blocks = list(list(head = "Tabs", df = readme, styles = c(S_BOLD, S_WRAP, S_WRAP))),
          cols = col_widths(list(c(1, 1, 24), c(2, 2, 90), c(3, 3, 60)))),

  sheet32("Merger rate by size", "Merger rate by asset size",
          sprintf("Share of credit unions of each size that merge or close within the horizon. Cohort %s.", cohort_lab),
          notes = c(if (CAT_FAMILY) "'Long-run' is the historical average from every credit union since 2005: in the category table, the share of institutions in that category, at any date, that had merged or closed within the horizon; in the size table, a smooth curve through the same record by asset size."
                    else "'Long-run' is the historical average for institutions of that size, from every credit union since 2005.",
                    sprintf("'Current' scales the long-run rate by %s. See History. A factor of 1.00 means that size class is merging at its long-run pace; 1.50 means half again as fast. The 'Factor' columns show the factor applied in each category at each horizon.", ENV_DESC),
                    "'Current, per year' is the current five-year rate expressed as a constant annual rate.",
                    if (CAT_FAMILY) "The category table carries the rates the growth forecast applies: institutions today times the current rate is the expected number of exits from that category, and it matches the With Mergers tab of the growth workbook. The size table is a guide to how the rate varies with size -- the smooth long-run curve at each asset level, times the current factor of the category that level falls in. It is not what the counts use, and it steps at the category lines because the factor does."
                    else "The category table is the average of the size curve over the institutions in each category today; the size table is the curve itself, so a credit union between two rows sits between their values.",
                    if (!CAT_FAMILY) "'Realised 5yr since 2005' is the raw record: the share of institutions in that category, at any point since 2005, that had exited five years later.",
                    "'Still operating in 5 yrs' is today's count less the expected exits at each five-year rate: the institutions still in the system in five years, whatever their size by then (some will have moved category)."),
          blocks = list(list(head = "By asset category: share exiting within the horizon (%)",
                             df = chr(T1b), styles = sty_T1b),   # styles by column NAME, see above
                        list(head = "By asset size: share exiting within the horizon (%)", df = chr(T1),
                             styles = c(S_NORM, rep(S_DEC, ncol(T1) - 1)))),
          cols = col_widths(list(c(1, 1, 16), c(2, 2, 12), c(3, ncol(T1b), 17))), freeze = list(x = 1, y = 0)),

  sheet32("History", "Merger rate by year",
          sprintf("Long-run averages: 1 year %.2f%%, 3 years %.2f%%, 5 years %.2f%%. System-wide environment factor at %s: %.2f.",
                  longrun, longrun_3, longrun_5, cohort_lab, env_factor_now),
          notes = c("Share of institutions active at the start of each year that had merged or closed one, three and five years later.",
                    sprintf("Three-year rates stop at %d and five-year rates at %d because later windows have not closed yet.", END_Y - 3L, END_Y - 5L),
                    sprintf("'In the normal-years average?' marks the starting years whose one-year windows enter the 'Long-run, normal years' rate on the Five-year rates tab. Left out: %s -- each episode and the year after it, because a merger completes six to twelve months after the trouble that starts it. The third table repeats the calculation with the periods drawn three other ways; if the rows agree, the choice of dates does not matter.", span_words(NORMAL_EXCLUDE)),
                    if (EXIT_MODEL == "cat_env2") "The environment factor compares the most recent two years' one-year rate with the long-run one-year average. The growth forecast does not apply the single system-wide figure: it applies the category factors in the last columns of the second table -- each category's own ratio, pulled toward the system-wide figure in proportion to how few institution-quarters stand behind it -- because the system-wide figure hides what is happening inside the categories. The longer the forecast reaches, the longer the memory of the factor, so a short-lived lull or surge in mergers is not projected five years ahead."
                    else "The environment factor compares the most recent two years' one-year rate with the long-run one-year average and scales the rates on the first tab.",
                    "The second table makes the same comparison within each category. The total can stay flat while categories move in opposite directions: the system has shifted toward larger institutions, which merge less, while mid-sized institutions have merged more often than their long-run rate. A factor above 1 means that category is merging faster than its own history; below 1, slower."),
          blocks = list(list(head = "By origin year", df = chr(T2), styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, rep(S_NORM, ncol(T2) - 5))),
                        list(head = "Merger pace by category: the last two years against the long-run (one-year rates)",
                             df = chr(T2b), styles = c(S_NORM, S_DEC, S_DEC, S_INT, rep(S_DEC, ncol(T2b) - 4))),
                        list(head = "Does the 'normal years' rate hinge on the dates? The same calculation with the abnormal periods drawn four ways",
                             df = chr(T2c), styles = c(S_WRAP, S_INT, S_INT, S_DEC, S_DEC, S_DEC, S_INT))),
          cols = col_widths(list(c(1, 1, 30), c(2, 9, 20)))),

  sheet32("Expected exits", "Expected mergers and closures from today's institutions",
          sprintf("Cohort %s. Total expected by %s: %.0f of %s (%.1f%%).", cohort_lab, H_LAB["20"],
                  sum(coh$p20, na.rm = TRUE), format(nrow(coh), big.mark = ","), 100 * mean(coh$p20, na.rm = TRUE)),
          notes = c(if (CAT_FAMILY) "Each institution carries the current exit rate of its asset category (first tab); the table adds those up within each group. These are expected values rounded to one decimal, not counts of named institutions."
                    else "Each institution's own merger probability (from the rate curve at its assets) added up within each group. These are expected values rounded to one decimal, not counts of named institutions.",
                    if (P5_FROM_26) "The five-year total is the 'Merged or closed' figure on the growth workbook's With Mergers tab: both are built from the same institution-level probabilities.",
                    "'5yr rate' is the group's average five-year probability."),
          blocks = list(list(head = "By asset category", df = chr(T3_cat), styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC)),
                        list(head = "By region and charter", df = chr(T3_cell), styles = c(S_NORM, S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC)),
                        list(head = "By state, largest first", df = chr(T3_state), styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC))),
          cols = col_widths(list(c(1, 2, 20), c(3, 7, 20)))),

  sheet32("Risk tiers", "Consolidation-risk tiers",
          sprintf("Cohort %s. Tiers are formed within each asset category.", cohort_lab),
          notes = c("Each credit union is ranked against the others in its own asset category on a model of five-year exit odds (size, growth over three and five years, volatility, history length, region, charter, acquisition history). High = top 10%, Elevated = next 15%, Typical = middle half, Low = bottom quarter.",
                    sprintf("The first table is what the tiers MEAN: the same tiers were formed at %s and the share that had exited by %s was counted. A tier is a measured frequency, not a prediction about any one institution.", qgrid$q_label[o_bt], cohort_lab),
                    "No institution is named in this workbook."),
          blocks = list(list(head = sprintf("What the tiers mean: formed at %s, exits counted by %s", qgrid$q_label[o_bt], cohort_lab),
                             df = chr(T5_hit), styles = c(S_NORM, S_INT, S_INT, S_DEC)),
                        list(head = "Today's institutions by category and tier", df = chr(T5_now),
                             styles = c(S_NORM, rep(S_INT, ncol(T5_now) - 1)))),
          cols = col_widths(list(c(1, 1, 20), c(2, 8, 18)))))

SHm <- append(SHm, list(
  sheet32("Five-year rates", "The five-year merger rate, six ways",
          sprintf("Share of credit unions in each asset category that merge or close within five years. Cohort %s.", cohort_lab),
          notes = c("Six readings of the same question side by side, so that no single number has to carry the weight. The first three are rates the forecast is built from; the last three are what actually happened over particular five-year stretches. The 'All institutions' figure in every column applies that column's category rates to TODAY's mix of institutions, so the columns can be compared with each other. It is not the rate the whole system experienced at the time, when there were many more small credit unions.",
                    if (CAT_FAMILY) "LONG-RUN, ALL YEARS: the share of credit unions in the category, at any date since 2005, that had merged or closed five years later."
                    else "LONG-RUN, ALL YEARS: the historical five-year rate for institutions of that size, from every credit union since 2005.",
                    sprintf("LONG-RUN, NORMAL YEARS: the same rate with the abnormal periods left out (%s). The periods run a year past the recessions themselves because a merger completes six to twelve months after the trouble that starts it. Five-year windows cannot be screened this way -- nearly every one touches an episode, and the few that do not all begin in the busiest stretch on record -- so the adjustment is measured on one-year windows (%d of %d avoid both periods) and applied to the long-run rate: long-run rate x (one-year rate in normal years / one-year rate in all years), category by category. The History tab shows how much the answer depends on the dates.",
                            span_words(NORMAL_EXCLUDE), NF$kept, NF$of),
                    "CURRENT: the rate the growth forecast applies -- the long-run rate scaled by how fast each size class has been merging lately (first tab and History).",
                    sprintf("LATEST FIVE YEARS, REALISED: what happened to the credit unions that were active at %s -- the share in each category that had merged or closed by %s. The third table gives the counts behind it.", qgrid$q_label[o_last], cohort_lab),
                    sprintf("BUSIEST and QUIETEST FIVE YEARS ON RECORD: of all the five-year stretches since %d, the two whose category rates would produce the most and the fewest exits among today's institutions (%s; %s). Read them as 'if the next five years look like the busiest we have seen' and 'like the quietest': bookends from real history, not forecasts.", START_YEAR, blk_lab(o_hi), blk_lab(o_lo)),
                    "There is deliberately no column 'matched to today's economy'. The record holds only two or three stretches resembling any given mix of interest rates, inflation and unemployment; what happens AFTER a starting date matters more than conditions at the start; and the merger pace has moved little through a financial crisis, zero interest rates, a pandemic and a tightening cycle. What does carry information about the present is the recent merger pace itself, by size class -- which is what the Current column uses.",
                    "The two largest categories rest on a handful of events: one exit among some twenty institutions moves a rate by five points. Read their realised columns as anecdote, not as rates."),
          blocks = list(list(head = "Share merging or closing within five years (%)", df = chr(T7),
                             styles = c(S_NORM, S_INT, rep(S_DEC, ncol(T7) - 2))),
                        list(head = "What each would mean: exits among today's institutions over five years", df = chr(T7b),
                             styles = c(S_NORM, rep(S_INT, ncol(T7b) - 1))),
                        list(head = sprintf("Behind the 'latest five years' column: the credit unions active at %s", qgrid$q_label[o_last]), df = chr(T7c),
                             styles = c(S_NORM, S_INT, S_INT, S_DEC))),
          cols = col_widths(list(c(1, 1, 28), c(2, 2, 12), c(3, 8, 24))), freeze = list(x = 1, y = 0))), after = 2)

if (!is.null(T4))
  SHm[[length(SHm) + 1]] <- sheet32("Who absorbs whom", "Mergers by size of target and acquirer",
          sprintf("All mergers since %d matched to both sides (%s events).", START_YEAR, format(sum(T4$Total), big.mark = ",")),
          notes = c("Rows: the asset category of the credit union that was absorbed, at its last report. Columns: the asset category of the acquirer one year before the merger (measured at the merger, the acquisition itself moves the acquirer up a category).",
                    "Read across a row to see who absorbs institutions of that size; read down a column to see what an acquirer of that size takes on."),
          blocks = list(list(head = "Number of mergers", df = chr(T4), styles = c(S_NORM, rep(S_INT, ncol(T4) - 1))),
                        list(head = "Row shares (%): of targets of this size, the share absorbed by acquirers of each size -- each row adds to 100",
                             df = chr(T4_row), styles = c(S_NORM, rep(S_INT, ncol(T4_row) - 1))),
                        list(head = "Column shares (%): of acquisitions by acquirers of this size, the share that were targets of each size -- each column adds to 100",
                             df = chr(T4_col), styles = c(S_NORM, rep(S_INT, ncol(T4_col) - 1)))),
          cols = col_widths(list(c(1, 1, 20), c(2, ncol(T4), 15))))

OUTm <- sprintf("CU_Merger_Tables_%s.xlsx", cohort_lab)
xlsx_write(SHm, OUTm)
cat("\nWritten:", normalizePath(OUTm), "\n")

saveRDS(list(T1 = T1, T1b = T1b, T2 = T2, T2b = T2b, T3_cat = T3_cat, T3_cell = T3_cell, T3_state = T3_state,
             T4 = T4, T4_row = if (exists("T4_row")) T4_row else NULL,
             T4_col = if (exists("T4_col")) T4_col else NULL, T5_hit = T5_hit, T5_now = T5_now, env_factor_now = env_factor_now,
             T7 = T7, T7b = T7b, T7c = T7c, T2c = T2c, NORMAL_EXCLUDE = NORMAL_EXCLUDE,
             normal_factor = NF[c("kept", "of", "rate_all", "rate_clean", "f_all", "f_cat", "raw_cat")],
             EXIT_MODEL = EXIT_MODEL, SCRIPT32_VERSION = SCRIPT32_VERSION),
        file = "panel_merger_tables.rds")
