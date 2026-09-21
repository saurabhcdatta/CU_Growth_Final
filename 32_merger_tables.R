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
library(dplyr); library(tidyr); library(splines)
SCRIPT32_VERSION <- "2026-09-24e"
cat("32_merger_tables.R version", SCRIPT32_VERSION, "\n")

## ---------------------------------------------------------------------
## [32.0] Objects
## ---------------------------------------------------------------------
if (!exists("feat"))      { fts <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv) }
if (!exists("fc"))        { prb <- readRDS("panel_probs.rds");    list2env(prb, .GlobalEnv) }
if (!exists("inst_out"))  { asg <- readRDS("panel_assign.rds");   list2env(asg, .GlobalEnv) }
if (!exists("P_EXIT_ALT") && file.exists("panel_exit_models.rds")) {
  .m <- readRDS("panel_exit_models.rds"); P_EXIT_ALT <- .m$P_EXIT_ALT; rm(.m)
}
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
cat("Using exit model:", EXIT_MODEL, "\n")

PUBLISH_WATCHLIST <- cfg_get("PUBLISH_WATCHLIST", FALSE)
ENV_WINDOW_Q      <- cfg_get("EXIT_ENV_WINDOW_Q", 8L)
H_LAB <- setNames(c("1yr", "3yr", "5yr"), as.character(H_SET))
cohort_lab <- qgrid$q_label[N_Q]

## ---------------------------------------------------------------------
## [32.1] T1 -- merger-rate curve by asset size
## ---------------------------------------------------------------------
## Refit the size curve on all usable history (as 30 did) and read it at
## a grid of asset levels. The environment factor is the same one 30 used.
env_factor_now <- {
  us <- feat$usable_h4 & feat$q_index <= N_Q - 4L
  e1 <- feat$exit_h4[us]; q1 <- feat$q_index[us]
  recent <- e1[q1 > N_Q - 4L - ENV_WINDOW_Q]
  min(max(mean(recent) / mean(e1), 0.5), 2)
}
cat(sprintf("Merger-environment factor at %s: %.2f (recent one-year rate / long-run)\n",
            cohort_lab, env_factor_now))

grid_usd <- c(1e6, 2e6, 5e6, 10e6, 25e6, 50e6, 100e6, 250e6, 500e6, 1e9, 2.5e9, 5e9, 10e9)
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
  T1[[paste0("Current ", H_LAB[as.character(h)], " (%)")]]  <- round(100 * pmin(p * env_factor_now, 1), 1)
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
T1b$Institutions <- as.integer(table(factor(fc_now$cat_k, levels = seq_len(N_CAT))))
for (h in H_SET) {
  us <- feat[[paste0("usable_h", h)]]
  d  <- feat[us, ]; d$ex <- d[[paste0("exit_h", h)]]
  m  <- glm(ex ~ ns(y, df = 5), data = d, family = binomial())
  p  <- predict(m, newdata = fc_now, type = "response")
  lr <- tapply(p, factor(fc_now$cat_k, levels = seq_len(N_CAT)), mean)
  T1b[[paste0("Long-run ", H_LAB[as.character(h)], " (%)")]] <- round(100 * as.numeric(lr), 1)
  T1b[[paste0("Current ", H_LAB[as.character(h)], " (%)")]]  <- round(100 * pmin(as.numeric(lr) * env_factor_now, 1), 1)
}
T1b[["Current, per year (%)"]] <- round(100 * (1 - (1 - T1b[["Current 5yr (%)"]] / 100)^(1/5)), 2)
## Realised history alongside, for the reader who wants the raw record:
## the share of institutions in each category since 2005 that exited
## within five years (26's category rate, full basis).
hist5 <- feat %>% filter(usable_h20) %>% group_by(cat_k) %>%
  summarise(r = 100 * mean(exit_h20), .groups = "drop")
T1b[["Realised 5yr since 2005 (%)"]] <- round(hist5$r[match(seq_len(N_CAT), hist5$cat_k)], 1)
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
rec1  <- us1 & feat$q_index > N_Q - 4L - ENV_WINDOW_Q
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
cat("\nT2b -- merger pace by category, last two years vs long-run:\n")
print(T2b, row.names = FALSE)
print(as.data.frame(T2), row.names = FALSE)

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
    "The chance that a credit union of a given size merges or closes within one, three or five years, for sizes from $1M to $10B. Read your institution's size down the first column.",
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

SHm <- list(
  sheet32("Read Me", "Credit union merger tables",
          sprintf("Cohort %s, %s federally insured credit unions. Office of the Chief Economist.",
                  cohort_lab, format(nrow(coh), big.mark = ",")),
          notes = c("These tables describe mergers and closures: how often they happen by size of institution, how many to expect over the next five years and where, who absorbs whom, and which kinds of institutions carry elevated risk.",
                    "Every rate is a frequency from the record of all federally insured credit unions since 2005. 'Exit' means merged into another credit union or closed; closures are under 1% of exits above $10M and about 5% below.",
                    "Companion to the growth forecast workbook; the population counts there use the rates on the first tab."),
          blocks = list(list(head = "Tabs", df = readme, styles = c(S_BOLD, S_WRAP, S_WRAP))),
          cols = col_widths(list(c(1, 1, 24), c(2, 2, 90), c(3, 3, 60)))),

  sheet32("Merger rate by size", "Merger rate by asset size",
          sprintf("Share of credit unions of each size that merge or close within the horizon. Cohort %s.", cohort_lab),
          notes = c("'Long-run' is the historical average for institutions of that size, from every credit union since 2005.",
                    sprintf("'Current' scales the long-run rate by the merger-environment factor, %.2f: the last two years' one-year rate divided by the long-run average (see History). A factor of 1.00 means the merger pace is at its long-run normal.", env_factor_now),
                    "'Current, per year' is the current five-year rate expressed as a constant annual rate.",
                    "The category table is the average of the size curve over the institutions in each category today; the size table is the curve itself, so a credit union between two rows sits between their values.",
                    "'Realised 5yr since 2005' is the raw record: the share of institutions in that category, at any point since 2005, that had exited five years later."),
          blocks = list(list(head = "By asset category: share exiting within the horizon (%)",
                             df = chr(T1b), styles = c(S_NORM, S_INT, rep(S_DEC, ncol(T1b) - 2))),
                        list(head = "By asset size: share exiting within the horizon (%)", df = chr(T1),
                             styles = c(S_NORM, rep(S_DEC, ncol(T1) - 1)))),
          cols = col_widths(list(c(1, 1, 16), c(2, 2, 12), c(3, ncol(T1b), 17))), freeze = list(x = 1, y = 0)),

  sheet32("History", "Merger rate by year",
          sprintf("Long-run averages: 1 year %.2f%%, 3 years %.2f%%, 5 years %.2f%%. Environment factor at %s: %.2f.",
                  longrun, longrun_3, longrun_5, cohort_lab, env_factor_now),
          notes = c("Share of institutions active at the start of each year that had merged or closed one, three and five years later.",
                    sprintf("Three-year rates stop at %d and five-year rates at %d because later windows have not closed yet.", END_Y - 3L, END_Y - 5L),
                    "The environment factor compares the most recent two years' one-year rate with the long-run one-year average and scales the rate curve on the first tab.",
                    "The second table makes the same comparison within each category. The total can stay flat while categories move in opposite directions: the system has shifted toward larger institutions, which merge less, while mid-sized institutions have merged more often than their long-run rate. A factor above 1 means that category is merging faster than its own history; below 1, slower."),
          blocks = list(list(head = "By origin year", df = chr(T2), styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC)),
                        list(head = "Merger pace by category: the last two years against the long-run (one-year rates)",
                             df = chr(T2b), styles = c(S_NORM, S_DEC, S_DEC, S_INT, S_DEC))),
          cols = col_widths(list(c(1, 1, 18), c(2, 5, 20)))),

  sheet32("Expected exits", "Expected mergers and closures from today's institutions",
          sprintf("Cohort %s. Total expected by %s: %.0f of %s (%.1f%%).", cohort_lab, H_LAB["20"],
                  sum(coh$p20, na.rm = TRUE), format(nrow(coh), big.mark = ","), 100 * mean(coh$p20, na.rm = TRUE)),
          notes = c("Each institution's own merger probability (from the rate curve at its assets) added up within each group. These are expected values rounded to one decimal, not counts of named institutions.",
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
             EXIT_MODEL = EXIT_MODEL, SCRIPT32_VERSION = SCRIPT32_VERSION),
        file = "panel_merger_tables.rds")
