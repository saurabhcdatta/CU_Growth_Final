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
SCRIPT32_VERSION <- "2026-09-23b"
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

## ---------------------------------------------------------------------
## [32.2] T2 -- the historical record and the environment factor
## ---------------------------------------------------------------------
T2 <- feat %>% filter(usable_h4) %>%
  mutate(year = START_YEAR + (q_index - 1) %/% 4) %>%
  group_by(year) %>%
  summarise(institutions = n_distinct(join_number),
            `1-yr merger rate (%)` = round(100 * mean(exit_h4), 2), .groups = "drop") %>%
  filter(year < END_Y)
longrun <- round(100 * mean(feat$exit_h4[feat$usable_h4]), 2)
cat("\nT2 -- one-year exit rate by origin year (long-run", longrun, "%):\n")
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
## [32.7] Write
## ---------------------------------------------------------------------
notes_T1 <- c("The probability that a credit union of a given size merges or closes within one, three or five years, from every credit union since 2005. 'Long-run' is the historical average; 'Current' scales it by how the last two years compare with that average (the merger-environment factor shown on the next tab). The published population counts use the 'Current' rates.",
              "Closures are under 1% of exits above $10M and about 5% below; these are effectively merger rates.")
notes_T5 <- c("Tiers rank each credit union's modelled exit odds against others in its own asset category: High is the top 10%, Elevated the next 15%, Typical the middle half, Low the bottom quarter. The model uses size, growth over three and five years, volatility, history length, region, charter and acquisition history.",
              "The first table is what the tiers MEAN: the same tiers were formed five years ago and the share that actually exited was counted. A tier is a frequency, not a prediction about any one institution.",
              "No institution is named in this workbook.")

out_ok <- FALSE
if (exists("mk_sheet") && exists("xlsx_write")) {
  ensure <- function(x) { x[] <- lapply(x, function(v) if (is.factor(v)) as.character(v) else v); x }
  SHm <- list(
    mk_sheet("Merger rate by size", "Merger rate by asset size", cohort_lab, notes = notes_T1,
             blocks = list(list(head = "Share of institutions exiting within the horizon (%)",
                                df = ensure(T1), styles = c(S_NORM, rep(S_DEC, ncol(T1) - 1)))),
             cols = col_widths(list(c(1, 1, 14), c(2, ncol(T1), 18)))),
    mk_sheet("History", "One-year merger rate by year",
             sprintf("Long-run average %.2f%%; environment factor at %s: %.2f", longrun, cohort_lab, env_factor_now),
             notes = "The environment factor is the last two years' one-year rate divided by the long-run average; it scales the long-run curve to current conditions.",
             blocks = list(list(head = "Origin year", df = ensure(T2), styles = c(S_INT, S_INT, S_DEC))),
             cols = col_widths(list(c(1, 3, 20)))),
    mk_sheet("Expected exits", "Expected exits from the cohort", cohort_lab,
             notes = c("Expected number of today's institutions merging or closing by each date, from the merger-rate curve applied to each institution's own assets. Rounded sums of probabilities; not a count of named institutions."),
             blocks = list(list(head = "By asset category", df = ensure(T3_cat), styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC)),
                           list(head = "By region and charter", df = ensure(T3_cell), styles = c(S_NORM, S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC)),
                           list(head = "By state", df = ensure(T3_state), styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC))),
             cols = col_widths(list(c(1, 2, 22), c(3, 7, 18)))),
    mk_sheet("Risk tiers", "Consolidation-risk tiers", cohort_lab, notes = notes_T5,
             blocks = list(list(head = sprintf("What the tiers mean: tiers formed at %s, exits counted by %s", qgrid$q_label[o_bt], cohort_lab),
                                df = ensure(T5_hit), styles = c(S_NORM, S_INT, S_INT, S_DEC)),
                           list(head = "Cohort institutions by category and tier", df = ensure(T5_now),
                                styles = c(S_NORM, rep(S_INT, ncol(T5_now) - 1)))),
             cols = col_widths(list(c(1, 1, 22), c(2, 8, 16)))))
  if (!is.null(T4))
    SHm[[length(SHm) + 1]] <- mk_sheet("Who absorbs whom", "Mergers by category of target and acquirer",
             sprintf("Merger events since %d matched to both sides; acquirer's category measured a year before the event", START_YEAR),
             notes = "Rows are the target's category when it exited; columns the acquirer's category four quarters before the event (measured at the event, the merger itself moves the acquirer up).",
             blocks = list(list(head = "Number of mergers", df = ensure(T4), styles = c(S_NORM, rep(S_INT, ncol(T4) - 1)))),
             cols = col_widths(list(c(1, 1, 22), c(2, ncol(T4), 14))))
  OUTm <- sprintf("CU_Merger_Tables_%s.xlsx", cohort_lab)
  xlsx_write(SHm, OUTm); out_ok <- TRUE
  cat("\nWritten:", normalizePath(OUTm), "\n")
}
if (!out_ok) {
  dir.create("merger_tables", showWarnings = FALSE)
  for (nm in c("T1", "T2", "T3_cat", "T3_cell", "T3_state", "T5_hit", "T5_now"))
    write.csv(get(nm), file.path("merger_tables", paste0(nm, "_", cohort_lab, ".csv")), row.names = FALSE)
  if (!is.null(T4)) write.csv(T4, file.path("merger_tables", paste0("T4_", cohort_lab, ".csv")), row.names = FALSE)
  cat("\n27's sheet helpers not in session -- tables written as CSV to Data/merger_tables/.\n")
}
saveRDS(list(T1 = T1, T2 = T2, T3_cat = T3_cat, T3_cell = T3_cell, T3_state = T3_state,
             T4 = T4, T5_hit = T5_hit, T5_now = T5_now, env_factor_now = env_factor_now,
             EXIT_MODEL = EXIT_MODEL, SCRIPT32_VERSION = SCRIPT32_VERSION),
        file = "panel_merger_tables.rds")
