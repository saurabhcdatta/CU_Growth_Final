## =====================================================================
## 10_cohort_prep.R  --  Frozen 2026Q2 cohort, asset histories, regressors
##
## New approach (supersedes 1-8):
##   The cohort is fixed at the credit unions active in 2026Q2. Nobody
##   enters, nobody leaves. Each one gets its own ARIMA on log assets with
##   structural-break regressors, and the seven asset buckets are populated
##   by where each institution's projection lands.
##
##   Because the cohort is closed, the total count is constant at every
##   horizon and the buckets only redistribute. That is deliberate: it is
##   what makes the individual and aggregate views reconcile exactly.
##
## Run block by block in RStudio.
## =====================================================================

library(haven)
library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

## *** SET THIS: the 2026Q2 combined file ***
DTA <- "S:/Data/OCE Data/oce do file archive/2026/2026q2/processing datasets/oce current/OCE_combined_2026q2_2000tocurrent.dta"
file.exists(DTA)

START_YEAR <- 2005          # sample start (2001 recession is outside this window)
END_Y <- 2026; END_Q <- 2   # cohort freeze point

## ---------------------------------------------------------------------
## [10.1] Load. Merger/event columns are pulled so we can build the break
## regressors; check their coding at [10.4] before trusting them.
## ---------------------------------------------------------------------
oce <- read_dta(DTA, col_select = c(join_number, cu_name, year, quarter,
                                    cu_type, region, state, assets_tot,
                                    acquiredcu, acquiredcu_ct, join_number_acquired,
                                    mergertype, latest_event_code))

raw <- oce %>%
  mutate(across(c(join_number, year, quarter, cu_type, region, assets_tot,
                  acquiredcu, acquiredcu_ct, join_number_acquired),
                ~ as.numeric(zap_labels(.))),
         cu_name = as.character(cu_name),
         state   = as.character(state))

table(raw$region, useNA = "ifany")
range(raw$year, na.rm = TRUE)

## ---------------------------------------------------------------------
## [10.2] Units check, categories (unchanged from prior work)
## ---------------------------------------------------------------------
print(quantile(raw$assets_tot, c(0, .25, .5, .75, 1), na.rm = TRUE))
ASSET_SCALE <- 1            # dollars per unit of assets_tot -- verify above

BREAKS_USD <- c(-Inf, 10e6, 50e6, 100e6, 500e6, 1e9, 10e9, Inf)
BREAKS     <- BREAKS_USD / ASSET_SCALE
CAT_LABELS <- c("A1_LT10M", "A2_10to50M", "A3_50to100M", "A4_100to500M",
                "A5_500Mto1B", "A6_1Bto10B", "A7_GE10B")
CAT_PRETTY <- c("Under $10M", "$10M - $50M", "$50M - $100M", "$100M - $500M",
                "$500M - $1B", "$1B - $10B", "$10B and over")
names(CAT_PRETTY) <- CAT_LABELS

## ---------------------------------------------------------------------
## [10.3] Freeze the cohort at 2026Q2
## ---------------------------------------------------------------------
panel <- raw %>%
  filter(cu_type %in% c(1, 2), region %in% c(1, 2, 3),
         !is.na(join_number), !is.na(assets_tot), assets_tot > 0,
         year >= START_YEAR,
         year < END_Y | (year == END_Y & quarter <= END_Q)) %>%
  mutate(q_index = (year - START_YEAR) * 4 + quarter) %>%
  arrange(join_number, q_index) %>%
  distinct(join_number, q_index, .keep_all = TRUE)

N_Q <- max(panel$q_index)            # 2026Q2 -> 86
N_Q

cohort <- panel %>%
  filter(q_index == N_Q) %>%
  mutate(asset_cat_now = as.character(cut(assets_tot, breaks = BREAKS,
                                          labels = CAT_LABELS, right = FALSE))) %>%
  select(join_number, cu_name, region, state, cu_type,
         assets_now = assets_tot, asset_cat_now)

nrow(cohort)
cohort %>% count(region, cu_type) %>% as.data.frame()
cohort %>% count(asset_cat_now) %>% as.data.frame()

## Histories restricted to cohort members only
hist <- panel %>% filter(join_number %in% cohort$join_number)

hist_len <- hist %>% count(join_number, name = "n_obs")
summary(hist_len$n_obs)
table(cut(hist_len$n_obs, c(0, 12, 24, 40, 60, 86)))

## ---------------------------------------------------------------------
## [10.4] Merger regressor -- CHECK THIS BLOCK BEFORE PROCEEDING
##
## We want a marker for quarters in which a cohort member ACQUIRED another
## credit union, because that is a permanent level shift in its assets.
## Institutions that were acquired away are not in the cohort, so only the
## acquirer side matters here.
##
## The three candidate columns are inspected below; pick whichever encodes
## "this CU acquired someone this quarter" in your file.
## ---------------------------------------------------------------------
hist %>% summarise(n_acquiredcu_nonzero = sum(acquiredcu > 0, na.rm = TRUE),
                   n_acquiredcu_ct_nonzero = sum(acquiredcu_ct > 0, na.rm = TRUE),
                   n_join_acq_nonmiss = sum(!is.na(join_number_acquired) &
                                              join_number_acquired > 0)) %>%
  as.data.frame()

hist %>% filter(acquiredcu_ct > 0) %>%
  select(join_number, cu_name, year, quarter, acquiredcu, acquiredcu_ct,
         join_number_acquired, mergertype) %>% head(15) %>% as.data.frame()

## Set this to the column that carries it
MERGER_COL <- "acquiredcu_ct"

hist <- hist %>%
  mutate(acq_event = as.numeric(replace_na(.data[[MERGER_COL]], 0) > 0))

sum(hist$acq_event)
hist %>% group_by(join_number) %>% summarise(n_acq = sum(acq_event)) %>%
  count(n_acq) %>% as.data.frame()

## ---------------------------------------------------------------------
## [10.5] Break and event regressors
##
##   acq_cum   cumulative acquisitions to date. A step per event, which on
##             a differenced log series becomes a pulse at the event -- the
##             right shape for a permanent jump in assets.
##   dins      1 from 2008Q4, when share insurance rose to $250k.
##   rec0709   NBER recession, 2007Q4-2009Q2.
##   rec2020   NBER recession, 2020Q1-Q2.
##   rateshock 2022Q2-2023Q4 tightening cycle (optional, see USE_RATESHOCK)
##
## The 2001 recession precedes the 2005 sample start. To include it, set
## START_YEAR to 2000 -- but the earlier data was judged less reliable.
## ---------------------------------------------------------------------
USE_RATESHOCK <- TRUE

qgrid <- tibble::tibble(q_index = 1:N_Q) %>%
  mutate(year = START_YEAR + (q_index - 1) %/% 4,
         quarter = (q_index - 1) %% 4 + 1,
         q_label = paste0(year, "Q", quarter),
         dins      = as.numeric(year > 2008 | (year == 2008 & quarter >= 4)),
         rec0709   = as.numeric((year == 2007 & quarter == 4) |
                                (year == 2008) |
                                (year == 2009 & quarter <= 2)),
         rec2020   = as.numeric(year == 2020 & quarter <= 2),
         rateshock = as.numeric((year == 2022 & quarter >= 2) | year == 2023))

if (!USE_RATESHOCK) qgrid$rateshock <- 0

XREG_COLS <- c("acq_cum", "dins", "rec0709", "rec2020",
               if (USE_RATESHOCK) "rateshock")

as.data.frame(qgrid[c(1, 15, 16, 60, 70, 86), ])

## ---------------------------------------------------------------------
## [10.6] Per-CU series: log assets plus its own regressor matrix
## ---------------------------------------------------------------------
cu_series <- vector("list", nrow(cohort))
names(cu_series) <- as.character(cohort$join_number)

for (i in seq_len(nrow(cohort))) {
  jn <- cohort$join_number[i]
  d  <- hist %>% filter(join_number == jn) %>% arrange(q_index)

  ## Contiguous run ending at 2026Q2 (ignore pre-charter gaps)
  d <- d %>% filter(q_index >= max(setdiff(1:N_Q, d$q_index), 0) + 1)

  qq <- qgrid %>% filter(q_index %in% d$q_index) %>%
    left_join(d %>% select(q_index, acq_event), by = "q_index") %>%
    mutate(acq_event = replace_na(acq_event, 0),
           acq_cum = cumsum(acq_event))

  cu_series[[i]] <- list(
    join_number = jn,
    q_index = d$q_index,
    y = log(d$assets_tot),
    xreg = as.matrix(qq[, XREG_COLS]),
    n = nrow(d))

  if (i %% 500 == 0) cat("built", i, "of", nrow(cohort), "\n")
}

n_obs_vec <- vapply(cu_series, function(s) s$n, 0)
summary(n_obs_vec)

## Future regressor values: no further acquisitions assumed, insurance limit
## stays, no recession or rate shock assumed. Stated on the Method tab.
H_MAX <- 20
future_xreg_base <- data.frame(
  dins = rep(1, H_MAX), rec0709 = 0, rec2020 = 0,
  rateshock = if (USE_RATESHOCK) 0 else NULL)

saveRDS(list(cohort = cohort, hist = hist, qgrid = qgrid, cu_series = cu_series,
             CAT_LABELS = CAT_LABELS, CAT_PRETTY = CAT_PRETTY, BREAKS = BREAKS,
             N_Q = N_Q, START_YEAR = START_YEAR, H_MAX = H_MAX,
             XREG_COLS = XREG_COLS, future_xreg_base = future_xreg_base,
             n_obs_vec = n_obs_vec),
        file = "cohort_prep.rds")
