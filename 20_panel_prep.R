## =====================================================================
## 20_panel_prep.R  --  Unfrozen estimation panel, exits, break grid
##
## Probability approach (parallel track to 10-16, does not replace them
## until 25 has scored both).
##
##   Scripts 10-16 freeze the cohort at 2026Q2 and project each member's
##   assets. That design cannot produce exits, so the total is constant by
##   construction, and it cannot learn from institutions that merged away,
##   so the estimated growth distribution is survivorship-biased.
##
##   This builds the panel the probability model is FITTED on: every
##   credit union alive in each quarter from 2005Q1, including the ones
##   that later disappeared, with the quarter and type of their exit coded.
##   The 2026Q2 cohort is still carried through, because that is the set we
##   FORECAST. Fit on everyone, forecast the survivors.
##
## CLOSED COHORT (stakeholder decision, Sept 2026)
##   The published total is held fixed at the 4,202 institutions active in
##   2026Q2. No exits, no mergers, for simplicity. Exit coding stays in
##   this script for three reasons that have nothing to do with publishing
##   exits:
##     1. it separates real disappearances from institutions that merely
##        left the region/charter filter, which is a data-quality question
##        either way and is what [20.5] exists for;
##     2. it tells script 21 which origins have an OBSERVABLE outcome, so
##        a merged-away institution is not read as a data hole;
##     3. it keeps pre-exit history in the estimation sample, which is what
##        removes the survivorship bias the frozen-cohort track carries.
##   Only the FORECAST is closed. The FIT still uses everyone.
##
## Run block by block in RStudio.
## =====================================================================

library(haven)
library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

DTA <- "S:/Data/OCE Data/oce do file archive/2026/2026q2/processing datasets/oce current/OCE_combined_2026q2_2000tocurrent.dta"
file.exists(DTA)

START_YEAR <- 2005
END_Y <- 2026; END_Q <- 2

## Hold the published total fixed at the 2026Q2 count. Set FALSE to
## re-enable the exit hazard in 22 and 23; nothing else has to change.
CLOSED_COHORT <- TRUE

## ---------------------------------------------------------------------
## [20.1] Load. Same columns as 10.1 -- the exit coding at [20.5] needs
## join_number_acquired and latest_event_code, so pull them here.
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
         state   = as.character(state),
         mergertype       = as.numeric(zap_labels(mergertype)),
         latest_event_code = as.numeric(zap_labels(latest_event_code)))

table(raw$region, useNA = "ifany")
range(raw$year, na.rm = TRUE)

## ---------------------------------------------------------------------
## [20.2] Units check and categories -- identical to 10.2 on purpose.
## Both tracks must bin on exactly the same edges or 25 cannot compare
## them.
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
N_CAT <- length(CAT_LABELS)

## Log boundary edges, used everywhere downstream. First is -Inf, last +Inf.
LOG_EDGE <- c(-Inf, log(BREAKS[2:(N_CAT)]), Inf)
LOG_EDGE

## ---------------------------------------------------------------------
## [20.3] The full quarterly grid, 2005Q1 to the freeze point
## ---------------------------------------------------------------------
qi <- function(y, q) (y - START_YEAR) * 4 + q
N_Q <- qi(END_Y, END_Q)
N_Q

qgrid <- tibble::tibble(q_index = 1:N_Q) %>%
  mutate(year = START_YEAR + (q_index - 1) %/% 4,
         quarter = (q_index - 1) %% 4 + 1,
         q_label = paste0(year, "Q", quarter),
         ## Same break definitions as 10.5. Kept identical so the two
         ## tracks disagree about the model and nothing else.
         dins      = as.numeric(year > 2008 | (year == 2008 & quarter >= 4)),
         rec0709   = as.numeric((year == 2007 & quarter == 4) |
                                (year == 2008) |
                                (year == 2009 & quarter <= 2)),
         rec2020   = as.numeric(year == 2020 & quarter <= 2),
         rateshock = as.numeric((year == 2022 & quarter >= 2) | year == 2023),
         ## Any of the three shock regimes. Used to build the forward-window
         ## share in script 21, which is the scenario lever.
         shock     = pmax(rec0709, rec2020, rateshock))

as.data.frame(qgrid[c(1, 12, 15, 16, 60, 61, 70, 86), ])

## ---------------------------------------------------------------------
## [20.4] The panel: everyone alive in each quarter, not just survivors
##
## Same filters as 10.3 EXCEPT the freeze. Regions 1-3 and cu_type 1/2 are
## applied row by row, so a credit union that changed region or charter
## type mid-sample leaves a hole. [20.5] separates those holes from real
## exits -- do not skip that check.
## ---------------------------------------------------------------------
panel <- raw %>%
  filter(cu_type %in% c(1, 2), region %in% c(1, 2, 3),
         !is.na(join_number), !is.na(assets_tot), assets_tot > 0,
         year >= START_YEAR,
         year < END_Y | (year == END_Y & quarter <= END_Q)) %>%
  mutate(q_index = qi(year, quarter)) %>%
  arrange(join_number, q_index) %>%
  distinct(join_number, q_index, .keep_all = TRUE)

n_distinct(panel$join_number)                 # everyone ever seen
panel %>% count(q_index) %>% tail(4)          # ~4,200 alive at the end

## Contiguous spell: keep the LAST unbroken run for each credit union.
## Pre-charter gaps and mid-sample filter holes are dropped, not bridged --
## a lagged growth rate across a three-year hole is not a growth rate.
panel <- panel %>%
  group_by(join_number) %>%
  arrange(q_index, .by_group = TRUE) %>%
  mutate(gap = c(0, diff(q_index)) > 1,
         spell = cumsum(gap)) %>%
  filter(spell == max(spell)) %>%
  select(-gap, -spell) %>%
  ungroup()

## How much did that cost
spell_len <- panel %>% count(join_number, name = "n_obs")
summary(spell_len$n_obs)
table(cut(spell_len$n_obs, c(0, 8, 12, 24, 40, 60, N_Q)))

## ---------------------------------------------------------------------
## [20.5] EXIT CODING -- CHECK THIS BLOCK BEFORE PROCEEDING
##
## Still required under CLOSED_COHORT. The published counts do not use
## exits, but the ESTIMATION does: without this block a merged-away
## institution's missing forward assets look identical to a missing
## observation, and script 21 would either drop good history or treat a
## merger as an outcome.
##
## This is the block that plays the same role as 10.4 and fails the same
## silent way. Three distinct things end a credit union's spell:
##
##   (a) it merged into another credit union      -> a real exit, modelled
##   (b) it liquidated or otherwise closed        -> a real exit, modelled
##   (c) it left the FILTER, not the industry     -> NOT an exit
##       (changed region, changed charter type, or fell to zero assets)
##
## Miscoding (c) as an exit inflates the estimated exit hazard and nothing
## errors. The test is simple: is the join_number still present in the
## UNFILTERED file after its spell ends?
## ---------------------------------------------------------------------
last_seen <- panel %>%
  group_by(join_number) %>%
  summarise(last_q = max(q_index),
            first_q = min(q_index),
            .groups = "drop")

## Presence in the unfiltered file, so we can tell (c) apart
raw_q <- raw %>%
  filter(!is.na(join_number),
         year >= START_YEAR,
         year < END_Y | (year == END_Y & quarter <= END_Q)) %>%
  mutate(q_index = qi(year, quarter)) %>%
  group_by(join_number) %>%
  summarise(raw_last_q = max(q_index), .groups = "drop")

## Who acquired whom, from the acquirer side. Every join_number appearing
## in join_number_acquired was absorbed by somebody in that quarter.
acquired_away <- raw %>%
  filter(!is.na(join_number_acquired), join_number_acquired > 0,
         year >= START_YEAR) %>%
  mutate(q_index = qi(year, quarter)) %>%
  group_by(join_number = join_number_acquired) %>%
  summarise(acq_away_q = min(q_index), .groups = "drop")

nrow(acquired_away)

exits <- last_seen %>%
  left_join(raw_q, by = "join_number") %>%
  left_join(acquired_away, by = "join_number") %>%
  mutate(
    censored     = last_q == N_Q,                       # still alive at freeze
    still_in_raw = !censored & raw_last_q > last_q,     # case (c)
    ## Merged if the acquirer side names it within a quarter or two of the
    ## spell ending. The window is deliberately loose: the acquirer books
    ## the event when it books it, not always the same quarter.
    merged = !censored & !is.na(acq_away_q) &
             acq_away_q >= last_q & acq_away_q <= last_q + 3,
    exit_type = case_when(
      censored     ~ "active",
      still_in_raw ~ "filter",        # left the filter, not the industry
      merged       ~ "merged",
      TRUE         ~ "closed_other"),
    exit_q = ifelse(exit_type %in% c("merged", "closed_other"), last_q + 1, NA))

table(exits$exit_type)

## ---- the three things to eyeball before going on --------------------

## 1. How many "closed_other" are there? Liquidations are RARE. If this is
##    a big number relative to merged, the acquirer-side match is failing
##    and the window above needs widening, or MERGER_COL is wrong.
exits %>% count(exit_type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>% as.data.frame()

## 2. Do the closed_other cases carry a merger code on their own last row?
##    If most of them do, they ARE mergers the acquirer side missed.
last_rows <- panel %>%
  inner_join(last_seen, by = "join_number") %>%
  filter(q_index == last_q) %>%
  select(join_number, cu_name, last_q, assets_tot, mergertype, latest_event_code)

last_rows %>%
  inner_join(exits %>% select(join_number, exit_type), by = "join_number") %>%
  filter(exit_type == "closed_other") %>%
  count(latest_event_code, mergertype) %>% arrange(desc(n)) %>%
  head(15) %>% as.data.frame()

## If the table above shows merger codes dominating, fold them in:
## MERGER_EVENT_CODES <- c( ... fill from the table ... )
## exits <- exits %>% mutate(
##   exit_type = ifelse(exit_type == "closed_other" &
##                      join_number %in% last_rows$join_number[
##                        last_rows$latest_event_code %in% MERGER_EVENT_CODES],
##                      "merged", exit_type))
## table(exits$exit_type)

## 3. Exits by year. Should be a few hundred a year and trending down --
##    NCUA charter counts fall roughly 3-4% a year, almost all by merger.
exits %>% filter(!is.na(exit_q)) %>%
  mutate(year = START_YEAR + (exit_q - 1) %/% 4) %>%
  count(year, exit_type) %>%
  pivot_wider(names_from = exit_type, values_from = n, values_fill = 0) %>%
  as.data.frame()

## Institutions we will NOT treat as exits, for the record
sum(exits$exit_type == "filter")

## ---- what the closed-cohort assumption is worth, in numbers ----------
##
## Print the realised exit rate over the last five years. This is the size
## of the thing being assumed away, and it belongs on the Method tab
## verbatim rather than as "mergers are outside the workbook by design".
## At roughly 3-4% a year it compounds to a fifth of the cohort over the
## five-year horizon.
recent_exits <- exits %>%
  filter(!is.na(exit_q), exit_q > N_Q - 20) %>% nrow()
alive_5y_ago <- panel %>% filter(q_index == N_Q - 20) %>% nrow()

cat("\nCLOSED COHORT assumption, for the Method tab:\n")
cat("  Alive 5 years ago:      ", alive_5y_ago, "\n")
cat("  Exited since:           ", recent_exits,
    sprintf(" (%.1f%% of the base, ~%.1f%% a year)\n",
            100 * recent_exits / alive_5y_ago,
            100 * (1 - (1 - recent_exits / alive_5y_ago)^(1/5))))
cat("  Published total is held fixed at the 2026Q2 count regardless.\n")

## ---------------------------------------------------------------------
## [20.6] Acquirer-side events -- unchanged logic from 10.4
## ---------------------------------------------------------------------
panel %>% summarise(n_acquiredcu_nonzero = sum(acquiredcu > 0, na.rm = TRUE),
                    n_acquiredcu_ct_nonzero = sum(acquiredcu_ct > 0, na.rm = TRUE),
                    n_join_acq_nonmiss = sum(!is.na(join_number_acquired) &
                                               join_number_acquired > 0)) %>%
  as.data.frame()

MERGER_COL <- "acquiredcu_ct"          # same column as 10.4

panel <- panel %>%
  mutate(acq_event = as.numeric(replace_na(.data[[MERGER_COL]], 0) > 0))

sum(panel$acq_event)
panel %>% group_by(join_number) %>% summarise(n_acq = sum(acq_event)) %>%
  count(n_acq) %>% as.data.frame()

## ---------------------------------------------------------------------
## [20.7] Attach the exit state and the asset category to every row
##
## exit_h in script 21 is built off exit_q, so it has to travel with the
## panel from here.
## ---------------------------------------------------------------------
panel <- panel %>%
  left_join(exits %>% select(join_number, exit_q, exit_type, first_q, last_q),
            by = "join_number") %>%
  mutate(
    ## Rows from a "filter" institution stay in the panel as history but
    ## must never be counted as an exit, so exit_q is NA for them already.
    y = log(assets_tot),
    cat_k = as.integer(cut(assets_tot, breaks = BREAKS, labels = FALSE,
                           right = FALSE)),
    cat_now = CAT_LABELS[cat_k])

stopifnot(!any(is.na(panel$cat_k)))

panel %>% count(cat_now) %>% as.data.frame()

## ---------------------------------------------------------------------
## [20.8] The forecast set: still the 2026Q2 cohort
##
## Fit on everyone, forecast the survivors. This is deliberately the SAME
## 4,202 institutions the frozen-cohort track publishes, so 25 compares
## like with like.
## ---------------------------------------------------------------------
cohort <- panel %>%
  filter(q_index == N_Q) %>%
  select(join_number, cu_name, region, state, cu_type,
         assets_now = assets_tot, asset_cat_now = cat_now, cat_k_now = cat_k)

nrow(cohort)
cohort %>% count(region, cu_type) %>% as.data.frame()
cohort %>% count(asset_cat_now) %>% as.data.frame()

## Sanity: does this reproduce the frozen-cohort count from script 10?
## Should be 4,202. If it is not, the spell logic at [20.4] differs from
## 10.6's contiguous-run rule and that has to be reconciled first.
cat("Cohort at 2026Q2:", nrow(cohort), " (script 10 had 4,202)\n")

## Estimation panel size
cat("Institutions ever in panel:", n_distinct(panel$join_number), "\n")
cat("Institution-quarters:      ", nrow(panel), "\n")
cat("Real exits observed:       ", sum(!is.na(exits$exit_q)), "\n")

saveRDS(list(panel = panel, cohort = cohort, exits = exits, qgrid = qgrid,
             CLOSED_COHORT = CLOSED_COHORT,
             CAT_LABELS = CAT_LABELS, CAT_PRETTY = CAT_PRETTY,
             BREAKS = BREAKS, LOG_EDGE = LOG_EDGE, N_CAT = N_CAT,
             N_Q = N_Q, START_YEAR = START_YEAR, ASSET_SCALE = ASSET_SCALE),
        file = "panel_prep.rds")
