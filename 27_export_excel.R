## =====================================================================
## 27_export_excel.R  --  The deliverable
##
## Same tab structure as script 14, rebuilt on the probability method.
## Uses 0_xlsx_helpers.R unchanged -- no openxlsx, no Java.
##
## WHAT IS DIFFERENT FROM 14, AND WHY IT MATTERS TO THE READER
##
##   Counts are sums of probabilities, not counts of point forecasts, so
##   they tie to the cohort exactly at every horizon and in every cell.
##   Script 13's top-down correction has no analogue here because there is
##   nothing to correct.
##
##   Every institution carries a probability next to its assigned category.
##   That column is the more informative one and the tabs say so.
##
##   The transition matrices are aggregations of institution-level
##   probabilities, not fitted objects, which is why they can be cut by
##   region and charter without fitting anything separately.
##
## THE THREE CAVEATS THAT MUST SURVIVE INTO THE WORKBOOK. If any of these
## is missing from the Method or Diagnostics tab, the workbook is not
## ready to circulate:
##   1. no mergers -- the total is held fixed and that is a large
##      assumption, roughly 3.6% of institutions a year
##   2. the $10B category is over-counted by about 40% out of sample
##   3. downward movement is under-predicted beyond one year
##
## Run block by block in RStudio.
## =====================================================================

library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")
source("0_xlsx_helpers.R")

## prep <- readRDS("panel_prep.rds");     list2env(prep, .GlobalEnv)
## prb  <- readRDS("panel_probs.rds");    list2env(prb,  .GlobalEnv)
## asg  <- readRDS("panel_assign.rds");   list2env(asg,  .GlobalEnv)
## cvr  <- readRDS("panel_cv.rds");       list2env(cvr,  .GlobalEnv)
## bkt  <- readRDS("panel_backtest.rds"); list2env(bkt,  .GlobalEnv)

stopifnot(exists("counts"), exists("inst_out"), exists("TRANS"),
          exists("cell_counts"), exists("movers"), exists("down_risk"),
          exists("count_tab"), exists("dir_tab"), exists("acc_tab"))

## ---------------------------------------------------------------------
## [27.1] Settings
## ---------------------------------------------------------------------
OUT <- sprintf("CU_Growth_Forecast_%s_probability.xlsx", qgrid$q_label[N_Q])

## cu_type 1 = federal charter, 2 = federally insured state charter.
## VERIFY against the 5300 codebook before circulating -- the labels
## appear on eight tab names and every institution row.
CT_LAB <- c("1" = "FCU", "2" = "FISCU")

REG_LAB <- c("1" = "Region 1", "2" = "Region 2", "3" = "Region 3",
             "8" = "ONES")

fmt_date <- format(Sys.Date(), "%d %B %Y")
horizon_label <- function(h) {
  y <- START_YEAR + (N_Q + h - 1) %/% 4
  q <- (N_Q + h - 1) %% 4 + 1
  paste0(y, "Q", q)
}
H_LAB <- vapply(H_SET, horizon_label, "")
names(H_LAB) <- as.character(H_SET)
H_LAB

## ---------------------------------------------------------------------
## [27.2] Sheet builder
##
## One helper so every tab gets the same furniture: title, subtitle, a
## note block, then the data. Tabs that look alike are read faster.
## ---------------------------------------------------------------------
mk_sheet <- function(name, title, subtitle = NULL, notes = NULL,
                     blocks = list(), cols = NULL, freeze = NULL,
                     autofilter = NULL) {
  rows <- character(0); r <- 1
  rows <- c(rows, xl_line(title, r, S_TITLE)); r <- r + 1
  if (!is.null(subtitle)) { rows <- c(rows, xl_line(subtitle, r, S_SUB)); r <- r + 1 }
  r <- r + 1
  if (length(notes)) {
    for (n in notes) { rows <- c(rows, xl_line(n, r, S_NORM)); r <- r + 1 }
    r <- r + 1
  }
  for (b in blocks) {
    if (!is.null(b$head)) { rows <- c(rows, xl_line(b$head, r, S_BOLD)); r <- r + 1 }
    bl <- xl_block(b$df, r, col_styles = b$styles)
    rows <- c(rows, bl$xml); r <- bl$next_row + 1
  }
  list(name = name, rows = rows, cols = cols, freeze = freeze,
       autofilter = autofilter)
}

SH <- list()

## ---------------------------------------------------------------------
## [27.3] Method
## ---------------------------------------------------------------------
method_notes <- c(
 "WHAT THIS IS",
 sprintf("For each of the %s credit unions active in %s, we estimate the probability of being in each asset category 1, 3 and 5 years out.",
         format(nrow(inst_out), big.mark = ","), qgrid$q_label[N_Q]),
 "Counts are the sum of those probabilities across institutions. They are not counts of point forecasts, which is why they tie to the cohort exactly.",
 "",
 "HOW THE PROBABILITIES ARE PRODUCED",
 "The category edges are fixed dollar amounts, so the probability of landing in a category is the distribution of h-step asset growth read off at those edges.",
 "That distribution is the empirical distribution of growth among historical credit unions in the same asset category. No parametric form, no fitted curve.",
 sprintf("Estimated on %s institution-quarters from %dQ1 to %s, including institutions that later merged away.",
         format(nrow(feat), big.mark = ","), START_YEAR, qgrid$q_label[N_Q]),
 "",
 "THREE ASSUMPTIONS THE READER MUST KNOW",
 "1. NO MERGERS. The total is held fixed at the current count. Mergers have removed roughly 3.6% of credit unions a year, about a sixth over five years. These are not forecasts of how many credit unions will exist; they show where these institutions would land if all survived.",
 "2. THRESHOLDS ARE NOMINAL. The category edges are fixed dollar amounts that have never been indexed. Much of the projected movement is the erosion of those thresholds rather than credit unions changing size. See the Bucket Growth tab.",
 "3. THE LARGEST CATEGORY IS OVER-COUNTED. Out of sample the $10B-and-over count runs about 40% high at five years. See Validation.",
 "",
 "REGIONS",
 "Region 8 is the Office of National Examinations and Supervision, a supervisory office rather than a geography. Its institutions are the largest in the industry and are reported as their own group.",
 "",
 "READING AN INDIVIDUAL ROW",
 "The aggregate counts are considerably more reliable than any single institution's line. Errors across thousands of institutions largely offset; a single credit union near a size threshold is close to a coin flip at five years.",
 "Every institution row carries the probability of its assigned category. Read that column, not just the category.")

settings_tbl <- data.frame(
  Setting = c("Cohort date", "Institutions", "Horizons", "Method",
              "Growth basis", "Price basis", "Bucket calibration",
              "Recency weighting", "Minimum pool", "Produced"),
  Value = c(qgrid$q_label[N_Q], format(nrow(inst_out), big.mark = ","),
            paste(H_LAB, collapse = ", "),
            "Empirical conditional distribution, by asset category",
            GROWTH_BASIS, PRICE_BASIS,
            ifelse(BUCKET_CALIB, "applied", "not applied -- see Validation"),
            ifelse(WEIGHTED, sprintf("half-life %d quarters", HALFLIFE), "none"),
            MIN_POOL, fmt_date),
  stringsAsFactors = FALSE)

SH[[length(SH) + 1]] <- mk_sheet(
  "Method", "Credit Union Growth Forecast",
  sprintf("Asset category projections from %s", qgrid$q_label[N_Q]),
  notes = method_notes,
  blocks = list(list(head = "Settings", df = settings_tbl)),
  cols = col_widths(list(c(1, 1, 26), c(2, 2, 60))))

## ---------------------------------------------------------------------
## [27.4] Total
## ---------------------------------------------------------------------
tot_tbl <- counts %>%
  transmute(Category = pretty, Today = now,
            !!H_LAB[1] := h4, !!H_LAB[2] := h12, !!H_LAB[3] := h20,
            Change = round(h20 - now, 1),
            `Pct change` = round(100 * (h20 - now) / pmax(now, 1), 1))

extra_tbl <- extra %>%
  transmute(Threshold = label, Today = now,
            !!H_LAB[1] := h4, !!H_LAB[2] := h12, !!H_LAB[3] := h20)

SH[[length(SH) + 1]] <- mk_sheet(
  "Total", "Projected counts by asset category",
  sprintf("All %s institutions. Totals are held fixed -- no mergers.",
          format(nrow(inst_out), big.mark = ",")),
  notes = c(
    "Counts are sums of probabilities and tie to the cohort exactly at every horizon.",
    "The supplementary thresholds below OVERLAP the table above -- a $15B institution is also counted in $10B and over. They are not a partition and must not be added to the total.",
    "The $10B-and-over figures should be read as upper estimates; see Validation."),
  blocks = list(
    list(head = "Counts by category", df = tot_tbl,
         styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC, S_DEC)),
    list(head = "Supplementary thresholds (overlapping, not a partition)",
         df = extra_tbl, styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC))),
  cols = col_widths(list(c(1, 1, 22), c(2, 7, 12))))

## ---------------------------------------------------------------------
## [27.5] Bucket Growth -- nominal vs real
## ---------------------------------------------------------------------
growth_tbl <- nominal_vs_real %>%
  transmute(Category = cat, Today = now,
            `5yr nominal` = nominal_h20, `5yr real` = real_h20,
            `Nominal change` = nominal_chg, `Real change` = real_chg,
            `Threshold drift` = threshold_drift)

SH[[length(SH) + 1]] <- mk_sheet(
  "Bucket Growth", "How much of the change is real",
  sprintf("Real column indexes the category edges at %.1f%% a year",
          100 * CPI_ASSUMPTION),
  notes = c(
    "The category edges are fixed dollar amounts and have never been indexed. As credit unions grow with the economy, institutions cross those edges without changing in real terms.",
    "The real column restates the same forecast with the edges rising at the assumed inflation rate. The gap between the two is threshold drift.",
    "The $10B threshold is statutory and is NOT indexed. For supervisory planning the nominal column is the relevant one; the real column explains why the nominal figures look as large as they do.")
  ,
  blocks = list(list(head = "Five-year change, nominal and real", df = growth_tbl,
                     styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC, S_DEC))),
  cols = col_widths(list(c(1, 1, 22), c(2, 7, 15))))

## ---------------------------------------------------------------------
## [27.6] Transition matrices
## ---------------------------------------------------------------------
trans_blocks <- lapply(H_SET, function(h) {
  m <- round(100 * TRANS[[as.character(h)]], 1)
  df <- data.frame(From = CAT_LABELS, m, check.names = FALSE,
                   stringsAsFactors = FALSE)
  list(head = sprintf("%s (%%)", H_LAB[as.character(h)]), df = df,
       styles = c(S_NORM, rep(S_DEC, N_CAT)))
})

SH[[length(SH) + 1]] <- mk_sheet(
  "Transitions", "Category transition probabilities",
  "Row = category today, column = category at the horizon",
  notes = c(
    "Each row is the average probability vector of the institutions starting in that category. It is a summary of institution-level probabilities, not a fitted matrix.",
    "That is why the matrices can be cut by region and charter without estimating anything separately, and why the five-year matrix is NOT the one-year matrix cubed -- each horizon is estimated directly.",
    "Rows sum to 100 because no institution leaves the cohort."),
  blocks = trans_blocks,
  cols = col_widths(list(c(1, 1, 18), c(2, 8, 14))))

## ---------------------------------------------------------------------
## [27.7] Region x charter tabs
## ---------------------------------------------------------------------
cells <- cell_counts %>% distinct(region, cu_type) %>% arrange(region, cu_type)

for (i in seq_len(nrow(cells))) {
  rg <- cells$region[i]; ct <- cells$cu_type[i]
  nm <- paste0("R", rg, "_", CT_LAB[as.character(ct)])

  d <- cell_counts %>% filter(region == rg, cu_type == ct)
  wide <- d %>%
    select(h, cat, now, fcst) %>%
    pivot_wider(names_from = h, values_from = fcst,
                names_prefix = "h") %>%
    transmute(Category = CAT_PRETTY[cat], Today = now,
              !!H_LAB[1] := h4, !!H_LAB[2] := h12, !!H_LAB[3] := h20,
              Change = round(h20 - now, 1))

  mv <- movers %>% filter(h == 20, region == rg, cu_type == ct) %>%
    arrange(desc(assets_now_m)) %>%
    transmute(`Credit union` = cu_name, From = CAT_PRETTY[from],
              To = CAT_PRETTY[to], `Assets ($M)` = assets_now_m,
              `Median 5yr ($M)` = med_m, Probability = p_assign)

  SH[[length(SH) + 1]] <- mk_sheet(
    nm, paste(REG_LAB[as.character(rg)], "-", CT_LAB[as.character(ct)]),
    sprintf("%s institutions",
            format(d$n_now[d$h == 20][1], big.mark = ",")),
    notes = c(
      "Counts tie to this cell's own institution count at every horizon.",
      "The movers list names institutions whose assigned category changes. Assignment is by forecast size and cannot move an institution downward -- see the Down Risk tab for that."),
    blocks = list(
      list(head = "Counts by category", df = wide,
           styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC, S_DEC)),
      list(head = "Institutions changing category by five years", df = mv,
           styles = c(S_NORM, S_NORM, S_NORM, S_DEC, S_DEC, S_DEC))),
    cols = col_widths(list(c(1, 1, 38), c(2, 7, 16))))
}

length(SH)

## ---------------------------------------------------------------------
## [27.8] Institutions -- the full list
## ---------------------------------------------------------------------
inst_tab <- inst_out %>%
  arrange(desc(assets_now)) %>%
  transmute(
    `Join number` = join_number, `Credit union` = cu_name,
    Region = REG_LAB[as.character(region)],
    Charter = CT_LAB[as.character(cu_type)], State = state,
    `Assets ($M)` = round(assets_now / 1e6, 1),
    `Category today` = CAT_PRETTY[asset_cat_now],
    `1yr` = CAT_PRETTY[cat_1y], `P(1yr)` = p_1y,
    `3yr` = CAT_PRETTY[cat_3y], `P(3yr)` = p_3y,
    `5yr` = CAT_PRETTY[cat_5y], `P(5yr)` = p_5y,
    Confidence = as.character(conf_5y),
    `Median 5yr ($M)` = round(assets_med_5y / 1e6, 1),
    `Low 5yr ($M)`    = round(assets_p10_5y / 1e6, 1),
    `High 5yr ($M)`   = round(assets_p90_5y / 1e6, 1),
    `P(up)` = p_up_5y, `P(same)` = p_same_5y, `P(down)` = p_down_5y,
    `Down risk` = ifelse(down_risk_5y, "yes", ""),
    `Short history` = ifelse(short_history, "yes", ""))

SH[[length(SH) + 1]] <- mk_sheet(
  "Institutions", "All institutions",
  sprintf("%s credit unions, sorted by current assets",
          format(nrow(inst_tab), big.mark = ",")),
  notes = c(
    "READ THE PROBABILITY COLUMN. A category assignment with a probability of 0.55 is close to a coin flip; one at 0.97 is close to certain. The assignment alone does not distinguish them.",
    "Low and High are the 10th and 90th percentiles of the forecast asset level.",
    "Down risk marks institutions with elevated probability of falling a category. They are still assigned by forecast size -- see the Down Risk tab.",
    "Short history marks institutions with too little data for their own trailing features; they are forecast from their category's distribution."),
  blocks = list(list(df = inst_tab,
                     styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_NORM,
                                S_INT, S_NORM, S_NORM, S_DEC, S_NORM, S_DEC,
                                S_NORM, S_DEC, S_NORM, S_INT, S_INT, S_INT,
                                S_DEC, S_DEC, S_DEC, S_NORM, S_NORM))),
  cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 5, 12),
                         c(6, 22, 14))),
  freeze = list(x = 2, y = 6),
  autofilter = sprintf("A6:V%d", 6 + nrow(inst_tab)))

## ---------------------------------------------------------------------
## [27.9] Down risk
## ---------------------------------------------------------------------
dr <- down_risk %>% filter(h == 20) %>%
  transmute(`Credit union` = cu_name,
            Region = REG_LAB[as.character(region)],
            Charter = CT_LAB[as.character(cu_type)],
            From = CAT_PRETTY[from], `At risk of` = CAT_PRETTY[at_risk_of],
            `Assets ($M)` = assets_now_m, `Low 5yr ($M)` = p10_m,
            `P(down)` = p_down, `Pool n` = pool_n)

SH[[length(SH) + 1]] <- mk_sheet(
  "Down Risk", "Institutions with elevated downward probability",
  "Five-year horizon",
  notes = c(
    "These institutions are NOT assigned to a lower category. The list says only that their probability of falling a category is elevated relative to peers.",
    "Category assignment is by forecast size, and because assets generally grow, no institution is assigned downward. The count of expected downward moves is published on the Total tab; this names the institutions carrying that risk.",
    sprintf("Expected downward moves at five years: %.1f across the whole cohort.",
            sum(inst_out$p_down_5y)),
    "Pool n is the number of comparable historical cases behind the estimate. A small number means the probability moves in coarse steps."),
  blocks = list(list(df = dr,
                     styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_NORM,
                                S_INT, S_INT, S_DEC, S_INT))),
  cols = col_widths(list(c(1, 1, 38), c(2, 9, 16))))

## ---------------------------------------------------------------------
## [27.10] Large institutions
## ---------------------------------------------------------------------
lg <- above15 %>%
  transmute(`Credit union` = cu_name,
            Region = REG_LAB[as.character(region)],
            Charter = CT_LAB[as.character(cu_type)],
            `Assets ($B)` = assets_now_b, `Median 5yr ($B)` = med_h20_b,
            `P(over $15B)` = p_above_15B, `Pool n` = pool_n)

SH[[length(SH) + 1]] <- mk_sheet(
  "Large Institutions", "Institutions at or approaching the largest thresholds",
  "Five-year horizon, 5% probability floor",
  notes = c(
    "For examination planning. These counts overlap the category table and must not be added to it.",
    "The probability of exceeding a dollar level comes from the same fitted distribution as the category probabilities -- no additional model.",
    "The $10B-and-over category is over-counted by roughly 40% out of sample. Treat these figures as upper estimates and the ordering as more reliable than the level."),
  blocks = list(
    list(head = "Counts above supplementary thresholds", df = extra_tbl,
         styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC)),
    list(head = "Institutions with a 5% or better chance of exceeding $15B",
         df = lg, styles = c(S_NORM, S_NORM, S_NORM, S_DEC, S_DEC, S_DEC, S_INT))),
  cols = col_widths(list(c(1, 1, 38), c(2, 7, 16))))

## ---------------------------------------------------------------------
## [27.11] Validation -- the backtest
## ---------------------------------------------------------------------
val_counts <- count_tab %>% filter(h == 20) %>%
  transmute(Category = CAT_PRETTY[cat], `At origin` = start,
            Predicted = pred, Actual = actual, Error = err, `Pct` = pct)

val_dir <- dir_tab %>%
  transmute(Horizon = H_LAB[as.character(h)],
            `Predicted up` = pred_up, `Actual up` = act_up,
            `Predicted down` = pred_down, `Actual down` = act_down,
            `Up ratio` = up_ratio, `Down ratio` = down_ratio)

val_acc <- acc_tab %>%
  transmute(Horizon = H_LAB[as.character(h)], Institutions = n,
            `Assigned correct (%)` = ranked,
            `Most likely correct (%)` = modal,
            `No change correct (%)` = stay)

SH[[length(SH) + 1]] <- mk_sheet(
  "Validation", "Out-of-sample backtest",
  sprintf("Forecast from %s and %s, compared against what actually happened by %s",
          qgrid$q_label[N_Q - 20], qgrid$q_label[N_Q - 4], qgrid$q_label[N_Q]),
  notes = c(
    "The method was re-estimated using only data available at each past origin, then used to forecast forward. Nothing after the origin was used.",
    "WHAT HELD UP: category counts. Five of seven categories are within 2% at five years.",
    sprintf("WHAT DID NOT: the $10B-and-over category came in %.0f%% high at five years, and downward movement is under-predicted beyond one year (down ratio %.2f at five years against 1.00 for a perfect forecast).",
            count_tab$pct[count_tab$h == 20 & count_tab$cat == CAT_LABELS[N_CAT]],
            dir_tab$down_ratio[dir_tab$h == 20]),
    sprintf("For comparison, the previous ARIMA-based method predicted %d upward moves against %d actual over five years, and %d downward against %d.",
            FROZEN_REF$up_5y_pred, FROZEN_REF$up_5y_act,
            FROZEN_REF$down_5y_pred, FROZEN_REF$down_5y_act),
    "The correction for the $10B category has NOT been applied to the published figures. Applying it would give a lower count; the uncorrected figure is published with this caveat instead."),
  blocks = list(
    list(head = "Five-year count accuracy", df = val_counts,
         styles = c(S_NORM, S_INT, S_DEC, S_INT, S_DEC, S_DEC)),
    list(head = "Direction of movement", df = val_dir,
         styles = c(S_NORM, S_DEC, S_INT, S_DEC, S_INT, S_DEC, S_DEC)),
    list(head = "Institution-level accuracy", df = val_acc,
         styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC))),
  cols = col_widths(list(c(1, 1, 24), c(2, 8, 16))))

## ---------------------------------------------------------------------
## [27.12] Diagnostics
## ---------------------------------------------------------------------
diag_cv <- cv_summary %>% filter(h == 20) %>%
  transmute(Method = spec, RPS = rps, `Count MAE` = count_mae,
            `Down ratio` = down_ratio, `Up ratio` = up_ratio) %>%
  head(10)

diag_pool <- data.frame(
  Category = CAT_PRETTY[CAT_LABELS],
  Source = attr(POOLS[["20"]], "source"),
  Observations = sapply(POOLS[["20"]], function(p) p$n),
  `Median annual growth (%)` = round(100 * (exp(sapply(POOLS[["20"]], pool_q, 0.5) *
                                                4 / 20) - 1), 2),
  check.names = FALSE, stringsAsFactors = FALSE)

SH[[length(SH) + 1]] <- mk_sheet(
  "Diagnostics", "Method selection and estimation detail",
  "For the analyst, not the field",
  notes = c(
    "METHOD SELECTION. Six candidate methods were compared by cross-validation on blocked origin folds. The empirical conditional distribution won at every horizon.",
    "Alternatives tested and rejected: conditioning on position within the band, on trailing growth, on volatility, and on region; a mean model with a fitted normal spread; and distribution regression with the full covariate set. None improved on the simpler method, and finer conditioning was worse on count accuracy.",
    "SCORING. Ranked probability score, which penalises being three categories off more than being one -- appropriate because the categories are ordered.",
    "POOL SOURCE. A category showing 'window' is forecast using its neighbours' growth distribution because it has too few observations of its own. Its figures should be read accordingly.",
    "KNOWN LIMITATIONS, in order of importance: no mergers; the $10B category over-counts by about 40%; downward movement under-predicted beyond one year; region 8 is a supervisory office, not a geography.")
  ,
  blocks = list(
    list(head = "Cross-validation, five-year horizon", df = diag_cv,
         styles = c(S_NORM, S_DEC, S_DEC, S_DEC, S_DEC)),
    list(head = "Estimation pools, five-year horizon", df = diag_pool,
         styles = c(S_NORM, S_NORM, S_INT, S_DEC))),
  cols = col_widths(list(c(1, 1, 24), c(2, 5, 18))))

## ---------------------------------------------------------------------
## [27.13] Write
## ---------------------------------------------------------------------
vapply(SH, function(s) s$name, "")
stopifnot(!any(duplicated(vapply(SH, function(s) s$name, ""))),
          all(nchar(vapply(SH, function(s) s$name, "")) <= 31))

xlsx_write(SH, OUT)

## Final tie-out, on the file's own numbers rather than on the objects
cat("\nTie-out:\n")
cat("  cohort            ", nrow(inst_out), "\n")
cat("  Total tab, 5yr    ", round(sum(counts$h20), 1), "\n")
cat("  Institutions tab  ", nrow(inst_tab), "\n")
cat("  assigned 5yr      ", sum(table(inst_out$cat_5y)), "\n")
stopifnot(abs(sum(counts$h20) - nrow(inst_out)) < 0.5,
          nrow(inst_tab) == nrow(inst_out))
cat("\nWorkbook written:", OUT, "\n")
