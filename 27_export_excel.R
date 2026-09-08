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

## The helpers live in the project root, not in Data. Search a few likely
## places rather than assuming, so this does not break when the working
## directory changes.
find_src <- function(fn) {
  cand <- c(fn, file.path("..", fn),
            file.path("S:/Projects/Credit_Union_Growth_Forecast", fn))
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop(fn, " not found. Searched: ",
                       paste(cand, collapse = ", "))
  cat("sourcing", normalizePath(hit), "\n")
  source(hit)
}

find_src("0_xlsx_helpers.R")

## MUST be sourced AFTER the helpers -- it redefines xlsx_write() to zip
## in base R. The helper's own three methods all need something this
## machine does not have: the `zip` package (blocked), zip.exe on the PATH
## (absent -- "the system cannot find the file specified"), or PowerShell
## resolvable by name. 0_xlsx_helpers.R itself is untouched, so scripts
## 14 and 16 keep their original behaviour.
find_src("0b_zip_base.R")
stopifnot(exists("zip_base"))

## prep <- readRDS("panel_prep.rds");     list2env(prep, .GlobalEnv)
## prb  <- readRDS("panel_probs.rds");    list2env(prb,  .GlobalEnv)
## asg  <- readRDS("panel_assign.rds");   list2env(asg,  .GlobalEnv)
## cvr  <- readRDS("panel_cv.rds");       list2env(cvr,  .GlobalEnv)
## bkt  <- readRDS("panel_backtest.rds"); list2env(bkt,  .GlobalEnv)

stopifnot(exists("counts"), exists("inst_out"), exists("TRANS"),
          exists("cell_counts"), exists("movers"), exists("down_risk"),
          exists("count_tab"), exists("dir_tab"), exists("acc_tab"),
          exists("A7_ONLY_CALIB"), exists("A7_FACTORS"),
          exists("apportion"), exists("inst"))

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
## A7 correction status -- every tab that mentions the $10B category reads
## these so the workbook cannot contradict what [23.6b] actually did.
A7_APPLIED <- isTRUE(A7_ONLY_CALIB)
a7_bias_5y <- count_tab$pct[count_tab$h == 20 & count_tab$cat == CAT_LABELS[N_CAT]]
a7_fac_txt <- paste(sprintf("%s: %.3f", H_LAB, A7_FACTORS[as.character(H_SET)]),
                    collapse = "; ")
a7_method_note <- if (A7_APPLIED) sprintf(
  "3. THE LARGEST CATEGORY IS CORRECTED. Out of sample the uncorrected $10B-and-over count ran about %.0f%% high at five years. The published figures scale that category's probabilities by backtest-derived factors (%s) and reallocate the released mass to the other categories within each institution's row. See Validation.",
  a7_bias_5y, a7_fac_txt) else sprintf(
  "3. THE LARGEST CATEGORY IS OVER-COUNTED. Out of sample the $10B-and-over count runs about %.0f%% high at five years. See Validation.",
  a7_bias_5y)
a7_short_note <- if (A7_APPLIED)
  "The $10B-and-over figures include a backtest-derived downward correction (see Method and Validation); read them as central estimates with wide uncertainty." else
  "The $10B-and-over figures should be read as upper estimates; see Validation."
a7_valid_note <- if (A7_APPLIED) sprintf(
  "The correction for the $10B category HAS been applied to the published figures: probabilities of the $10B-and-over category were scaled by %s, the actual/predicted ratios from this backtest. The backtest figures on this tab are UNCORRECTED so the reader can see the bias the correction addresses.",
  a7_fac_txt) else
  "The correction for the $10B category has NOT been applied to the published figures. Applying it would give a lower count; the uncorrected figure is published with this caveat instead."
a7_limit_txt <- if (A7_APPLIED)
  sprintf("the $10B category over-counted by about %.0f%% before correction (correction applied)", a7_bias_5y) else
  sprintf("the $10B category over-counts by about %.0f%%", a7_bias_5y)

method_notes <- c(
 "WHAT THIS IS",
 sprintf("For each of the %s credit unions active in %s, we estimate the probability of being in each asset category 1, 3 and 5 years out.",
         format(nrow(inst_out), big.mark = ","), qgrid$q_label[N_Q]),
 "Published counts are whole numbers of institutions. The probability sums are rounded to integers by largest remainder, and each institution is then assigned to one category by forecast size so that the assignments reproduce those integers exactly. The Total tab, the regional tabs and the Institutions tab therefore agree to the institution.",
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
 a7_method_note,
 "",
 "REGIONS",
 "Region 8 is the Office of National Examinations and Supervision, a supervisory office rather than a geography. Its institutions are the largest in the industry and are reported as their own group.",
 "",
 "READING AN INDIVIDUAL ROW",
 "The aggregate counts are considerably more reliable than any single institution's line. Errors across thousands of institutions largely offset; a single credit union near a size threshold is close to a coin flip at five years.",
 "Every institution row carries the probability of its assigned category. Read that column, not just the category.")

settings_tbl <- data.frame(
  Setting = c("Cohort date", "Institutions", "Horizons", "Method",
              "Growth basis", "Price basis", "$10B category correction",
              "Recency weighting", "Minimum pool", "Produced"),
  Value = c(qgrid$q_label[N_Q], format(nrow(inst_out), big.mark = ","),
            paste(H_LAB, collapse = ", "),
            "Empirical conditional distribution, by asset category",
            GROWTH_BASIS, PRICE_BASIS,
            if (A7_APPLIED) paste("applied --", a7_fac_txt) else
              if (BUCKET_CALIB) "full-matrix raking applied" else
                "not applied -- see Validation",
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
## [27.3b] PUBLISHED COUNTS ARE WHOLE NUMBERS, COUNTED OFF THE LIST
##
## Every count on the Total and regional tabs is a tabulation of the
## institution-level assignment from script 24 (cat_1y / cat_3y / cat_5y
## in inst_out). Those assignments were sized to the largest-remainder
## rounding of the probability sums ([24.1]) and tie to the cohort at every
## horizon, so: Total tab == sum of the eight regional tabs == what you get
## by filtering the Institutions tab. No fractions anywhere the field sees
## them. The unrounded probability sums are kept on Diagnostics.
## ---------------------------------------------------------------------
if (!all(c("assets_med_1y", "assets_med_3y") %in% names(inst_out))) {
  ## 24 was run before [24.8] carried these; pull them from inst instead.
  inst_out <- inst_out %>%
    left_join(inst %>% select(join_number, assets_med_1y = assets_med_h4,
                              assets_med_3y = assets_med_h12),
              by = "join_number")
}

H_COL <- c("4" = "cat_1y", "12" = "cat_3y", "20" = "cat_5y")

tab_cats <- function(d) {
  out <- data.frame(cat = CAT_LABELS, stringsAsFactors = FALSE)
  out$now <- as.integer(table(factor(d$asset_cat_now, levels = CAT_LABELS)))
  for (h in H_SET)
    out[[paste0("h", h)]] <-
      as.integer(table(factor(d[[H_COL[as.character(h)]]], levels = CAT_LABELS)))
  out
}

counts_int <- tab_cats(inst_out)
stopifnot(all(colSums(counts_int[, -1]) == nrow(inst_out)))

## Integer counts must equal the largest-remainder rounding of the soft
## counts -- otherwise 24 and 23 disagree and neither should be published.
for (h in H_SET) {
  soft <- counts[[paste0("h", h)]]
  stopifnot(identical(counts_int[[paste0("h", h)]],
                      as.integer(apportion(soft, nrow(inst_out)))))
}
cat("Integer counts reproduce largest-remainder rounding at every horizon.\n")

cell_counts_int <- inst_out %>%
  group_by(region, cu_type) %>%
  group_modify(~ tab_cats(.x)) %>% ungroup()

## Regional tabs must add to the Total tab, category by category
stopifnot(identical(
  cell_counts_int %>% group_by(cat) %>%
    summarise(across(c(now, h4, h12, h20), sum), .groups = "drop") %>%
    arrange(match(cat, CAT_LABELS)) %>% select(now, h4, h12, h20) %>%
    as.data.frame(),
  counts_int %>% select(now, h4, h12, h20) %>% as.data.frame()))
cat("Eight regional tabs add to the Total tab exactly.\n")

## ---------------------------------------------------------------------
## [27.4] Total
## ---------------------------------------------------------------------
tot_tbl <- counts_int %>%
  transmute(Category = CAT_PRETTY[cat], Today = now,
            !!H_LAB[1] := h4, !!H_LAB[2] := h12, !!H_LAB[3] := h20,
            Change = h20 - now,
            `Pct change` = round(100 * (h20 - now) / pmax(now, 1), 1))

## Supplementary thresholds: whole numbers, and nested inside the
## published A7 count. These are rounded probability sums, not a count of
## named institutions -- there is no assignment above $10B.
extra_tbl <- extra %>%
  transmute(Threshold = label, Today = as.integer(now),
            !!H_LAB[1] := pmin(round(h4),  counts_int$h4[N_CAT]),
            !!H_LAB[2] := pmin(round(h12), counts_int$h12[N_CAT]),
            !!H_LAB[3] := pmin(round(h20), counts_int$h20[N_CAT]))

SH[[length(SH) + 1]] <- mk_sheet(
  "Total", "Projected counts by asset category",
  sprintf("All %s institutions. Totals are held fixed -- no mergers.",
          format(nrow(inst_out), big.mark = ",")),
  notes = c(
    "Every count is a whole number of institutions and matches the Institutions tab exactly: filter that tab by category and horizon and you will get the figure here. The eight regional tabs add to this one.",
    "The supplementary thresholds below OVERLAP the table above -- a $15B institution is also counted in $10B and over. They are not a partition and must not be added to the total.",
    a7_short_note),
  blocks = list(
    list(head = "Counts by category", df = tot_tbl,
         styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT, S_DEC)),
    list(head = "Supplementary thresholds (overlapping, not a partition)",
         df = extra_tbl, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT))),
  cols = col_widths(list(c(1, 1, 22), c(2, 7, 12))))

## ---------------------------------------------------------------------
## [27.5] Bucket Growth -- nominal vs real
## ---------------------------------------------------------------------
real_int <- as.integer(apportion(nominal_vs_real$real_h20, nrow(inst_out)))
growth_tbl <- data.frame(
  Category = nominal_vs_real$cat, Today = counts_int$now,
  `5yr nominal` = counts_int$h20, `5yr real` = real_int,
  check.names = FALSE, stringsAsFactors = FALSE) %>%
  mutate(`Nominal change` = `5yr nominal` - Today,
         `Real change` = `5yr real` - Today,
         `Threshold drift` = `5yr nominal` - `5yr real`)
stopifnot(sum(growth_tbl$`5yr real`) == nrow(inst_out))

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
                     styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT, S_INT))),
  cols = col_widths(list(c(1, 1, 22), c(2, 7, 15))))

## ---------------------------------------------------------------------
## [27.6] Transition matrices
## ---------------------------------------------------------------------
## Whole numbers, counted off the Institutions tab: rows are category
## today, columns the assigned category at the horizon. Row totals equal
## the Today column of the Total tab, column totals equal that horizon's
## column. The assignment cannot move an institution downward, so the
## lower triangle is zero by construction -- the probability of downward
## movement is on the Down Risk tab, not here.
trans_int <- lapply(H_SET, function(h) {
  m <- table(factor(inst_out$asset_cat_now, levels = CAT_LABELS),
             factor(inst_out[[H_COL[as.character(h)]]], levels = CAT_LABELS))
  m <- matrix(as.integer(m), N_CAT, N_CAT,
              dimnames = list(CAT_LABELS, CAT_LABELS))
  stopifnot(identical(as.integer(rowSums(m)), counts_int$now),
            identical(as.integer(colSums(m)), counts_int[[paste0("h", h)]]))
  m
})
names(trans_int) <- as.character(H_SET)

trans_blocks <- unlist(lapply(H_SET, function(h) {
  m  <- trans_int[[as.character(h)]]
  df <- data.frame(From = CAT_PRETTY[CAT_LABELS], m, check.names = FALSE,
                   stringsAsFactors = FALSE)
  names(df)[-1] <- CAT_PRETTY[CAT_LABELS]
  df$Total <- as.integer(rowSums(m))
  ## Row percentages, whole numbers, largest-remainder so each row is 100
  pct <- t(apply(m, 1, function(r)
    if (sum(r) > 0) apportion(100 * r / sum(r), 100L) else rep(0L, N_CAT)))
  dfp <- data.frame(From = CAT_PRETTY[CAT_LABELS], pct, check.names = FALSE,
                    stringsAsFactors = FALSE)
  names(dfp)[-1] <- CAT_PRETTY[CAT_LABELS]
  list(
    list(head = sprintf("%s -- number of institutions", H_LAB[as.character(h)]),
         df = df, styles = c(S_NORM, rep(S_INT, N_CAT + 1))),
    list(head = sprintf("%s -- share of row (%%)", H_LAB[as.character(h)]),
         df = dfp, styles = c(S_NORM, rep(S_INT, N_CAT))))
}), recursive = FALSE)

SH[[length(SH) + 1]] <- mk_sheet(
  "Transitions", "Category transition probabilities",
  "Row = category today, column = assigned category at the horizon",
  notes = c(
    "Whole numbers of institutions, counted off the Institutions tab. Row totals are today's counts; column totals are the horizon's counts on the Total tab.",
    "Institutions are assigned by forecast size, which cannot place an institution below its current category. Expected downward movement is on the Down Risk tab.",
    "Each horizon is estimated directly; the five-year table is NOT the one-year table applied five times."),
  blocks = trans_blocks,
  cols = col_widths(list(c(1, 1, 18), c(2, 9, 14))))

## ---------------------------------------------------------------------
## [27.7] Region x charter tabs
## ---------------------------------------------------------------------
cells <- cell_counts_int %>% distinct(region, cu_type) %>% arrange(region, cu_type)

for (i in seq_len(nrow(cells))) {
  rg <- cells$region[i]; ct <- cells$cu_type[i]
  nm <- paste0("R", rg, "_", CT_LAB[as.character(ct)])

  d <- cell_counts_int %>% filter(region == rg, cu_type == ct)
  wide <- d %>%
    transmute(Category = CAT_PRETTY[cat], Today = now,
              !!H_LAB[1] := h4, !!H_LAB[2] := h12, !!H_LAB[3] := h20,
              Change = h20 - now)
  n_cell <- sum(d$now)
  stopifnot(all(colSums(d[, c("h4", "h12", "h20")]) == n_cell))

  mv <- movers %>% filter(h == 20, region == rg, cu_type == ct) %>%
    left_join(inst_out %>%
                select(join_number, cat_1y, assets_med_1y,
                       cat_3y, assets_med_3y),
              by = "join_number") %>%
    arrange(desc(assets_now_m)) %>%
    transmute(`Credit union` = cu_name, Today = CAT_PRETTY[from],
              `Assets ($M)` = assets_now_m,
              `1yr` = CAT_PRETTY[cat_1y],
              `Median 1yr ($M)` = round(assets_med_1y / 1e6, 1),
              `3yr` = CAT_PRETTY[cat_3y],
              `Median 3yr ($M)` = round(assets_med_3y / 1e6, 1),
              `5yr` = CAT_PRETTY[to],
              `Median 5yr ($M)` = med_m, `P(5yr)` = p_assign)

  SH[[length(SH) + 1]] <- mk_sheet(
    nm, paste(REG_LAB[as.character(rg)], "-", CT_LAB[as.character(ct)]),
    sprintf("%s institutions", format(n_cell, big.mark = ",")),
    notes = c(
      "Whole numbers of institutions. Counts tie to this cell's own institution count at every horizon and match the Institutions tab filtered to this region and charter.",
      "The movers list names institutions whose assigned category changes. Assignment is by forecast size and cannot move an institution downward -- see the Down Risk tab for that."),
    blocks = list(
      list(head = "Counts by category", df = wide,
           styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT)),
      list(head = "Institutions changing category by five years", df = mv,
           styles = c(S_NORM, S_NORM, S_INT, S_NORM, S_INT, S_NORM, S_INT,
                      S_NORM, S_INT, S_DEC))),
    cols = col_widths(list(c(1, 1, 38), c(2, 10, 16))))
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
    `Median 1yr ($M)` = round(assets_med_1y / 1e6, 1),
    `3yr` = CAT_PRETTY[cat_3y], `P(3yr)` = p_3y,
    `Median 3yr ($M)` = round(assets_med_3y / 1e6, 1),
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
    "Median 1yr / 3yr / 5yr are the forecast asset levels used to assign categories. Low and High are the 10th and 90th percentiles at five years.",
    "Counting this tab by category and horizon reproduces the Total tab and each regional tab exactly.",
    "Down risk marks institutions with elevated probability of falling a category. They are still assigned by forecast size -- see the Down Risk tab.",
    "Short history marks institutions with too little data for their own trailing features; they are forecast from their category's distribution."),
  blocks = list(list(df = inst_tab,
                     styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_NORM,
                                S_INT, S_NORM,
                                S_NORM, S_DEC, S_INT,
                                S_NORM, S_DEC, S_INT,
                                S_NORM, S_DEC, S_NORM, S_INT, S_INT, S_INT,
                                S_DEC, S_DEC, S_DEC, S_NORM, S_NORM))),
  cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 5, 12),
                         c(6, 24, 14))),
  freeze = list(x = 2, y = 6),
  autofilter = sprintf("A6:X%d", 6 + nrow(inst_tab)))

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
    if (A7_APPLIED) "These counts carry the same backtest-derived correction as the $10B-and-over category (see Method). The ordering of institutions is more reliable than the level." else
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
    a7_valid_note),
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
    sprintf("KNOWN LIMITATIONS, in order of importance: no mergers; %s; downward movement under-predicted beyond one year; region 8 is a supervisory office, not a geography.", a7_limit_txt))
  ,
  blocks = list(
    list(head = "Probability sums before rounding (published counts are the largest-remainder rounding of these)",
         df = counts %>% transmute(Category = pretty, Today = now,
                                   !!H_LAB[1] := h4, !!H_LAB[2] := h12,
                                   !!H_LAB[3] := h20),
         styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC)),
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

## ---------------------------------------------------------------------
## [27.14] What went into the workbook, on screen
##
## The same numbers as the tabs, printed so the run can be checked without
## opening Excel. If a figure here looks wrong, it is wrong in the file.
## ---------------------------------------------------------------------
rule <- function(t) cat("\n", strrep("=", 74), "\n", t, "\n",
                        strrep("=", 74), "\n", sep = "")

rule(sprintf("COUNTS BY ASSET CATEGORY -- %s cohort, %s institutions",
             qgrid$q_label[N_Q], format(nrow(inst_out), big.mark = ",")))
print(as.data.frame(tot_tbl), row.names = FALSE)
cat("\nTotals:", paste(sprintf("%s %d", c("today", H_LAB),
      c(sum(counts_int$now), sum(counts_int$h4), sum(counts_int$h12),
        sum(counts_int$h20))), collapse = "   "), "\n")

rule("SUPPLEMENTARY THRESHOLDS -- overlapping, not part of the total")
print(as.data.frame(extra_tbl), row.names = FALSE)

rule(sprintf("NOMINAL VS REAL AT FIVE YEARS -- edges indexed at %.1f%%/yr",
             100 * CPI_ASSUMPTION))
print(as.data.frame(growth_tbl), row.names = FALSE)
cat("\nThreshold drift is the part of the change that is the yardstick",
    "\nmoving rather than credit unions changing size.\n")

rule("MOVEMENT")
for (h in H_SET)
  cat(sprintf("  %-7s  E[down] %6.1f   E[same] %8.1f   E[up] %6.1f\n",
              H_LAB[as.character(h)],
              sum(inst[[paste0("p_down_h", h)]]),
              sum(inst[[paste0("p_same_h", h)]]),
              sum(inst[[paste0("p_up_h", h)]])))
cat("\n  Named as moving up at 5yr: ", sum(movers$h == 20),
    "\n  Named on the down-risk list:", sum(down_risk$h == 20),
    "\n  (assignment is by forecast size and cannot move an institution",
    "\n   downward, so downward risk is published as its own list)\n")

rule("CONFIDENCE OF THE FIVE-YEAR ASSIGNMENT")
print(inst_out %>% count(conf_5y) %>%
        mutate(pct = round(100 * n / sum(n), 1)) %>% as.data.frame(),
      row.names = FALSE)

rule("BY REGION AND CHARTER -- five years")
print(cell_counts %>% filter(h == 20) %>%
        group_by(Region = REG_LAB[as.character(region)],
                 Charter = CT_LAB[as.character(cu_type)]) %>%
        summarise(Today = first(n_now), `5yr` = round(sum(fcst_exact), 1),
                  .groups = "drop") %>% as.data.frame(), row.names = FALSE)

rule("VALIDATION -- five-year backtest")
print(as.data.frame(val_counts), row.names = FALSE)
cat("\n")
print(as.data.frame(val_dir), row.names = FALSE)

rule("THE THREE CAVEATS THAT MUST TRAVEL WITH THESE NUMBERS")
cat("  1. No mergers. The total is held fixed. Realised exit is ~3.6%/yr,\n",
    "    about a sixth over five years. These are not population counts.\n")
cat(sprintf("  2. %s over-counts by %.0f%% out of sample at five years%s.\n",
            CAT_PRETTY[CAT_LABELS[N_CAT]], a7_bias_5y,
            if (A7_APPLIED) paste0(" -- CORRECTED in published figures (", a7_fac_txt, ")")
            else " -- NOT corrected"))
cat(sprintf(paste0("  3. Downward movement under-predicted beyond one year",
                   "\n     (down ratio %.2f at five years; 1.00 would be perfect).\n"),
            dir_tab$down_ratio[dir_tab$h == 20]))

## ---------------------------------------------------------------------
## [27.15] Tie-out
## ---------------------------------------------------------------------
rule("TIE-OUT")
cat("  cohort             ", nrow(inst_out), "\n")
cat("  Total tab, 5yr     ", sum(counts_int$h20), "\n")
cat("  Institutions tab   ", nrow(inst_tab), "\n")
cat("  assigned 5yr       ", sum(table(inst_out$cat_5y)), "\n")
cat("  region x charter   ", sum(cell_counts_int$h20), "\n")
cat("  soft sum, 5yr      ", round(sum(counts$h20), 1), "\n")
stopifnot(sum(counts_int$h20) == nrow(inst_out),
          nrow(inst_tab) == nrow(inst_out),
          sum(cell_counts_int$h20) == nrow(inst_out))
cat("\nWorkbook written:", normalizePath(OUT), "\n")
