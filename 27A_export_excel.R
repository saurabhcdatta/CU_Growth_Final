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

## ---- config (production) --------------------------------------------
## Settings come from 00_config.R in the project root when it is present;
## the defaults written below still apply when it is not.
if (!exists("CONFIG_LOADED")) {
  for (.p in c("00_config.R", "../00_config.R",
               "S:/Projects/Credit_Union_Growth_Forecast/00_config.R"))
    if (file.exists(.p)) { source(.p); break }
  rm(.p)
}
if (!exists("cfg_get")) cfg_get <- function(name, default) default
setwd(cfg_get("DATA_DIR", "S:/Projects/Credit_Union_Growth_Forecast/Data"))

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
          exists("A7_ONLY_CALIB"), exists("A7_FACTORS"), exists("A7_FACTOR_SCOPE"),
          exists("apportion"), exists("inst"))

## ---------------------------------------------------------------------
## [27.1] Settings
## ---------------------------------------------------------------------
## How categories are labelled everywhere in this workbook (set in 24):
##   "median" -- the band an institution's median forecast falls in
##   "counts" -- the ranking cut that reproduces the probability sums
AB <- if (exists("ASSIGN_BASIS")) ASSIGN_BASIS else "counts"
med_a7_5y  <- sum(inst_out$assets_med_5y >= 10e9)        # tally at $10B
if (!exists("PROB") && file.exists("panel_probs.rds")) {
  .prb <- readRDS("panel_probs.rds"); PROB <- .prb$PROB; rm(.prb)
}
prob_a7_5y <- if (exists("PROB")) as.integer(round(sum(PROB[["20"]][, N_CAT]))) else NA_integer_
cat("Category basis:", AB, "| $10B at 5yr -- by median", med_a7_5y,
    "| weighted by chance of crossing", prob_a7_5y, "\n")
OUT <- sprintf("CU_Growth_Forecast_%s_probability.xlsx", qgrid$q_label[N_Q])

## cu_type 1 = federal charter, 2 = federally insured state charter.
## VERIFY against the 5300 codebook before circulating -- the labels
## appear on eight tab names and every institution row.
CT_LAB <- cfg_get("CT_LAB", c("1" = "FCU", "2" = "FISCU"))

REG_LAB <- cfg_get("REG_LAB", c("1" = "Region 1", "2" = "Region 2",
                                "3" = "Region 3", "8" = "ONES"))

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
a7_method_note <- if (AB == "median") sprintf(
  "3. THE LARGEST CATEGORY IS NOT CORRECTED IN THESE COUNTS. Every count in this workbook is a tally of institutions by their projected (median) assets, which is what the field asked for. Out of sample that rule over-states crossings of $10B: institutions already above the line are projected almost exactly right, but about %.0f%% more are projected to cross than historically do. Weighting each institution by its chance of crossing instead would give a five-year count of about %d rather than %d. Treat the $10B figure here as the upper end and see Validation.",
  100 * (1 / 0.496 - 1), prob_a7_5y, med_a7_5y)
else if (A7_APPLIED) sprintf(
  "3. THE LARGEST CATEGORY IS CORRECTED. Out of sample the uncorrected $10B-and-over count ran about %.0f%% high at five years, and the excess was in institutions projected to CROSS $10B rather than in those already above it. The published figures scale the probability of crossing by backtest-derived factors (%s) for institutions below $10B today, and reallocate the released mass to the other categories within each institution's row. Institutions already above $10B are not scaled. See Validation.",
  a7_bias_5y, a7_fac_txt) else sprintf(
  "3. THE LARGEST CATEGORY IS OVER-COUNTED. Out of sample the $10B-and-over count runs about %.0f%% high at five years. See Validation.",
  a7_bias_5y)
a7_short_note <- if (AB == "median")
  "The $10B-and-over figures count institutions whose projected assets reach $10B. Institutions near the line historically cross less often than typical growth implies, so read these as the upper end of the range; see Method and Validation."
else if (A7_APPLIED)
  "The $10B-and-over figures include a backtest-derived downward correction (see Method and Validation); read them as central estimates with wide uncertainty." else
  "The $10B-and-over figures should be read as upper estimates; see Validation."
a7_valid_note <- if (AB == "median") sprintf(
  "The counts in this workbook are tallies by projected assets and carry NO correction for the $10B category. The table above shows what the count would be if each institution were weighted by its chance of crossing (%s applied to crossings), which is the version this backtest supports: about %d at five years against the %d published. The difference is entirely institutions whose projected assets clear $10B but whose chance of actually being there is nearer one in two.",
  a7_fac_txt, prob_a7_5y, med_a7_5y)
else if (A7_APPLIED) sprintf(
  "The correction for the $10B category HAS been applied to the published figures: for institutions below $10B today, the probability of being in the $10B-and-over category was scaled by %s, the ratio of actual to predicted entrants from this backtest. Institutions already above $10B are not scaled. The backtest figures on this tab are UNCORRECTED so the reader can see the bias the correction addresses.",
  a7_fac_txt) else
  "The correction for the $10B category has NOT been applied to the published figures. Applying it would give a lower count; the uncorrected figure is published with this caveat instead."
a7_limit_txt <- if (AB == "median")
  sprintf("the $10B category is counted by projected assets and is an upper estimate (%d here against about %d if weighted by chance of crossing)",
          med_a7_5y, prob_a7_5y)
else if (A7_APPLIED)
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
 if (HAVE_EXIT) "1. TWO SETS OF COUNTS. The Total, Transitions and regional count tables hold every institution in the system (no mergers) so that they reconcile to the institution lists. The With Mergers tab and the matching block on each regional tab apply historical exit rates by category and show how many of today's institutions are expected still to be operating. Read the first for where institutions are heading, the second for how many there will be." else
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
              "Growth basis", "Price basis", "Category assigned by",
              "$10B category correction",
              "Recency weighting", "Minimum pool", "Produced"),
  Value = c(qgrid$q_label[N_Q], format(nrow(inst_out), big.mark = ","),
            paste(H_LAB, collapse = ", "),
            "Empirical conditional distribution, by asset category",
            GROWTH_BASIS, PRICE_BASIS,
            if (AB == "median") "median projected assets" else "probability-matched ranking",
            if (AB == "median") "not applied -- counts are tallies by projected assets" else
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
## [27.3a] Merger adjustment from script 26, if it has run
## ---------------------------------------------------------------------
HAVE_EXIT <- exists("pop_counts") && exists("pop_cells") && exists("exit_rates")
if (!HAVE_EXIT && file.exists("panel_exit.rds")) {
  exr <- readRDS("panel_exit.rds"); list2env(exr, .GlobalEnv); HAVE_EXIT <- TRUE
}
cat("Merger adjustment:", if (HAVE_EXIT) "available -- With Mergers tab will be written"
                          else "not available (run 26 to add it)", "\n")

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

## Under ASSIGN_BASIS = "counts" the tally must equal the largest-remainder
## rounding of the probability sums. Under "median" it is a tally of where
## each institution's median forecast lands and will differ; report the
## gap rather than assert it away.
for (h in H_SET) {
  soft <- counts[[paste0("h", h)]]
  tgt  <- as.integer(apportion(soft, nrow(inst_out)))
  if (AB == "median") {
    d <- counts_int[[paste0("h", h)]] - tgt
    if (any(d != 0))
      cat(sprintf("h=%2d  median-based tally vs probability sums: %s\n", h,
                  paste(sprintf("%s %+d", CAT_LABELS[d != 0], d[d != 0]),
                        collapse = ", ")))
  } else {
    stopifnot(identical(counts_int[[paste0("h", h)]], tgt))
  }
}
cat("Counts assigned by:", AB, "\n")

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

## Supplementary thresholds. Under ASSIGN_BASIS = "median" these are
## counted the same way as every other figure in the workbook -- the number
## of institutions whose median forecast reaches the level -- so a reader
## can reproduce them from the Institutions tab and they nest inside the
## $10B row by construction. Under "counts" they are rounded probability
## sums, capped at the published $10B count.
MED_COL <- c("4" = "assets_med_1y", "12" = "assets_med_3y", "20" = "assets_med_5y")
med_above <- function(h, L) sum(inst_out[[MED_COL[as.character(h)]]] >= L)

if (AB == "median") {
  extra_tbl <- data.frame(Threshold = extra$label,
                          Today = as.integer(extra$now),
                          stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(H_SET))
    extra_tbl[[H_LAB[i]]] <- sapply(extra$threshold, function(L) med_above(H_SET[i], L))
  ## Must nest inside the $10B row, which is itself a median tally
  for (i in seq_along(H_SET))
    stopifnot(all(extra_tbl[[H_LAB[i]]] <= counts_int[[paste0("h", H_SET[i])]][N_CAT]))
  cat("Supplementary thresholds counted by median forecast.\n")
} else {
  extra_tbl <- extra %>%
    transmute(Threshold = label, Today = as.integer(now),
              !!H_LAB[1] := pmin(round(h4),  counts_int$h4[N_CAT]),
              !!H_LAB[2] := pmin(round(h12), counts_int$h12[N_CAT]),
              !!H_LAB[3] := pmin(round(h20), counts_int$h20[N_CAT]))
}

## Two PARTITIONS of the $10B-and-over row, as the stakeholders asked:
##   Part A: $10B-$15B | $15B and over
##   Part B: $10B-$15B | $15B-$20B | $20B and over
## Each part adds exactly to the published A7 count at every date, so the
## reader can check it against the table above. Built from the corrected
## $15B+ / $20B+ sums (rounded, capped at A7) by subtraction.
i15 <- which(extra$threshold == 15e9); i20 <- which(extra$threshold == 20e9)
stopifnot(length(i15) == 1, length(i20) == 1)
a7_line <- c(counts_int$now[N_CAT], counts_int$h4[N_CAT], counts_int$h12[N_CAT], counts_int$h20[N_CAT])
l15 <- unlist(extra_tbl[i15, c("Today", H_LAB)]); l20 <- unlist(extra_tbl[i20, c("Today", H_LAB)])
l20 <- pmin(l20, l15)                      # nesting
mk_part <- function(labels, rows) {
  d <- data.frame(Band = labels, stringsAsFactors = FALSE)
  d$Today <- sapply(rows, function(r) r[1])
  for (i in seq_along(H_LAB)) d[[H_LAB[i]]] <- sapply(rows, function(r) r[i + 1])
  d <- bind_rows(d, data.frame(Band = "$10B and over (total)", Today = sum(d$Today),
                               setNames(as.list(colSums(d[, H_LAB, drop = FALSE])), H_LAB),
                               check.names = FALSE))
  stopifnot(all(d[nrow(d), -1] == a7_line), all(d[, -1] >= 0))
  d
}
part_a <- mk_part(c("$10B - $15B", "$15B and over"),
                  list(a7_line - l15, l15))
part_b <- mk_part(c("$10B - $15B", "$15B - $20B", "$20B and over"),
                  list(a7_line - l15, l15 - l20, l20))

SH[[length(SH) + 1]] <- mk_sheet(
  "Total", "Projected counts by asset category",
  sprintf("All %s institutions. Totals are held fixed -- no mergers.",
          format(nrow(inst_out), big.mark = ",")),
  notes = c(
    if (AB == "median")
      "Every count is a whole number of institutions and matches the Institutions tab exactly: a credit union is counted in the category its projected (median) assets fall in, so the category shown against any institution is the band its projected size lands in. The eight regional tabs add to this one."
    else
      "Every count is a whole number of institutions and matches the Institutions tab exactly: filter that tab by category and horizon and you will get the figure here. The eight regional tabs add to this one.",
    "The two supplementary tables below split the $10B-and-over row into bands. Part A uses one cut at $15B; Part B uses cuts at $15B and $20B. Each part adds exactly to the $10B-and-over row above it. They are two views of the same institutions, not additional categories, and must not be added to the total.",
    a7_short_note),
  blocks = list(
    list(head = "Counts by category", df = tot_tbl,
         styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT, S_DEC)),
    list(head = "Part A: $10B and over split at $15B",
         df = part_a, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT)),
    list(head = "Part B: $10B and over split at $15B and $20B",
         df = part_b, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT))),
  cols = col_widths(list(c(1, 1, 22), c(2, 7, 12))))

## ---------------------------------------------------------------------
## [27.4b] With Mergers -- expected population counts
## ---------------------------------------------------------------------
if (HAVE_EXIT && AB == "median") {
  ## Population counts must rest on the same assignment as everything else:
  ## take each institution's assigned category and multiply by its
  ## category's survival rate, rather than the probability vector 26 used.
  pop_counts <- data.frame(state = STATE_LAB,
                           now = c(counts_int$now, 0L), stringsAsFactors = FALSE)
  exits_by_origin <- data.frame(cat = CAT_LABELS, now = counts_int$now,
                                stringsAsFactors = FALSE)
  for (i in seq_along(H_SET)) {
    hh <- as.character(H_SET[i]); surv <- 1 - P_EXIT[[hh]]
    k  <- inst_out[[c("cat_1y", "cat_3y", "cat_5y")[i]]]
    soft <- tapply(surv, factor(k, levels = CAT_LABELS), sum)
    soft[is.na(soft)] <- 0
    ex   <- nrow(inst_out) - sum(soft)
    pop_counts[[paste0("h", hh)]] <- as.integer(apportion(c(soft, ex), nrow(inst_out)))
    exits_by_origin[[paste0("h", hh)]] <-
      as.integer(apportion(tapply(P_EXIT[[hh]],
                                  factor(inst_out$asset_cat_now, levels = CAT_LABELS), sum),
                           pop_counts[[paste0("h", hh)]][N_CAT + 1]))
  }
  compare_pop <- data.frame(cat = CAT_LABELS, now = counts_int$now,
                            no_mergers_5y = counts_int$h20,
                            with_mergers_5y = pop_counts$h20[1:N_CAT],
                            difference = pop_counts$h20[1:N_CAT] - counts_int$h20)
  pop_cells <- bind_rows(lapply(split(seq_len(nrow(inst_out)),
                                      list(inst_out$region, inst_out$cu_type), drop = TRUE),
    function(idx) {
      out <- data.frame(region = as.character(inst_out$region[idx[1]]),
                        cu_type = as.character(inst_out$cu_type[idx[1]]),
                        state = STATE_LAB,
                        now = c(as.integer(table(factor(inst_out$asset_cat_now[idx],
                                                        levels = CAT_LABELS))), 0L),
                        stringsAsFactors = FALSE)
      for (i in seq_along(H_SET)) {
        hh <- as.character(H_SET[i]); surv <- 1 - P_EXIT[[hh]][idx]
        k <- inst_out[[c("cat_1y", "cat_3y", "cat_5y")[i]]][idx]
        soft <- tapply(surv, factor(k, levels = CAT_LABELS), sum); soft[is.na(soft)] <- 0
        out[[paste0("h", hh)]] <- as.integer(apportion(c(soft, length(idx) - sum(soft)),
                                                       length(idx)))
      }
      out
    }))
  cat("With Mergers rebuilt on the median-based assignment.\n")
}

if (HAVE_EXIT) {
  pc <- pop_counts
  pop_tbl <- data.frame(
    Category = c(CAT_PRETTY[CAT_LABELS], "Merged or closed", "Still operating"),
    Today = c(pc$now[1:N_CAT], 0L, sum(pc$now[1:N_CAT])),
    stringsAsFactors = FALSE, check.names = FALSE)
  for (i in seq_along(H_SET)) {
    hh <- paste0("h", H_SET[i])
    pop_tbl[[H_LAB[i]]] <- c(pc[[hh]][1:N_CAT], pc[[hh]][N_CAT + 1], sum(pc[[hh]][1:N_CAT]))
  }
  pop_tbl$Change <- pop_tbl[[H_LAB[3]]] - pop_tbl$Today

  side_tbl <- compare_pop %>%
    transmute(Category = CAT_PRETTY[cat], Today = now,
              `No mergers (Total tab)` = no_mergers_5y,
              `With mergers` = with_mergers_5y,
              Difference = difference)
  side_tbl <- bind_rows(side_tbl, data.frame(
    Category = "Total", Today = sum(side_tbl$Today),
    `No mergers (Total tab)` = sum(side_tbl$`No mergers (Total tab)`),
    `With mergers` = sum(side_tbl$`With mergers`),
    Difference = sum(side_tbl$Difference), check.names = FALSE))

  exit_tbl <- exits_by_origin %>%
    transmute(`Category at cohort date` = CAT_PRETTY[cat], Today = now,
              !!H_LAB[1] := h4, !!H_LAB[2] := h12, !!H_LAB[3] := h20)
  tot_row <- data.frame(`Category at cohort date` = "Total", Today = sum(exit_tbl$Today),
                        check.names = FALSE)
  for (hl in H_LAB) tot_row[[hl]] <- sum(exit_tbl[[hl]])
  exit_tbl <- bind_rows(exit_tbl, tot_row)

  rate_tbl <- exit_rates %>%
    transmute(Category = CAT_PRETTY[cat], Horizon = H_LAB[match(h, H_SET)],
              `Share exiting within horizon (%)` = rate_pct,
              `Per year (%)` = annual_pct, Observations = n)

  bt_tbl <- exit_bt_tot %>%
    transmute(Horizon = H_LAB[match(h, H_SET)], Origin = origin,
              Institutions = n, `Predicted exits` = predicted,
              `Actual exits` = actual, `Actual / predicted` = ratio)

  SH[[length(SH) + 1]] <- mk_sheet(
    "With Mergers", "Projected counts by asset category, allowing for mergers and liquidations",
    sprintf("All %s institutions at %s. Totals FALL as institutions leave.",
            format(nrow(inst_out), big.mark = ","), qgrid$q_label[N_Q]),
    notes = c(
      "The Total tab holds every institution in the system; this tab does not. Each institution's category probabilities are multiplied by its category's historical survival rate, and the remainder is counted as merged or closed. 'Still operating' is the number of today's credit unions expected to exist at each date.",
      "Exit rates depend on asset category only, estimated from every credit union since 2005: the share in each category that merged away or closed within one, three and five years. Small institutions exit far more often than large ones. New charters are not included (a handful a year).",
      "These are expected counts, rounded to whole numbers. Unlike the Total and regional tabs they cannot be reproduced by counting a list: no institution is identified as likely to merge, and no such column exists anywhere in this workbook. A category-level rate says nothing about any particular credit union.",
      "The regional tabs carry a matching block. Because each is rounded to its own cell, the eight blocks can differ from this table by one or two institutions in a category."),
    blocks = list(
      list(head = "Population counts by category (institutions still operating)",
           df = pop_tbl, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT)),
      list(head = sprintf("Five years out: without and with mergers (%s)", H_LAB[3]),
           df = side_tbl, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT)),
      list(head = "Expected exits by category at the cohort date (how many of today's institutions in each category will be gone)",
           df = exit_tbl, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT)),
      list(head = "Exit rates used",
           df = rate_tbl, styles = c(S_NORM, S_NORM, S_DEC, S_DEC, S_INT)),
      list(head = "Backtest: exits predicted from past dates vs actual by the cohort date",
           df = bt_tbl, styles = c(S_NORM, S_NORM, S_INT, S_DEC, S_INT, S_DEC))),
    cols = col_widths(list(c(1, 1, 26), c(2, 7, 20))))
}

## ---------------------------------------------------------------------
## [27.5] Bucket Growth -- nominal vs real
## ---------------------------------------------------------------------
## Real column: same rule as the nominal one. Under the median basis it is
## a tally of medians against edges indexed forward at CPI_ASSUMPTION.
if (AB == "median") {
  defl5   <- (1 + CPI_ASSUMPTION)^(20 / 4)
  real_k  <- findInterval(inst_out$assets_med_5y / defl5, BREAKS[-1]) + 1L
  real_k  <- pmin(pmax(real_k, 1L), N_CAT)
  real_int <- as.integer(table(factor(real_k, levels = seq_len(N_CAT))))
} else {
  real_int <- as.integer(apportion(nominal_vs_real$real_h20, nrow(inst_out)))
}
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

  ## EVERY institution in the cell, largest first. The count table above is
  ## a tally of exactly these rows, so the list must be complete -- a
  ## movers-only list left field staff unable to find their own credit
  ## unions and unable to reproduce the counts.
  cell_inst <- inst_out %>%
    filter(region == rg, cu_type == ct) %>%
    arrange(desc(assets_now)) %>%
    transmute(`Join number` = join_number,
              `Credit union` = cu_name,
              State = state,
              Today = CAT_PRETTY[asset_cat_now],
              `Assets ($M)` = round(assets_now / 1e6, 1),
              `1yr` = CAT_PRETTY[cat_1y],
              `Median 1yr ($M)` = round(assets_med_1y / 1e6, 1),
              `3yr` = CAT_PRETTY[cat_3y],
              `Median 3yr ($M)` = round(assets_med_3y / 1e6, 1),
              `5yr` = CAT_PRETTY[cat_5y],
              `Median 5yr ($M)` = round(assets_med_5y / 1e6, 1),
              `P(5yr)` = p_5y,
              `Changes category by 5yr` = ifelse(cat_5y != asset_cat_now, "yes", ""),
              `Down risk` = ifelse(down_risk_5y, "yes", ""))
  stopifnot(nrow(cell_inst) == n_cell)

  pop_block <- list()
  if (HAVE_EXIT) {
    pcz <- pop_cells %>% filter(region == as.character(rg), cu_type == as.character(ct))
    if (nrow(pcz) == N_CAT + 1) {
      pw <- data.frame(Category = c(CAT_PRETTY[CAT_LABELS], "Merged or closed", "Still operating"),
                       Today = c(pcz$now[1:N_CAT], 0L, sum(pcz$now[1:N_CAT])),
                       stringsAsFactors = FALSE)
      for (i in seq_along(H_SET)) {
        hh <- paste0("h", H_SET[i])
        pw[[H_LAB[i]]] <- c(pcz[[hh]][1:N_CAT], pcz[[hh]][N_CAT + 1], sum(pcz[[hh]][1:N_CAT]))
      }
      pw$Change <- pw[[H_LAB[3]]] - pw$Today
      pop_block <- list(list(
        head = "With mergers: expected population counts (see the With Mergers tab; not a count of the list below)",
        df = pw, styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT)))
    }
  }

  SH[[length(SH) + 1]] <- mk_sheet(
    nm, paste(REG_LAB[as.character(rg)], "-", CT_LAB[as.character(ct)]),
    sprintf("%s institutions", format(n_cell, big.mark = ",")),
    notes = c(
      "Whole numbers of institutions. Counts tie to this cell's own institution count at every horizon; the list below contains every institution in the cell, and counting it by category reproduces the table.",
      "Assignment is by forecast size and cannot move an institution downward. 'Changes category by 5yr' marks institutions whose assigned category differs from today's; 'Down risk' marks elevated probability of falling a category -- see the Down Risk tab."),
    blocks = c(list(
      list(head = "Counts by category (no mergers)", df = wide,
           styles = c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT))),
      pop_block,
      list(list(head = sprintf("All %s institutions, largest first",
                          format(n_cell, big.mark = ",")),
           df = cell_inst,
           styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_INT, S_NORM, S_INT,
                      S_NORM, S_INT, S_NORM, S_INT, S_DEC, S_NORM, S_NORM)))),
    cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 3, 8),
                           c(4, 14, 16))))
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
    sprintf("WHAT HELD UP: category counts. At five years %d of 7 categories are within 2%% and %d of 7 within 5%%; the average error is %.0f institutions on a cohort of %s.",
            sum(abs(count_tab$pct[count_tab$h == 20]) <= 2),
            sum(abs(count_tab$pct[count_tab$h == 20]) <= 5),
            mean(abs(count_tab$err[count_tab$h == 20])),
            format(sum(count_tab$actual[count_tab$h == 20]), big.mark = ",")),
    sprintf("The under-$10M category came in %.0f%% low: the model expected more small institutions to grow out of it than did, because 2022-23 deposit outflows pushed institutions back below the line. This is the same weakness as the downward-movement shortfall below.",
            abs(count_tab$pct[count_tab$h == 20 & count_tab$cat == CAT_LABELS[1]])),
    sprintf("WHAT DID NOT: the $10B-and-over category came in %.0f%% high at five years, and downward movement is under-predicted beyond one year (down ratio %.2f at five years against 1.00 for a perfect forecast).",
            count_tab$pct[count_tab$h == 20 & count_tab$cat == CAT_LABELS[N_CAT]],
            dir_tab$down_ratio[dir_tab$h == 20]),
    sprintf("For comparison, the previous ARIMA-based method predicted %d upward moves against %d actual over five years, and %d downward against %d.",
            FROZEN_REF$up_5y_pred, FROZEN_REF$up_5y_act,
            FROZEN_REF$down_5y_pred, FROZEN_REF$down_5y_act),
    a7_valid_note,
    if (exists("a7_sensitivity")) "The first table shows what the $10B-and-over count would read under each defensible choice of correction factor. The published figures use a single factor pooled across the three- and five-year backtest origins; the per-horizon estimates differ mainly because their windows fall in different growth regimes."),
  blocks = c(
    if (exists("a7_sensitivity")) list(list(
      head = "$10B-and-over count under alternative correction factors (1yr / 3yr / 5yr)",
      df = a7_sensitivity %>%
        transmute(Choice = choice, `Entrant factors` = factors,
                  !!H_LAB[1] := round(h4), !!H_LAB[2] := round(h12),
                  !!H_LAB[3] := round(h20)),
      styles = c(S_NORM, S_NORM, S_INT, S_INT, S_INT))),
    list(
    list(head = "Five-year count accuracy", df = val_counts,
         styles = c(S_NORM, S_INT, S_DEC, S_INT, S_DEC, S_DEC)),
    list(head = "Direction of movement", df = val_dir,
         styles = c(S_NORM, S_DEC, S_INT, S_DEC, S_INT, S_DEC, S_DEC)),
    list(head = "Institution-level accuracy", df = val_acc,
         styles = c(S_NORM, S_INT, S_DEC, S_DEC, S_DEC)))),
  cols = col_widths(list(c(1, 1, 40), c(2, 8, 16))))

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

## ---------------------------------------------------------------------
## [27.13b] START HERE -- reading guide and findings, first tab
##
## Written for examiners who will open the workbook cold. Every number in
## the findings is computed from the objects that built the other tabs, so
## this tab cannot drift from them on a refresh. Prepended to SH so it is
## the first thing Excel opens on.
## ---------------------------------------------------------------------
cohort_lab <- qgrid$q_label[N_Q]
N_ALL      <- nrow(inst_out)
g_row <- function(section, item, text)
  data.frame(Section = section, Item = item, `What it means` = text,
             check.names = FALSE, stringsAsFactors = FALSE)
G <- list()

## ---- 1. what this is ------------------------------------------------
G[[length(G) + 1]] <- g_row("1. What this workbook is", "Purpose",
  sprintf("For every one of the %s federally insured credit unions active at %s, the forecast estimates which asset-size category it is likely to be in one, three and five years from now (%s). Counts by category, by region and charter, and a full institution list all come from the same assignment and agree with each other exactly.",
          format(N_ALL, big.mark = ","), cohort_lab, paste(H_LAB, collapse = ", ")))
G[[length(G) + 1]] <- g_row("1. What this workbook is", "How it was built",
  if (AB == "median")
    "For a credit union of a given size, the model looks at every credit union of that size since 2005 and asks where they were one, three and five years later. The middle of that range of outcomes is the institution's projected assets, and its category at each date is simply the band those projected assets fall in. Every count in this workbook is a tally of those categories. No forecast of the economy, no assumptions about management -- just what has historically happened to institutions of that size."
  else
    "For a credit union of a given size, the model looks at every credit union of that size since 2005 and asks where they were one, three and five years later. That spread of historical outcomes gives each institution a probability for each category. No forecast of the economy, no assumptions about management -- just what has historically happened to institutions of that size.")
G[[length(G) + 1]] <- g_row("1. What this workbook is", "Three things to keep in mind",
  sprintf("(1) NO MERGERS: the total is held at %s throughout; these are not forecasts of how many credit unions will exist. (2) THRESHOLDS ARE IN TODAY'S DOLLARS: some of the movement is inflation carrying institutions across fixed lines. (3) THE COUNTS ARE FAR MORE RELIABLE THAN ANY ONE ROW: an institution near a threshold is close to a coin flip at five years -- always read the probability.",
          format(N_ALL, big.mark = ",")))

## ---- 2. findings (computed) ------------------------------------------
chg <- counts_int$h20 - counts_int$now
find_txt <- paste(sprintf("%s: %s -> %s (%+d)", CAT_PRETTY[counts_int$cat],
                          format(counts_int$now, big.mark = ","),
                          format(counts_int$h20, big.mark = ","), chg),
                  collapse = "; ")
G[[length(G) + 1]] <- g_row("2. Key findings", "Five-year counts by category", find_txt)
G[[length(G) + 1]] <- g_row("2. Key findings", "Direction",
  sprintf("The system keeps shifting upward: the categories under $50M lose %d institutions over five years and every category from $100M up gains, %d in total. The $1B-$10B group gains the most (%+d).",
          -sum(chg[1:2]), sum(chg[4:7]), chg[6]))
i_a7 <- N_CAT
G[[length(G) + 1]] <- g_row("2. Key findings", "The largest institutions",
  sprintf("$10B and over: %d today -> %d (%s) -> %d (%s) -> %d (%s). The growth comes from institutions crossing $10B, not from those already above it. %s",
          counts_int$now[i_a7], counts_int$h4[i_a7], H_LAB[1],
          counts_int$h12[i_a7], H_LAB[2], counts_int$h20[i_a7], H_LAB[3],
          paste(sprintf("%s: %d -> %d", extra_tbl$Threshold, extra_tbl$Today,
                        extra_tbl[[H_LAB[3]]]), collapse = "; ")))
if (exists("POOLS") && exists("fc")) {
  a6_med  <- pool_q(POOLS[["20"]][["6"]], 0.50)
  need10  <- 10e9 / exp(a6_med)
  n_reach <- sum(fc$cat_k == 6 & fc$assets_now >= need10)
  n_9b    <- sum(fc$cat_k == 6 & fc$assets_now >= 9e9)
  G[[length(G) + 1]] <- g_row("2. Key findings", "Why the $10B count rises",
    sprintf("At the typical five-year growth rate for $1B-$10B institutions (%.1f%% a year), any institution above $%.1fB today would reach $10B by %s. %d institutions are above that line, %d of them already above $9B. The forecast has %d crossing -- fewer than the pipeline implies, because historically about half the crossings the raw growth record predicts actually happen ($10B is a regulatory threshold, and some institutions manage to stay under it).",
            100 * (exp(a6_med / 5) - 1), need10 / 1e9, H_LAB[3], n_reach, n_9b,
            counts_int$h20[i_a7] - counts_int$now[i_a7]))
}
if (exists("growth_tbl")) {
  G[[length(G) + 1]] <- g_row("2. Key findings", "How much is inflation",
    sprintf("With the category lines indexed for inflation, the under-$10M count would change by %+d instead of %+d, and the $1B-$10B count by %+d instead of %+d. Most of the decline in the smallest category is the line standing still while institutions grow slowly; most of the growth in $1B-$10B is real.",
            growth_tbl$`Real change`[1], growth_tbl$`Nominal change`[1],
            growth_tbl$`Real change`[6], growth_tbl$`Nominal change`[6]))
}
if (HAVE_EXIT) {
  G[[length(G) + 1]] <- g_row("2. Key findings", "Allowing for mergers",
    sprintf("Historically about %.1f%% of credit unions merge or close each year, mostly small ones. Applying those rates, %s of today's %s institutions are expected still to be operating in %s (%s exits), and the under-$10M category falls to %s rather than %s. See the With Mergers tab.",
            100 * (1 - (1 - pop_counts$h20[N_CAT + 1] / nrow(inst_out))^(1/5)),
            format(nrow(inst_out) - pop_counts$h20[N_CAT + 1], big.mark = ","),
            format(nrow(inst_out), big.mark = ","), H_LAB[3],
            format(pop_counts$h20[N_CAT + 1], big.mark = ","),
            pop_counts$h20[1], counts_int$h20[1]))
}
n_move <- sum(inst_out$cat_5y != inst_out$asset_cat_now)
n_down <- sum(inst_out$down_risk_5y)
G[[length(G) + 1]] <- g_row("2. Key findings", "At the institution level",
  sprintf("%d institutions (%.0f%%) are assigned a higher category by %s; %d are flagged with elevated risk of falling a category. Of the %d moving up, %d have a probability of 0.75 or more and %d are below 0.50.",
          n_move, 100 * n_move / N_ALL, H_LAB[3], n_down, n_move,
          sum(inst_out$cat_5y != inst_out$asset_cat_now & inst_out$p_5y >= 0.75),
          sum(inst_out$cat_5y != inst_out$asset_cat_now & inst_out$p_5y < 0.50)))
G[[length(G) + 1]] <- g_row("2. Key findings", "How accurate",
  sprintf("Tested by forecasting from past dates and comparing with what happened: at five years the average count error was %.0f institutions and %d of 7 categories were within 5%%; the institution-level assignment was right %.0f%% of the time at five years and %.0f%% at one year. Weak spots: downward moves are under-predicted beyond one year, and the $10B count runs high before correction (corrected here). See Validation.",
          mean(abs(count_tab$err[count_tab$h == 20])),
          sum(abs(count_tab$pct[count_tab$h == 20]) <= 5),
          acc_tab$ranked[acc_tab$h == 20], acc_tab$ranked[acc_tab$h == 4]))

## ---- 3. tab by tab ---------------------------------------------------
tab_help <- list(
  c("Method", "Two-paragraph description of the approach, the three assumptions above, and every setting used in this run.", "Read first if you are asked how the numbers were produced.", "Nothing to filter; it is text."),
  c("Total", "System-wide counts by category today and at each horizon, with the five-year change. Below it, supplementary lines for $10B-$15B, $15B+ and $20B+.", "Read across a row to follow a category over time; read down a column for the size distribution at one date. Every number is a whole number of institutions that you can reproduce by filtering the Institutions tab.", "The supplementary lines OVERLAP the main table (a $16B institution is in $10B and over AND in $15B and over). Do not add them to the total."),
  c("Bucket Growth", "The five-year counts two ways: with today's dollar thresholds, and with thresholds raised for inflation. The difference is 'threshold drift'.", "If the real column barely moves while the nominal column falls, the category is shrinking because the line is fixed, not because institutions are shrinking.", "Only the nominal figures are the published forecast; the real column is context."),
  c("Transitions", "For each horizon, a grid of how many institutions move from each category (rows) to each category (columns), followed by the same grid as row percentages.", "Read along a row: 'of the institutions in $50M-$100M today, N stay and M move up.' Row totals are today's counts; column totals are the horizon's counts.", "Institutions are never assigned downward, so the cells below the diagonal are zero by construction. Expected downward movement is on the Down Risk tab."),
  c("R1_FCU ... R8_FISCU", "One tab per region and charter type (ONES = Office of National Examinations and Supervision, coded region 8). Each has the count table for that cell and a complete list of every institution in it, largest first, with category and projected assets at each horizon.", "Find your institutions by name or join number (Ctrl+F). 'Changes category by 5yr' = yes marks the movers; 'Down risk' = yes marks elevated risk of falling. Counting the list by category reproduces the table above it, and the eight tabs add to the Total tab.", "ONES is a supervisory office, not a geography; its two tabs hold the largest institutions and should not be compared to a regional tab as if it were a fourth region."),
  c("Institutions", "All institutions in one list with every forecast column: category and probability at each horizon, projected assets (median, and the 10th/90th percentile at five years), the probabilities of moving up / staying / down, and the down-risk and short-history flags.", "Use the filters in the header row. Sort by any column. The probability column is the most important one on the sheet -- see section 4 below.", "A row with probability 0.55 and a row with probability 0.97 look identical in the category column. They are not equally reliable."),
  c("Down Risk", "The institutions with the highest probability of falling to a lower category by five years, with their current and projected assets.", "These are the institutions where the assignment (which cannot move anyone down) says least. If an institution you examine is here, its category on the other tabs should be read with that in mind.", "Absence from this list does not mean zero risk; it means below the listing threshold."),
  c("Large Institutions", "Institutions at or approaching $10B and $15B, with projected assets and the probability of being above each level.", "The named list behind the $10B, $15B and $20B lines on the Total tab. For planning around the largest institutions, this is the tab.", "Probabilities here move in coarse steps because there are few historical institutions this size."),
  c("Validation", "How the method performed when run from past dates (2021, 2023, 2025) and compared with actual 2026 outcomes: count accuracy by category, up/down movement, institution-level hit rates, and the $10B correction under alternative choices.", "Use it to answer 'how much should I trust this?' The first table shows what the $10B count would be with no correction and with the alternatives; the published figure is the middle choice.", "The backtest figures are UNCORRECTED on purpose, so the bias the correction addresses is visible."),
  c("Diagnostics", "Model-selection results and estimation detail, including the unrounded expected counts behind the whole numbers.", "Analysts only. Nothing here changes how the other tabs are read.", "The unrounded counts are fractions; do not quote them to the field.")
)
for (t in tab_help) {
  G[[length(G) + 1]] <- g_row("3. Tab by tab", paste(t[1], "- what it shows"), t[2])
  G[[length(G) + 1]] <- g_row("3. Tab by tab", paste(t[1], "- how to read it"), t[3])
  G[[length(G) + 1]] <- g_row("3. Tab by tab", paste(t[1], "- watch out for"), t[4])
}

## ---- 4. reading an institution row ----------------------------------
row_help <- list(
  c("Today / Assets ($M)", "The institution's current category and total assets in millions, from the cohort Call Report."),
  c("1yr / 3yr / 5yr", "The category the institution is assigned to at each horizon. Assignment is by projected size, so it never moves an institution below today's category."),
  c("P(1yr) / P(3yr) / P(5yr)", "The share of historically comparable institutions that ended up in that category. 0.97 is close to certain; 0.55 is close to a coin flip. This is the column to read before acting on any category."),
  c("Confidence", "The five-year probability in words: high (0.75 or more), medium (0.50-0.75), low (under 0.50)."),
  c("Median 1yr / 3yr / 5yr ($M)", "The middle of the projected asset range: half of comparable institutions historically ended above it, half below. It is context, not a target."),
  c("Low 5yr / High 5yr ($M)", "The 10th and 90th percentiles of projected assets at five years -- the range four institutions in five would fall inside."),
  c("P(up) / P(same) / P(down)", "The probabilities of ending in a higher, the same, or a lower category at five years. P(down) is the information the assignment cannot show."),
  c("Down risk", "'yes' when P(down) is materially elevated. The institution is still assigned by projected size; the flag adds what the assignment leaves out."),
  c("Short history", "'yes' for institutions with too little Call Report history to compute their own trailing measures. Their forecast uses their category's experience, which is what every forecast uses, so this does not make the row less reliable.")
)
for (t in row_help)
  G[[length(G) + 1]] <- g_row("4. Reading an institution row", t[1], t[2])

## ---- 5. quick answers ------------------------------------------------
qa <- list(
  c("My credit union is shown moving up and I know it is not growing.", "The model knows only its size. Your knowledge of a pending merger, a shrinking sponsor or a decision to hold the balance sheet flat should take precedence. Check the probability: below about 0.7 the model itself is not confident."),
  c("Why does nobody on my regional tab move down?", "Assignment is by projected size, which grows for every category, so it cannot place an institution below today's category. The Down Risk tab names the institutions most likely to fall."),
  c("Why is the under-$10M count falling when these institutions are not disappearing?", "No mergers are in these numbers, so none of the decline is institutions leaving. Almost all of it is slow growth across a $10M line that has never moved -- see Bucket Growth."),
  c("Is the $10B number a prediction that these specific institutions will cross?", "It is a count: the sum of each institution's probability of crossing, rounded and assigned. The Large Institutions tab names the candidates with their probabilities; use those, not the category alone."),
  c("Can I add the $15B and $20B lines to the table?", "No. They overlap the $10B-and-over row. The $10B-$15B line is the one that does not overlap."),
  c("How often will this be refreshed?", "Each time a new Call Report quarter is processed. Counts will move modestly with each refresh.")
)
for (t in qa)
  G[[length(G) + 1]] <- g_row("5. Quick answers", t[1], t[2])

## ---- 6. do / don't ----------------------------------------------------
G[[length(G) + 1]] <- g_row("6. Using it well", "Appropriate uses",
  "Planning examination resources by region and asset category two to five years out. Anticipating which institutions are likely to cross $1B or $10B, and when. Answering questions about the direction of consolidation. Providing a documented baseline against which examiner judgment about a specific institution can be compared.")
G[[length(G) + 1]] <- g_row("6. Using it well", "Uses to avoid",
  "Treating any single institution's five-year category as a prediction, especially below probability 0.75. Reading the counts as a forecast of how many credit unions will exist. Adding the supplementary threshold lines to the main table. Comparing an ONES tab to a regional tab as if ONES were a region. Using the counts in the smallest categories as a floor -- if deposit outflows recur, actual counts there will be higher than forecast.")

guide_df <- bind_rows(G)
guide_sheet <- mk_sheet(
  "Start Here", "How to read this workbook, and what it finds",
  sprintf("Credit Union Growth Forecast -- cohort %s, %s institutions -- Office of the Chief Economist",
          cohort_lab, format(N_ALL, big.mark = ",")),
  notes = c("Read section 1 before anything else. Section 2 is the findings; section 3 is a tab-by-tab reading guide; section 4 explains every column on the institution lists.",
            "A plain-language report with a fuller FAQ and a technical appendix accompanies this workbook."),
  blocks = list(list(head = NULL, df = guide_df,
                     styles = c(S_WRAP, S_WRAP, S_WRAP))),
  cols = col_widths(list(c(1, 1, 26), c(2, 2, 44), c(3, 3, 120))),
  freeze = list(x = 0, y = 6))
SH <- c(list(guide_sheet), SH)
cat("Start Here tab:", nrow(guide_df), "rows\n")

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
            if (AB == "median") " -- NOT corrected; counts are tallies by projected assets"
            else if (A7_APPLIED) paste0(" -- CORRECTED in published figures (", a7_fac_txt, ")")
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
