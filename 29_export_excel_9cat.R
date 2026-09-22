## =====================================================================
## 29_export_excel_9cat.R  --  The 27 workbook with the top category split
##
##   $10B and over  ->  $10B-$15B  |  $15B-$20B  |  $20B and over
##
## Nine categories instead of seven. Everything else is the same run:
## same cohort, same medians, same assignment rule (ASSIGN_BASIS =
## "median"), same merger rates. The finer bands are simply finer edges
## laid over the same projected assets, so every count here is a tally of
## the same institution list, and the seven-category totals on the main
## workbook are reproduced by adding the three top bands together.
##
## WHAT IS HERE            Start Here, Method, Total, Transitions, the eight
##                         regional tabs, Institutions, With Mergers.
## WHAT IS NOT             Validation, Diagnostics, Rule Comparison, Bucket
##                         Growth, Down Risk, Large Institutions. Those are
##                         seven-category diagnostics and do not change when
##                         the top band is split; the main workbook carries
##                         them. This file says so on its Method tab.
##
## RUN AFTER 27 in the same session. It borrows 27's objects (inst_out,
## counts_int, the helpers) and writes
##   CU_Growth_Forecast_<cohort>_probability_9cat.xlsx
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
## [29.0] Objects from 27's session
## ---------------------------------------------------------------------
SCRIPT29_VERSION <- "2026-09-21a"
cat("29_export_excel_9cat.R version", SCRIPT29_VERSION, "\n")
need <- c("inst_out", "qgrid", "N_Q", "H_SET", "H_LAB", "BREAKS", "mk_sheet",
          "xlsx_write", "col_widths", "S_NORM", "S_INT", "S_DEC", "S_WRAP",
          "REG_LAB", "CT_LAB", "apportion")
miss <- setdiff(need, ls(.GlobalEnv))
if (length(miss))
  stop("29 needs objects from 27's session: ", paste(miss, collapse = ", "),
       ". Run 27 first (it defines the sheet helpers).")
if (!exists("S_MIXED")) stop("Run [27.0b] first -- it adds the S_MIXED number format.")
AB <- if (exists("ASSIGN_BASIS")) ASSIGN_BASIS else "counts"
if (AB != "median")
  stop("29 is written for ASSIGN_BASIS = \"median\": a nine-band label is the band ",
       "the median falls in. Under the counts basis there is no nine-band assignment.")

## ---------------------------------------------------------------------
## [29.1] The nine-category scheme
## ---------------------------------------------------------------------
BREAKS9  <- c(BREAKS[1:7], 15e9, 20e9, Inf)   # -Inf,10M,50M,100M,500M,1B,10B,15B,20B,Inf
CAT9     <- c("A1_LT10M", "A2_10to50M", "A3_50to100M", "A4_100to500M",
              "A5_500Mto1B", "A6_1Bto10B", "A7_10to15B", "A8_15to20B", "A9_GE20B")
PRETTY9  <- c("Under $10M", "$10M - $50M", "$50M - $100M", "$100M - $500M",
              "$500M - $1B", "$1B - $10B", "$10B - $15B", "$15B - $20B",
              "$20B and over")
names(PRETTY9) <- CAT9
N9 <- length(CAT9)
cat9_of <- function(x) CAT9[pmin(pmax(findInterval(x, BREAKS9[-1]) + 1L, 1L), N9)]

## Relabel the institution table. Medians are the same numbers 27 used;
## only the edges are finer.
i9 <- inst_out %>%
  mutate(cat9_now = cat9_of(assets_now),
         cat9_1y  = cat9_of(assets_med_1y),
         cat9_3y  = cat9_of(assets_med_3y),
         cat9_5y  = cat9_of(assets_med_5y))

## The seven-category labels must be recoverable by collapsing the top
## three -- otherwise something has drifted between 24 and here.
collapse7 <- function(c9) ifelse(c9 %in% CAT9[7:9], "A7_GE10B", c9)
stopifnot(identical(collapse7(i9$cat9_now), as.character(i9$asset_cat_now)),
          identical(collapse7(i9$cat9_5y),  as.character(i9$cat_5y)))
cat("Nine-band labels collapse to the published seven-band labels. OK.\n")

H9COL <- c("4" = "cat9_1y", "12" = "cat9_3y", "20" = "cat9_5y")
tab9 <- function(d) {
  out <- data.frame(cat = CAT9, stringsAsFactors = FALSE)
  out$now <- as.integer(table(factor(d$cat9_now, levels = CAT9)))
  for (h in H_SET)
    out[[paste0("h", h)]] <- as.integer(table(factor(d[[H9COL[as.character(h)]]], levels = CAT9)))
  out
}
counts9 <- tab9(i9)
stopifnot(all(colSums(counts9[, -1]) == nrow(i9)))
cat("\nNine-category counts:\n"); print(counts9, row.names = FALSE)

cells9 <- i9 %>% group_by(region, cu_type) %>% group_modify(~ tab9(.x)) %>% ungroup()

SH9 <- list()
cohort_lab <- qgrid$q_label[N_Q]
N_ALL <- nrow(i9)
pretty_tbl <- function(ct) {
  d <- data.frame(Category = PRETTY9[ct$cat], Today = ct$now, stringsAsFactors = FALSE,
                  check.names = FALSE)
  for (i in seq_along(H_SET)) d[[H_LAB[i]]] <- ct[[paste0("h", H_SET[i])]]
  d$Change <- d[[H_LAB[3]]] - d$Today
  d <- bind_rows(d, data.frame(Category = "TOTAL", Today = sum(d$Today),
                               setNames(as.list(colSums(d[, H_LAB])), H_LAB),
                               Change = 0L, check.names = FALSE))
  d
}
sty_cnt <- c(S_NORM, S_INT, S_INT, S_INT, S_INT, S_INT)

## ---------------------------------------------------------------------
## [29.2] Start Here + Method (short; points to the main workbook)
## ---------------------------------------------------------------------
SH9[[length(SH9) + 1]] <- mk_sheet(
  "Start Here", "How to read this workbook",
  sprintf("Credit Union Growth Forecast -- cohort %s, %s institutions -- nine asset categories",
          cohort_lab, format(N_ALL, big.mark = ",")),
  notes = c(
    "This is the companion to the main forecast workbook. It is the same forecast -- same institutions, same projected assets, same category rule -- with the $10B-and-over category split into three: $10B-$15B, $15B-$20B and $20B and over.",
    "Every count is a whole number of institutions, and a credit union is counted in the band its projected (median) assets fall in. Filter the Institutions tab by category and date and you reproduce any figure here. Adding the three top bands together reproduces the $10B-and-over figures on the main workbook.",
    "Read the main workbook's Start Here and Caveats tabs for how the projections are built, how accurate they have been, and what the $10B figures do and do not mean. Those points apply here unchanged; splitting the top band does not make it more certain.",
    "Nothing above $10B is adjusted for the tendency of institutions to cross that line less often than typical growth implies (see the main workbook). Treat the three top bands as upper estimates."),
  blocks = list(), cols = col_widths(list(c(1, 1, 160))))

SH9[[length(SH9) + 1]] <- mk_sheet(
  "Method", "Method",
  "Same run as the main workbook; only the top category is split",
  notes = c(
    sprintf("Cohort %s, %s federally insured credit unions, regions %s, horizons %s.",
            cohort_lab, format(N_ALL, big.mark = ","),
            paste(sort(unique(i9$region)), collapse = ", "), paste(H_LAB, collapse = ", ")),
    "For each institution, projected assets at each horizon are the middle of the range of outcomes reached by credit unions of its size since 2005. Its category is the band those projected assets fall in. Counts are tallies of those categories.",
    "Growth distributions are estimated on the seven original categories; the three top bands share the distribution of the $10B-and-over category. Splitting the band changes where the lines are drawn, not how institutions are projected.",
    "Validation, diagnostics, the rule comparison, real-terms restatement and the down-risk list are seven-category exhibits and are not repeated here. See the main workbook.",
    sprintf("Produced %s from the same session as the main workbook.", format(Sys.Date()))),
  blocks = list(), cols = col_widths(list(c(1, 1, 160))))

## ---------------------------------------------------------------------
## [29.3] Total
## ---------------------------------------------------------------------
tot9 <- pretty_tbl(counts9)
SH9[[length(SH9) + 1]] <- mk_sheet(
  "Total", "Projected counts by asset category",
  sprintf("All %s institutions. Totals are held fixed -- no mergers.", format(N_ALL, big.mark = ",")),
  notes = c(
    "Every count is a whole number of institutions and matches the Institutions tab exactly. The eight regional tabs add to this one.",
    "The three bands from $10B upward together equal the $10B-and-over category on the main workbook. Read them as upper estimates; see the main workbook's Caveats tab."),
  blocks = list(list(head = "Counts by category", df = tot9, styles = sty_cnt)),
  cols = col_widths(list(c(1, 1, 22), c(2, 6, 14))))

## ---------------------------------------------------------------------
## [29.4] Transitions
## ---------------------------------------------------------------------
trans_blocks9 <- unlist(lapply(H_SET, function(h) {
  m <- table(factor(i9$cat9_now, levels = CAT9), factor(i9[[H9COL[as.character(h)]]], levels = CAT9))
  m <- matrix(as.integer(m), N9, N9, dimnames = list(CAT9, CAT9))
  df <- data.frame(From = PRETTY9[CAT9], m, check.names = FALSE, stringsAsFactors = FALSE)
  names(df)[-1] <- PRETTY9[CAT9]; df$Total <- as.integer(rowSums(m))
  pct <- t(apply(m, 1, function(r) if (sum(r) > 0) apportion(100 * r / sum(r), 100L) else rep(0L, N9)))
  dfp <- data.frame(From = PRETTY9[CAT9], pct, check.names = FALSE, stringsAsFactors = FALSE)
  names(dfp)[-1] <- PRETTY9[CAT9]
  list(list(head = sprintf("%s -- number of institutions", H_LAB[match(h, H_SET)]),
            df = df, styles = c(S_NORM, rep(S_INT, N9 + 1))),
       list(head = sprintf("%s -- share of row (%%)", H_LAB[match(h, H_SET)]),
            df = dfp, styles = c(S_NORM, rep(S_INT, N9))))
}), recursive = FALSE)
SH9[[length(SH9) + 1]] <- mk_sheet(
  "Transitions", "Category transitions",
  "Row = category at the cohort date, column = category at the horizon",
  notes = c("Whole numbers of institutions counted off the Institutions tab. Row totals are today's counts; column totals are the horizon's counts on the Total tab.",
            "Each horizon is a separate comparison with the cohort date; the five-year table is not the one-year table applied five times.",
            "Institutions are labelled by projected size, which grows for every category, so nothing appears below the diagonal."),
  blocks = trans_blocks9, cols = col_widths(list(c(1, 1, 18), c(2, 11, 14))))

## ---------------------------------------------------------------------
## [29.5] Regional tabs -- counts, optional merger block, full list
## ---------------------------------------------------------------------
HAVE_EXIT9 <- exists("P_EXIT") && exists("STATE_LAB")
cells <- cells9 %>% distinct(region, cu_type) %>% arrange(region, cu_type)
for (i in seq_len(nrow(cells))) {
  rg <- cells$region[i]; ct <- cells$cu_type[i]
  nm <- paste0("R", rg, "_", CT_LAB[as.character(ct)])
  d  <- cells9 %>% filter(region == rg, cu_type == ct)
  idx <- which(i9$region == rg & i9$cu_type == ct)
  n_cell <- length(idx)
  wide <- pretty_tbl(d)

  pop_block <- list()
  if (HAVE_EXIT9) {
    pw <- data.frame(Category = c(PRETTY9, "Merged or closed", "Still operating"),
                     Today = c(d$now, 0L, sum(d$now)), stringsAsFactors = FALSE)
    for (j in seq_along(H_SET)) {
      hh <- as.character(H_SET[j]); surv <- 1 - P_EXIT[[hh]][idx]
      k <- i9[[H9COL[hh]]][idx]
      soft <- tapply(surv, factor(k, levels = CAT9), sum); soft[is.na(soft)] <- 0
      v <- as.integer(apportion(c(soft, n_cell - sum(soft)), n_cell))
      pw[[H_LAB[j]]] <- c(v[1:N9], v[N9 + 1], sum(v[1:N9]))
    }
    pw$Change <- pw[[H_LAB[3]]] - pw$Today
    pop_block <- list(list(head = "With mergers: expected population counts (not a count of the list below)",
                           df = pw, styles = sty_cnt))
  }

  lst <- i9[idx, ] %>% arrange(desc(assets_now)) %>%
    transmute(`Join number` = join_number, `Credit union` = cu_name, State = state,
              Today = PRETTY9[cat9_now], `Assets ($M)` = round(assets_now / 1e6, 4),
              `1yr` = PRETTY9[cat9_1y], `Median 1yr ($M)` = round(assets_med_1y / 1e6, 4),
              `3yr` = PRETTY9[cat9_3y], `Median 3yr ($M)` = round(assets_med_3y / 1e6, 4),
              `5yr` = PRETTY9[cat9_5y], `Median 5yr ($M)` = round(assets_med_5y / 1e6, 4),
              Confidence = as.character(conf_5y),
              `Changes category by 5yr` = ifelse(cat9_5y != cat9_now, "yes", ""))

  SH9[[length(SH9) + 1]] <- mk_sheet(
    nm, paste(REG_LAB[as.character(rg)], "-", CT_LAB[as.character(ct)]),
    sprintf("%s institutions", format(n_cell, big.mark = ",")),
    notes = c("Whole numbers of institutions. Counting the list by category reproduces the table above; the eight regional tabs add to the Total tab.",
              "'Changes category by 5yr' marks institutions whose five-year band differs from today's. Confidence is high / medium / low for the five-year band."),
    blocks = c(list(list(head = "Counts by category (no mergers)", df = wide, styles = sty_cnt)),
               pop_block,
               list(list(head = sprintf("All %s institutions, largest first", format(n_cell, big.mark = ",")),
                         df = lst,
                         styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_MIXED, S_NORM, S_MIXED,
                                    S_NORM, S_MIXED, S_NORM, S_MIXED, S_NORM, S_NORM)))),
    cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 3, 8), c(4, 13, 16))))
}

## ---------------------------------------------------------------------
## [29.6] Institutions
## ---------------------------------------------------------------------
inst9 <- i9 %>% arrange(desc(assets_now)) %>%
  transmute(`Join number` = join_number, `Credit union` = cu_name,
            Region = REG_LAB[as.character(region)], Charter = CT_LAB[as.character(cu_type)],
            State = state, `Assets ($M)` = round(assets_now / 1e6, 4),
            `Category today` = PRETTY9[cat9_now],
            `1yr` = PRETTY9[cat9_1y], `Median 1yr ($M)` = round(assets_med_1y / 1e6, 4),
            `3yr` = PRETTY9[cat9_3y], `Median 3yr ($M)` = round(assets_med_3y / 1e6, 4),
            `5yr` = PRETTY9[cat9_5y], `Median 5yr ($M)` = round(assets_med_5y / 1e6, 4),
            Confidence = as.character(conf_5y),
            `Low 5yr ($M)` = round(assets_p10_5y / 1e6, 4),
            `High 5yr ($M)` = round(assets_p90_5y / 1e6, 4),
            `Down risk` = ifelse(down_risk_5y, "yes", ""),
            `Short history` = ifelse(short_history, "yes", ""))
SH9[[length(SH9) + 1]] <- mk_sheet(
  "Institutions", "All institutions",
  sprintf("%s credit unions, sorted by current assets", format(N_ALL, big.mark = ",")),
  notes = c("A credit union's category at each date is the band its projected (median) assets fall in. Counting this tab by category and date reproduces the Total tab and each regional tab exactly.",
            "Confidence is high / medium / low for the five-year band. Low and High are the 10th and 90th percentiles of projected assets at five years. Figures below $1M are shown to four decimal places."),
  blocks = list(list(head = NULL, df = inst9,
                     styles = c(S_NORM, S_NORM, S_NORM, S_NORM, S_NORM, S_MIXED, S_NORM,
                                S_NORM, S_MIXED, S_NORM, S_MIXED, S_NORM, S_MIXED, S_NORM,
                                S_MIXED, S_MIXED, S_NORM, S_NORM))),
  cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 5, 12), c(6, 18, 14))),
  freeze = list(x = 2, y = 6), autofilter = sprintf("A6:R%d", 6 + nrow(inst9)))

## ---------------------------------------------------------------------
## [29.7] With Mergers (nine bands) -- if 26 has run
## ---------------------------------------------------------------------
if (HAVE_EXIT9) {
  pop9 <- data.frame(Category = c(PRETTY9, "Merged or closed", "Still operating"),
                     Today = c(counts9$now, 0L, N_ALL), stringsAsFactors = FALSE)
  for (j in seq_along(H_SET)) {
    hh <- as.character(H_SET[j]); surv <- 1 - P_EXIT[[hh]]
    soft <- tapply(surv, factor(i9[[H9COL[hh]]], levels = CAT9), sum); soft[is.na(soft)] <- 0
    v <- as.integer(apportion(c(soft, N_ALL - sum(soft)), N_ALL))
    pop9[[H_LAB[j]]] <- c(v[1:N9], v[N9 + 1], sum(v[1:N9]))
  }
  pop9$Change <- pop9[[H_LAB[3]]] - pop9$Today
  SH9[[length(SH9) + 1]] <- mk_sheet(
    "With Mergers", "Projected counts allowing for mergers and liquidations",
    sprintf("All %s institutions at %s. Totals FALL as institutions leave.", format(N_ALL, big.mark = ","), cohort_lab),
    notes = c(sprintf("Each institution's category is weighted by its chance of still operating at each date; the remainder is counted as merged or closed. Its chance of merging or closing is %s -- the rates on the main workbook's With Mergers tab (the three top bands share the $10B-and-over rate).",
                      if (exists("exit_words")) exit_words()$how else "the exit rate of its asset category"),
              "Expected counts, rounded to whole numbers. They cannot be reproduced by counting a list: no institution is identified as likely to merge."),
    blocks = list(list(head = "Population counts by category (institutions still operating)", df = pop9, styles = sty_cnt)),
    cols = col_widths(list(c(1, 1, 26), c(2, 6, 16))))
}

## ---------------------------------------------------------------------
## [29.8] Write
## ---------------------------------------------------------------------
OUT9 <- sprintf("CU_Growth_Forecast_%s_probability_9cat.xlsx", cohort_lab)
xlsx_write(SH9, OUT9)
cat("\nWritten:", normalizePath(OUT9), "\n")
cat("Tie-outs: Total", sum(counts9$h20), "| regional", sum(cells9$h20),
    "| institutions", nrow(inst9), "\n")
stopifnot(sum(counts9$h20) == N_ALL, sum(cells9$h20) == N_ALL, nrow(inst9) == N_ALL)
