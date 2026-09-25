## =====================================================================
## 34_merger_table_by_region.R -- the reconciled "then and now" tables
##   by region x charter type, one tab each, summing to the Total tab
## =====================================================================
## Asked for by the field (25 Sep 2026): the aggregate table that every
## row adds across --
##     start - merged or closed + net moves + new charters = end
## disaggregated by region and charter type (R1_FCU, R1_FISCU, ...), with
## a check that the tabs sum to the total.
##
## Two tables per tab:
##   A  the last five years (2021Q2 -> 2026Q2), as they happened, from the
##      panel: exits by exit_q, survivors placed by their class / region /
##      charter at the cohort date, entrants = today's institutions absent
##      five years ago;
##   B  the next five years (2026Q2 -> 2031Q2), as forecast: expected exits
##      = the exit probabilities 26 applied (P_EXIT), survivors placed by
##      the growth model's median assignment (inst_out$cat_5y), the same
##      basis as the With Mergers tab.
## Cells are region x cu_type, and each credit union is kept in the cell it
## had at the START of each table (2021Q2 for A, today for B): the few that
## changed region or charter type are not moved (user's simplifying
## assumption, 25 Sep 2026; the count of such cases is printed and noted on
## the Total tab). The moves column is therefore between size classes only,
## and the cell tabs sum exactly to the Total tab.
## Whole numbers on every tab are allocated by largest remainder within
## each class so that the cell tabs add to the Total tab's integers; the
## moves column on the forecast table is the residual that closes the row.
## Run after 26 and 27 (needs panel_exit.rds and panel_with_mergers.rds).
## ---------------------------------------------------------------------
if (!exists("CONFIG_LOADED")) {
  for (.p in c("00_config.R", "../00_config.R",
               "S:/Projects/Credit_Union_Growth_Forecast/00_config.R"))
    if (file.exists(.p)) { source(.p); break }
  rm(.p)
}
if (!exists("cfg_get")) cfg_get <- function(name, default) default
setwd(cfg_get("DATA_DIR", "S:/Projects/Credit_Union_Growth_Forecast/Data"))
library(dplyr); library(tidyr)
SCRIPT34_VERSION <- "2026-09-25b"
cat("34_merger_table_by_region.R version", SCRIPT34_VERSION, "\n")

## ---------------------------------------------------------------------
## [34.0] Objects
## ---------------------------------------------------------------------
if (!exists("CAT_LABELS") || !exists("N_Q")) {
  .pp <- readRDS("panel_prep.rds")
  for (.k in c("CAT_LABELS", "CAT_PRETTY", "N_CAT", "N_Q", "START_YEAR", "qgrid", "REGIONS"))
    if (!exists(.k) && !is.null(.pp[[.k]])) assign(.k, .pp[[.k]])
  rm(.pp, .k)
}
if (!exists("panel"))    { .pp <- readRDS("panel_prep.rds"); panel <- .pp$panel; rm(.pp) }
if (!exists("fc"))       { prb <- readRDS("panel_probs.rds");  list2env(prb, .GlobalEnv) }
if (!exists("inst_out")) { asg <- readRDS("panel_assign.rds"); list2env(asg, .GlobalEnv) }
if (!exists("REG_LAB")) REG_LAB <- cfg_get("REG_LAB", c("1" = "Region 1", "2" = "Region 2", "3" = "Region 3", "8" = "ONES"))
if (!exists("CT_LAB"))  CT_LAB  <- cfg_get("CT_LAB",  c("1" = "FCU", "2" = "FISCU"))
EXIT_MODEL <- cfg_get("EXIT_MODEL", "cat_env2")
.ex <- readRDS("panel_exit.rds")
stopifnot(identical(.ex$EXIT_MODEL, EXIT_MODEL), length(.ex$P_EXIT[["20"]]) == nrow(fc))
p20 <- .ex$P_EXIT[["20"]]                      # five-year exit probability, aligned to fc$join_number
rm(.ex)
WM <- if (file.exists("panel_with_mergers.rds")) readRDS("panel_with_mergers.rds") else NULL
cohort_lab <- qgrid$q_label[N_Q]; o_hist <- N_Q - 20L; hist_lab <- qgrid$q_label[o_hist]
H5_LAB <- { y <- START_YEAR + (N_Q + 20L - 1L) %/% 4L; q <- (N_Q + 20L - 1L) %% 4L + 1L; sprintf("%dQ%d", y, q) }
cat("Cohort", cohort_lab, "| history from", hist_lab, "| forecast to", H5_LAB, "| exit model", EXIT_MODEL, "\n")

cat_pretty <- unname(CAT_PRETTY[CAT_LABELS])
cell_of <- function(region, cu_type) sprintf("R%s_%s", region, CT_LAB[as.character(cu_type)])
## largest-remainder rounding: integers that sum to `total`
lr_alloc <- function(x, total) {
  x[is.na(x)] <- 0; total <- as.integer(round(total))
  if (sum(x) <= 0) return(rep(0L, length(x)))
  raw <- x / sum(x) * total; fl <- floor(raw); k <- total - sum(fl)
  if (k > 0) { o <- order(raw - fl, decreasing = TRUE)[seq_len(k)]; fl[o] <- fl[o] + 1 }
  as.integer(fl)
}

## ---------------------------------------------------------------------
## [34.1] Table A -- the last five years, per (cell, class)
## ---------------------------------------------------------------------
h0 <- panel %>% filter(q_index == o_hist) %>%
  transmute(join_number, k0 = cat_k, c0 = cell_of(region, cu_type), exit_q)
h1 <- panel %>% filter(q_index == N_Q) %>%
  transmute(join_number, k1 = cat_k, c1 = cell_of(region, cu_type))
h0 <- h0 %>% left_join(h1, by = "join_number") %>%
  mutate(status = ifelse(!is.na(exit_q) & exit_q <= N_Q, "exit", ifelse(!is.na(k1), "survivor", "other")))
## the simplifying assumption: a survivor keeps its start cell whatever its region / charter today
N_CELL_CHANGED <- sum(h0$status == "survivor" & h0$c1 != h0$c0)
h0$c1 <- ifelse(h0$status == "survivor", h0$c0, h0$c1)
today <- inst_out %>% filter(join_number %in% fc$join_number) %>%
  transmute(join_number, k = match(as.character(asset_cat_now), CAT_LABELS), cell = cell_of(region, cu_type))
stopifnot(!anyNA(today$k))
entr <- today %>% filter(!(join_number %in% h0$join_number))          # new since the start date
surv <- h0 %>% filter(status == "survivor")
cells <- sort(unique(c(h0$c0, today$cell)))
grid  <- expand.grid(cell = cells, k = seq_len(N_CAT), stringsAsFactors = FALSE)
cnt <- function(df, cellcol, kcol) {
  if (!nrow(df)) return(rep(0L, nrow(grid)))
  t <- table(factor(df[[cellcol]], levels = cells), factor(df[[kcol]], levels = seq_len(N_CAT)))
  as.integer(t[cbind(match(grid$cell, cells), grid$k)])
}
A <- grid
A$start <- cnt(h0, "c0", "k0")
A$exits <- cnt(h0 %>% filter(status == "exit"), "c0", "k0")
A$other <- cnt(h0 %>% filter(status == "other"), "c0", "k0")
A$new   <- cnt(entr, "cell", "k")
## survivors: arrivals at their end class minus departures from their start class, within the cell
mv <- surv %>% filter(k1 != k0)
A$moves <- cnt(mv, "c1", "k1") - cnt(mv, "c0", "k0")
## the end count on a cell tab: survivors in their START cell (by today's class) plus entrants in today's cell
A$end   <- cnt(surv, "c1", "k1") + cnt(entr, "cell", "k")
A$chk   <- A$start - A$exits - A$other + A$new + A$moves - A$end
if (any(A$chk != 0)) {
  print(A[A$chk != 0, ])
  stop("[34.1] the past table does not close in some (cell, class).")
}
## the class totals must still be today's cohort exactly (cells only redistribute)
stopifnot(identical(as.integer(tapply(A$end, A$k, sum)), as.integer(table(factor(today$k, levels = seq_len(N_CAT))))))
cat(sprintf("\nA -- %s to %s: %d institutions, %d merged or closed, %d other departures, %d survivors, %d new; %d cells; %d survivors changed region or charter type and are kept in their %s cell\n",
            hist_lab, cohort_lab, nrow(h0), sum(A$exits), sum(A$other), nrow(surv), nrow(entr), length(cells), N_CELL_CHANGED, hist_lab))

## ---------------------------------------------------------------------
## [34.2] Table B -- the next five years, per (cell, class)
## ---------------------------------------------------------------------
fcx <- today %>% mutate(p = p20[match(join_number, fc$join_number)],
                        k5 = match(as.character(inst_out$cat_5y[match(join_number, inst_out$join_number)]), CAT_LABELS))
stopifnot(!anyNA(fcx$p), !anyNA(fcx$k5))
B <- grid
B$today   <- cnt(fcx, "cell", "k")
.M_ex  <- tapply(fcx$p,     list(factor(fcx$cell, levels = cells), factor(fcx$k,  levels = seq_len(N_CAT))), sum)
.M_end <- tapply(1 - fcx$p, list(factor(fcx$cell, levels = cells), factor(fcx$k5, levels = seq_len(N_CAT))), sum)
.M_ex[is.na(.M_ex)] <- 0; .M_end[is.na(.M_end)] <- 0
B$exits_u <- .M_ex[cbind(match(B$cell, cells), B$k)]      # expected exits FROM the cell and class today
B$end_u   <- .M_end[cbind(match(B$cell, cells), B$k)]     # expected survivors IN the class at the horizon
rm(.M_ex, .M_end)
## class-level integers to allocate: the With Mergers tab's own if 27 saved them
tot_end <- if (!is.null(WM) && identical(WM$cohort_lab, cohort_lab)) as.integer(WM$pc$h20[seq_len(N_CAT)]) else
  as.integer(round(tapply(B$end_u, B$k, sum)))
tot_ex  <- { x <- tapply(B$exits_u, B$k, sum); lr_alloc(x, sum(x)) }   # 613, split by class by largest remainder
B <- B %>% group_by(k) %>%
  mutate(exits = lr_alloc(exits_u, tot_ex[k[1]]),
         end   = lr_alloc(end_u, tot_end[k[1]])) %>% ungroup() %>%
  mutate(moves = end - today + exits)                                   # the residual closes every row
rate_k <- tapply(fcx$p, factor(fcx$k, levels = seq_len(N_CAT)), mean)   # the rate applied, by class
cat(sprintf("B -- %s to %s: %d today, %d expected to merge or close, %d in %s; cross-cell moves net %d (must be 0)\n",
            cohort_lab, H5_LAB, sum(B$today), sum(B$exits), sum(B$end), H5_LAB, sum(B$moves)))
stopifnot(sum(B$moves) == 0, sum(B$exits) == sum(tot_ex), sum(B$end) == sum(tot_end))

## ---------------------------------------------------------------------
## [34.3] The tables, per cell and for the Total
## ---------------------------------------------------------------------
tblA <- function(d, moves_lab) {
  out <- data.frame(`Size class` = cat_pretty, check.names = FALSE, stringsAsFactors = FALSE)
  out[[sprintf("Credit unions, %s", hist_lab)]]           <- d$start
  out[[sprintf("Merged or closed by %s", cohort_lab)]]    <- d$exits
  out[["Share merged or closed (%)"]]                     <- round(100 * d$exits / pmax(d$start, 1), 1)
  out[[moves_lab]]                                        <- d$moves
  out[["New charters, net"]]                              <- d$new - d$other
  out[[sprintf("Credit unions, %s", cohort_lab)]]         <- d$end
  out[nrow(out) + 1, ] <- list("All size classes", sum(d$start), sum(d$exits), round(100 * sum(d$exits) / max(sum(d$start), 1), 1),
                               sum(d$moves), sum(d$new - d$other), sum(d$end))
  out
}
tblB <- function(d, moves_lab) {
  out <- data.frame(`Size class` = cat_pretty, check.names = FALSE, stringsAsFactors = FALSE)
  out[[sprintf("Credit unions, %s", cohort_lab)]]                    <- d$today
  out[[sprintf("Expected to merge or close by %s", H5_LAB)]]         <- d$exits
  out[["Rate applied (%)"]]                                          <- round(100 * as.numeric(rate_k[d$k]), 1)
  out[[moves_lab]]                                                   <- d$moves
  out[[sprintf("Credit unions, %s (forecast, with mergers)", H5_LAB)]] <- d$end
  out[nrow(out) + 1, ] <- list("All size classes", sum(d$today), sum(d$exits), round(100 * sum(d$exits) / max(sum(d$today), 1), 1), sum(d$moves), sum(d$end))
  out
}
MV_TOT  <- "Moved to / from other size classes, net"
MV_CELL <- MV_TOT
A_tot <- A %>% group_by(k) %>% summarise(across(c(start, exits, other, new, moves, end), sum), .groups = "drop")
B_tot <- B %>% group_by(k) %>% summarise(across(c(today, exits, moves, end), sum), .groups = "drop")
TA <- list(Total = tblA(A_tot, MV_TOT)); TB <- list(Total = tblB(B_tot, MV_TOT))
for (cl in cells) {
  TA[[cl]] <- tblA(A %>% filter(cell == cl) %>% arrange(k), MV_CELL)
  TB[[cl]] <- tblB(B %>% filter(cell == cl) %>% arrange(k), MV_CELL)
}
cat("\nTotal, table A:\n"); print(TA$Total, row.names = FALSE)
cat("\nTotal, table B:\n"); print(TB$Total, row.names = FALSE)

## ---------------------------------------------------------------------
## [34.4] Check -- the cell tabs add to the Total tab, column by column
## ---------------------------------------------------------------------
sum_cells <- function(L, j) Reduce(`+`, lapply(L[cells], function(t) as.numeric(t[[j]])))
.jA <- setdiff(seq_along(TA$Total), c(1, 4)); .jB <- setdiff(seq_along(TB$Total), c(1, 4))
chkA <- data.frame(Column = names(TA$Total)[.jA], stringsAsFactors = FALSE)
chkA$`Sum of region tabs` <- vapply(.jA, function(j) sum_cells(TA, j)[N_CAT + 1], 0)
chkA$`Total tab`          <- vapply(.jA, function(j) as.numeric(TA$Total[[j]][N_CAT + 1]), 0)
chkB <- data.frame(Column = names(TB$Total)[.jB], stringsAsFactors = FALSE)
chkB$`Sum of region tabs` <- vapply(.jB, function(j) sum_cells(TB, j)[N_CAT + 1], 0)
chkB$`Total tab`          <- vapply(.jB, function(j) as.numeric(TB$Total[[j]][N_CAT + 1]), 0)
rm(.jA, .jB)
CHK <- rbind(cbind(Table = sprintf("A: %s to %s", hist_lab, cohort_lab), chkA), cbind(Table = sprintf("B: %s to %s", cohort_lab, H5_LAB), chkB))
CHK$Difference <- CHK$`Sum of region tabs` - CHK$`Total tab`
## and every row of every table closes
rowsA <- all(vapply(TA, function(t) all(abs(t[[2]] - t[[3]] + t[[5]] + t[[6]] - t[[7]]) < 1e-9), NA))
rowsB <- all(vapply(TB, function(t) all(abs(t[[2]] - t[[3]] + t[[5]] - t[[6]]) < 1e-9), NA))
cat("\nCheck -- region tabs vs Total:\n"); print(CHK, row.names = FALSE)
cat("Every row closes: table A", rowsA, "| table B", rowsB, "\n")
stopifnot(all(CHK$Difference == 0), rowsA, rowsB)

## ---------------------------------------------------------------------
## [34.5] Write -- one workbook, a tab per cell, Total first, Check last
## ---------------------------------------------------------------------
find_src <- function(fn) {
  cand <- c(fn, file.path("..", fn), file.path("S:/Projects/Credit_Union_Growth_Forecast", fn))
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop(fn, " not found. Searched: ", paste(cand, collapse = ", "))
  source(hit)
}
if (!exists("xl_block")) find_src("0_xlsx_helpers.R")
if (!exists("zip_base")) find_src("0b_zip_base.R")
writer_ok <- function() exists("xlsx_write") && grepl("topRight", paste(deparse(xlsx_write), collapse = ""), fixed = TRUE)
if (!writer_ok()) { find_src("0_xlsx_helpers.R"); find_src("0b_zip_base.R") }
if (!writer_ok()) warning("The xlsx writer in use predates the 22 Sep 2026 pane fix; Excel will open the workbook as 'Repaired'.")

sheet34 <- function(name, title, subtitle = NULL, notes = NULL, blocks = list(), cols = NULL) {
  rows <- character(0); r <- 1
  rows <- c(rows, xl_line(title, r, S_TITLE)); r <- r + 1
  if (!is.null(subtitle)) { rows <- c(rows, xl_line(subtitle, r, S_SUB)); r <- r + 1 }
  r <- r + 1
  for (n in notes) { rows <- c(rows, xl_line(n, r, S_NORM)); r <- r + 1 }
  if (length(notes)) r <- r + 1
  for (b in blocks) {
    if (!is.null(b$head)) { rows <- c(rows, xl_line(b$head, r, S_BOLD)); r <- r + 1 }
    bl <- xl_block(b$df, r, col_styles = b$styles); rows <- c(rows, bl$xml); r <- bl$next_row + 1
  }
  list(name = name, rows = rows, cols = cols, freeze = NULL, autofilter = NULL)
}
styA <- c(S_NORM, S_INT, S_INT, S_DEC, S_INT, S_INT, S_INT); styB <- c(S_NORM, S_INT, S_INT, S_DEC, S_INT, S_INT)
notes_common <- c(
  "Every row adds across: credit unions at the start, less merged or closed, plus net moves, plus new charters (net), equals credit unions at the end.",
  "'Merged or closed' means merged into another credit union or liquidated; closures are under 1% of these above $10M. Federally insured credit unions only (FCU and FISCU).",
  sprintf("New charters, net: credit unions chartered since %s, less any that left the data for a reason other than a merger or closure.", hist_lab),
  "The forecast table shows expected numbers for each size class, not lists of institutions; it assumes no new charters, and its moves come from the growth forecast (the same model as the Total tab of the growth workbook). The rate applied is the current five-year merger rate for the size class; where it implies fewer than one institution the expected number shows as 0.")
cell_title <- function(cl) { rg <- sub("^R(\\d+)_.*$", "\\1", cl); ct <- sub("^R\\d+_", "", cl); sprintf("%s, %s", REG_LAB[rg], ct) }
mk_tab <- function(nm, ttl, note_extra = character(0)) {
  sheet34(nm, ttl, sprintf("Credit union mergers: the last five years (%s to %s) and the next five (%s to %s). Cohort %s.", hist_lab, cohort_lab, cohort_lab, H5_LAB, cohort_lab),
          notes = c(notes_common, note_extra),
          blocks = list(list(head = sprintf("The last five years, %s to %s, as they happened", hist_lab, cohort_lab), df = TA[[nm]], styles = styA),
                        list(head = sprintf("The next five years, %s to %s, as forecast", cohort_lab, H5_LAB), df = TB[[nm]], styles = styB)),
          cols = col_widths(list(c(1, 1, 20), c(2, 7, 18))))
}
SH <- list(mk_tab("Total", "All regions and charter types",
                  sprintf("The region tabs add exactly to this tab. Each credit union is kept in the region and charter type it had at the start of each table; %d survivors changed region or charter type between %s and %s and are shown in their %s region and charter type.", N_CELL_CHANGED, hist_lab, cohort_lab, hist_lab)))
for (cl in cells) SH[[length(SH) + 1]] <- mk_tab(cl, cell_title(cl),
  sprintf("Each credit union is kept in the region and charter type it had at the start of each table (%s for the first, %s for the second); the few that changed region or charter type are not moved. In the forecast table the expected numbers are rounded within each size class so that the region tabs add to the Total; the moves column absorbs that rounding, so its total on a region tab can read 1 or -1 rather than 0.", hist_lab, cohort_lab))
SH[[length(SH) + 1]] <- sheet34("Check", "Do the region tabs add to the Total tab?",
  "Column totals summed across the region tabs, beside the Total tab. Every difference should be 0.",
  blocks = list(list(head = NULL, df = CHK, styles = c(S_NORM, S_NORM, S_INT, S_INT, S_INT))),
  cols = col_widths(list(c(1, 1, 28), c(2, 2, 52), c(3, 5, 16))))
OUT34 <- sprintf("CU_Merger_Table_by_region_%s.xlsx", cohort_lab)
xlsx_write(SH, OUT34)
cat("\nWrote", OUT34, "with", length(SH), "tabs:", paste(vapply(SH, `[[`, "", "name"), collapse = ", "), "\n")
