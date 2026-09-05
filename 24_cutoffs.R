## =====================================================================
## 24_cutoffs.R  --  One bucket per institution, tying to the aggregate
##
## Script 23 produces a probability distribution per institution. The
## counts come from summing those probabilities. But the regional tabs
## need a single named bucket per credit union, and that assignment has to
## reconcile to the published counts or the workbook contradicts itself.
##
## THE RULE: rank all 4,202 institutions by their median forecast asset
## level, then cut the ranking at the positions the soft counts dictate.
## Top N7 go to A7, next N6 to A6, and so on.
##
## Why not argmax. Two reasons, and the second is the fatal one:
##   1. Modal assignment over-counts the middle categories, where mass
##      concentrates, and under-counts the tails, where it is spread thin.
##      A7 is the extreme case: institutions with 0.3 probability each of
##      being $10B+ contribute 46 to the expected count and zero to the
##      modal count.
##   2. The modal counts do not equal the soft counts, so the institution
##      lists would not add up to the headline numbers. Someone will check.
##
## Why not a per-bucket probability threshold, which is what I first
## proposed. It reconciles each bucket in isolation, but an institution
## can clear two thresholds or none, so some rows get two assignments and
## others get none. The ranking cut gives everyone exactly one, preserves
## size order, and hits the marginals exactly.
##
## Run block by block in RStudio.
## =====================================================================

library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

## prep <- readRDS("panel_prep.rds");  list2env(prep, .GlobalEnv)
## prb  <- readRDS("panel_probs.rds"); list2env(prb,  .GlobalEnv)

## Cohort size is NOT hardcoded. It was 4,202 on regions 1-3; adding
## region 8 (ONES) took it to 4,214, and a fixed number here would have
## failed the whole script on a legitimate change to the panel.
stopifnot(exists("inst"), exists("PROB"), exists("counts"),
          exists("fc"), nrow(inst) == nrow(fc))
N_COHORT <- nrow(inst)
cat("Cohort:", N_COHORT, "institutions\n")

## The thin-pool columns come from [23.7]. If 23 was run before those were
## added, re-run it rather than patching around them here -- 27 needs the
## flag and a silently missing column becomes a silently missing caveat.
stopifnot(all(c("pool_n_h20", "thin_h20") %in% names(inst)))

## ---------------------------------------------------------------------
## [24.1] Integer targets
##
## Soft counts are fractional and must become integers that still sum to
## 4,202. Largest-remainder (Hamilton) apportionment: floor everything,
## then hand the leftover seats to the largest fractional parts. Rounding
## each category independently would not sum to the cohort.
## ---------------------------------------------------------------------
apportion <- function(x, total) {
  base <- floor(x)
  rem  <- total - sum(base)
  if (rem > 0) {
    ord <- order(x - base, decreasing = TRUE)
    base[ord[seq_len(rem)]] <- base[ord[seq_len(rem)]] + 1
  }
  if (rem < 0) {                      # can happen only from float error
    ord <- order(x - base)
    base[ord[seq_len(-rem)]] <- base[ord[seq_len(-rem)]] - 1
  }
  base
}

TARGET <- lapply(H_SET, function(h) {
  soft <- colSums(PROB[[as.character(h)]])
  n    <- apportion(soft, N_COHORT)
  stopifnot(sum(n) == N_COHORT, all(n >= 0))
  data.frame(cat = CAT_LABELS, k = seq_len(N_CAT),
             soft = round(soft, 2), target = n)
})
names(TARGET) <- as.character(H_SET)

for (h in H_SET) {
  cat("\nh =", h, "\n")
  print(TARGET[[as.character(h)]] %>%
          mutate(rounding = round(target - soft, 2)) %>% as.data.frame())
}

## ---------------------------------------------------------------------
## [24.2] The ranking cut
##
## Rank on the MEDIAN FORECAST ASSET LEVEL, not on any probability. That
## keeps the assignment monotone in size -- a larger institution can never
## be placed in a smaller category than a smaller one -- which is the
## property a supervisor will assume holds and would notice immediately if
## it did not.
##
## Ties are broken by current assets, then join_number, so the assignment
## is reproducible across refreshes rather than depending on row order.
## ---------------------------------------------------------------------
assign_cut <- function(med, target, tie1, tie2) {
  n <- length(med)
  stopifnot(sum(target) == n)
  ord <- order(med, tie1, tie2, decreasing = TRUE)   # largest first
  ## Seats from the top category downward
  lab <- rep(NA_integer_, n)
  pos <- 1
  for (k in rev(seq_len(N_CAT))) {
    if (target[k] == 0) next
    lab[ord[pos:(pos + target[k] - 1)]] <- k
    pos <- pos + target[k]
  }
  stopifnot(!anyNA(lab))
  lab
}

for (h in H_SET) {
  med <- inst[[paste0("assets_med_h", h)]]
  k   <- assign_cut(med, TARGET[[as.character(h)]]$target,
                    inst$assets_now, inst$join_number)
  inst[[paste0("k_assign_h", h)]]   <- k
  inst[[paste0("cat_assign_h", h)]] <- CAT_LABELS[k]
  ## The probability the model gives the bucket the institution was
  ## actually assigned. This is the honesty column and it belongs NEXT TO
  ## the assignment on the tab, not in a footnote.
  inst[[paste0("p_assign_h", h)]] <-
    PROB[[as.character(h)]][cbind(seq_len(nrow(inst)), k)]
}

## ---------------------------------------------------------------------
## [24.3] Reconciliation -- the whole point of the exercise
## ---------------------------------------------------------------------
for (h in H_SET) {
  got <- as.numeric(table(factor(inst[[paste0("k_assign_h", h)]],
                                 levels = 1:N_CAT)))
  tgt <- TARGET[[as.character(h)]]$target
  stopifnot(identical(as.integer(got), as.integer(tgt)))
  cat(sprintf("h=%2d  assignment ties to target exactly (total %d)\n",
              h, sum(got)))
}

## Monotone in size: sort by median forecast assets and the assigned
## category must never decrease going up.
for (h in H_SET) {
  o <- order(inst[[paste0("assets_med_h", h)]])
  stopifnot(all(diff(inst[[paste0("k_assign_h", h)]][o]) >= 0))
}
cat("Assignment is monotone in forecast size at every horizon.\n")

## ---------------------------------------------------------------------
## [24.4] Ranking cut against modal assignment
##
## Where the two disagree is where the count correction is doing its work.
## Expect the disagreements to sit near category edges and in the thin
## categories, not scattered at random.
## ---------------------------------------------------------------------
for (h in H_SET) {
  ka <- inst[[paste0("k_assign_h", h)]]
  km <- inst[[paste0("k_modal_h", h)]]
  cat(sprintf("\nh=%2d  ranking cut differs from modal for %d of %d (%.1f%%)\n",
              h, sum(ka != km), length(ka), 100 * mean(ka != km)))
  print(table(modal = CAT_LABELS[km], assigned = CAT_LABELS[ka]))
}

## Counts under each rule, side by side. The modal column is what a naive
## argmax would publish, and the gap is the reason not to.
for (h in H_SET) {
  cat("\nh =", h, "\n")
  print(data.frame(
    cat    = CAT_LABELS,
    soft   = round(colSums(PROB[[as.character(h)]]), 1),
    ranked = as.numeric(table(factor(inst[[paste0("k_assign_h", h)]],
                                     levels = 1:N_CAT))),
    modal  = as.numeric(table(factor(inst[[paste0("k_modal_h", h)]],
                                     levels = 1:N_CAT)))) %>%
    mutate(modal_err = modal - round(soft, 1)) %>% as.data.frame())
}

## ---------------------------------------------------------------------
## [24.5] Confidence, for the institution tabs
##
## The aggregate is far more reliable than any single row: errors across
## 4,202 institutions largely cancel, and one credit union's five-year
## category is close to a coin flip whenever it sits near an edge. Publish
## this column beside the assignment or the assignment gets read as a
## prediction about that specific institution.
## ---------------------------------------------------------------------
for (h in H_SET)
  inst[[paste0("conf_assign_h", h)]] <- cut(
    inst[[paste0("p_assign_h", h)]], c(0, 0.5, 0.75, 1),
    labels = c("low", "medium", "high"), include.lowest = TRUE)

for (h in H_SET) {
  cat("\nh =", h, "\n")
  print(inst %>% count(.data[[paste0("conf_assign_h", h)]]) %>%
          mutate(pct = round(100 * n / sum(n), 1)) %>% as.data.frame())
}

## ---------------------------------------------------------------------
## [24.6] A7 explicitly
##
## A7 is the category to check by hand every refresh. It holds 13
## institutions today, its forecast count is driven almost entirely by A6
## crossing $10B rather than by the incumbents, and it ran 28-55% high out
## of fold. The named list below is the single most scrutinised output in
## the workbook.
## ---------------------------------------------------------------------
a7 <- inst %>% filter(k_assign_h20 == 7) %>%
  arrange(desc(assets_med_h20)) %>%
  transmute(cu_name, region, cu_type,
            assets_now_b = round(assets_now / 1e9, 2),
            med_h20_b    = round(assets_med_h20 / 1e9, 2),
            p10_h20_b    = round(assets_p10_h20 / 1e9, 2),
            p90_h20_b    = round(assets_p90_h20 / 1e9, 2),
            now = asset_cat_now, p_assign = round(p_assign_h20, 3),
            conf = conf_assign_h20)

nrow(a7)
as.data.frame(a7)

cat("\nA7 probabilities rest on", unique(inst$pool_n_h20[inst$cat_k == 7]),
    "historical observations. Repeated p_assign values across\n",
    "institutions are that granularity, not agreement between them.\n")

## Already $10B+ today and forecast to stay there
a7 %>% filter(now == "A7_GE10B") %>% nrow()
## Crossing into $10B+ from A6
a7 %>% filter(now != "A7_GE10B") %>% nrow()

## Boundary sensitivity. The institutions sitting within ten ranks either
## side of a cut are the ones whose assignment would flip on a small change
## in the counts. Field staff should see these as "near the line", not as
## a firm call.
boundary <- function(h, window = 10) {
  med <- inst[[paste0("assets_med_h", h)]]
  ord <- order(med, decreasing = TRUE)
  tgt <- TARGET[[as.character(h)]]$target
  cuts <- cumsum(rev(tgt))                  # rank positions of each cut
  cuts <- cuts[-length(cuts)]
  idx  <- unlist(lapply(cuts, function(c) ord[max(1, c - window + 1):
                                              min(length(ord), c + window)]))
  inst[unique(idx), ] %>%
    transmute(join_number, cu_name, asset_cat_now,
              assets_now_m = round(assets_now / 1e6, 1),
              assigned = .data[[paste0("cat_assign_h", h)]],
              p_assign = round(.data[[paste0("p_assign_h", h)]], 3))
}

near_line <- boundary(20)
nrow(near_line)
head(as.data.frame(near_line), 25)

inst$near_line_h20 <- inst$join_number %in% near_line$join_number

## ---------------------------------------------------------------------
## [24.7] Named movement lists, for the regional tabs
## ---------------------------------------------------------------------
movers <- lapply(H_SET, function(h) {
  inst %>%
    filter(.data[[paste0("k_assign_h", h)]] != cat_k) %>%
    transmute(h = h, join_number, cu_name, region, cu_type,
              from = asset_cat_now,
              to = .data[[paste0("cat_assign_h", h)]],
              direction = ifelse(.data[[paste0("k_assign_h", h)]] > cat_k,
                                 "up", "down"),
              assets_now_m = round(assets_now / 1e6, 1),
              med_m = round(.data[[paste0("assets_med_h", h)]] / 1e6, 1),
              p_assign = round(.data[[paste0("p_assign_h", h)]], 3))
})
movers <- bind_rows(movers)

movers %>% count(h, direction) %>%
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) %>%
  as.data.frame()

movers %>% filter(h == 20) %>% count(from, to) %>% as.data.frame()

## Region x charter movement. Now EIGHT cells, not six: region 8 is ONES,
## a supervisory office rather than a geography, and its institutions are
## the largest in the industry. The tab must label it as such -- a reader
## who takes it for a fourth region will misread every comparison.
movers %>% filter(h == 20) %>% count(region, cu_type, direction) %>%
  pivot_wider(names_from = direction, values_from = n, values_fill = 0) %>%
  as.data.frame()

## ---------------------------------------------------------------------
## [24.8] The institution table 27 exports
## ---------------------------------------------------------------------
inst_out <- inst %>%
  transmute(
    join_number, cu_name, region, cu_type, state,
    assets_now, asset_cat_now, short_history,
    cat_1y = cat_assign_h4,  p_1y = round(p_assign_h4, 3),
    conf_1y = conf_assign_h4,
    cat_3y = cat_assign_h12, p_3y = round(p_assign_h12, 3),
    conf_3y = conf_assign_h12,
    cat_5y = cat_assign_h20, p_5y = round(p_assign_h20, 3),
    conf_5y = conf_assign_h20,
    ## From [23.7]. Rows drawn from a thin historical pool -- A7 at five
    ## years sits here with about 21 observations, so its probabilities
    ## move in ~5-point steps and repeat across institutions. 27 must
    ## surface this on the tab; an unexplained 0.000 next to a $19B credit
    ## union will be read as a claim rather than as an absence of cases.
    pool_n_5y = pool_n_h20,
    thin_5y = thin_h20,
    assets_med_5y = assets_med_h20,
    assets_p10_5y = assets_p10_h20,
    assets_p90_5y = assets_p90_h20,
    p_up_5y   = round(p_up_h20, 3),
    p_same_5y = round(p_same_h20, 3),
    p_down_5y = round(p_down_h20, 3))

nrow(inst_out)
head(as.data.frame(inst_out), 10)

## Final tie-out: the institution table must reproduce the headline counts
for (h in c(4, 12, 20)) {
  col <- c("4" = "cat_1y", "12" = "cat_3y", "20" = "cat_5y")[as.character(h)]
  got <- as.numeric(table(factor(inst_out[[col]], levels = CAT_LABELS)))
  stopifnot(identical(as.integer(got),
                      as.integer(TARGET[[as.character(h)]]$target)))
}
cat("\nInstitution table reproduces the published counts at all horizons.\n")

saveRDS(list(inst = inst, inst_out = inst_out, TARGET = TARGET,
             movers = movers, a7 = a7, near_line = near_line,
             apportion = apportion, assign_cut = assign_cut),
        file = "panel_assign.rds")
