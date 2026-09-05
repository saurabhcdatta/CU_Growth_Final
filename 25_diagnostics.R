## =====================================================================
## 25_diagnostics.R  --  Backtest, and the decision on bucket calibration
##
## Script 22 cross-validated the METHOD. This tests the PIPELINE: stand at
## a past quarter, build the pools from data available then, forecast that
## quarter's cohort forward, and compare against what actually happened.
##
## Three backtests, one per horizon, each ending at 2026Q2 so the outcome
## is fully observed:
##
##   h= 4   origin 2025Q2 -> outcome 2026Q2
##   h=12   origin 2023Q2 -> outcome 2026Q2
##   h=20   origin 2021Q2 -> outcome 2026Q2
##
## Pools are rebuilt from origins whose own outcome window CLOSED before
## the backtest origin. Nothing that happened after the origin is used to
## forecast it. That is stricter than the CV in 22, which allowed training
## origins up to the fold boundary.
##
## THE THREE QUESTIONS THIS SCRIPT ANSWERS:
##   1. Do the bucket counts hold up out of sample, and is the A7 bias the
##      +28-55% seen in CV? That decides BUCKET_CALIB.
##   2. Does the method reproduce DOWNWARD movement, where the frozen-
##      cohort track fails badly (46 predicted, 87 actual over five years)?
##   3. Does the ranking-cut assignment from 24 beat naive alternatives at
##      the institution level?
##
## Run block by block in RStudio.
## =====================================================================

library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

## prep <- readRDS("panel_prep.rds");  list2env(prep, .GlobalEnv)
## fts  <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv)
## prb  <- readRDS("panel_probs.rds"); list2env(prb,  .GlobalEnv)
## asg  <- readRDS("panel_assign.rds"); list2env(asg, .GlobalEnv)

stopifnot(exists("feat"), exists("make_pool"), exists("pool_cdf"),
          exists("emp_bucket_probs"), exists("apportion"),
          exists("assign_cut"))

## ---------------------------------------------------------------------
## [25.1] Backtest design
##
## Reference figures from the frozen-cohort track, section 6 of the
## handoff. Hardcoded because that pipeline is not re-run here; update
## them if 10-16 is refreshed.
## ---------------------------------------------------------------------
BT <- data.frame(h = H_SET, origin = N_Q - H_SET)
BT$origin_label  <- qgrid$q_label[BT$origin]
BT$outcome_label <- qgrid$q_label[BT$origin + BT$h]
BT

FROZEN_REF <- list(
  hit_2y      = 95.2,     # category hit rate, two years
  up_5y_pred  = 46,       # upward moves predicted over five years
  up_5y_act   = 454,
  down_5y_pred = 46,
  down_5y_act  = 87,
  a7_bias_pct = 54)       # A7 over-count

## ---------------------------------------------------------------------
## [25.2] Backtest cohorts
##
## Institutions alive at the origin AND still present h quarters later.
## Conditioning on survival is not a convenience -- it is what the method
## claims to do, and what the closed-cohort assumption asserts. The exit
## share is printed because it IS the size of that assumption over the
## backtest window, and it belongs on the Method tab.
## ---------------------------------------------------------------------
bt_cohort <- function(h, origin) {
  us <- feat[[paste0("usable_surv_h", h)]]
  d  <- feat[us & feat$q_index == origin, ]
  d$dy      <- d[[paste0("dy_h", h)]]
  d$cat_act <- d[[paste0("cat_f_h", h)]]
  d <- d[!is.na(d$dy) & !is.na(d$cat_act), ]
  d
}

## Everyone alive at the origin, survivors or not, for the exit share
bt_exit_share <- function(h, origin) {
  us <- feat[[paste0("usable_h", h)]]
  d  <- feat[us & feat$q_index == origin, ]
  mean(d[[paste0("exit_h", h)]])
}

for (i in seq_len(nrow(BT))) {
  d <- bt_cohort(BT$h[i], BT$origin[i])
  cat(sprintf("h=%2d  origin %s  survivors %d  exited %.1f%%\n",
              BT$h[i], BT$origin_label[i], nrow(d),
              100 * bt_exit_share(BT$h[i], BT$origin[i])))
}

## ---------------------------------------------------------------------
## [25.3] Pools as they would have been built at the time
##
## Training origins must have their OUTCOME closed before the backtest
## origin: q_index + h <= origin. Same fallback chain and MIN_POOL as
## script 23, so this tests the pipeline rather than a variant of it.
## ---------------------------------------------------------------------
bt_pools <- function(h, origin) {
  us <- feat[[paste0("usable_surv_h", h)]]
  d  <- feat[us & (feat$q_index + h) <= origin, ]
  d$dy <- d[[paste0("dy_h", h)]]
  d <- d[!is.na(d$dy), ]

  pools <- list(); src <- character(N_CAT)
  for (k in seq_len(N_CAT)) {
    idx <- which(d$cat_k == k); src[k] <- "own"
    if (length(idx) < MIN_POOL) {
      idx <- which(abs(d$cat_k - k) <= 1); src[k] <- "window"
    }
    if (length(idx) < MIN_POOL) {
      idx <- seq_len(nrow(d)); src[k] <- "pooled"
    }
    pools[[as.character(k)]] <- make_pool(d$dy[idx])
  }
  attr(pools, "source") <- src
  attr(pools, "n_train") <- nrow(d)
  pools
}

BT_POOLS <- lapply(seq_len(nrow(BT)), function(i)
  bt_pools(BT$h[i], BT$origin[i]))
names(BT_POOLS) <- as.character(BT$h)

for (i in seq_len(nrow(BT))) {
  pl <- BT_POOLS[[as.character(BT$h[i])]]
  cat(sprintf("\nh=%2d  training rows %d\n", BT$h[i], attr(pl, "n_train")))
  print(data.frame(cat = CAT_LABELS, src = attr(pl, "source"),
                   n = sapply(pl, function(p) p$n)))
}

## ---------------------------------------------------------------------
## [25.4] Forecast and actual
## ---------------------------------------------------------------------
BT_RES <- lapply(seq_len(nrow(BT)), function(i) {
  h <- BT$h[i]
  d <- bt_cohort(h, BT$origin[i])
  P <- emp_bucket_probs(BT_POOLS[[as.character(h)]], d$cat_k, d$y_raw,
                        LOG_EDGE)
  list(h = h, d = d, P = P,
       soft = colSums(P),
       hard = as.numeric(table(factor(d$cat_act, levels = 1:N_CAT))),
       start = as.numeric(table(factor(d$cat_k, levels = 1:N_CAT))))
})
names(BT_RES) <- as.character(BT$h)

## Reconciliation still has to hold out of sample
for (r in BT_RES)
  stopifnot(abs(sum(r$soft) - nrow(r$P)) < 1e-6,
            sum(r$hard) == nrow(r$P))
cat("Backtest totals reconcile at every horizon.\n")

## ---------------------------------------------------------------------
## [25.5] Count accuracy -- the BUCKET_CALIB decision
##
## If a category's bias here has the same sign and rough size as the CV
## bias in calib_factors, the correction is real and worth applying. If it
## flips sign or collapses, it was fold noise and BUCKET_CALIB stays off.
## ---------------------------------------------------------------------
count_tab <- bind_rows(lapply(BT_RES, function(r)
  data.frame(h = r$h, cat = CAT_LABELS, start = r$start,
             pred = round(r$soft, 1), actual = r$hard,
             err = round(r$soft - r$hard, 1),
             pct = round(100 * (r$soft - r$hard) / pmax(r$hard, 1), 1))))

as.data.frame(count_tab)

## Headline accuracy
count_tab %>% group_by(h) %>%
  summarise(n = sum(actual),
            mae = round(mean(abs(err)), 1),
            mape = round(mean(abs(pct)), 1),
            worst = cat[which.max(abs(pct))],
            worst_pct = round(max(abs(pct)), 1), .groups = "drop") %>%
  as.data.frame()

## Backtest bias against the CV bias, category by category. calib_factors
## is actual/predicted, so a factor below 1 means the CV over-predicted.
bt_factor <- count_tab %>%
  transmute(h, cat, bt_factor = round(actual / pmax(pred, 1e-9), 3))

compare_bias <- bt_factor %>%
  inner_join(calib_factors %>% rename(cv_factor = f), by = c("h", "cat")) %>%
  mutate(cv_factor = round(cv_factor, 3),
         agree = sign(1 - bt_factor) == sign(1 - cv_factor))

as.data.frame(compare_bias)

cat("\nCategories where backtest and CV bias agree in direction:\n")
compare_bias %>% group_by(cat) %>%
  summarise(n_agree = sum(agree), n = n(),
            mean_bt = round(mean(bt_factor), 3),
            mean_cv = round(mean(cv_factor), 3), .groups = "drop") %>%
  as.data.frame()

## ---------------------------------------------------------------------
## [25.6] Direction -- the comparison that matters most
##
## The frozen-cohort track predicts 46 upward and 46 downward moves over
## five years against 454 and 87 actual. Its down_ratio is about 0.53 and
## its up_ratio about 0.10. Anything close to 1 here is a large
## improvement, and it is the single most defensible reason to switch.
## ---------------------------------------------------------------------
dir_tab <- bind_rows(lapply(BT_RES, function(r) {
  kn <- r$d$cat_k; ka <- r$d$cat_act
  data.frame(
    h = r$h,
    pred_up   = sum(rowSums(r$P * outer(kn, seq_len(N_CAT), "<"))),
    act_up    = sum(ka > kn),
    pred_down = sum(rowSums(r$P * outer(kn, seq_len(N_CAT), ">"))),
    act_down  = sum(ka < kn))
})) %>%
  mutate(up_ratio   = round(pred_up / pmax(act_up, 1), 2),
         down_ratio = round(pred_down / pmax(act_down, 1), 2),
         pred_up = round(pred_up, 1), pred_down = round(pred_down, 1))

as.data.frame(dir_tab)

cat("\nFor reference, the frozen-cohort track over five years:\n")
cat(sprintf("  up   %d predicted vs %d actual  (ratio %.2f)\n",
            FROZEN_REF$up_5y_pred, FROZEN_REF$up_5y_act,
            FROZEN_REF$up_5y_pred / FROZEN_REF$up_5y_act))
cat(sprintf("  down %d predicted vs %d actual  (ratio %.2f)\n",
            FROZEN_REF$down_5y_pred, FROZEN_REF$down_5y_act,
            FROZEN_REF$down_5y_pred / FROZEN_REF$down_5y_act))

## ---------------------------------------------------------------------
## [25.7] Institution-level accuracy
##
## Three rules on the same forecasts:
##   ranked  the ranking cut from 24 -- ties to the counts by construction
##   modal   argmax, what a naive implementation would do
##   stay    assume nobody moves. The benchmark everything must beat, and
##           at one year it is a genuinely strong one.
## ---------------------------------------------------------------------
## Median forecast asset level, computed once per backtest and reused.
## One quantile per category, mapped onto rows -- the pool is the same for
## every institution in a category.
bt_median <- function(r) {
  pl <- BT_POOLS[[as.character(r$h)]]
  qk <- vapply(seq_len(N_CAT),
               function(k) pool_q(pl[[as.character(k)]], 0.5), 0)
  r$d$y_raw + qk[r$d$cat_k]
}

BT_ASSIGN <- lapply(BT_RES, function(r) {
  tgt <- apportion(r$soft, nrow(r$P))
  list(ranked = assign_cut(bt_median(r), tgt, exp(r$d$y_raw),
                           r$d$join_number),
       modal  = max.col(r$P),
       stay   = r$d$cat_k)
})
names(BT_ASSIGN) <- names(BT_RES)

acc_tab <- bind_rows(lapply(names(BT_RES), function(nm) {
  r <- BT_RES[[nm]]; a <- BT_ASSIGN[[nm]]
  data.frame(h = r$h, n = nrow(r$P),
             ranked = round(100 * mean(a$ranked == r$d$cat_act), 1),
             modal  = round(100 * mean(a$modal  == r$d$cat_act), 1),
             stay   = round(100 * mean(a$stay   == r$d$cat_act), 1))
}))

as.data.frame(acc_tab)
cat("\nFrozen-cohort track, two-year category hit rate:",
    FROZEN_REF$hit_2y, "%\n")

## Where the ranking cut and modal disagree, which is right more often.
## This is the direct test of the choice made in 24.
for (nm in names(BT_RES)) {
  r <- BT_RES[[nm]]; a <- BT_ASSIGN[[nm]]
  dif <- a$ranked != a$modal
  if (!any(dif)) { cat(sprintf("h=%2s  no disagreements\n", nm)); next }
  cat(sprintf("h=%2s  %d rows differ; ranked right %d, modal right %d\n",
              nm, sum(dif), sum(a$ranked[dif] == r$d$cat_act[dif]),
              sum(a$modal[dif]  == r$d$cat_act[dif])))
}

## ---------------------------------------------------------------------
## [25.8] Calibration out of sample
## ---------------------------------------------------------------------
for (r in BT_RES) {
  pm <- apply(r$P, 1, max); km <- max.col(r$P)
  cat("\nh =", r$h, "\n")
  print(data.frame(p_max = pm, hit = km == r$d$cat_act) %>%
    mutate(bin = cut(p_max, seq(0, 1, 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(n = n(), mean_p = round(mean(p_max), 3),
              realised = round(mean(hit), 3), .groups = "drop") %>%
    as.data.frame())
}

## ---------------------------------------------------------------------
## [25.9] Verdict
## ---------------------------------------------------------------------
a7 <- count_tab %>% filter(cat == "A7_GE10B")
cat("\n---- A7 ----\n")
print(as.data.frame(a7))
cat("\nCV bias for A7 was +28% to +55%. If the backtest shows the same\n",
    "sign and rough size, set BUCKET_CALIB <- TRUE in [23.1] and re-run\n",
    "23 and 24. If it flips or collapses, leave it off and say so on the\n",
    "Diagnostics tab -- an uncorrected known bias that did not reproduce\n",
    "is a finding, not an oversight.\n")

verdict <- data.frame(
  test = c("count MAE, 5y", "down_ratio, 5y", "hit rate ranked, 5y",
           "beats stay-put, 5y", "A7 bias, 5y (%)"),
  value = c(
    round(mean(abs(count_tab$err[count_tab$h == 20])), 1),
    dir_tab$down_ratio[dir_tab$h == 20],
    acc_tab$ranked[acc_tab$h == 20],
    acc_tab$ranked[acc_tab$h == 20] - acc_tab$stay[acc_tab$h == 20],
    a7$pct[a7$h == 20]))
as.data.frame(verdict)

saveRDS(list(BT = BT, BT_POOLS = BT_POOLS, BT_RES = BT_RES,
             BT_ASSIGN = BT_ASSIGN,
             count_tab = count_tab, dir_tab = dir_tab, acc_tab = acc_tab,
             compare_bias = compare_bias, verdict = verdict,
             FROZEN_REF = FROZEN_REF),
        file = "panel_backtest.rds")
