## =====================================================================
## 16_forecast_review.R  --  Adjudicate every forecast, then aim the review
##
## Script 15 asks whether each institution was MODELLED properly. This asks
## whether each resulting FORECAST makes sense, and how much confidence the
## bucket assignment deserves.
##
## Nobody can hand-review 4,202 forecasts. So every forecast is tested
## automatically against four independent reference points, graded, and only
## the genuinely doubtful ones are queued for a person to look at.
##
##   1 CROSS-METHOD    the ARIMA, the simple growth rate and peer growth are
##                     three near-independent estimates. Where all three land
##                     an institution in the same bucket, the assignment is
##                     robust to method choice. Where they disagree, it isn't.
##   2 BOUNDARY        an institution projected to $99M and one to $140M are
##                     equally valid forecasts, but only the first flips
##                     bucket on a rounding error. Since the deliverable is
##                     COUNTS, accuracy depends mostly on institutions near
##                     boundaries. Each gets a probability of being in its
##                     assigned bucket, from its own forecast uncertainty.
##   3 DIRECTION       does the forecast point the same way as the last three
##                     years of actual history
##   4 PEER            where the growth rate sits in its peer distribution
##
## The soft counts at [16.5] are the headline: summing bucket probabilities
## instead of hard assignments gives the count with its uncertainty. If the
## soft and hard counts differ materially for a bucket, that bucket's number
## is resting on institutions that could fall either way.
##
## Run after 12 and 15, before 14.
## =====================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

source("0_xlsx_helpers.R")

## prep <- readRDS("cohort_prep.rds");        list2env(prep, .GlobalEnv)
## fitr <- readRDS("cohort_fits.rds");        list2env(fitr, .GlobalEnv)
## dgn  <- readRDS("cohort_diagnostics.rds"); list2env(dgn,  .GlobalEnv)

REVIEW_DIR <- file.path(getwd(), "review")
dir.create(REVIEW_DIR, showWarnings = FALSE)

## The published growth rates carry the bias correction from script 12. The
## three comparison methods below must carry it too, or they sit
## systematically higher than the published figure and agreement is
## understated for no real reason.
BIAS_PA_APPLIED <- if (exists("BIAS_PA")) BIAS_PA else
                   if (exists("guardrails")) guardrails$BIAS_PA else 0
BIAS_Q_APPLIED  <- log(1 + BIAS_PA_APPLIED) / 4
cat("Comparison methods adjusted by", round(100 * BIAS_PA_APPLIED, 2), "% a year\n")

P_STRONG <- 0.70      # bucket probability above this: assignment is solid
P_WEAK   <- 0.40      # below this: the assignment is a coin toss
SIGMA_MIN <- 0.03     # floor on forecast uncertainty, log scale

## ---------------------------------------------------------------------
## [16.1] Cross-method agreement
## Three estimates of the same thing, from largely different information.
## ---------------------------------------------------------------------
rev <- cu %>%
  mutate(
    a_arima  = ifelse(is.na(g_arima), NA_real_,
                      exp(log_now + (g_arima  - BIAS_Q_APPLIED) * 20)),
    a_simple = ifelse(is.na(g_simple), NA_real_,
                      exp(log_now + (g_simple - BIAS_Q_APPLIED) * 20)),
    a_peer   = exp(log_now + (g_peer - BIAS_Q_APPLIED) * 20),
    b_arima  = as.character(cut(a_arima,  BREAKS, CAT_LABELS, right = FALSE)),
    b_simple = as.character(cut(a_simple, BREAKS, CAT_LABELS, right = FALSE)),
    b_peer   = as.character(cut(a_peer,   BREAKS, CAT_LABELS, right = FALSE)),
    n_agree = (!is.na(b_arima)  & b_arima  == cat_5Yr) +
              (!is.na(b_simple) & b_simple == cat_5Yr) +
              (!is.na(b_peer)   & b_peer   == cat_5Yr),
    n_avail = (!is.na(b_arima)) + (!is.na(b_simple)) + (!is.na(b_peer)),
    agree_rate = ifelse(n_avail > 0, n_agree / n_avail, NA_real_))

table(rev$n_agree, useNA = "ifany")
round(100 * mean(rev$agree_rate == 1, na.rm = TRUE), 1)   # % fully agreed

## ---------------------------------------------------------------------
## [16.2] Boundary fragility
## Probability the institution is actually in its assigned bucket, given the
## forecast and its own out-of-sample error. Uses the CV error at the
## 5-year horizon as the standard deviation on the log scale, floored so a
## suspiciously tidy model does not claim certainty it has not earned.
## ---------------------------------------------------------------------
rev <- rev %>%
  mutate(
    sigma = pmax(replace_na(cv_rmse_h20, 0.35), SIGMA_MIN),
    mu    = log(assets_5Yr),
    k     = match(cat_5Yr, CAT_LABELS),
    lo_edge = BREAKS[k],
    hi_edge = BREAKS[k + 1],
    p_lo = ifelse(is.finite(lo_edge) & lo_edge > 0,
                  pnorm((log(lo_edge) - mu) / sigma), 0),
    p_hi = ifelse(is.finite(hi_edge), pnorm((log(hi_edge) - mu) / sigma), 1),
    p_bucket = pmax(pmin(p_hi - p_lo, 1), 0),
    ## distance to the nearer boundary, in units of its own forecast error
    d_lo = ifelse(is.finite(lo_edge) & lo_edge > 0, (mu - log(lo_edge)) / sigma, Inf),
    d_hi = ifelse(is.finite(hi_edge), (log(hi_edge) - mu) / sigma, Inf),
    d_edge = round(pmin(d_lo, d_hi), 2))

summary(rev$p_bucket)
cat("Assignments with probability under", P_WEAK, ":",
    sum(rev$p_bucket < P_WEAK), "of", nrow(rev), "\n")

## ---------------------------------------------------------------------
## [16.3] Direction and [16.4] peer position
## ---------------------------------------------------------------------
recent_dir <- bind_rows(lapply(cu_series, function(s) {
  n <- s$n
  if (n < 13) return(data.frame(join_number = s$join_number, hist_dir = NA_real_))
  data.frame(join_number = s$join_number,
             hist_dir = (s$y[n] - s$y[n - 12]) / 3)     # annual log growth, 3y
}))

rev <- rev %>%
  left_join(recent_dir, by = "join_number") %>%
  group_by(region, cu_type, asset_cat_now) %>%
  mutate(peer_pct = round(100 * rank(g_final) / n(), 0)) %>%
  ungroup() %>%
  mutate(dir_conflict = !is.na(hist_dir) & sign(hist_dir) != sign(g_final) &
                        abs(hist_dir) > 0.01 & abs(g_final) > 0.01)

sum(rev$dir_conflict, na.rm = TRUE)

## ---------------------------------------------------------------------
## [16.5] Soft counts -- the count with its uncertainty
## Instead of assigning each institution to one bucket, spread it across
## buckets by probability and add those up. Where the soft and hard counts
## diverge, that bucket is resting on institutions that could fall either way.
## ---------------------------------------------------------------------
soft <- matrix(0, nrow = nrow(rev), ncol = length(CAT_LABELS),
               dimnames = list(NULL, CAT_LABELS))
for (j in seq_along(CAT_LABELS)) {
  lo <- BREAKS[j]; hi <- BREAKS[j + 1]
  plo <- ifelse(is.finite(lo) & lo > 0, pnorm((log(lo) - rev$mu) / rev$sigma), 0)
  phi <- ifelse(is.finite(hi), pnorm((log(hi) - rev$mu) / rev$sigma), 1)
  soft[, j] <- pmax(phi - plo, 0)
}
soft <- soft / rowSums(soft)

soft_counts <- data.frame(
  asset_cat = CAT_LABELS,
  hard_count = as.integer(table(factor(rev$cat_5Yr, levels = CAT_LABELS))),
  soft_count = round(colSums(soft), 1),
  ## how many institutions in this bucket are within one standard error
  ## of a boundary, i.e. could plausibly be counted elsewhere
  fragile = as.integer(table(factor(rev$cat_5Yr[rev$d_edge < 1],
                                    levels = CAT_LABELS)))) %>%
  mutate(gap = round(soft_count - hard_count, 1),
         pct_fragile = round(100 * fragile / pmax(hard_count, 1), 1))

print(as.data.frame(soft_counts), row.names = FALSE)
cat("\nSum of hard counts:", sum(soft_counts$hard_count),
    " soft:", round(sum(soft_counts$soft_count), 1), "\n")

## ---------------------------------------------------------------------
## [16.6] Grade every forecast
## ---------------------------------------------------------------------
rev <- rev %>%
  left_join(diag_cu %>% select(join_number, triage, bt_cat_hit, bt_pct_err),
            by = "join_number") %>%
  mutate(
    grade = case_when(
      grepl("EXCLUDE", triage)                                  ~ "D",
      p_bucket < 0.30 | agree_rate < 0.34                       ~ "D",
      p_bucket < P_WEAK | dir_conflict | agree_rate < 0.67      ~ "C",
      p_bucket < P_STRONG | triage == "REVIEW"                  ~ "B",
      TRUE                                                       ~ "A"),
    review_note = trimws(paste0(
      ifelse(p_bucket < P_WEAK,
             sprintf("bucket only %.0f%% likely; ", 100 * p_bucket), ""),
      ifelse(d_edge < 1, sprintf("within %.1f SE of a boundary; ", d_edge), ""),
      ifelse(agree_rate < 1 & !is.na(agree_rate),
             sprintf("%d of %d methods agree; ", n_agree, n_avail), ""),
      ifelse(dir_conflict, "forecast direction opposes last 3 years; ", ""),
      ifelse(!is.na(bt_cat_hit) & !bt_cat_hit, "backtest missed category; ", ""),
      ifelse(grepl("capped", basis), "growth capped by guardrail; ", ""),
      ifelse(grepl("^Peer", basis), "peer growth used; ", ""))))

table(rev$grade)
round(100 * table(rev$grade) / nrow(rev), 1)

## Grades by bucket -- where is the weak evidence concentrated
rev %>% count(cat_5Yr, grade) %>%
  pivot_wider(names_from = grade, values_from = n, values_fill = 0) %>%
  arrange(match(cat_5Yr, CAT_LABELS)) %>% as.data.frame()

## ---------------------------------------------------------------------
## [16.7] The review queue, ordered by how much the answer would move
## An institution is worth a person's time in proportion to how uncertain
## its assignment is AND how much its bucket's count depends on it.
## ---------------------------------------------------------------------
bucket_n <- rev %>% count(cat_5Yr, name = "bucket_size")

queue <- rev %>%
  filter(grade %in% c("C", "D")) %>%
  left_join(bucket_n, by = "cat_5Yr") %>%
  mutate(impact = round((1 - p_bucket) / pmax(bucket_size, 1) * 1000, 3)) %>%
  arrange(desc(impact)) %>%
  transmute(`Join Number` = join_number, `CU Name` = cu_name, Region = region,
            State = state, `CU Type` = cu_type,
            `Assets now ($)` = round(assets_now),
            `Category now` = unname(CAT_PRETTY[asset_cat_now]),
            `Projected 5Yr ($)` = round(assets_5Yr),
            `Category 5Yr` = unname(CAT_PRETTY[cat_5Yr]),
            `Growth % p.a.` = growth_pa,
            `Bucket probability` = round(p_bucket, 2),
            `SEs from boundary` = d_edge,
            `Methods agreeing` = paste0(n_agree, " of ", n_avail),
            `Grade` = grade, `Impact rank` = impact,
            `Why flagged` = review_note,
            `Reviewer` = "", `Decision` = "", `Reviewer note` = "")

nrow(queue)
cat("\nReview queue:", nrow(queue), "institutions",
    sprintf("(%.1f%% of the cohort)", 100 * nrow(queue) / nrow(rev)), "\n")
cat("At 3 minutes each that is about",
    round(nrow(queue) * 3 / 60, 1), "hours of review.\n")

head(as.data.frame(queue %>% select(`CU Name`, `Category now`, `Category 5Yr`,
                                    `Bucket probability`, Grade, `Why flagged`)), 15)

## ---------------------------------------------------------------------
## [16.8] One chart per queued institution, for the reviewer
## ---------------------------------------------------------------------
pdf(file.path(REVIEW_DIR, "review_queue_charts.pdf"), width = 9, height = 5)
for (jn in head(queue$`Join Number`, 300)) {
  s <- cu_series[[which(vapply(cu_series, function(z) z$join_number, 0) == jn)]]
  info <- rev %>% filter(join_number == jn)
  h <- data.frame(date = as.Date(paste0(qgrid$year[s$q_index], "-",
                                        (qgrid$quarter[s$q_index] - 1) * 3 + 1, "-01")),
                  assets = exp(s$y))
  f <- data.frame(date = seq(max(h$date), by = "3 months", length.out = 21)[-1],
                  assets = exp(info$log_now + info$g_final * (1:20)))
  edges <- BREAKS[is.finite(BREAKS) & BREAKS > 0]

  print(
    ggplot() +
      geom_hline(yintercept = edges, colour = "grey75", linetype = "dashed",
                 linewidth = 0.35) +
      geom_ribbon(data = f, aes(date, ymin = exp(log(assets) - 1.28 * info$sigma),
                                ymax = exp(log(assets) + 1.28 * info$sigma)),
                  fill = "#C0392B", alpha = 0.15) +
      geom_line(data = h, aes(date, assets), colour = "#1F3B63", linewidth = 0.7) +
      geom_line(data = f, aes(date, assets), colour = "#C0392B",
                linewidth = 0.8, linetype = "22") +
      scale_y_log10(labels = label_dollar(scale_cut = cut_short_scale())) +
      labs(title = paste0(info$cu_name, "   [grade ", info$grade, "]"),
           subtitle = sprintf("%s -> %s | %.1f%% p.a. | bucket %.0f%% likely | %.1f SE from boundary",
                              info$asset_cat_now, info$cat_5Yr, info$growth_pa,
                              100 * info$p_bucket, info$d_edge),
           caption = paste0(info$review_note, "\nDashed lines are category boundaries. ",
                            "Shaded band is the 80% range implied by this model's own error."),
           x = NULL, y = "Assets (log scale)") +
      theme_minimal(base_size = 10, base_family = "sans") +
      theme(plot.title = element_text(face = "bold", size = 11),
            plot.subtitle = element_text(size = 9, colour = "grey30"),
            plot.caption = element_text(size = 8, colour = "grey45", hjust = 0),
            panel.grid.minor = element_blank()))
}
dev.off()

## ---------------------------------------------------------------------
## [16.9] Reviewer workbook
## ---------------------------------------------------------------------
SH <- list()

instructions <- c(
  "FORECAST REVIEW QUEUE",
  "",
  paste("Prepared:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste0("Cohort: ", nrow(rev), " credit unions. Queued for review: ", nrow(queue), "."),
  "",
  "WHY ONLY THESE",
  "  Every forecast in the cohort was tested automatically against four",
  "  reference points: whether three different methods agree on the category,",
  "  how far the projection sits from a category boundary relative to its own",
  "  forecast error, whether it points the same way as the last three years,",
  "  and where its growth sits among its peers. Forecasts graded A or B passed.",
  "  Only C and D are here.",
  "",
  "  Reviewing all 4,202 individually is not achievable and would not be a",
  "  good use of the time: most assignments are not close calls. This queue is",
  "  ordered so the institutions whose category is both uncertain AND",
  "  consequential for a bucket count come first.",
  "",
  "WHAT TO DO WITH EACH ROW",
  "  The matching chart is in review/review_queue_charts.pdf, in this order.",
  "  Look at whether the projected path is a reasonable continuation of the",
  "  history. Then record one of:",
  "    ACCEPT   the projection is reasonable, leave it",
  "    CATEGORY the growth looks right but the category is wrong -- say which",
  "    REJECT   the projection is not credible -- say what it should be",
  "    UNKNOWN  cannot judge from the data shown",
  "  Add anything you know that the data does not show: a pending merger, a",
  "  field-of-membership change, a conversion, a known reporting problem.",
  "  Local knowledge is the whole reason a person is looking at this.",
  "",
  "WHAT THE GRADES MEAN",
  "  A  three methods agree, comfortably inside its category",
  "  B  minor doubt: near a boundary, or flagged in the diagnostics",
  "  C  methods disagree, or the category is close to a coin toss",
  "  D  category under 30% likely, or the institution should not be reported",
  "     individually at all",
  "",
  "A NOTE ON WHAT THIS DOES NOT ESTABLISH",
  "  Grade A means the assignment is robust to method choice and sits well",
  "  inside its category. It does not mean the forecast is right. Five-year",
  "  projections of individual institutions carry real uncertainty whatever",
  "  the grade, and the category counts are more reliable than any single row.")

SH[[1]] <- list(name = "Instructions",
  rows = c(xl_line(instructions[1], 1, S_TITLE),
           vapply(seq_along(instructions)[-1], function(i)
             xl_line(instructions[i], i, S_NORM), "")),
  cols = col_widths(list(c(1, 1, 100))), freeze = NULL, autofilter = NULL,
  images = list())

gsum <- rev %>% count(grade, name = "institutions") %>%
  mutate(pct = round(100 * institutions / sum(institutions), 1))
b <- xl_block(as.data.frame(gsum), 3, col_styles = c(S_NORM, S_INT, S_DEC))
b2 <- xl_block(as.data.frame(soft_counts), b$next_row + 2,
               col_styles = c(S_NORM, S_INT, S_DEC, S_INT, S_DEC, S_DEC))
SH[[2]] <- list(name = "Confidence Summary",
  rows = c(xl_line("How much confidence each forecast earned", 1, S_TITLE),
           b$xml,
           xl_line("Hard counts vs probability-weighted soft counts", b$next_row + 1, S_SUB),
           b2$xml,
           xl_line("A large gap means that category's count rests on institutions near a boundary.",
                   b2$next_row + 1),
           xl_line("'Fragile' counts institutions within one standard error of a category edge.",
                   b2$next_row + 2)),
  cols = col_widths(list(c(1, 1, 24), c(2, 6, 18))),
  freeze = NULL, autofilter = NULL, images = list())

b <- xl_block(as.data.frame(queue), 3,
              col_styles = c(S_INT, S_NORM, S_INT, S_NORM, S_INT, S_INT, S_NORM,
                             S_INT, S_NORM, S_DEC, S_DEC, S_DEC, S_NORM, S_NORM,
                             S_DEC, S_NORM, S_NORM, S_NORM, S_NORM))
SH[[3]] <- list(name = "Review Queue",
  rows = c(xl_line("Ordered by uncertainty weighted by effect on the bucket count", 1, S_TITLE),
           b$xml),
  cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 5, 9), c(6, 6, 18),
                         c(7, 7, 18), c(8, 8, 20), c(9, 9, 18), c(10, 15, 15),
                         c(16, 16, 60), c(17, 18, 16), c(19, 19, 50))),
  freeze = list(x = 2, y = 3),
  autofilter = sprintf("A3:%s%d", xl_col(ncol(queue)), 3 + nrow(queue)),
  images = list())

full <- rev %>%
  transmute(`Join Number` = join_number, `CU Name` = cu_name, Region = region,
            `CU Type` = cu_type, `Category now` = asset_cat_now,
            `Category 5Yr` = cat_5Yr, `Growth % p.a.` = growth_pa,
            Basis = basis, `Bucket probability` = round(p_bucket, 2),
            `SEs from boundary` = d_edge, `Methods agreeing` = n_agree,
            `Peer percentile` = peer_pct, `Direction conflict` = dir_conflict,
            Grade = grade, `Notes` = review_note)
b <- xl_block(as.data.frame(full), 3,
              col_styles = c(S_INT, S_NORM, S_INT, S_INT, S_NORM, S_NORM, S_DEC,
                             S_NORM, S_DEC, S_DEC, S_INT, S_INT, S_NORM, S_NORM,
                             S_NORM))
SH[[4]] <- list(name = "All Forecasts Graded",
  rows = c(xl_line("Every forecast in the cohort, with its confidence grade", 1, S_TITLE),
           b$xml),
  cols = col_widths(list(c(1, 1, 12), c(2, 2, 38), c(3, 4, 9), c(5, 6, 16),
                         c(7, 13, 15), c(14, 14, 8), c(15, 15, 60))),
  freeze = list(x = 2, y = 3),
  autofilter = sprintf("A3:%s%d", xl_col(ncol(full)), 3 + nrow(full)),
  images = list())

xlsx_write(SH, file.path(getwd(),
  paste0("CU_Forecast_Review_", format(Sys.Date(), "%Y%m%d"), ".xlsx")))

saveRDS(list(rev = rev, queue = queue, soft_counts = soft_counts,
             soft = soft, P_STRONG = P_STRONG, P_WEAK = P_WEAK),
        file = "cohort_review.rds")
