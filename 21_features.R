## =====================================================================
## 21_features.R  --  Origin-level design matrix and h-step outcomes
##
## One row per (institution, origin quarter). Everything on the row is
## known AT the origin. The outcome columns are what happened h quarters
## later, for h = 4, 12, 20 -- the 1, 3 and 5 year horizons.
##
## Two things are modelled downstream and both outcomes are built here:
##
##   dy_h     h-step log asset growth, conditional on surviving to t+h.
##            Script 22 fits its whole conditional DISTRIBUTION, not its
##            mean, which is the entire point of the exercise: the bucket
##            probability is that distribution evaluated at the category
##            boundaries, and the boundaries are known constants.
##
##   exit_h   whether the institution merged away or closed within h
##            quarters.
##
## Under CLOSED_COHORT (set in script 20) exit_h is NOT forecast. It is
## still built, because it is what tells the growth model which rows have
## a real outcome and which are merger-truncated. Treating a merger as a
## missing observation would quietly bias the growth distribution; treating
## it as an outcome would be worse.
##
## One consequence to carry to the Method tab: because dy_h only exists for
## institutions that survived to t+h, the fitted distribution is
## SURVIVOR-CONDITIONAL. Shrinking institutions merge away more often, so
## that distribution sits a little high. With exits switched off in the
## forecast, the published counts inherit that tilt -- they answer "where
## would these 4,202 land if none of them merged", not "how many credit
## unions will exist".
##
## Note what is NOT here: no forward-looking variable, no full-sample
## statistic, nothing computed from the outcome window. Anything of that
## kind leaks and the cross-validation in 22 will not catch it.
##
## Run block by block in RStudio.
## =====================================================================

library(dplyr)
library(tidyr)

setwd("S:/Projects/Credit_Union_Growth_Forecast/Data")

## prep <- readRDS("panel_prep.rds"); list2env(prep, .GlobalEnv)

H_SET    <- c(4, 12, 20)     # 1, 3, 5 years
MIN_HIST <- 12               # quarters of history required at the origin
CAP_D    <- 4                # cap on log distance to an open-ended edge
VOL_W    <- 12               # window for growth volatility
ACQ_W    <- 20               # window for the acquisition count

## ---------------------------------------------------------------------
## [21.1] Trailing features
##
## Growth is per-quarter mean log change over a trailing window, so g4 and
## g20 are on the same scale and comparable. Where the institution is too
## young for the full window, the longest available span is used and
## hist_len records how thin it was -- the model can then learn to trust a
## 5-year trend more than a 3-quarter one, which is not something a
## per-institution ARIMA can do.
## ---------------------------------------------------------------------
lag_or_first <- function(y, k) {
  n <- length(y)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) out[i] <- if (i > k) y[i - k] else y[1]
  out
}
span_or_first <- function(k, n) pmin(k, seq_len(n) - 1)

roll_sd <- function(d, w) {
  ## d is diff(y) padded with NA at position 1
  cs  <- c(0, cumsum(ifelse(is.na(d), 0, d)))
  cs2 <- c(0, cumsum(ifelse(is.na(d), 0, d^2)))
  cn  <- c(0, cumsum(!is.na(d)))
  n <- length(d); out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- max(0, i - w)
    k <- cn[i + 1] - cn[lo + 1]
    if (k < 3) next
    m1 <- (cs[i + 1] - cs[lo + 1]) / k
    m2 <- (cs2[i + 1] - cs2[lo + 1]) / k
    out[i] <- sqrt(max(m2 - m1^2, 0) * k / (k - 1))
  }
  out
}

roll_sum <- function(x, w) {
  cs <- c(0, cumsum(x))
  n <- length(x)
  vapply(seq_len(n), function(i) cs[i + 1] - cs[max(0, i - w) + 1], 0)
}

feat <- panel %>%
  arrange(join_number, q_index) %>%
  group_by(join_number) %>%
  mutate(
    hist_len = seq_len(n()),
    dq       = c(NA, diff(y)),
    g4       = (y - lag_or_first(y,  4)) / pmax(span_or_first(4,  n()), 1),
    g12      = (y - lag_or_first(y, 12)) / pmax(span_or_first(12, n()), 1),
    g20      = (y - lag_or_first(y, 20)) / pmax(span_or_first(20, n()), 1),
    vol      = roll_sd(dq, VOL_W),
    acq_w    = roll_sum(acq_event, ACQ_W),
    acq_cum  = cumsum(acq_event),
    q_last_acq = cummax(ifelse(acq_event > 0, q_index, -Inf)),
    q_since_acq = pmin(ifelse(is.finite(q_last_acq), q_index - q_last_acq, 99), 40)) %>%
  ungroup() %>%
  filter(hist_len >= MIN_HIST)

nrow(feat)
summary(feat$g20 * 4)          # annualised, sanity: mostly 0 to 0.10
summary(feat$vol)

## ---------------------------------------------------------------------
## [21.2] Position inside the category
##
## The dominant predictor of a category change is not size, it is DISTANCE
## TO THE EDGE. Two institutions in A4 with $110M and $480M have nothing in
## common for this purpose. The frozen-cohort track only sees this through
## the point forecast landing on one side or the other; here it is an
## explicit continuous covariate, on the log scale, in the same units as
## the growth being modelled.
## ---------------------------------------------------------------------
feat <- feat %>%
  mutate(
    lo_edge = LOG_EDGE[cat_k],
    hi_edge = LOG_EDGE[cat_k + 1],
    d_dn = pmin(ifelse(is.finite(lo_edge), y - lo_edge, CAP_D), CAP_D),
    d_up = pmin(ifelse(is.finite(hi_edge), hi_edge - y, CAP_D), CAP_D),
    ## Where in the band, 0 at the floor and 1 at the ceiling. A1 and A7
    ## are open-ended so the ratio is undefined; set them to the midpoint
    ## rather than NA. Leaving NA here would make complete.cases drop every
    ## A1 and A7 row inside glm, silently, and A7 is the category the
    ## handoff already flags as least reliable.
    pos_in_cat = ifelse(is.finite(lo_edge) & is.finite(hi_edge),
                        (y - lo_edge) / (hi_edge - lo_edge), 0.5),
    open_cat = as.numeric(!is.finite(lo_edge) | !is.finite(hi_edge)))

summary(feat$d_up); summary(feat$d_dn)
feat %>% group_by(cat_now) %>%
  summarise(n = n(), med_d_up = round(median(d_up), 2),
            med_d_dn = round(median(d_dn), 2)) %>% as.data.frame()

## ---------------------------------------------------------------------
## [21.3] Peer growth at the origin
##
## Median trailing 3-year growth of the region x charter x category cell,
## computed from the origin quarter only. This is the pooled analogue of
## the peer fallback in script 12, but here it is a regressor available to
## every institution rather than a last resort for the ones that failed.
## ---------------------------------------------------------------------
peer <- feat %>%
  group_by(q_index, region, cu_type, cat_k) %>%
  summarise(g_peer = median(g12, na.rm = TRUE), n_peer = n(), .groups = "drop")

## Thin cells get the wider region x type median instead
peer_wide <- feat %>%
  group_by(q_index, region, cu_type) %>%
  summarise(g_peer_wide = median(g12, na.rm = TRUE), .groups = "drop")

feat <- feat %>%
  left_join(peer, by = c("q_index", "region", "cu_type", "cat_k")) %>%
  left_join(peer_wide, by = c("q_index", "region", "cu_type")) %>%
  mutate(g_peer = ifelse(n_peer < 10 | is.na(g_peer), g_peer_wide, g_peer))

summary(feat$g_peer * 4)

## ---------------------------------------------------------------------
## [21.4] Regime variables -- at the origin, and over the forward window
##
## Two different things, and conflating them is the easy mistake.
##
##   *_now   the regime the origin sits in. Absorbs the fact that growth
##           measured in 2008 behaves differently from growth measured in
##           2015, so the baseline distribution is not contaminated by
##           crisis periods.
##
##   *_fwd   the share of the FORECAST WINDOW spent in that regime. This is
##           what actually drives the outcome, and it is the scenario lever.
##           For the 2026Q2 forecast origin we set shock_fwd = 0, which is
##           the same no-recession assumption the frozen-cohort track makes
##           silently. The difference is that here it is a dial: set it to
##           the 2007Q4 value and you get a recession scenario, or average
##           over values and you get a predictive distribution that admits
##           the future may contain one.
## ---------------------------------------------------------------------
cs_shock <- c(0, cumsum(qgrid$shock))
cs_dins  <- c(0, cumsum(qgrid$dins))
cs_rec   <- c(0, cumsum(pmax(qgrid$rec0709, qgrid$rec2020)))

fwd_share <- function(cs, t, h) {
  hi <- pmin(t + h, length(cs) - 1)
  (cs[hi + 1] - cs[t + 1]) / h
}

feat <- feat %>%
  left_join(qgrid %>% select(q_index, dins_now = dins, shock_now = shock,
                             rec_now = rec0709, cov_now = rec2020,
                             rate_now = rateshock),
            by = "q_index") %>%
  mutate(shock_trail = (cs_shock[q_index + 1] - cs_shock[pmax(q_index - 12, 0) + 1]) /
                        pmin(q_index, 12))

for (h in H_SET) {
  feat[[paste0("shock_fwd_h", h)]] <- fwd_share(cs_shock, feat$q_index, h)
  feat[[paste0("rec_fwd_h",   h)]] <- fwd_share(cs_rec,   feat$q_index, h)
  feat[[paste0("dins_fwd_h",  h)]] <- fwd_share(cs_dins,  feat$q_index, h)
}

feat %>% select(q_index, shock_now, shock_fwd_h20, rec_fwd_h20) %>%
  distinct() %>% slice(c(1, 12, 16, 40, 60, 80)) %>% as.data.frame()

## ---------------------------------------------------------------------
## [21.5] Outcomes
##
## Joined forward rather than lead()-ed, so a hole in the spell cannot
## quietly supply the wrong quarter's assets.
## ---------------------------------------------------------------------
fwd_vals <- panel %>% select(join_number, q_index, y_f = y, cat_k_f = cat_k)

for (h in H_SET) {
  nm_dy <- paste0("dy_h", h); nm_cat <- paste0("cat_f_h", h)
  nm_ex <- paste0("exit_h", h); nm_us <- paste0("usable_h", h)

  jf <- fwd_vals %>% mutate(q_index = q_index - h) %>%
    rename(!!nm_dy := y_f, !!nm_cat := cat_k_f)

  feat <- feat %>% left_join(jf, by = c("join_number", "q_index"))
  feat[[nm_dy]] <- feat[[nm_dy]] - feat$y

  ## Exited within the window: merged away or closed. "filter" institutions
  ## have exit_q = NA by construction at [20.5] and are never counted here.
  feat[[nm_ex]] <- as.numeric(!is.na(feat$exit_q) & feat$exit_q <= feat$q_index + h)

  ## The window must be fully inside the sample, and the outcome must
  ## either be observed or be an exit. Anything else is a hole, not data.
  feat[[nm_us]] <- (feat$q_index + h <= N_Q) &
                   (!is.na(feat[[nm_dy]]) | feat[[nm_ex]] == 1)

  ## Rows the GROWTH model can use: window inside the sample and the
  ## institution actually there at t+h to be measured. This is the
  ## estimation sample under CLOSED_COHORT; usable_h stays for the exit
  ## model, which only runs if the closed-cohort assumption is lifted.
  feat[[paste0("usable_surv_h", h)]] <-
    feat[[nm_us]] & feat[[nm_ex]] == 0 & !is.na(feat[[nm_dy]])
}

## What survived
for (h in H_SET) {
  u <- feat[[paste0("usable_h", h)]]
  e <- feat[[paste0("exit_h", h)]]
  s <- feat[[paste0("usable_surv_h", h)]]
  cat(sprintf("h=%2d  usable %7d   exits %6d (%.1f%%)   growth sample %7d\n",
              h, sum(u), sum(u & e == 1), 100 * mean(e[u] == 1), sum(s)))
}

## Growth distribution by horizon, annualised. The LEFT TAIL here is the
## thing the frozen-cohort track cannot reproduce -- check it is present.
for (h in H_SET) {
  d <- feat[[paste0("dy_h", h)]][feat[[paste0("usable_surv_h", h)]]]
  cat(sprintf("h=%2d  ", h))
  print(round(100 * (exp(quantile(d, c(.01,.05,.10,.25,.50,.75,.90,.95,.99),
                                  na.rm = TRUE) * 4 / h) - 1), 1))
}

## Realised category moves in the data, for later comparison against the
## forecast. Script 15 found 454 up / 87 down over five years; the same
## shape should appear here, computed on the unfrozen panel.
for (h in H_SET) {
  u <- feat[[paste0("usable_surv_h", h)]]
  mv <- feat[[paste0("cat_f_h", h)]][u] - feat$cat_k[u]
  cat(sprintf("h=%2d  down %6d  same %7d  up %6d\n",
              h, sum(mv < 0, na.rm = TRUE), sum(mv == 0, na.rm = TRUE),
              sum(mv > 0, na.rm = TRUE)))
}

## ---------------------------------------------------------------------
## [21.6] The forecast rows: the 2026Q2 origin
##
## Same construction, no outcome. Kept separate so nothing about the
## forecast origin can wander into a training fold.
## ---------------------------------------------------------------------
fc_rows <- feat %>%
  filter(q_index == N_Q, join_number %in% cohort$join_number)

nrow(fc_rows)                                    # should approach 4,202
setdiff(cohort$join_number, fc_rows$join_number) %>% length()   # too-young CUs

## Cohort members with under MIN_HIST quarters get the peer-cell
## distribution in script 23 rather than their own covariates. Flag them
## now instead of discovering it there.
short_cu <- cohort %>% filter(!join_number %in% fc_rows$join_number)
nrow(short_cu); short_cu %>% count(region, cu_type) %>% as.data.frame()

## Baseline scenario for the forecast origin: no recession, no rate shock,
## insurance limit unchanged, and -- under CLOSED_COHORT -- no exits.
## Overwrite these to run a scenario.
for (h in H_SET) {
  fc_rows[[paste0("shock_fwd_h", h)]] <- 0
  fc_rows[[paste0("rec_fwd_h",   h)]] <- 0
  fc_rows[[paste0("dins_fwd_h",  h)]] <- 1
}

## ---------------------------------------------------------------------
## [21.7] The model frame
##
## One place that names the covariates, so 22 and 23 cannot drift apart.
## ---------------------------------------------------------------------
FEAT_NUM <- c("y", "d_up", "d_dn", "g4", "g12", "g20", "vol",
              "g_peer", "acq_w", "acq_cum", "q_since_acq",
              "hist_len", "shock_now", "shock_trail")
FEAT_FAC <- c("region", "cu_type", "cat_k")
FEAT_FWD <- function(h) paste0(c("shock_fwd_h", "rec_fwd_h"), h)

## Centre and scale the numeric block on the FULL panel, once, and keep the
## constants. 23 must reuse these -- rescaling the forecast rows on their
## own would silently shift every coefficient.
SCALE_MU <- vapply(feat[FEAT_NUM], function(z) mean(z, na.rm = TRUE), 0)
SCALE_SD <- vapply(feat[FEAT_NUM], function(z) sd(z, na.rm = TRUE), 0)
SCALE_SD[SCALE_SD == 0 | !is.finite(SCALE_SD)] <- 1
round(rbind(SCALE_MU, SCALE_SD), 4)

apply_scale <- function(d) {
  ## The boundaries live on the raw log-asset scale, so keep an unscaled
  ## copy before standardising. 22 places the category edges with y_raw.
  d$y_raw <- d$y
  for (v in FEAT_NUM) d[[v]] <- (d[[v]] - SCALE_MU[v]) / SCALE_SD[v]
  d$region  <- factor(d$region,  levels = c(1, 2, 3))
  d$cu_type <- factor(d$cu_type, levels = c(1, 2))
  d$cat_f   <- factor(d$cat_k,   levels = 1:N_CAT)
  d
}

feat_s    <- apply_scale(feat)
fc_rows_s <- apply_scale(fc_rows)

## Missingness check -- glm drops incomplete rows silently and the fold
## sizes then stop matching, which is very hard to spot later.
colSums(is.na(feat_s[c(FEAT_NUM, FEAT_FAC, "pos_in_cat", "y_raw")]))
colSums(is.na(fc_rows_s[c(FEAT_NUM, FEAT_FAC, "pos_in_cat", "y_raw")]))

saveRDS(list(feat = feat_s, fc_rows = fc_rows_s, short_cu = short_cu,
             H_SET = H_SET, FEAT_NUM = FEAT_NUM, FEAT_FAC = FEAT_FAC,
             FEAT_FWD = FEAT_FWD, SCALE_MU = SCALE_MU, SCALE_SD = SCALE_SD,
             apply_scale = apply_scale, MIN_HIST = MIN_HIST, CAP_D = CAP_D),
        file = "panel_features.rds")
