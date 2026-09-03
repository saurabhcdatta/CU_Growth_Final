## =====================================================================
## 12_cu_forecast.R  --  Final fits, GUARDRAILS, bucket assignment
##
## Per-institution ARIMA fails in ways the bucket-level models did not,
## because there is no averaging to absorb it. A single credit union with a
## short growth spurt, an acquisition near the end of its history, or a
## restatement can produce a projection that is arithmetically valid and
## obviously wrong. The guardrails below catch those cases and step down to
## simpler methods, recording which rule fired for every institution.
##
## THE LADDER, in order. The first basis that passes is used.
##   1  ARIMA (CV)          the cross-validated model, if it passes every rule
##   2  ARIMA rank 2..6     the next-best cross-validated model that passes.
##                          Script 11 already screened every candidate's
##                          full-sample path, so a lower-ranked ARIMA that
##                          forecasts sensibly is preferred to abandoning
##                          ARIMA altogether
##   3  ARIMA, capped       the winner's direction kept, growth clipped
##   4  Simple growth rate  median quarterly log change, robust to spikes
##   5  Peer growth         median growth of the same region, charter type
##                          and current size band
##
## Note that script 11 now also runs the simple growth rate and a flat path
## as competitors inside cross-validation, so for many institutions the
## honest answer -- that no ARIMA beats a straight growth rate -- is reached
## by selection rather than by fallback.
##
## THE RULES
##   R1 ACCURACY   CV error at the 5-year horizon must be under RMSE_MAX
##   R2 BAND       implied growth must sit inside [G_MIN_PA, G_MAX_PA]
##   R3 JUMP       an institution may not cross more than MAX_JUMP buckets
##   R4 NOT FLAT   a projection may not be flat when the institution has a
##                 clear trend, or it can never move bucket
##   R5 ENVELOPE   growth must sit inside the range this institution has
##                 actually achieved over its own rolling five-year windows,
##                 measured ORGANICALLY (acquisition quarters removed), so a
##                 serial acquirer is not licensed to keep growing at a pace
##                 it only reached by buying people
## Every basis is then shrunk toward peer growth by an amount that depends
## on how well its model actually forecast out of sample.
## =====================================================================

library(dplyr)
library(tidyr)
library(forecast)
library(parallel)

## prep <- readRDS("cohort_prep.rds"); list2env(prep, .GlobalEnv)
## cvr  <- readRDS("cohort_cv.rds");   list2env(cvr,  .GlobalEnv)

H_SET    <- c(4, 12, 20)
H_NAMES  <- c("1Yr", "3Yr", "5Yr")
FC_START <- c(2026, 3)

## ---- Guardrail settings ---------------------------------------------
RMSE_MAX  <- 0.40     # log-scale CV RMSE at h=20; ~40% typical error
G_MAX_PA  <- 0.25     # +25% a year is the ceiling for a sustained 5-yr path
G_MIN_PA  <- -0.15    # -15% a year is the floor
MAX_JUMP  <- 2        # buckets an institution may cross in five years
FLAT_TOL  <- 0.005    # under 0.5% a year counts as flat
TREND_P   <- 0.10     # trend significance for the not-flat rule
SHRINK_K  <- 0.15     # shrinkage half-weight point on CV RMSE

## Historical envelope (R5)
ENV_Q     <- 0.90     # quantile of own rolling windows, not the raw max:
                      # with ~66 overlapping windows the maximum is an
                      # upward-biased extreme and rarely binds
ENV_WIDEN <- 0.02     # widen 2pp each side, so a flat institution is not
                      # locked in place by its own quiet history
ENV_MIN_W <- 8        # windows required before an institution's own envelope
                      # is trusted; below this its peer group is used

## ---------------------------------------------------------------------
## [12.1] Refit the CV winner and project (unchanged from before)
## ---------------------------------------------------------------------
fit_one <- function(s, cv_scores, H_MAX, G_MAX_PA, G_MIN_PA) {

  ## Take the best-ranked PLAUSIBLE candidate, whatever kind it is.
  ##
  ## Earlier this searched for the best plausible ARIMA regardless of rank,
  ## which silently discarded every benchmark victory: if "flat" or "simple
  ## growth" won on cross-validation and any ARIMA sat below it, the ARIMA
  ## was used anyway. That defeated the purpose of letting the benchmarks
  ## compete. Cross-validation decides; this function honours the decision.
  sc <- cv_scores[cv_scores$join_number == s$join_number, ]
  sc <- sc[order(sc$rank), ]
  if (nrow(sc) == 0) return(NULL)

  pick <- which(sc$plausible)[1]
  if (is.na(pick)) pick <- 1L
  cn <- sc[pick, ]
  w  <- cn

  ## A benchmark winner needs no refitting: its growth rate is already known
  if (cn$cand_id[1] < 0 || is.na(cn$p[1])) {
    g <- cn$g_pa_full[1] / 4                      # per quarter
    return(data.frame(join_number = s$join_number, spec = cn$spec[1],
                      cv_rank_used = cn$rank[1], log_now = tail(s$y, 1),
                      a_1Yr = tail(s$y, 1) + g * 4,
                      a_3Yr = tail(s$y, 1) + g * 12,
                      a_5Yr = tail(s$y, 1) + g * 20,
                      stringsAsFactors = FALSE))
  }

  y <- ts(s$y, frequency = 4); X <- s$xreg
  keep <- apply(X, 2, function(z) length(unique(z)) > 1)
  Xf <- if (any(keep)) X[, keep, drop = FALSE] else NULL

  Xnew <- NULL
  if (!is.null(Xf)) {
    last <- X[nrow(X), , drop = FALSE]
    Xnew <- matrix(rep(last[, keep, drop = FALSE], each = H_MAX), nrow = H_MAX,
                   dimnames = list(NULL, colnames(X)[keep]))
    for (cc in intersect(colnames(Xnew), c("rec0709", "rec2020", "rateshock")))
      Xnew[, cc] <- 0
  }

  fit <- tryCatch(forecast::Arima(y, order = c(cn$p, cn$d, cn$q),
      seasonal = list(order = c(cn$P, cn$D, cn$Q), period = 4),
      xreg = Xf, include.drift = cn$drift, method = "ML"),
    error = function(e) NULL, warning = function(w) NULL)
  if (is.null(fit)) return(NULL)

  fc <- tryCatch(forecast::forecast(fit, h = H_MAX, xreg = Xnew),
                 error = function(e) NULL)
  if (is.null(fc) || any(!is.finite(as.numeric(fc$mean)))) return(NULL)

  m <- as.numeric(fc$mean)
  data.frame(join_number = s$join_number, spec = w$spec[1],
             cv_rank_used = w$rank[1], log_now = tail(s$y, 1),
             a_1Yr = m[4], a_3Yr = m[12], a_5Yr = m[20],
             stringsAsFactors = FALSE)
}

t0 <- Sys.time()
cl <- makeCluster(max(1, parallel::detectCores(logical = FALSE) - 1))
clusterEvalQ(cl, library(forecast))
clusterExport(cl, c("fit_one", "cv_scores", "H_MAX", "G_MAX_PA", "G_MIN_PA"),
              envir = globalenv())
fit_list <- parLapplyLB(cl, cu_series, function(s)
  fit_one(s, cv_scores, H_MAX, G_MAX_PA, G_MIN_PA))
stopCluster(cl)
difftime(Sys.time(), t0, units = "mins")

arima_tbl <- bind_rows(fit_list[!sapply(fit_list, is.null)])
nrow(arima_tbl)

## How far down the ranking we had to go
table(arima_tbl$cv_rank_used)
cat("Institutions served by CV rank 1:",
    sum(arima_tbl$cv_rank_used == 1), "of", nrow(arima_tbl), "\n")

## ---------------------------------------------------------------------
## [12.2] Simple growth rate, per institution
## Median of quarterly log changes over the last five years. The median is
## used rather than the mean so that one acquisition or restatement moves
## it hardly at all -- which is exactly why it makes a good fallback.
## ---------------------------------------------------------------------
simple_g <- bind_rows(lapply(cu_series, function(s) {
  n <- s$n
  if (n < 9) return(data.frame(join_number = s$join_number,
                               g_simple = NA_real_, trend_p = NA_real_, n_obs = n))
  win <- s$y[max(1, n - 19):n]
  d <- diff(win)
  tp <- tryCatch(summary(lm(win ~ seq_along(win)))$coefficients[2, 4],
                 error = function(e) NA_real_)
  data.frame(join_number = s$join_number, g_simple = median(d),
             trend_p = tp, n_obs = n)
}))

summary(simple_g$g_simple * 4)     # annual log growth

## ---------------------------------------------------------------------
## [12.3] Historical growth envelope, per institution
##
## Rolling 20-quarter (five-year) growth from the institution's own history,
## with acquisition quarters neutralised so the envelope reflects organic
## growth only. Horizon-matched: five-year windows bound a five-year path.
## ---------------------------------------------------------------------
env_tbl <- bind_rows(lapply(cu_series, function(s) {
  n <- s$n
  out <- data.frame(join_number = s$join_number, env_hi = NA_real_,
                    env_lo = NA_real_, env_n = 0L, env_max = NA_real_)
  if (n < 24) return(out)

  d <- diff(s$y)
  ## Neutralise quarters in which this institution acquired someone: replace
  ## the jump with its own typical organic change rather than dropping it,
  ## so window lengths stay comparable.
  acq_q <- which(diff(c(0, s$xreg[, "acq_cum"])) > 0)
  acq_d <- acq_q - 1                       # index into diff()
  acq_d <- acq_d[acq_d >= 1 & acq_d <= length(d)]
  if (length(acq_d)) d[acq_d] <- median(d[-acq_d])

  W <- 20
  if (length(d) < W) return(out)
  ## Annualised log growth over each rolling five-year window
  roll <- zoo_like <- rep(NA_real_, length(d) - W + 1)
  cs <- cumsum(c(0, d))
  for (i in seq_along(roll)) roll[i] <- (cs[i + W] - cs[i]) / 5

  out$env_hi  <- as.numeric(quantile(roll, ENV_Q, na.rm = TRUE))
  out$env_lo  <- as.numeric(quantile(roll, 1 - ENV_Q, na.rm = TRUE))
  out$env_max <- max(roll, na.rm = TRUE)
  out$env_n   <- length(roll)
  out
}))

summary(env_tbl$env_n)
summary(100 * (exp(env_tbl$env_hi) - 1))    # upper envelope, % a year

## ---------------------------------------------------------------------
## [12.4] Peer growth: same region, charter type, current bucket
## ---------------------------------------------------------------------
base <- cohort %>%
  left_join(simple_g, by = "join_number") %>%
  left_join(arima_tbl, by = "join_number") %>%
  left_join(cv_winners %>% select(join_number, cv_rmse_h20 = rmse_h20),
            by = "join_number") %>%
  mutate(log_now = ifelse(is.na(log_now), log(assets_now), log_now),
         g_arima = (a_5Yr - log_now) / 20)

peer <- base %>%
  filter(!is.na(g_simple)) %>%
  group_by(region, cu_type, asset_cat_now) %>%
  summarise(g_peer = median(g_simple, na.rm = TRUE), n_peer = n(), .groups = "drop")

base <- base %>%
  left_join(peer, by = c("region", "cu_type", "asset_cat_now")) %>%
  mutate(g_peer = ifelse(is.na(g_peer) | n_peer < 5,
                         median(simple_g$g_simple, na.rm = TRUE), g_peer)) %>%
  left_join(env_tbl, by = "join_number")

## Peer envelope, used where an institution's own history is too short
peer_env <- base %>% filter(env_n >= ENV_MIN_W) %>%
  group_by(region, cu_type, asset_cat_now) %>%
  summarise(penv_hi = median(env_hi, na.rm = TRUE),
            penv_lo = median(env_lo, na.rm = TRUE), .groups = "drop")

base <- base %>%
  left_join(peer_env, by = c("region", "cu_type", "asset_cat_now")) %>%
  mutate(penv_hi = replace_na(penv_hi, quantile(env_tbl$env_hi, .75, na.rm = TRUE)),
         penv_lo = replace_na(penv_lo, quantile(env_tbl$env_lo, .25, na.rm = TRUE)),
         use_own_env = env_n >= ENV_MIN_W & !is.na(env_hi),
         env_up = ifelse(use_own_env, env_hi, penv_hi) + ENV_WIDEN / 4,
         env_dn = ifelse(use_own_env, env_lo, penv_lo) - ENV_WIDEN / 4)

## ---------------------------------------------------------------------
## [12.4] Bucket-jump ceiling
## Growth per quarter that would take an institution exactly MAX_JUMP
## buckets up or down over 20 quarters, from where it sits today.
## ---------------------------------------------------------------------
cat_idx <- match(base$asset_cat_now, CAT_LABELS)
up_idx   <- pmin(cat_idx + MAX_JUMP, length(CAT_LABELS))
down_idx <- pmax(cat_idx - MAX_JUMP, 1L)

## Upper edge of the bucket MAX_JUMP above; lower edge of the one below
up_edge   <- BREAKS[up_idx + 1]        # may be Inf for the top bucket
down_edge <- BREAKS[down_idx]          # may be -Inf for the bottom bucket

base <- base %>%
  mutate(g_cap_up = ifelse(is.finite(up_edge),
                           (log(up_edge) - log_now) / 20, Inf),
         g_cap_dn = ifelse(is.finite(down_edge) & down_edge > 0,
                           (log(down_edge) - log_now) / 20, -Inf),
         g_cap_up = pmin(g_cap_up, G_MAX_PA / 4),
         g_cap_dn = pmax(g_cap_dn, G_MIN_PA / 4),
         ## R5: whichever of the three ceilings binds tightest wins
         g_cap_up = pmin(g_cap_up, env_up),
         g_cap_dn = pmax(g_cap_dn, env_dn))

## ---------------------------------------------------------------------
## [12.5] The ladder
## ---------------------------------------------------------------------
base <- base %>%
  mutate(
    has_arima = !is.na(g_arima),
    has_trend = !is.na(trend_p) & trend_p < TREND_P,

    ## R1 accuracy
    fail_rmse = is.na(cv_rmse_h20) | cv_rmse_h20 > RMSE_MAX,
    ## R2 band
    fail_band = has_arima & (g_arima * 4 > G_MAX_PA | g_arima * 4 < G_MIN_PA),
    ## R3 jump
    fail_jump = has_arima & (g_arima > g_cap_up | g_arima < g_cap_dn),
    ## R4 flat while trending
    fail_flat = has_arima & abs(g_arima * 4) < FLAT_TOL & has_trend,
    ## R5 outside its own historical envelope
    fail_env = has_arima & (g_arima > env_up | g_arima < env_dn),

    ## A flat result is honoured where the institution has no detectable
    ## trend: cross-validation found no change to be the best prediction, and
    ## with a fixed cohort an institution simply staying in its category is a
    ## realistic forecast. Where there IS a trend, a flat path is overridden,
    ## because then it could never move category however clearly it is growing.
    flat_won = grepl("^Flat", spec),

    basis = case_when(
      !has_arima & flat_won & !has_trend          ~ "Flat (won on CV, no trend)",
      !has_arima & flat_won & has_trend           ~ "Simple growth rate (flat won but trend present)",
      !has_arima & grepl("^Simple", spec)         ~ "Simple growth rate (won on CV)",
      !has_arima & !is.na(g_simple)               ~ "Simple growth rate (no ARIMA)",
      !has_arima                                   ~ "Peer growth (history too short)",
      fail_rmse                                    ~ "Simple growth rate (ARIMA unreliable)",
      fail_flat                                    ~ "Simple growth rate (ARIMA flat)",
      fail_band | fail_jump | fail_env             ~ "ARIMA, growth capped",
      cv_rank_used > 1                             ~ "ARIMA (CV rank 2+, plausible)",
      TRUE                                         ~ "ARIMA (CV selected)"),

    g_raw = case_when(
      basis == "Flat (won on CV, no trend)" ~ 0,
      grepl("^Simple growth rate", basis)   ~ g_simple,
      basis == "ARIMA (CV selected)"  ~ g_arima,
      basis == "ARIMA, growth capped" ~ pmin(pmax(g_arima, g_cap_dn), g_cap_up),
      grepl("^Simple", basis)         ~ g_simple,
      TRUE                            ~ g_peer),

    ## Final clip: nothing escapes the band or the jump ceiling
    g_clip = pmin(pmax(g_raw, g_cap_dn), g_cap_up),

    ## Shrink toward peers by how badly the model actually forecast.
    ## A clean CV record keeps its own growth; a poor one is pulled in.
    shrink_w = ifelse(is.na(cv_rmse_h20), 0.4,
                      SHRINK_K / (SHRINK_K + cv_rmse_h20)),
    shrink_w = ifelse(grepl("^Peer", basis), 0, shrink_w),
    g_final = shrink_w * g_clip + (1 - shrink_w) *
              pmin(pmax(g_peer, g_cap_dn), g_cap_up))

table(base$basis)
round(100 * table(base$basis) / nrow(base), 1)

## ---------------------------------------------------------------------
## [12.6] Projections from the final growth rate
## Constant-growth paths: smooth, monotone, always positive, and they
## cannot cross more buckets than the ceiling allows.
## ---------------------------------------------------------------------
cu <- base %>%
  mutate(assets_1Yr = exp(log_now + g_final * 4),
         assets_3Yr = exp(log_now + g_final * 12),
         assets_5Yr = exp(log_now + g_final * 20),
         growth_pa  = round(100 * (exp(g_final * 4) - 1), 2))

for (nm in H_NAMES)
  cu[[paste0("cat_", nm)]] <- as.character(
    cut(cu[[paste0("assets_", nm)]], breaks = BREAKS, labels = CAT_LABELS,
        right = FALSE))

cu <- cu %>%
  mutate(jump_5Yr = match(cat_5Yr, CAT_LABELS) - match(asset_cat_now, CAT_LABELS),
         move_5Yr = case_when(jump_5Yr > 0 ~ "Up", jump_5Yr < 0 ~ "Down",
                              TRUE ~ "Same"))

## ---------------------------------------------------------------------
## [12.7] Guardrail audit -- read this before exporting
## ---------------------------------------------------------------------
audit <- cu %>%
  summarise(
    n = n(),
    arima_clean   = sum(basis == "ARIMA (CV selected)"),
    arima_rank2   = sum(basis == "ARIMA (CV rank 2+, plausible)"),
    simple_won_cv = sum(basis == "Simple growth rate (won on CV)"),
    flat_honoured = sum(basis == "Flat (won on CV, no trend)"),
    flat_overridden = sum(basis == "Simple growth rate (flat won but trend present)"),
    arima_capped  = sum(basis == "ARIMA, growth capped"),
    simple_rate   = sum(grepl("^Simple", basis)),
    peer_rate     = sum(grepl("^Peer", basis)),
    failed_rmse   = sum(fail_rmse, na.rm = TRUE),
    failed_band   = sum(fail_band, na.rm = TRUE),
    failed_jump   = sum(fail_jump, na.rm = TRUE),
    failed_flat   = sum(fail_flat, na.rm = TRUE),
    failed_env    = sum(fail_env, na.rm = TRUE),
    own_envelope  = sum(use_own_env),
    peer_envelope = sum(!use_own_env),
    max_growth_pa = max(growth_pa), min_growth_pa = min(growth_pa),
    max_jump      = max(abs(jump_5Yr)),
    moved_up      = sum(move_5Yr == "Up"), moved_down = sum(move_5Yr == "Down"))

as.data.frame(t(audit))

## Nothing should exceed the ceilings
stopifnot(max(abs(cu$jump_5Yr)) <= MAX_JUMP,
          max(cu$growth_pa) <= 100 * (exp(G_MAX_PA) - 1) + 0.01,
          min(cu$growth_pa) >= 100 * (exp(G_MIN_PA) - 1) - 0.01)

## Where does each published growth rate sit against that institution's own
## history? Near 1 means we are forecasting it at the top of anything it has
## ever sustained -- worth a look even though the rule allowed it.
cu <- cu %>%
  mutate(env_position = ifelse(is.na(env_hi) | is.na(env_lo) | env_hi <= env_lo,
                               NA_real_,
                               round((g_final - env_lo) / (env_hi - env_lo), 2)))
summary(cu$env_position)
sum(cu$env_position > 1, na.rm = TRUE)    # above its own 90th percentile

## The most extreme survivors, for a quick eyeball
cu %>% arrange(desc(growth_pa)) %>%
  select(cu_name, asset_cat_now, cat_5Yr, growth_pa, basis) %>%
  head(15) %>% as.data.frame() %>% print(row.names = FALSE)
cu %>% arrange(growth_pa) %>%
  select(cu_name, asset_cat_now, cat_5Yr, growth_pa, basis) %>%
  head(15) %>% as.data.frame() %>% print(row.names = FALSE)

basis_by_size <- cu %>% count(asset_cat_now, basis) %>%
  pivot_wider(names_from = basis, values_from = n, values_fill = 0)
as.data.frame(basis_by_size)

## ---------------------------------------------------------------------
## [12.8] Bottom-up counts. Total is fixed by construction -- verify.
## ---------------------------------------------------------------------
bu_counts <- bind_rows(lapply(H_NAMES, function(nm)
  cu %>% count(region, cu_type, asset_cat = .data[[paste0("cat_", nm)]],
               name = "count") %>% mutate(horizon = nm))) %>%
  bind_rows(cu %>% count(region, cu_type, asset_cat = asset_cat_now,
                         name = "count") %>% mutate(horizon = "Now"))

bu_counts %>% group_by(horizon) %>% summarise(total = sum(count)) %>%
  as.data.frame()

saveRDS(list(cu = cu, bu_counts = bu_counts, audit = audit,
             basis_by_size = basis_by_size, env_tbl = env_tbl,
             H_SET = H_SET, H_NAMES = H_NAMES, FC_START = FC_START,
             guardrails = list(RMSE_MAX = RMSE_MAX, G_MAX_PA = G_MAX_PA,
                               G_MIN_PA = G_MIN_PA, MAX_JUMP = MAX_JUMP,
                               ENV_Q = ENV_Q, ENV_MIN_W = ENV_MIN_W)),
        file = "cohort_fits.rds")
