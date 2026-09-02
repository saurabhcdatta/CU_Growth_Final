## =====================================================================
## 11_cu_cv.R  --  Per-credit-union model selection by expanding-window CV
##
## One ARIMA per cohort member, on log assets, with the structural-break
## regressors from script 10. Quarterly origins, full candidate grid.
##
## This is the expensive step: roughly 4,200 credit unions x 8 candidates
## x 45 origins. Sequential that is several hours; the parallel block below
## cuts it to well under an hour on this machine. Start it and leave it.
## =====================================================================

library(dplyr)
library(forecast)
library(parallel)

## prep <- readRDS("cohort_prep.rds"); list2env(prep, .GlobalEnv)

MIN_TRAIN   <- 40      # 10 years before the first origin
ORIGIN_STEP <- 1       # quarterly origins
H_CV        <- 20
H_REPORT    <- c(4, 12, 20)
N_CORES     <- max(1, detectCores() - 2)
N_CORES

## Credit unions with too little history for CV get a peer-based fallback
## in script 12 instead; they are flagged, not silently modelled.
MIN_FOR_CV <- MIN_TRAIN + 8

## Plausibility band used to screen candidates. A model that forecasts
## outside this is not eligible to win, however well it cross-validated.
G_MAX_PA <- 0.25
G_MIN_PA <- -0.15
cv_ids <- which(n_obs_vec >= MIN_FOR_CV)
length(cv_ids); nrow(cohort) - length(cv_ids)

## ---------------------------------------------------------------------
## [11.1] Candidate grid. d = 1 on logs, so drift is constant percentage
## growth and the forecast can never go to zero or turn negative.
## ---------------------------------------------------------------------
cand_grid <- tibble::tribble(
  ~p, ~d, ~q, ~P, ~D, ~Q, ~drift, ~label,
   0,  1,  0,  0,  0,  0,  TRUE,  "ARIMA(0,1,0) w/ drift",
   0,  1,  1,  0,  0,  0,  TRUE,  "ARIMA(0,1,1) w/ drift",
   1,  1,  0,  0,  0,  0,  TRUE,  "ARIMA(1,1,0) w/ drift",
   1,  1,  1,  0,  0,  0,  TRUE,  "ARIMA(1,1,1) w/ drift",
   2,  1,  1,  0,  0,  0,  TRUE,  "ARIMA(2,1,1) w/ drift",
   1,  1,  2,  0,  0,  0,  TRUE,  "ARIMA(1,1,2) w/ drift",
   0,  1,  1,  0,  1,  1,  FALSE, "ARIMA(0,1,1)(0,1,1)[4]",
   1,  1,  1,  1,  0,  0,  TRUE,  "ARIMA(1,1,1)(1,0,0)[4] w/ drift"
)
nrow(cand_grid)

## Two non-ARIMA benchmarks compete on the same origins:
##   Simple growth  random walk with drift set to the MEDIAN quarterly log
##                  change, which one acquisition barely moves
##   Flat           last value carried forward
## Putting them in the competition rather than holding them as a fallback
## means cross-validation decides whether an ARIMA is worth having at all.
## They carry cand_id -1 and -2 so script 12 can recognise them.

## ---------------------------------------------------------------------
## [11.2] The worker. One credit union in, its CV scores out.
## Regressor columns that are constant inside a training window are dropped
## for that window -- otherwise Arima fails on a collinear xreg.
## ---------------------------------------------------------------------
cv_one <- function(s, cand_grid, MIN_TRAIN, ORIGIN_STEP, H_CV, H_REPORT,
                   G_MAX_PA, G_MIN_PA) {

  y <- s$y; X <- s$xreg; n <- s$n
  origins <- seq(MIN_TRAIN, n - 1, by = ORIGIN_STEP)
  if (length(origins) < 4) return(NULL)

  NB <- 2                                  # benchmark slots
  err <- array(NA_real_, dim = c(length(origins), H_CV, nrow(cand_grid) + NB))

  for (oi in seq_along(origins)) {
    n_tr <- origins[oi]
    h_o  <- min(H_CV, n - n_tr)
    ytr  <- ts(y[1:n_tr], frequency = 4)
    Xtr  <- X[1:n_tr, , drop = FALSE]
    Xfu  <- X[(n_tr + 1):(n_tr + h_o), , drop = FALSE]

    keep <- apply(Xtr, 2, function(z) length(unique(z)) > 1)
    Xtr2 <- if (any(keep)) Xtr[, keep, drop = FALSE] else NULL
    Xfu2 <- if (any(keep)) Xfu[, keep, drop = FALSE] else NULL

    for (k in seq_len(nrow(cand_grid))) {
      fit <- tryCatch(
        forecast::Arima(ytr,
          order    = c(cand_grid$p[k], cand_grid$d[k], cand_grid$q[k]),
          seasonal = list(order = c(cand_grid$P[k], cand_grid$D[k],
                                    cand_grid$Q[k]), period = 4),
          xreg = Xtr2, include.drift = cand_grid$drift[k], method = "ML"),
        error = function(e) NULL, warning = function(w) NULL)
      if (is.null(fit)) next

      fc <- tryCatch(as.numeric(forecast::forecast(fit, h = h_o, xreg = Xfu2)$mean),
                     error = function(e) NULL)
      if (is.null(fc) || any(!is.finite(fc))) next

      ## Error measured on the log scale: proportional, so large and small
      ## credit unions are scored comparably.
      err[oi, seq_len(h_o), k] <- y[(n_tr + 1):(n_tr + h_o)] - fc
    }

    ## Benchmarks, scored on exactly the same origins and horizons
    act <- y[(n_tr + 1):(n_tr + h_o)]
    g_s <- stats::median(diff(y[1:n_tr]))
    err[oi, seq_len(h_o), nrow(cand_grid) + 1] <- act - (y[n_tr] + g_s * seq_len(h_o))
    err[oi, seq_len(h_o), nrow(cand_grid) + 2] <- act - y[n_tr]
  }

  sc <- data.frame(
    cand_id = c(seq_len(nrow(cand_grid)), -1L, -2L),
    spec = c(cand_grid$label, "Simple growth rate (median drift)",
             "Flat (last value carried forward)"),
    stringsAsFactors = FALSE)
  sc$n_fits   <- apply(err, 3, function(m) sum(!is.na(m[, 1])))
  sc$rmse_all <- apply(err, 3, function(m) sqrt(mean(m^2, na.rm = TRUE)))
  for (h in H_REPORT)
    sc[[paste0("rmse_h", h)]] <- apply(err, 3, function(m)
      sqrt(mean(m[, h]^2, na.rm = TRUE)))

  ## -------------------------------------------------------------------
  ## Full-sample plausibility screen. Fit each candidate on everything and
  ## look at where it actually goes over 20 quarters. A candidate that runs
  ## outside the growth band is marked implausible and cannot win, however
  ## good its cross-validated error was. Script 12 walks this ranking.
  ## -------------------------------------------------------------------
  yfull <- ts(y, frequency = 4)
  keepf <- apply(X, 2, function(z) length(unique(z)) > 1)
  Xff <- if (any(keepf)) X[, keepf, drop = FALSE] else NULL
  Xnf <- NULL
  if (!is.null(Xff)) {
    lastr <- X[nrow(X), keepf, drop = FALSE]
    Xnf <- matrix(rep(lastr, each = 20), nrow = 20,
                  dimnames = list(NULL, colnames(X)[keepf]))
    for (cc in intersect(colnames(Xnf), c("rec0709", "rec2020", "rateshock")))
      Xnf[, cc] <- 0
  }

  sc$g_pa_full <- NA_real_
  for (k in seq_len(nrow(cand_grid))) {
    ff <- tryCatch(forecast::Arima(yfull,
        order = c(cand_grid$p[k], cand_grid$d[k], cand_grid$q[k]),
        seasonal = list(order = c(cand_grid$P[k], cand_grid$D[k],
                                  cand_grid$Q[k]), period = 4),
        xreg = Xff, include.drift = cand_grid$drift[k], method = "ML"),
      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(ff)) next
    fcf <- tryCatch(as.numeric(forecast::forecast(ff, h = 20, xreg = Xnf)$mean),
                    error = function(e) NULL)
    if (is.null(fcf) || any(!is.finite(fcf))) next
    sc$g_pa_full[k] <- (fcf[20] - y[n]) / 5      # annual log growth
  }
  sc$g_pa_full[sc$cand_id == -1L] <- stats::median(diff(y)) * 4
  sc$g_pa_full[sc$cand_id == -2L] <- 0

  sc$plausible <- !is.na(sc$g_pa_full) &
    sc$g_pa_full <= G_MAX_PA & sc$g_pa_full >= G_MIN_PA

  sc$eligible <- sc$n_fits >= 0.8 * length(origins) & is.finite(sc$rmse_all)
  ## Rank on error, but implausible candidates sort below plausible ones
  sc <- sc[order(-sc$eligible, -sc$plausible, sc$rmse_all), ]
  sc$rank <- seq_len(nrow(sc))
  sc$join_number <- s$join_number
  sc[1:min(6, nrow(sc)), ]      # top six, so script 12 has a ranking to walk
}

## ---------------------------------------------------------------------
## [11.3] Run it
## ---------------------------------------------------------------------
t0 <- Sys.time()

USE_PARALLEL <- TRUE

if (USE_PARALLEL) {
  cl <- makeCluster(N_CORES)
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
  clusterEvalQ(cl, library(forecast))
  clusterExport(cl, c("cand_grid", "MIN_TRAIN", "ORIGIN_STEP", "H_CV", "H_REPORT"),
                envir = environment())
  clusterExport(cl, c("G_MAX_PA", "G_MIN_PA"), envir = environment())
  cv_list <- parLapplyLB(cl, cu_series[cv_ids], function(s)
    cv_one(s, cand_grid, MIN_TRAIN, ORIGIN_STEP, H_CV, H_REPORT,
           G_MAX_PA, G_MIN_PA))
  stopCluster(cl)
} else {
  cv_list <- vector("list", length(cv_ids))
  for (j in seq_along(cv_ids)) {
    cv_list[[j]] <- cv_one(cu_series[[cv_ids[j]]], cand_grid, MIN_TRAIN,
                           ORIGIN_STEP, H_CV, H_REPORT, G_MAX_PA, G_MIN_PA)
    if (j %% 100 == 0)
      cat(sprintf("%5d/%5d  (%s elapsed)\n", j, length(cv_ids),
                  format(round(difftime(Sys.time(), t0, units = "mins"), 1))))
  }
}

difftime(Sys.time(), t0, units = "mins")

cv_scores <- bind_rows(cv_list[!sapply(cv_list, is.null)])
nrow(cv_scores)

cv_winners <- cv_scores %>% filter(rank == 1) %>%
  select(join_number, spec, cand_id, rmse_all, rmse_h4, rmse_h12, rmse_h20,
         g_pa_full, plausible)

nrow(cv_winners)
table(cv_winners$spec)

## How often did an ARIMA actually beat the simple benchmarks
cat("\nWon by simple growth rate:", sum(cv_winners$cand_id == -1), "\n")
cat("Won by flat:", sum(cv_winners$cand_id == -2), "\n")
cat("Won by an ARIMA:", sum(cv_winners$cand_id > 0), "\n")

## How much of the candidate space the plausibility screen removed
cv_scores %>% group_by(cand_id) %>%
  summarise(spec = first(spec), n = n(),
            pct_implausible = round(100 * mean(!plausible), 1)) %>%
  arrange(desc(n)) %>% as.data.frame()

## Winners whose own forecast still sits near the edge of the band
cv_winners %>%
  mutate(pa = round(100 * (exp(g_pa_full) - 1), 1)) %>%
  filter(abs(pa) > 20) %>% nrow()

## Which credit unions did not get a CV-selected model
no_cv <- setdiff(cohort$join_number, cv_winners$join_number)
length(no_cv)

## Accuracy spread -- log-scale RMSE, so 0.10 is roughly 10 percent
summary(cv_winners$rmse_h4)
summary(cv_winners$rmse_h20)

saveRDS(list(cv_scores = cv_scores, cv_winners = cv_winners, no_cv = no_cv,
             cand_grid = cand_grid, MIN_TRAIN = MIN_TRAIN, H_CV = H_CV),
        file = "cohort_cv.rds")
