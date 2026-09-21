## =====================================================================
## 31_peer_groups.R  --  Unsupervised peer groups and atypicality,
##                       feeding the exit-hazard candidates in 30
##
## WHAT THIS ADDS
##   Two things the exit model cannot get from the raw features:
##
##   PEER GROUPS  k-means on size and trajectory (log assets, trailing
##                growth, volatility, acquisition history, history length).
##                Institutions in a cluster resemble each other in more
##                than size, so an empirical exit rate per cluster is a
##                sharper "category rate": calibrated by construction, on
##                peers that share a trajectory. 30 gets it as candidate
##                "peer" (and "peer_env"), and as cluster dummies inside
##                the logit ("logit_cl").
##
##   ATYPICALITY  Mahalanobis distance of an institution from the centre
##                of its size band on the same features. Institutions
##                unlike their size-peers exit more, in both directions.
##                30 gets it as a covariate "atyp".
##
## LEAKAGE
##   Clustering never sees the outcome, but it does see the feature
##   distribution. To keep the backtest honest, 30 refits the clusters
##   INSIDE each training fold using the functions defined here; what is
##   fit on the full history below is for interpretation only.
##
## BASE R THROUGHOUT: kmeans, prcomp, mahalanobis, scale.
## RUN AFTER 22 (needs feat). Seconds to a minute. Then run 30.
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
library(dplyr); library(tidyr)

if (!exists("feat")) { fts <- readRDS("panel_features.rds"); list2env(fts, .GlobalEnv) }
stopifnot(exists("feat"), exists("CAT_LABELS"), exists("N_CAT"), exists("N_Q"))
SCRIPT31_VERSION <- "2026-09-21a"
cat("31_peer_groups.R version", SCRIPT31_VERSION, "\n")

## ---------------------------------------------------------------------
## [31.1] Settings
## ---------------------------------------------------------------------
## Features that describe size and trajectory. All exist in feat from 21.
## Financial ratios from 30 (FIN_VARS) are added if they are present.
CL_VARS  <- cfg_get("CL_VARS", c("y", "g12", "g20", "vol", "hist_len", "acq_cum"))
CL_VARS  <- CL_VARS[CL_VARS %in% names(feat)]
CL_K     <- cfg_get("CL_K", 8L)          # number of peer groups; [31.3] shows the elbow
CL_NSTART <- cfg_get("CL_NSTART", 5L)
CL_MIN_N <- cfg_get("CL_MIN_N", 300L)    # a cluster smaller than this borrows its rate
set.seed(cfg_get("CL_SEED", 20260920L))

fin_extra <- intersect(c("nw_ratio", "roa", "delinq", "mem_chg8", "loans_shares"), names(feat))
if (length(fin_extra)) { CL_VARS <- union(CL_VARS, fin_extra); cat("Including financials:", paste(fin_extra, collapse = ", "), "\n") }
cat("Clustering on:", paste(CL_VARS, collapse = ", "), "\n")

## ---------------------------------------------------------------------
## [31.2] Fitting functions -- these are what 30 calls inside each fold
## ---------------------------------------------------------------------
## Standardise with training statistics, winsorise at the 1st/99th
## percentile so a handful of extreme growth rates do not own a cluster,
## then k-means. Returns everything needed to assign new rows.
cl_prep <- function(D, vars = CL_VARS, stats = NULL) {
  X <- as.data.frame(D[, vars, drop = FALSE])
  for (v in vars) X[[v]] <- as.numeric(X[[v]])
  if (is.null(stats)) {
    stats <- lapply(vars, function(v) {
      x <- X[[v]]; q <- quantile(x, c(.01, .99), na.rm = TRUE)
      x <- pmin(pmax(x, q[1]), q[2])
      list(lo = q[1], hi = q[2], mu = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
    })
    names(stats) <- vars
  }
  for (v in vars) {
    s <- stats[[v]]
    x <- pmin(pmax(X[[v]], s$lo), s$hi)
    x[is.na(x)] <- s$mu
    X[[v]] <- (x - s$mu) / ifelse(s$sd > 0, s$sd, 1)
  }
  list(X = as.matrix(X), stats = stats)
}

cl_fit <- function(D, k = CL_K, vars = CL_VARS) {
  pr <- cl_prep(D, vars)
  km <- kmeans(pr$X, centers = k, nstart = CL_NSTART, iter.max = 100,
               algorithm = "Lloyd")
  list(centers = km$centers, stats = pr$stats, vars = vars, k = k,
       size = km$size, withinss = km$tot.withinss)
}

cl_assign <- function(D, fit) {
  X <- cl_prep(D, fit$vars, fit$stats)$X
  ## nearest centre
  d2 <- sapply(seq_len(nrow(fit$centers)), function(j)
    rowSums((X - matrix(fit$centers[j, ], nrow(X), ncol(X), byrow = TRUE))^2))
  if (is.null(dim(d2))) d2 <- matrix(d2, nrow = 1)
  max.col(-d2)
}

## Atypicality: Mahalanobis distance from the centre of the institution's
## own size band, in the same standardised feature space. Fit statistics
## per band on training data; distance computed for any rows.
atyp_fit <- function(D, vars = CL_VARS) {
  pr <- cl_prep(D, vars)
  bands <- sort(unique(D$cat_k))
  per <- lapply(bands, function(k) {
    Xi <- pr$X[D$cat_k == k, , drop = FALSE]
    if (nrow(Xi) < 50) return(NULL)
    S <- cov(Xi); S <- S + diag(1e-6, ncol(S))
    list(mu = colMeans(Xi), Sinv = solve(S))
  })
  names(per) <- bands
  list(per = per, stats = pr$stats, vars = vars)
}
atyp_score <- function(D, fit) {
  X <- cl_prep(D, fit$vars, fit$stats)$X
  out <- rep(NA_real_, nrow(D))
  for (k in names(fit$per)) {
    if (is.null(fit$per[[k]])) next
    i <- which(D$cat_k == as.integer(k))
    if (length(i)) out[i] <- sqrt(mahalanobis(X[i, , drop = FALSE], fit$per[[k]]$mu,
                                              fit$per[[k]]$Sinv, inverted = TRUE))
  }
  out[is.na(out)] <- median(out, na.rm = TRUE)
  log1p(out)     # long right tail; log keeps the logit well-behaved
}

## ---------------------------------------------------------------------
## [31.3] How many peer groups? Elbow on the full history (interpretation)
## ---------------------------------------------------------------------
## Sub-sample for speed; the elbow is stable well below the full panel.
samp <- feat[sample(nrow(feat), min(150000L, nrow(feat))), ]
pr_s <- cl_prep(samp)
elbow <- sapply(2:12, function(k) kmeans(pr_s$X, k, nstart = 3, iter.max = 100,
                                          algorithm = "Lloyd")$tot.withinss)
elbow_tbl <- data.frame(k = 2:12, within_ss = round(elbow),
                        drop_pct = round(100 * c(NA, -diff(elbow) / head(elbow, -1)), 1))
cat("\nWithin-cluster sum of squares by k (look for where the drop flattens):\n")
print(elbow_tbl, row.names = FALSE)
cat("CL_K is", CL_K, "-- change it in the config if the elbow says otherwise.\n")

## ---------------------------------------------------------------------
## [31.4] The peer groups on the full history -- what they are
## ---------------------------------------------------------------------
CL_FULL <- cl_fit(feat)
feat$peer <- cl_assign(feat, CL_FULL)

## Describe each cluster in the units a reader knows, and its realised
## exit rate at each horizon. This table is the interpretation exhibit.
## feat's features are STANDARDISED (21's apply_scale): y, g12, g20 and vol
## are z-scores. Fine for clustering, wrong for description. Unscale with
## the statistics 21/22 saved; fall back to y_raw for assets.
unscale <- function(x, v) {
  if (exists("SCALE_MU") && exists("SCALE_SD") && v %in% names(SCALE_MU))
    x * SCALE_SD[[v]] + SCALE_MU[[v]] else x
}
feat_desc <- feat %>%
  mutate(assets_raw = if ("y_raw" %in% names(feat)) exp(y_raw) else exp(unscale(y, "y")),
         g12_raw = unscale(g12, "g12"), g20_raw = unscale(g20, "g20"),
         vol_raw = unscale(vol, "vol"))
if (!("y_raw" %in% names(feat)) && !exists("SCALE_MU"))
  cat("NOTE: neither y_raw nor SCALE_MU found -- growth and assets below are in z-score units.\n")

desc <- feat_desc %>%
  group_by(peer) %>%
  summarise(n = n(),
            median_assets_M = round(median(assets_raw) / 1e6, 1),
            growth_3y_pct = round(100 * (exp(median(g12_raw, na.rm = TRUE)) - 1), 1),
            growth_5y_pct = round(100 * (exp(median(g20_raw, na.rm = TRUE)) - 1), 1),
            volatility = round(median(vol_raw, na.rm = TRUE), 3),
            acquisitions = round(mean(acq_cum > 0), 2),
            exit_1y_pct = round(100 * mean(exit_h4[usable_h4]), 2),
            exit_5y_pct = round(100 * mean(exit_h20[usable_h20]), 1),
            .groups = "drop") %>%
  arrange(median_assets_M)
cat("\nPeer groups on the full history (interpretation only -- 30 refits per fold):\n")
print(as.data.frame(desc), row.names = FALSE)

## Cross-tab against the categories: do the clusters cut across bands?
xt <- table(feat$peer, CAT_LABELS[feat$cat_k])
cat("\nPeer group x asset category (share of each cluster, %):\n")
print(round(100 * prop.table(xt, 1)))

## Name the clusters from the description, for the field. Edit by hand
## once the table above has been read; the labels are only used in text.
peer_labels <- setNames(paste0("Peer ", seq_len(CL_K)), seq_len(CL_K))
## Cluster numbers change between fits (k-means labels are arbitrary), so
## name groups by what they are, from the description table, not by
## number. This helper does it from the realised exit rate and growth.
name_peers <- function(d) {
  lab <- character(nrow(d))
  for (i in seq_len(nrow(d))) {
    lab[i] <- paste(
      if (d$median_assets_M[i] < 10) "small" else if (d$median_assets_M[i] < 100) "mid" else "large",
      if (is.finite(d$growth_5y_pct[i]) && d$growth_5y_pct[i] < -10) "shrinking"
      else if (is.finite(d$growth_5y_pct[i]) && d$growth_5y_pct[i] > 30) "growing" else "flat",
      if (d$acquisitions[i] >= 0.5) "acquirer" else "",
      if (d$volatility[i] > quantile(d$volatility, .75, na.rm = TRUE)) "volatile" else "")
  }
  setNames(trimws(gsub("  +", " ", lab)), d$peer)
}
peer_labels <- name_peers(desc)
cat("\nProvisional names:\n"); print(peer_labels)

## ---------------------------------------------------------------------
## [31.5] Atypicality on the full history -- what it predicts
## ---------------------------------------------------------------------
AT_FULL <- atyp_fit(feat)
feat$atyp <- atyp_score(feat, AT_FULL)
atyp_tbl <- feat %>%
  filter(usable_h20) %>%
  mutate(atyp_q = cut(atyp, quantile(atyp, seq(0, 1, .2)), include.lowest = TRUE,
                      labels = c("typical", "2", "3", "4", "most unusual"))) %>%
  group_by(atyp_q) %>%
  summarise(n = n(), exit_5y_pct = round(100 * mean(exit_h20), 1), .groups = "drop")
cat("\nFive-year exit rate by atypicality quintile (unusual for their size):\n")
print(as.data.frame(atyp_tbl), row.names = FALSE)
cat("A rising pattern here is the signal 30 will test.\n")

## ---------------------------------------------------------------------
## [31.6] Save. 30 uses the functions (cl_fit, cl_assign, atyp_fit,
## atyp_score) to refit inside each fold; the full-history objects are
## for interpretation and for scoring the current cohort.
## ---------------------------------------------------------------------
saveRDS(list(CL_VARS = CL_VARS, CL_K = CL_K, CL_MIN_N = CL_MIN_N,
             CL_FULL = CL_FULL, AT_FULL = AT_FULL, desc = desc, elbow_tbl = elbow_tbl,
             atyp_tbl = atyp_tbl, peer_labels = peer_labels,
             cl_prep = cl_prep, cl_fit = cl_fit, cl_assign = cl_assign,
             atyp_fit = atyp_fit, atyp_score = atyp_score,
             SCRIPT31_VERSION = SCRIPT31_VERSION),
        file = "panel_peers.rds")
PEERS_READY <- TRUE
cat("\nSaved panel_peers.rds. Run 30: it will pick up 'peer', 'peer_env', 'logit_cl' and 'atyp'.\n")
