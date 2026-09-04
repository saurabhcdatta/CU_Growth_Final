## =====================================================================
## 23b_rerun_pools.R  --  Rebuild [23.3] onward after changing MIN_POOL
##
## The previous run left POOLS untouched, so [23.4] recomputed from the
## old pools and the counts came back byte-identical. This rebuilds the
## pools and every object that depends on them, in order, in one paste.
##
## Requires the 23 session (fc, feat, POOLS helpers) to be live.
## =====================================================================

MIN_POOL <- 60          # was 150; see the note in [23.1]

stopifnot(exists("fc"), exists("build_pools"), exists("emp_bucket_probs"))

## ---- rebuild pools ---------------------------------------------------
POOLS <- lapply(H_SET, build_pools)
names(POOLS) <- as.character(H_SET)

## THE CHECK. Any category still showing "window" is being forecast with
## its neighbours' growth distribution rather than its own.
for (h in H_SET) {
  pl <- POOLS[[as.character(h)]]
  cat("\nh =", h, "  training rows:", attr(pl, "n_origins"), "\n")
  print(data.frame(
    cat   = CAT_LABELS,
    src   = attr(pl, "source"),
    n     = sapply(pl, function(p) p$n),
    p_neg = round(100 * sapply(pl, function(p) mean(p$dy < 0)), 1),
    p10   = round(100 * (exp(sapply(pl, pool_q, 0.10) * 4 / h) - 1), 1),
    med   = round(100 * (exp(sapply(pl, pool_q, 0.50) * 4 / h) - 1), 1),
    p90   = round(100 * (exp(sapply(pl, pool_q, 0.90) * 4 / h) - 1), 1)))
}

## ---- probabilities and guardrails ------------------------------------
PROB <- lapply(H_SET, function(h)
  emp_bucket_probs(POOLS[[as.character(h)]], fc$cat_k, fc$y_raw))
names(PROB) <- as.character(H_SET)

for (h in H_SET) {
  P <- PROB[[as.character(h)]]
  stopifnot(all(abs(rowSums(P) - 1) < 1e-9),
            abs(sum(P) - nrow(fc)) < 1e-6)
  cat(sprintf("h=%2d  total %.4f\n", h, sum(P)))
}

## ---- institution level ----------------------------------------------
inst <- fc %>% select(join_number, cu_name, region, cu_type, state,
                      assets_now, cat_k, short_history) %>%
  mutate(asset_cat_now = CAT_LABELS[cat_k])

for (h in H_SET) {
  P  <- PROB[[as.character(h)]]
  pl <- POOLS[[as.character(h)]]
  qk <- function(p) vapply(seq_len(N_CAT),
                           function(k) pool_q(pl[[as.character(k)]], p), 0)

  inst[[paste0("assets_med_h", h)]] <- exp(fc$y_raw + qk(0.50)[fc$cat_k])
  inst[[paste0("assets_p10_h", h)]] <- exp(fc$y_raw + qk(0.10)[fc$cat_k])
  inst[[paste0("assets_p90_h", h)]] <- exp(fc$y_raw + qk(0.90)[fc$cat_k])

  inst[[paste0("p_down_h", h)]] <-
    rowSums(P * outer(fc$cat_k, seq_len(N_CAT), ">"))
  inst[[paste0("p_same_h", h)]] <- P[cbind(seq_len(nrow(P)), fc$cat_k)]
  inst[[paste0("p_up_h", h)]]   <-
    rowSums(P * outer(fc$cat_k, seq_len(N_CAT), "<"))

  inst[[paste0("k_modal_h", h)]]   <- max.col(P)
  inst[[paste0("p_modal_h", h)]]   <- apply(P, 1, max)
  inst[[paste0("cat_modal_h", h)]] <- CAT_LABELS[max.col(P)]
  inst[[paste0("conf_h", h)]] <- cut(
    apply(P, 1, max), c(0, 0.6, 0.8, 1),
    labels = c("weak", "moderate", "strong"), include.lowest = TRUE)
}

## ---- counts, transitions --------------------------------------------
counts <- data.frame(cat = CAT_LABELS, pretty = CAT_PRETTY[CAT_LABELS],
                     now = as.numeric(table(factor(fc$cat_k,
                                                   levels = 1:N_CAT))))
for (h in H_SET)
  counts[[paste0("h", h)]] <- round(colSums(PROB[[as.character(h)]]), 1)

TRANS <- lapply(H_SET, trans); names(TRANS) <- as.character(H_SET)

## ---- real-dollar sensitivity ----------------------------------------
real_counts <- lapply(H_SET, function(h) {
  infl <- (1 + CPI_ASSUMPTION) ^ (h / 4)
  LE   <- LOG_EDGE + log(infl)
  LE[1] <- -Inf; LE[N_CAT + 1] <- Inf
  old_edge <- LOG_EDGE
  assign("LOG_EDGE", LE, envir = .GlobalEnv)
  P <- emp_bucket_probs(POOLS[[as.character(h)]], fc$cat_k, fc$y_raw)
  assign("LOG_EDGE", old_edge, envir = .GlobalEnv)
  colSums(P)
})
names(real_counts) <- paste0("real_h", H_SET)

nominal_vs_real <- data.frame(
  cat = CAT_LABELS, now = counts$now,
  nominal_h20 = round(counts$h20, 1),
  real_h20    = round(real_counts$real_h20, 1)) %>%
  mutate(nominal_chg = round(nominal_h20 - now, 1),
         real_chg    = round(real_h20 - now, 1),
         of_which_nominal = round(nominal_h20 - real_h20, 1))

## ---- the three things to read ---------------------------------------
cat("\n--- counts ---\n")
print(counts %>% transmute(cat, now, h4, h12, h20,
                           chg = round(h20 - now, 1),
                           pct = round(100 * (h20 - now) / pmax(now, 1), 1)) %>%
        as.data.frame())

cat("\n--- nominal vs real ---\n")
print(as.data.frame(nominal_vs_real))

cat("\n--- direction ---\n")
for (h in H_SET)
  cat(sprintf("h=%2d  E[down] %6.1f  E[same] %7.1f  E[up] %6.1f  ratio %5.1f:1\n",
              h, sum(inst[[paste0("p_down_h", h)]]),
              sum(inst[[paste0("p_same_h", h)]]),
              sum(inst[[paste0("p_up_h", h)]]),
              sum(inst[[paste0("p_up_h", h)]]) /
                pmax(sum(inst[[paste0("p_down_h", h)]]), 1e-9)))

cat("\n--- A7 ---\n")
print(inst %>% filter(cat_k == 7) %>%
        transmute(cu_name, assets_b = round(assets_now / 1e9, 2),
                  p_down_h20 = round(p_down_h20, 3),
                  p_same_h20 = round(p_same_h20, 3)) %>% as.data.frame())

saveRDS(list(fc = fc, inst = inst, PROB = PROB, POOLS = POOLS,
             TRANS = TRANS, counts = counts,
             nominal_vs_real = nominal_vs_real, real_counts = real_counts,
             CPI_ASSUMPTION = CPI_ASSUMPTION, MIN_POOL = MIN_POOL,
             SPEC = SPEC, WEIGHTED = WEIGHTED, SCENARIO = SCENARIO,
             BUCKET_CALIB = BUCKET_CALIB, P_FLOOR = P_FLOOR,
             make_pool = make_pool, pool_cdf = pool_cdf, pool_q = pool_q,
             build_pools = build_pools,
             emp_bucket_probs = emp_bucket_probs, rake = rake),
        file = "panel_probs.rds")
