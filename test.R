h <- 20
us  <- feat[[paste0("usable_surv_h", h)]]
d20 <- feat[us, ]; d20$dy <- d20[[paste0("dy_h", h)]]
d20 <- d20[!is.na(d20$dy), ]
d20$yr <- START_YEAR + (d20$q_index - 1) %/% 4

us12 <- feat[["usable_surv_h12"]]
d12 <- feat[us12, ]; d12$dy <- d12[["dy_h12"]]
d12 <- d12[!is.na(d12$dy), ]
d12$yr <- START_YEAR + (d12$q_index - 1) %/% 4

ann <- function(x, H) 100 * (exp(median(x) * 4 / H) - 1)

pooled_ann   <- ann(d20$dy[d20$cat_k == 6], 20)
presurge_ann <- ann(d20$dy[d20$cat_k == 6 & d20$yr <= 2014], 20)
recent_ann   <- ann(d12$dy[d12$cat_k == 6 & d12$yr >= 2022], 12)

cat(sprintf("A6 median annual growth\n  pooled (in use) %.2f%%\n  pre-2015 origins %.2f%%\n  post-surge (h=12) %.2f%%\n",
            pooled_ann, presurge_ann, recent_ann))

## ---- variant A: learn A6 growth from pre-2015 origins only -----------
P_A <- POOLS[["20"]]
P_A[["6"]] <- make_pool(d20$dy[d20$cat_k == 6 & d20$yr <= 2014])
cnt_A <- colSums(emp_bucket_probs(P_A, fc$cat_k, fc$y_raw))

## ---- variant B: recentre A6 on post-surge growth ---------------------
delta  <- (log(1 + recent_ann/100) - log(1 + pooled_ann/100)) / 4
P_B <- POOLS[["20"]]
P_B[["6"]]$dy <- P_B[["6"]]$dy + delta * 20
cnt_B <- colSums(emp_bucket_probs(P_B, fc$cat_k, fc$y_raw))

data.frame(cat = CAT_LABELS,
           now = counts$now,
           baseline  = round(counts$h20, 1),
           pre2015   = round(cnt_A, 1),
           postsurge = round(cnt_B, 1))