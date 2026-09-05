for (H in c(4, 12, 20)) {
  us <- feat[[paste0("usable_surv_h", H)]]
  d <- feat[us, ]; d$dy <- d[[paste0("dy_h", H)]]
  d <- d[!is.na(d$dy) & d$cat_k == 6, ]
  cat("\nh =", H, "\n")
  print(d %>% mutate(yr = START_YEAR + (q_index - 1) %/% 4) %>%
          group_by(yr) %>%
          summarise(n = n(),
                    med_ann = round(100 * (exp(median(dy) * 4 / H) - 1), 2),
                    .groups = "drop") %>% as.data.frame())
}

delta_q <- (log(1 + recent_ann/100) - log(1 + hist_ann/100)) / 4   # per quarter
POOLS[["20"]][["6"]]$dy <- POOLS[["20"]][["6"]]$dy + delta_q * 20
POOLS[["20"]][["7"]]$dy <- POOLS[["20"]][["7"]]$dy + delta_q * 20
PROB[["20"]] <- emp_bucket_probs(POOLS[["20"]], fc$cat_k, fc$y_raw)
round(colSums(PROB[["20"]]), 1)