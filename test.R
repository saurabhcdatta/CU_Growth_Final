## Assets needed today to reach $10B in five years at A6 median growth
a6_med <- pool_q(POOLS[["20"]][["6"]], 0.50)
need   <- 10e9 / exp(a6_med)
cat(sprintf("A6 median growth %.1f%%/yr over 5y; need $%.2fB today\n",
            100 * (exp(a6_med / 5) - 1), need / 1e9))

## A6 institutions by $1B band, with their model-implied crossing mass
fc %>%
  filter(cat_k == 6) %>%
  mutate(band = cut(assets_now / 1e9, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    right = FALSE, dig.lab = 3),
         p_a7 = PROB[["20"]][cbind(seq_len(nrow(fc)), N_CAT)][cat_k == 6]) %>%
  group_by(band) %>%
  summarise(n = n(),
            within_reach = sum(assets_now >= need),
            expected_crossings = round(sum(p_a7), 1),
            .groups = "drop") %>%
  as.data.frame()

## $10B+ count at each June, 2011 to 2026, and the five-year change
panel %>%
  filter(cat_k == N_CAT, quarter == 2, year >= 2011) %>%
  count(year, name = "n_A7") %>%
  mutate(change_5y = n_A7 - lag(n_A7, 5)) %>%
  as.data.frame()

## Same for $15B+, since that line is on the Total tab too
panel %>%
  filter(assets_tot >= 15e9, quarter == 2, year >= 2011) %>%
  count(year, name = "n_15B") %>%
  as.data.frame()