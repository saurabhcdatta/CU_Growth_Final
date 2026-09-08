fc %>%
  filter(cat_k == 6) %>%
  mutate(band = cut(assets_now / 1e9, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    right = FALSE, dig.lab = 3)) %>%
  group_by(band) %>%
  summarise(n = n(),
            within_reach = sum(assets_now >= need),
            expected_crossings = round(sum(p_a7_5y), 1),
            .groups = "drop") %>%
  as.data.frame()

a7_spread %>%
  filter(h %in% c(12, 20)) %>%
  summarise(pooled_ent = round(sum(act_ent) / sum(pred_ent), 3),
            n_origins = n()) %>%
  as.data.frame()