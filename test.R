hist %>% filter(q_index %in% c(N_Q - 20, N_Q)) %>%
  mutate(cat = cut(assets_tot, BREAKS, CAT_LABELS, right = FALSE)) %>%
  select(join_number, q_index, cat) %>%
  pivot_wider(names_from = q_index, values_from = cat) %>%
  setNames(c("jn", "then", "now")) %>%
  filter(!is.na(then), !is.na(now)) %>%
  summarise(up = sum(as.integer(now) > as.integer(then)),
            down = sum(as.integer(now) < as.integer(then)),
            same = sum(now == then))