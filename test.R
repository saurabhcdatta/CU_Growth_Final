pipe_conv <- function(y0, lo = 8e9, hi = 10e9, line = 10e9, h = 5) {
  start <- panel %>%
    filter(year == y0, quarter == 2, assets_tot >= lo, assets_tot < hi) %>%
    select(join_number, a0 = assets_tot)
  end <- panel %>%
    filter(year == y0 + h, quarter == 2) %>%
    select(join_number, a1 = assets_tot)
  d <- left_join(start, end, by = "join_number")
  data.frame(origin = y0, pipeline = nrow(d),
             still_present = sum(!is.na(d$a1)),
             crossed = sum(d$a1 >= line, na.rm = TRUE),
             conversion = round(mean(d$a1 >= line, na.rm = TRUE), 2))
}
bind_rows(lapply(c(2011, 2013, 2014, 2016, 2019, 2021), pipe_conv))