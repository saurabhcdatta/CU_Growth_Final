###############################################################################
# maps.R  --  Ten county maps for the Rural Credit Unions Study
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# REQUIRES  source("rq_load.R") first: county, cr, cu, sites, offices_q, QZ.
#           A Census API key in the environment:  Sys.setenv(CENSUS_API_KEY = "...")
#           Packages: sf, tigris, ggplot2, jsonlite (jsonlite ships with tigris).
#
# DESIGN RULES, applied to every map
#   * Albers via tigris::shift_geometry (AK/HI inset). NEVER pass crs= to coord_sf
#     -- shift_geometry returns ESRI:102003 and re-projecting misplaces points.
#   * Non-rural counties are always the same light grey. The story is rural.
#   * "No office" is a distinct fill, never the top or bottom bin of a ratio.
#   * Ratios are suppressed below a population floor (POP_FLOOR) so a county of
#     900 people cannot dominate a legend.
#   * Every map: title says the finding, subtitle says the measure, caption says
#     the sources and the quarter. 11 x 7 in, 300 dpi.
#
# Sequential blocks. Run one, look, move on.
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(sf); library(ggplot2); library(tigris) })
options(tigris_use_cache = TRUE)
MAP_DIR <- file.path(OUT_DIR, "maps"); dir.create(MAP_DIR, recursive = TRUE, showWarnings = FALSE)
stopifnot(exists("county"), exists("offices_q"), exists("cr"), exists("cu"))

## ---- palette and helpers ----------------------------------------------------
C_NONRURAL <- "#E6E9E9"; C_STATE <- "#5B6B72"; C_CTY <- "#FFFFFF"
C_NOOFFICE <- "#C8553D"; C_TEAL <- c("#DCE6E4", "#7FB0AE", "#1F6F78", "#0E4C55")
TH <- theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0),
        plot.subtitle = element_text(size = 10.5, colour = "#5B6B72", hjust = 0),
        plot.caption = element_text(size = 8, colour = "#5B6B72", hjust = 0),
        legend.position = c(0.90, 0.28), legend.title = element_text(size = 9), legend.text = element_text(size = 8.5),
        plot.margin = margin(8, 8, 8, 8))
sv <- function(p, name) ggsave(file.path(MAP_DIR, paste0(name, ".png")), p, width = 11, height = 7, dpi = 300, bg = "white")
CAP <- function(extra = "") paste0("Rural = county neither in nor adjacent to a metro area (12 CFR 1026.35, via USDA-ERS 2024 Urban Influence Codes). ",
                                   "Offices = headquarters and branches, ", q_lab(QZ), ". ", extra, " Preliminary.")
POP_FLOOR <- 2000

## ---- base geometry ----------------------------------------------------------
cty_sf <- counties(cb = TRUE, resolution = "20m", year = 2023) |> shift_geometry()
cty_sf$fips <- cty_sf$GEOID
st_sf  <- states(cb = TRUE, resolution = "20m", year = 2023) |> shift_geometry()
st_sf  <- st_sf[!st_sf$STUSPS %in% c("PR","GU","VI","AS","MP"), ]
cty_sf <- cty_sf[substr(cty_sf$fips, 1, 2) <= "56", ]

## county-level facts at the latest quarter
oz <- offices_q[qidx == QZ]
K <- merge(county[is_state_or_dc == TRUE], oz[, .(fips, offices, hqs, branches, cus)], by = "fips", all.x = TRUE)
K[is.na(offices), `:=`(offices = 0L, hqs = 0L, branches = 0L, cus = 0L)]
K[, has_office := offices > 0L]
K[, pop := fifelse(!is.na(pop2024), pop2024, pop2020)]
M <- merge(cty_sf, K, by = "fips", all.x = TRUE)   # sf with facts

## rural HQ points (from cr, latest quarter), placed at county centroids
hq <- cr[qidx == QZ & rural == 1L, .(cu_number, fips, assets_tot)]
cent <- st_centroid(st_geometry(cty_sf)); cent_dt <- data.table(fips = cty_sf$fips, st_coordinates(cent))
hq <- merge(hq, cent_dt, by = "fips")

base_layers <- function(fill_col) list(
  geom_sf(data = M, aes(fill = .data[[fill_col]]), colour = C_CTY, linewidth = 0.05),
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3))

## ---- MAP 1  where the rural credit unions are -------------------------------
M$m1 <- ifelse(M$rural24 == 1L, "Rural county", "Non-rural county")
p1 <- ggplot() + base_layers("m1") +
  scale_fill_manual(values = c("Rural county" = C_TEAL[1], "Non-rural county" = C_NONRURAL), name = NULL) +
  geom_point(data = hq, aes(X, Y, size = assets_tot / 1e6), colour = C_TEAL[4], alpha = 0.75, shape = 16) +
  scale_size_area(max_size = 7, breaks = c(10, 100, 500, 1000), labels = c("$10m", "$100m", "$500m", "$1bn"), name = "Assets") +
  labs(title = sprintf("%d rural credit unions, headquartered in %d of %d rural counties",
                       nrow(hq), uniqueN(hq$fips), K[rural24 == 1L, .N]),
       subtitle = "One dot per rural credit union headquarters, sized by assets. Rural counties shaded.",
       caption = CAP()) + TH
sv(p1, "map01_rural_cu_distribution")

## ---- MAP 2  rural residents per office --------------------------------------
K[, res_per_office := fifelse(rural24 == 1L & offices > 0L & pop >= POP_FLOOR, pop / offices, NA_real_)]
K[, m2 := fifelse(rural24 == 0L, "Non-rural",
          fifelse(offices == 0L, "No office",
          fifelse(pop < POP_FLOOR, "Under 2,000 residents",
                  as.character(cut(res_per_office, c(0, 2500, 5000, 10000, Inf),
                                   labels = c("Under 2,500 per office", "2,500-5,000", "5,000-10,000", "Over 10,000 per office"))))))]
M <- merge(cty_sf, K, by = "fips", all.x = TRUE)
p2 <- ggplot() + base_layers("m2") +
  scale_fill_manual(values = c("Non-rural" = C_NONRURAL, "No office" = C_NOOFFICE, "Under 2,000 residents" = "#F3F5F4",
                               "Under 2,500 per office" = C_TEAL[4], "2,500-5,000" = C_TEAL[3], "5,000-10,000" = C_TEAL[2], "Over 10,000 per office" = C_TEAL[1]),
                    name = "Residents per\ncredit union office", drop = FALSE) +
  labs(title = "How many rural residents each credit union office serves",
       subtitle = "Rural counties only. Red = no office of any kind. Counties under 2,000 residents not rated.",
       caption = CAP("Population: Census county estimates, 2024.")) + TH
sv(p2, "map02_residents_per_office")

## ---- MAP 3  the counties with no office, shaded by people -------------------
K[, m3 := fifelse(rural24 == 0L, "Non-rural", fifelse(offices > 0L, "Has an office",
          as.character(cut(pop, c(0, 5000, 15000, 30000, Inf), labels = c("Under 5,000", "5,000-15,000", "15,000-30,000", "Over 30,000")))))]
M <- merge(cty_sf, K, by = "fips", all.x = TRUE)
n_no <- K[rural24 == 1L & offices == 0L, .N]; pop_no <- K[rural24 == 1L & offices == 0L, sum(pop, na.rm = TRUE)]
p3 <- ggplot() + base_layers("m3") +
  scale_fill_manual(values = c("Non-rural" = C_NONRURAL, "Has an office" = "#F3F5F4",
                               "Under 5,000" = "#F2D3CB", "5,000-15,000" = "#E5A48F", "15,000-30,000" = "#D4785B", "Over 30,000" = "#A63E27"),
                    name = "Residents in counties\nwith NO office", drop = FALSE) +
  labs(title = sprintf("%d rural counties have no credit union office \u2014 %.1f million people", n_no, pop_no / 1e6),
       subtitle = "Darker = more people living in a county with nothing. Non-rural counties and covered rural counties in grey.",
       caption = CAP("Population: Census county estimates, 2024.")) + TH
sv(p3, "map03_no_office_by_population")

## ---- MAP 4  one merger away: single-provider counties -----------------------
K[, m4 := fifelse(rural24 == 0L, "Non-rural", fifelse(offices == 0L, "No office", fifelse(cus == 1L, "Exactly one credit union", "Two or more")))]
M <- merge(cty_sf, K, by = "fips", all.x = TRUE)
## the single provider's HQ county: is it in the county it serves?
single <- sites[qidx == QZ & !foreign & !terr & !is.na(fips)][K[rural24 == 1L & cus == 1L, .(fips)], on = "fips", nomatch = 0L]
single <- unique(single[, .(fips, cu_number)])
single[cu[, .(cu_number, fips_last)], on = "cu_number", hq_fips := i.fips_last]
single[, hq_elsewhere := hq_fips != fips]
p4 <- ggplot() + base_layers("m4") +
  scale_fill_manual(values = c("Non-rural" = C_NONRURAL, "No office" = "#F3F5F4", "Exactly one credit union" = "#E8A24A", "Two or more" = C_TEAL[2]),
                    name = NULL, drop = FALSE) +
  labs(title = sprintf("%d rural counties are served by exactly one credit union", K[rural24 == 1L & cus == 1L, .N]),
       subtitle = sprintf("In %d of them the sole provider is headquartered in another county. One merger away from none.",
                          single[hq_elsewhere == TRUE, uniqueN(fips)]),
       caption = CAP()) + TH
sv(p4, "map04_single_provider")

## ---- Census ACS pull (one call per variable, county level) ------------------
## Uses your key from the environment. If the API is blocked behind the proxy,
## download the same tables from data.census.gov as CSV and read them here.
acs_get <- function(vars, year = 2023, survey = "acs5") {
  key <- Sys.getenv("CENSUS_API_KEY"); if (!nzchar(key)) stop("Set CENSUS_API_KEY")
  url <- sprintf("https://api.census.gov/data/%d/acs/%s?get=NAME,%s&for=county:*&key=%s",
                 year, survey, paste(vars, collapse = ","), key)
  j <- jsonlite::fromJSON(url)
  d <- as.data.table(j[-1, ]); setnames(d, j[1, ])
  d[, fips := paste0(state, county)]
  for (v in vars) d[, (v) := as.numeric(get(v))]
  d[, c("fips", vars), with = FALSE]
}
acs <- acs_get(c("B28002_001E", "B28002_004E",        # households; with broadband subscription
                 "B01002_001E",                       # median age
                 "B08201_001E", "B08201_002E",        # households; with no vehicle
                 "B17001_001E", "B17001_002E"))       # poverty universe; below poverty
acs[, `:=`(broadband_pct = 100 * B28002_004E / B28002_001E, median_age = B01002_001E,
           no_vehicle_pct = 100 * B08201_002E / B08201_001E, poverty_pct = 100 * B17001_002E / B17001_001E)]
K <- merge(K, acs[, .(fips, broadband_pct, median_age, no_vehicle_pct, poverty_pct)], by = "fips", all.x = TRUE)

## bivariate helper: a Census measure (low/mid/high tercile among RURAL counties) x office presence
biv <- function(var, lab, name, title, low_is_bad = TRUE) {
  q <- K[rural24 == 1L & !is.na(get(var)), quantile(get(var), c(1/3, 2/3), na.rm = TRUE)]
  K[, tier := fifelse(get(var) <= q[1], "low", fifelse(get(var) <= q[2], "mid", "high"))]
  if (!low_is_bad) K[, tier := fifelse(tier == "low", "high", fifelse(tier == "high", "low", tier))]  # so "low" always = worse
  K[, cell := fifelse(rural24 == 0L, "Non-rural", paste(fifelse(has_office, "office", "no office"), tier, sep = " | "))]
  M <- merge(cty_sf, K, by = "fips", all.x = TRUE)
  pal <- c("Non-rural" = C_NONRURAL,
           "office | high" = "#DCE6E4", "office | mid" = "#9FC3C1", "office | low" = "#5E9C99",
           "no office | high" = "#F2D3CB", "no office | mid" = "#E08C72", "no office | low" = "#A63E27")
  n_worst <- K[cell == "no office | low", .N]; pop_worst <- K[cell == "no office | low", sum(pop, na.rm = TRUE)]
  p <- ggplot() + geom_sf(data = M, aes(fill = cell), colour = C_CTY, linewidth = 0.05) +
    geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
    scale_fill_manual(values = pal, name = paste0("Office presence |\n", lab), breaks = names(pal)[-1], drop = FALSE) +
    labs(title = sprintf(title, n_worst, pop_worst / 1e6),
         subtitle = sprintf("Rural counties in terciles of %s (among rural counties), crossed with whether any credit union office exists. Dark red = worst on both.", tolower(lab)),
         caption = CAP("Census ACS 2019-2023 five-year estimates.")) + TH
  sv(p, name); p
}

## ---- MAP 5  broadband x offices ---------------------------------------------
p5 <- biv("broadband_pct", "Broadband subscription", "map05_broadband_x_office",
          "%d rural counties have no credit union office AND the lowest broadband \u2014 %.1f million people for whom online banking is not the answer")

## ---- MAP 6  age x offices ---------------------------------------------------
p6 <- biv("median_age", "Median age", "map06_age_x_office",
          "%d rural counties are among the oldest AND have no credit union office \u2014 %.1f million people", low_is_bad = FALSE)

## ---- MAP 7  no vehicle x offices --------------------------------------------
p7 <- biv("no_vehicle_pct", "Households without a vehicle", "map07_no_vehicle_x_office",
          "%d rural counties have no office AND the most households without a car \u2014 %.1f million people", low_is_bad = FALSE)

## ---- MAP 8  poverty x offices -----------------------------------------------
p8 <- biv("poverty_pct", "Poverty rate", "map08_poverty_x_office",
          "%d rural counties have no office AND the highest poverty \u2014 %.1f million people", low_is_bad = FALSE)

## ---- MAP 9  farming-dependent counties and credit union presence ------------
## USDA ERS County Typology Codes (2015 edition, farming_2015_update flag).
typ_file <- dl("https://www.ers.usda.gov/media/10763/2015countytypologycodes.csv",
               file.path(CACHE_DIR, "2015countytypologycodes.csv"))
typ <- fread(typ_file, colClasses = "character")
setnames(typ, tolower(gsub("[^A-Za-z0-9]+", "_", names(typ))))
fcol <- grep("^farming", names(typ), value = TRUE)[1]; fips_col <- grep("fip", names(typ), value = TRUE)[1]
typ <- typ[, .(fips = sprintf("%05d", as.integer(get(fips_col))), farming = as.integer(get(fcol)) == 1L)]
K <- merge(K, typ, by = "fips", all.x = TRUE)
K[, m9 := fifelse(rural24 == 0L, "Non-rural",
          fifelse(farming %in% TRUE & !has_office, "Farming-dependent, no office",
          fifelse(farming %in% TRUE, "Farming-dependent, has office",
          fifelse(!has_office, "Other rural, no office", "Other rural, has office"))))]
M <- merge(cty_sf, K, by = "fips", all.x = TRUE)
p9 <- ggplot() + base_layers("m9") +
  scale_fill_manual(values = c("Non-rural" = C_NONRURAL, "Farming-dependent, no office" = "#A63E27", "Farming-dependent, has office" = "#0E4C55",
                               "Other rural, no office" = "#F2D3CB", "Other rural, has office" = "#DCE6E4"), name = NULL, drop = FALSE) +
  labs(title = sprintf("Of %d farming-dependent rural counties, %d have no credit union office",
                       K[rural24 == 1L & farming %in% TRUE, .N], K[rural24 == 1L & farming %in% TRUE & !has_office, .N]),
       subtitle = "USDA ERS county typology: farming accounts for a large share of county earnings or employment.",
       caption = CAP("USDA ERS County Typology Codes, 2015 edition.")) + TH
sv(p9, "map09_farming_counties_x_office")

## ---- MAP 10  reclassification 2013 -> 2024 (Q12) ----------------------------
K[, m10 := fifelse(is.na(rural13), "No 2013 code",
           fifelse(rural13 == 1L & rural24 == 0L, "Was rural, now not",
           fifelse(rural13 == 0L & rural24 == 1L, "Became rural",
           fifelse(rural24 == 1L, "Rural both", "Non-rural both"))))]
M <- merge(cty_sf, K, by = "fips", all.x = TRUE)
caught <- cu[cu_number %in% cr[qidx == QZ, unique(cu_number)]][
  K[m10 == "Was rural, now not", .(fips)], on = .(fips_last = fips), nomatch = 0L]
caught <- merge(caught[, .(cu_number, fips = fips_last)], cent_dt, by = "fips")
p10 <- ggplot() + base_layers("m10") +
  scale_fill_manual(values = c("Non-rural both" = C_NONRURAL, "Rural both" = C_TEAL[1], "Was rural, now not" = "#C8553D",
                               "Became rural" = "#1F6F78", "No 2013 code" = "#FFFFFF"), name = NULL, drop = FALSE) +
  geom_point(data = caught, aes(X, Y), colour = "#12202E", size = 1.6, shape = 21, fill = "#E8A24A") +
  labs(title = sprintf("%d counties stopped being rural between the 2013 and 2024 classifications; %d became rural",
                       K[m10 == "Was rural, now not", .N], K[m10 == "Became rural", .N]),
       subtitle = sprintf("Dots: the %d credit unions headquartered in counties that lost rural status \u2014 rural in 2013 terms, not today.", nrow(caught)),
       caption = CAP("Vintage comparison: ERS 2013 vs 2024 Urban Influence Codes.")) + TH
sv(p10, "map10_reclassification_2013_2024")

cat("\nTen maps written to", MAP_DIR, "\n")
list.files(MAP_DIR, "^map[0-9]{2}")
