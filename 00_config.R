## =====================================================================
## 00_config.R -- Credit Union Growth Forecast, probability track
##
## THE ONE FILE TO EDIT FOR A QUARTERLY REFRESH.
##
## Every numbered script (20-27) sources this file if it has not been
## loaded yet and reads its settings through cfg_get(). A setting missing
## from CFG falls back to the default written in the script, so the
## scripts still run block by block with no config present.
##
## Lives in the project root next to 0_xlsx_helpers.R. The scripts run
## with the working directory set to DATA_DIR below.
## =====================================================================

CFG <- list(

  ## ---- paths -------------------------------------------------------
  DATA_DIR = "S:/Projects/Credit_Union_Growth_Forecast/Data",
  DTA      = "S:/Data/OCE Data/oce do file archive/2026/2026q2/processing datasets/oce current/OCE_combined_2026q2_2000tocurrent.dta",

  ## ---- cohort ------------------------------------------------------
  START_YEAR = 2005,          # first panel year used for estimation
  END_Y      = 2026,          # cohort quarter = last quarter of the panel
  END_Q      = 2,
  REGIONS    = c(1, 2, 3, 8), # 8 = ONES; a supervisory office, not a place
  CLOSED_COHORT = TRUE,       # TRUE: total held fixed, no exits modelled
  MERGER_COL = "acquiredcu_ct",

  ## ---- horizons ----------------------------------------------------
  H_SET = c(4, 12, 20),       # quarters: 1, 3, 5 years

  ## ---- method (settled by cross-validation, script 22) -------------
  SPEC         = "emp_cell",  # empirical CDF within own asset category
  GROWTH_BASIS = "full",      # "full" | "pre2015" | "post_surge"
  PRICE_BASIS  = "nominal",   # "nominal" | "real_approx" | "real_exact"
  SCENARIO     = "baseline",  # "baseline" | "shock" | "calm"
  WEIGHTED     = FALSE,       # recency weighting -- not cross-validated
  MIN_POOL     = 20,
  THIN_POOL    = 100,
  CPI_ASSUMPTION = 0.025,     # forward CPI for the real-terms restatement

  ## ---- $10B correction (script 23 [23.6b]; factors from 25 [25.8b]) -
  A7_ONLY_CALIB   = TRUE,
  A7_FACTOR_SCOPE = "entrants",   # scale crossings only; incumbents untouched
  A7_FACTORS      = c("4" = 0.496, "12" = 0.496, "20" = 0.496),
  BUCKET_CALIB    = FALSE,        # full-matrix raking -- never use
  EXTRA_THRESHOLDS = c(15e9, 20e9),

  ## ---- how an institution's category is labelled (script 24) --------
  ## "median" = the band its median forecast falls in (label and number
  ## always agree; the $10B correction does not reach the counts).
  ## "counts" = the ranking cut that reproduces the probability sums.
  ASSIGN_BASIS = "median",

  ## ---- merger adjustment (script 26) --------------------------------
  EXIT_BASIS    = "recent", # used only when EXIT_MODEL = "cat"; size_env replaces 26's rates entirely
  EXIT_RECENT_Q = 60L,      # 15 years of origins
  MIN_EXIT_POOL = 200L,     # thin category borrows the rate from the one below
  EXIT_MODEL    = "cat_env2", # verdict of 30 [30.6], 21 Sep 2026: level 1.01, allocation 10.8% (5yr CV) # "cat" | "cat_env" | "size" | "size_env" | "logit" | "cat_logit" | "size_logit" | "peer" | "peer_env" | "logit_cl" | "logit_fin" | "tree" -- decided by 30 [30.6]
  EXIT_ENV_WINDOW_Q = 8L,   # merger-environment window for cat_env
  FIN_VARS      = NULL,
  PUBLISH_WATCHLIST = FALSE,  # 32: named consolidation-risk list -- leave FALSE unless leadership approves
  CL_K          = 8L,       # peer groups in 31; see the elbow table [31.3]
  CL_VARS       = c("y", "g12", "g20", "vol", "hist_len", "acq_cum"),     # e.g. c(nw_ratio = "networth_ratio", roa = "roa", members = "members"); see 30 [30.2]

  ## ---- diagnostics that do NOT run on a refresh ---------------------
  RUN_DR   = FALSE,   # distribution-regression candidates in 22 (slow; settled)
  FIT_EXIT = FALSE,   # exit hazard in 22/23 (never run; see open items)
  A7_ORIGIN_STEP = 2, # 25 [25.8b]: quarters between backtest origins
  A7_N_ORIGIN    = 6, #             origins per horizon

  ## ---- workbook ----------------------------------------------------
  DOWN_N   = 30,      # down-risk list length
  DOWN_MIN = 0.02,    # do not name below this probability
  CT_LAB   = c("1" = "FCU", "2" = "FISCU"),
  REG_LAB  = c("1" = "Region 1", "2" = "Region 2", "3" = "Region 3",
               "8" = "ONES")
)

## Read a setting, falling back to the script's own default when the
## config does not supply it.
cfg_get <- function(name, default) {
  if (exists("CFG", inherits = TRUE) && !is.null(CFG[[name]])) CFG[[name]]
  else default
}

CONFIG_LOADED <- TRUE
cat("00_config.R loaded: cohort", CFG$END_Y, "Q", CFG$END_Q,
    "| regions", paste(CFG$REGIONS, collapse = ","),
    "| A7 factors", paste(CFG$A7_FACTORS, collapse = "/"), "\n")
