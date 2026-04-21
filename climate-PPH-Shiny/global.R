# =============================================================================
# global.R — Climate CEA Dashboard: HSC vs Oxytocin for PPH in LMICs
# =============================================================================

# =============================================================================
# SECTION 0: Packages
# =============================================================================
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyverse)
library(minpack.lm)
library(wbstats)
library(plotly)
library(readr)
library(scales)
library(tidyr)
library(tibble)
library(shiny)
library(bslib)

# =============================================================================
# SECTION 1: LMIC country list & WB regions
# =============================================================================
wb_all <- wb_countries()

lmics <- wb_all %>%
  filter(income_level %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
  select(iso3c, country)

wb_regions <- wb_all %>%
  filter(iso3c %in% lmics$iso3c) %>%
  select(iso3 = iso3c, region)

# =============================================================================
# SECTION 2: Data Cache
# =============================================================================
cache_file <- "data_cache_v3.rds"

if (file.exists(cache_file)) {
  cache            <- readRDS(cache_file)
  baseline         <- cache$baseline
  future_near      <- cache$future_near
  future_mid       <- cache$future_mid
  births_panel1    <- cache$births_panel1
  births_panel2    <- cache$births_panel2
  births_annual    <- cache$births_annual
  wdi_facility_raw <- cache$wdi_facility_raw
} else {
  stop("Data cache not found. Ensure 'data_cache_v3.rds' is in the app directory.")
}

# =============================================================================
# SECTION 3: Model Constants & Parameters
# =============================================================================
HR          <- 1.60
ipcc_offset <- 0.85
Ea          <- 122000
R_gas       <- 8.314
k_anch      <- 0.151
tau         <- 3 / 12    # 3 months — panel model (Viz 1 & 2)
tau_annual  <- 6 / 12    # 6 months — annual model (Viz 3)
P_PPH       <- 0.1125
P_oxy       <- 0.80
disc_rate   <- 0.03
base_year   <- 2020
cost_oxy    <- 0.385
cost_HSC    <- 0.40
DALY_weight <- 0.29
DALY_dur    <- 0.024
icer_threshold <- 100
K_cap       <- 0.95      # logistic ceiling for facility delivery coverage

# =============================================================================
# SECTION 4: Helper Functions
# =============================================================================
arrhenius_k <- function(T_c) {
  T_K <- T_c + 273.15
  k_anch * exp((Ea / R_gas) * (1 / 303 - 1 / T_K))
}

degrad_prob <- function(T_c, tau_yr) {
  1 - exp(-arrhenius_k(T_c) * tau_yr)
}

# make_warming: joins baseline + future cache objects, computes warming & tasmax_proj
# FIX: adds tasmax_proj column needed by Section 12
make_warming <- function(base_df, fut_df, label) {
  inner_join(base_df, fut_df, by = "iso3", suffix = c("_base", "_fut")) %>%
    mutate(
      tas_base    = tas_base   + ipcc_offset,
      tas_future  = tas_fut    + ipcc_offset,
      tasmax_proj = tasmax_fut,
      warming     = round(tas_future - tas_base, 2),
      period      = label,
      cold_chain_risk = case_when(
        tasmax_fut >= 35 ~ "Critical (>=35C)",
        tasmax_fut >= 30 ~ "High (30-35C)",
        TRUE             ~ "Moderate (<30C)"
      )
    ) %>%
    left_join(lmics, by = c("iso3" = "iso3c")) %>%
    filter(!is.na(country))
}

# =============================================================================
# SECTION 5: Temperature Warming (Viz 1)
# =============================================================================
warming_near <- make_warming(baseline, future_near, "2020-2039")
warming_mid  <- make_warming(baseline, future_mid,  "2040-2059")

cat("warming_near rows:", nrow(warming_near), "\n")
cat("warming_mid rows:",  nrow(warming_mid),  "\n")

# =============================================================================
# SECTION 6: Regional PCCF Baselines
# =============================================================================
regional_pccf <- tibble(
  region    = c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific",
                "Latin America & Caribbean",
                "Middle East, North Africa, Afghanistan & Pakistan",
                "Europe & Central Asia"),
  P_CCF_reg = c(0.46, 0.40, 0.35, 0.25, 0.30, 0.25)
)

# =============================================================================
# SECTION 7: Logistic Facility Delivery Model
# =============================================================================
fit_logistic <- function(df) {
  if (nrow(df) < 3) {
    return(tibble(r = NA_real_, t0 = NA_real_, pf_flat = mean(df$pf_obs, na.rm = TRUE)))
  }
  tryCatch({
    fit <- nlsLM(
      pf_obs ~ K_cap / (1 + exp(-r * (year - t0))),
      data    = df,
      start   = list(r = 0.08, t0 = 2015),
      lower   = c(r = 0.01, t0 = 2010),
      upper   = c(r = 0.30, t0 = 2035),
      control = nls.lm.control(maxiter = 150)
    )
    tibble(r = coef(fit)[["r"]], t0 = coef(fit)[["t0"]], pf_flat = NA_real_)
  }, error = function(e) {
    tibble(r = NA_real_, t0 = NA_real_, pf_flat = mean(tail(df$pf_obs, 3), na.rm = TRUE))
  })
}

logistic_params <- wdi_facility_raw %>%
  group_by(iso3) %>%
  nest() %>%
  mutate(params = map(data, fit_logistic)) %>%
  unnest(params) %>%
  select(iso3, r, t0, pf_flat) %>%
  mutate(r = pmin(r, 0.15, na.rm = TRUE))

# =============================================================================
# SECTION 8: Project PF at Panel Midpoints & Annual PF for Viz 3
# =============================================================================
pf_projected <- logistic_params %>%
  mutate(
    P_F_2029 = case_when(
      !is.na(r) ~ K_cap / (1 + exp(-r * (2029 - t0))),
      TRUE      ~ pf_flat
    ),
    P_F_2049 = case_when(
      !is.na(r) ~ K_cap / (1 + exp(-r * (2049 - t0))),
      TRUE      ~ pf_flat
    ),
    P_F_2029 = pmin(pmax(P_F_2029, 0.01), K_cap),
    P_F_2049 = pmin(pmax(P_F_2049, 0.01), K_cap)
  ) %>%
  select(iso3, P_F_2029, P_F_2049)

model_years <- 2020:2059

annual_pf <- crossing(iso3 = lmics$iso3c, year = model_years) %>%
  left_join(logistic_params, by = "iso3") %>%
  mutate(
    PFt = case_when(
      !is.na(r) ~ K_cap / (1 + exp(-r * (year - t0))),
      TRUE      ~ pf_flat
    ),
    PFt = pmin(pmax(PFt, 0.01), K_cap)
  ) %>%
  select(iso3, year, PFt)

# =============================================================================
# SECTION 9: master_df — panel-level model
# =============================================================================
master_df <- warming_mid %>%
  select(
    iso3, country,
    T_proj_2049    = tas_future,
    Tmax_proj_2049 = tasmax_proj,
    warming_2049   = warming,
    cold_chain_risk
  ) %>%
  left_join(
    warming_near %>% select(iso3, T_proj_2029 = tas_future, warming_2029 = warming),
    by = "iso3"
  ) %>%
  left_join(wb_regions,    by = "iso3") %>%
  left_join(births_panel1, by = "iso3") %>%
  left_join(births_panel2, by = "iso3") %>%
  left_join(pf_projected,  by = "iso3") %>%
  left_join(regional_pccf, by = "region") %>%
  mutate(
    delta_T_2029      = warming_2029 + ipcc_offset,
    delta_T_2049      = warming_2049 + ipcc_offset,
    P_CCF_2029        = pmin(P_CCF_reg * HR^delta_T_2029, 1.0),
    P_CCF_2049        = pmin(P_CCF_reg * HR^delta_T_2049, 1.0),
    CCF_capped_2049   = (P_CCF_reg * HR^delta_T_2049) > 1.0,
    D_2029            = degrad_prob(T_proj_2029, tau),
    D_2049            = degrad_prob(T_proj_2049, tau),
    W_risk_2029       = total_births_p1 * P_F_2029 * P_PPH * P_oxy * P_CCF_2029 * D_2029,
    W_risk_2049       = total_births_p2 * P_F_2049 * P_PPH * P_oxy * P_CCF_2049 * D_2049,
    W_risk_delta      = W_risk_2049 - W_risk_2029,
    W_risk_pct_change = (W_risk_delta / W_risk_2029) * 100
  )

cat("master_df rows:", nrow(master_df), "\n")

# =============================================================================
# SECTION 10: Bubble Chart Data (Viz 2)
# Includes W_riskPPH (women needing oxy for PPH) and D_oxy (degradation prob)
# =============================================================================
birth_max <- max(master_df$total_births_p2, na.rm = TRUE)
birth_min <- min(master_df$total_births_p2, na.rm = TRUE)

region_colors <- c(
  "Sub-Saharan Africa"                                = "#27ae60",
  "South Asia"                                        = "#e74c3c",
  "East Asia & Pacific"                               = "#1abc9c",
  "Latin America & Caribbean"                         = "#8e44ad",
  "Middle East, North Africa, Afghanistan & Pakistan" = "#f39c12",
  "Europe & Central Asia"                             = "#2980b9"
)

plot_p1 <- master_df %>%
  filter(!is.na(W_risk_2029), !is.na(T_proj_2029), W_risk_2029 > 0) %>%
  mutate(
    W_risk      = W_risk_2029,
    T_proj      = T_proj_2029,
    warming     = warming_2029,
    P_F         = P_F_2029,
    P_CCF       = P_CCF_2029,
    W_riskPPH   = total_births_p1 * P_F_2029 * P_PPH * P_oxy,
    D_oxy       = D_2029,
    bubble_size = ((total_births_p1 - birth_min) / (birth_max - birth_min))^(1/3) * 70 + 6
  )

plot_p2 <- master_df %>%
  filter(!is.na(W_risk_2049), !is.na(T_proj_2049), W_risk_2049 > 0) %>%
  mutate(
    W_risk      = W_risk_2049,
    T_proj      = T_proj_2049,
    warming     = warming_2049,
    P_F         = P_F_2049,
    P_F_prev    = P_F_2029,
    P_CCF       = P_CCF_2049,
    W_riskPPH   = total_births_p2 * P_F_2049 * P_PPH * P_oxy,
    D_oxy       = D_2049,
    bubble_size = ((total_births_p2 - birth_min) / (birth_max - birth_min))^(1/3) * 70 + 6
  )

cat("plot_p1 rows:", nrow(plot_p1), "\n")
cat("plot_p2 rows:", nrow(plot_p2), "\n")

# =============================================================================
# SECTION 11: Annual Temperature Interpolation (Viz 3)
# =============================================================================
country_climate <- warming_mid %>%
  select(iso3, Tproj2049 = tas_future, Tmaxproj2049 = tasmax_proj,
         warming2049 = warming) %>%
  left_join(
    warming_near %>% select(iso3, Tproj2029 = tas_future, warming2029 = warming),
    by = "iso3"
  ) %>%
  left_join(wb_regions,    by = "iso3") %>%
  left_join(regional_pccf, by = "region")

annual_temp <- country_climate %>%
  crossing(year = model_years) %>%
  mutate(
    interp_wt = pmax(0, pmin(1, (year - 2029) / (2049 - 2029))),
    Tprojt    = Tproj2029 + interp_wt * (Tproj2049 - Tproj2029),
    Tmaxprojt = Tmaxproj2049,
    warmingt  = warming2029 + interp_wt * (warming2049 - warming2029),
    delta_Tt  = warmingt + ipcc_offset
  )

# =============================================================================
# SECTION 12: Annual ICER Model (Viz 3)
# FIX: uses Bt (not births) — births_annual column is named Bt
# =============================================================================
annual_df <- annual_temp %>%
  left_join(births_annual, by = c("iso3", "year")) %>%
  left_join(annual_pf,     by = c("iso3", "year")) %>%
  mutate(
    PCCFt          = pmin(P_CCF_reg * HR^delta_Tt, 1.0),
    Dt             = degrad_prob(Tprojt, tau_annual),
    WriskPPH       = Bt * PFt * P_PPH * P_oxy,        # FIX: Bt not births
    Wriskt         = WriskPPH * PCCFt * Dt,
    disc_wt        = 1 / (1 + disc_rate)^(year - base_year),
    cost_oxy_ct    = cost_oxy * (1 + PCCFt * Dt),
    total_cost_oxy = WriskPPH * cost_oxy_ct,
    total_cost_HSC = WriskPPH * cost_HSC,
    cost_oxy_disc  = total_cost_oxy * disc_wt,
    cost_HSC_disc  = total_cost_HSC * disc_wt,
    DALYoxy        = Wriskt * DALY_weight * DALY_dur,
    DALYoxy_disc   = DALYoxy * disc_wt,
    delta_cost_disc = cost_HSC_disc - cost_oxy_disc,
    ICERt          = ifelse(DALYoxy_disc > 0, delta_cost_disc / DALYoxy_disc, NA_real_)
  )

# =============================================================================
# SECTION 13: Cumulative ICER & Threshold-Crossing Year Map (Viz 3)
# =============================================================================
icer_cumulative <- annual_df %>%
  filter(!is.na(Wriskt)) %>%
  group_by(iso3) %>%
  summarise(
    total_delta_cost_disc = sum(delta_cost_disc, na.rm = TRUE),
    total_DALYs_disc      = sum(DALYoxy_disc,    na.rm = TRUE),
    total_Wrisk           = sum(Wriskt,           na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ICER_cumulative = ifelse(
      total_DALYs_disc > 0,
      total_delta_cost_disc / total_DALYs_disc,
      NA_real_
    )
  )

threshold_map <- annual_df %>%
  filter(!is.na(ICERt)) %>%
  group_by(iso3) %>%
  summarise(
    first_ce_year = suppressWarnings(
      min(year[ICERt < icer_threshold & !is.na(ICERt)], na.rm = TRUE)
    ),
    min_icer = min(ICERt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    first_ce_year = if_else(is.infinite(first_ce_year), NA_integer_, as.integer(first_ce_year))
  ) %>%
  left_join(
    icer_cumulative %>% select(iso3, ICER_cumulative, total_DALYs_disc, total_delta_cost_disc),
    by = "iso3"
  ) %>%
  left_join(master_df %>% select(iso3, country, region), by = "iso3") %>%
  mutate(
    ce_category = case_when(
      ICER_cumulative < 0                            ~ "Dominant",
      !is.na(first_ce_year) & first_ce_year <= 2035 ~ "Cost-effective by 2035",
      !is.na(first_ce_year) & first_ce_year <= 2059 ~ "Cost-effective by 2059",
      TRUE                                           ~ "Not cost-effective"
    ),
    ce_color = case_when(
      ce_category == "Dominant"               ~ "#01696f",
      ce_category == "Cost-effective by 2035" ~ "#4f98a3",
      ce_category == "Cost-effective by 2059" ~ "#a8cdd0",
      TRUE                                    ~ "#bab9b4"
    ),
    ce_z = case_when(
      ce_category == "Dominant"               ~ 1L,
      ce_category == "Cost-effective by 2035" ~ 2L,
      ce_category == "Cost-effective by 2059" ~ 3L,
      TRUE                                    ~ 4L
    )
  )

cat("threshold_map rows:", nrow(threshold_map), "\n")
cat("Dominant countries:", sum(threshold_map$ce_category == "Dominant", na.rm = TRUE), "\n")
cat("global.R loaded successfully.\n")
