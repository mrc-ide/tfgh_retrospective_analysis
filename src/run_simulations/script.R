# ---------------------------------------------------------------------------- #
# 1. Set and read in global variables/data from previous runs
# ---------------------------------------------------------------------------- #

# Files to be read in
income_boosts <- readRDS("income_boosts.Rds")

# Make sure provided parameters are the correct class
vaccine_counterfactual <- as.character(vaccine_counterfactual)
iso <- as.character(iso3c)

# Read in the vaccine counterfactual scenarios and filter to the one for this scenario and country
vac_df <- read.csv("vaccine_counterfactuals.csv", as.is = TRUE)
vac_df$date <- as.Date(vac_df$date)
vac_df_i <- vac_df %>% filter(scenario == vaccine_counterfactual & iso3c == iso)

# end date for simulations
end_date <- as.Date("2022-01-01")

# directories for storing results
dir.create("plots", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# ---------------------------------------------------------------------------- #
# 2. Get fit and make it ready to run
# ---------------------------------------------------------------------------- #

## Get fit from folder
fit <-  readRDS(paste0("fits/", iso3c, ".rds"))

## Update model fit objects
fit$inputs$data <- fit$inputs$data %>% filter((date_start <= end_date))
fit$squire_model <- squire.page:::nimue_booster_min_model(use_dde = TRUE, use_difference = FALSE)
fit$model <- fit$squire_model$odin_model(
  user = squire.page:::assign_infections(
    squire.page:::setup_parameters(fit$squire_model, fit$parameters),
    mean(fit$inputs$initial_infections_interval)
  ),
  unused_user_action = "ignore"
)

# run model first here to get time series of infections
original_out <- squire.page::generate_draws(fit)
dih <- get_deaths_infections_hosps_time(original_out)

# Plot our model fit
wdcp_plot <- weekly_death_comp_plot(dih, fit)
ggsave("plots/wdcp_plot.pdf", wdcp_plot, width = 8.3, height = 6)

# ---------------------------------------------------------------------------- #
# 3. Run our vaccine counterfactual
# ---------------------------------------------------------------------------- #

# work out the t in the mode that the vaccine counterfactual dates relate to
vac_df_i$tt <- as.integer(as.Date(vac_df_i$date) - as.Date(rownames(original_out$output)[1]))

# and now update the vaccination in the model fit parameters
cf <- update_fit_vaccinations(fit, vac_df_i)

# Plot of our vaccination for the scenario
vacc_plot <- vacc_allocation_plot(fit, cf, cf_name = vaccine_counterfactual, combine = FALSE, end_date)

# Generate counterfactual
cf_fit <- squire.page::generate_draws(cf)

# get deaths, infections, hospitalisations for counterfactual
cf_dih <- get_deaths_infections_hosps_time(cf_fit)

# Combine with our baseline
res_out <- rbind(
  dih %>% mutate(scenario = "Baseline"),
  cf_dih %>% mutate(scenario = vaccine_counterfactual)
)

# ---------------------------------------------------------------------------- #
# 4. Create our summary plots and outputs
# ---------------------------------------------------------------------------- #

# Add counterfactual comparisons
death_plot <- plot_deaths(res_out)
death_averted_plot <- plot_deaths_averted(scenarios, FALSE)

# produce our final scenario plot
scenario_plot <- cowplot::plot_grid(
  cowplot::plot_grid(vacc_plot[[2]], vacc_plot[[4]], death_plot, ncol = 1),
  death_averted_plot, align = "v",
  ncol = 2)
title <- cowplot::ggdraw() +
  cowplot::draw_label(paste(iso3c, "-", vaccine_counterfactual), fontface = 'bold', x = 0.5, hjust = 0)
final <- cowplot::plot_grid(title, scenario_plot, ncol = 1, rel_heights = c(0.1, 1))

ggsave("plots/scenario_plot.pdf", final, width = 12, height = 12)

# save the raw simulation data
saveRDS(res_out %>% mutate(iso3c = iso), "data/dih_out.rds")

# save the vaccine coverage data
vac1 <- get_vacc_coverage(fit) %>% mutate(scenario = "Baseline")
vac2 <- get_vacc_coverage(cf) %>% mutate(scenario = vaccine_counterfactual)
saveRDS(rbind(vac1, vac2) %>% mutate(iso3c = iso) , "data/vacc_out.rds")
