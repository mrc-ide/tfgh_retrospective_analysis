library(tidyverse)
library(tidyverse)
library(readxl)

# ---------------------------------------------- #
# 0. Create your vaccine counterfactuals ---------
# ---------------------------------------------- #

# we first need to create a file inside src/run_simulations which is where
# we store what the vaccine counterfactuals are for the coverage

# to get what the coverage is for a specific country we read in one of the
# fits saved inside src/run_simulations. these are previously run fits of a
# covid-19 transmission model to excess mortality per country using `squire.page`

# e.g. for Angola
#fit <- readRDS("src/run_simulations/fits/MNG.rds")

# to get the vaccine coverage we can use the following function
get_vacc_coverage <- function(fit) {

  # create our vaccine allocation dataframe
  df <- data.frame(
    # convert everything to character for dates - just safer
    "date" = as.character(fit$parameters$tt_booster_doses + fit$inputs$start_date),
    "primary_doses" = fit$parameters$primary_doses,
    "booster_doses" = fit$parameters$booster_doses
  )

  return(df)

}

# Step A: Get the vaccine coverage for countries

files <- list.files("src/run_simulations/fits", full.names = TRUE)

# All except Kosovo and Mongolia (start with these)
ctrys <- c("Albania", "Armenia", "Belize", "Bhutan", "Bolivia", "Burkina Faso",
           "Cote d'Ivoire", "Cambodia", "Costa Rica", "Democratic Republic of Congo",
           "El Salvador", "Eswatini", "Ethiopia", "Georgia", "Ghana", "Guatemala",
           "Honduras", "Indonesia", "Iraq", "Jamaica", "Kazakhstan", "Kenya",
           "Kyrgyz Republic", "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Madagascar",
           "Malawi", "Mali", "Montenegro", "Mozambique", "Namibia", "Nepal",
           "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Paraguay",
           "Philippines", "Moldova", "Rwanda", "Senegal", "Serbia",
           "Sierra Leone", "South Sudan", "Sri Lanka", "Tajikistan", "Tanzania",
           "Thailand", "Togo", "Tunisia", "Uganda", "Ukraine", "Uzbekistan",
           "Vietnam", "Zambia", "Zimbabwe")
isos <- countrycode::countrycode(ctrys, "country.name.en", "iso3c")
files <- grep(paste0(isos, collapse = "|"), files, value = TRUE)
final_result <- data.frame()
for (file in files) {
  data <- readRDS(file)
  iso <- gsub(".rds", "", basename(file), fixed = TRUE)
  summary <- get_vacc_coverage(data) %>%
    mutate(iso3c = iso)
  # getting same population size used in model fitting in case values in coverage are different
  summary <- summary %>%
    mutate(pop = sum(squire::get_population(iso3c = iso)$n))

  final_result <- rbind(final_result, summary)
}

# Step B: Load our vaccine data

# Load data and calculate doses per country and day
vaccine_doses <-
  read_xlsx("analysis/data_raw/Coverage.xlsx", guess_max = 5000) %>%
  mutate(date = as.character(covdate)) %>%
  select(iso3c = iso, date, covmedianfinal) %>%
  filter(iso3c != "KSV") %>%
  filter(iso3c != "MNG") %>%
  full_join(final_result, ., by = c("iso3c", "date")) %>%
  distinct() %>%
  arrange(iso3c, date) %>%
  group_by(iso3c) %>%
  fill(iso3c, .direction = "updown") %>%
  fill(pop, .direction = "updown") %>%
  fill(covmedianfinal, .direction = "up") %>%
  # this is coverage i think (otherwise the doses were lower than what actually
  # happened in a number of countries) so swapped to pop and then do cum diff
  mutate(p_doses = round((covmedianfinal * pop)),
  # just using the booster doses that did go out
         b_doses = replace_na(booster_doses, 0)) %>%
  # the coverage was cumulative so create the cumulative difference
  mutate(p_doses = c(p_doses[1], diff(p_doses))) %>%
  # here I opted to actuallyuse the ratio between what the coverage was suggesting and use that to project
  # forward the boses where covmedianfinal was na
  mutate(ratio = mean(p_doses[seq(-8, -1, 1) + which(is.na(p_doses))[1]] / primary_doses[seq(-8, -1, 1) + which(is.na(p_doses))[1]])) %>%
  mutate(p_doses = replace(p_doses, which(is.na(p_doses)), primary_doses[which(is.na(p_doses))]*unique(ratio)))  %>%
  mutate(p_doses = replace(b_doses, which(is.na(b_doses)), primary_doses[which(is.na(b_doses))]*unique(ratio)))  %>%
  ungroup() %>%
  arrange(iso3c, date) %>%
  select(date, p_doses, b_doses, iso3c) %>%
  rename(primary_doses = p_doses,
         booster_doses = b_doses) %>%
  ungroup() %>%
  mutate(scenario = "New Coverage")


# let assume we want to create two scenarios with 20% more and less vaccination
# Do you still need this? I thought you just were comparing your new coverage values
# against base? If not the case then re add this in

# low_df <- vaccine_doses %>%
#   mutate(primary_doses = primary_doses*0.8,
#          booster_doses = booster_doses*0.8,
#          scenario = "Decrease")
# high_df <- vaccine_doses %>%
#   mutate(primary_doses = primary_doses*1.2,
#          booster_doses = booster_doses*1.2,
#          scenario = "Increase")

# counterfactuals <- vaccine_doses %>%
#   full_join(low_df) %>%
#   full_join(high_df) %>%
#   arrange(iso3c, date) %>%
#   select(date, primary_doses, booster_doses, scenario, iso3c)

counterfactuals <- vaccine_doses %>%
  arrange(iso3c, date) %>%
  select(date, primary_doses, booster_doses, scenario, iso3c)

# we then combine these, and assign the iso3c and save it
write.csv(counterfactuals,
          "src/run_simulations/vaccine_counterfactuals.csv",
          row.names = FALSE)


# ---------------------------------------------- #
# 1. Run our simulations ---------
# ---------------------------------------------- #

# Countries in analysis

list_iso <- read.csv("src/run_simulations/vaccine_counterfactuals.csv",
                     na.strings = c("", "NA"),
                     strip.white = TRUE,
                     stringsAsFactors = FALSE) %>%
  select(iso3c) %>%
  unique()

iso_codes <- list_iso$iso3c

list_iso_3 <- list_iso %>% filter(iso3c %in% c("MNG", "TGO", "ZWE")) %>% pull()
list_iso_4 <- as.character(pull(head(list_iso, 4)))
list_iso_complete <- as.character(pull(list_iso))

# Make data frame with each country's start date (30+ days before first cases),
# using data used by Imperial
# files <- list.files("src/run_simulations/fits", full.names = TRUE)
# final_result <- data.frame()
#
# for (file in files) {
#   data <- readRDS(file)
#   summary <- get_vacc_coverage(data) %>%
#     mutate(#country = data$parameters$country,
#            iso3c = substr(basename(file), 1, 3))
#
#   final_result <- rbind(final_result, summary)
# }
#
#
# # Only keeping country and dates. Vaccination data to be completed with TFGH values
# final_result <- final_result %>%
#   filter(iso3c %in% list_iso$iso3c) %>%
#   select(-c(primary_doses, booster_doses))
#


# Initialize empty data frame to collect results
# timings_df <- data.frame()
#
# # Loop over ISO codes and collect start_date
# for (ISO in iso_codes) {
#   fit <- read_rds(paste0("src/run_simulations/fits/", ISO, ".rds"))
#
#   #timings_df <- bind_rows(timings_df, tibble(
#   timings_df <- bind_rows(timings_df, data.frame(
#     country = fit$parameters$country,
#     iso = ISO,
#     start_date = as.character(fit$parameters$tt_booster_doses + fit$inputs$start_date)))
#
# }



# Load data and calculate doses per country and day
# vaccine_doses <- read_xlsx("src/run_simulations/data/Coverage.xlsx", guess_max = 5000) %>%
#   select(iso, covdate, pop, covmedianfinal) %>%
#   mutate(date = as.character(covdate)) %>%
#   full_join(timings_df, by = c("iso" = "iso", "date" = "start_date")) %>%
#   arrange(iso, covdate) %>%
#   fill(pop, .direction = "up") %>%
#   fill(covmedianfinal, .direction = "down") %>%
#   mutate(covmedianfinal = ifelse(is.na(covmedianfinal), 0, covmedianfinal),
#          primary_doses = round((covmedianfinal * pop) /100),
#          booster_doses = 0) %>%
#   rename(iso3c = iso) %>%
#   select(date, primary_doses, booster_doses, iso3c)
#
#
# write.csv(vaccine_doses,
#           "src/run_simulations/vaccine_counterfactuals.csv",
#           row.names = FALSE)



# run these lines to load all data
id1 <- orderly::orderly_run("generate_vaccine_profiles")
id2 <- orderly::orderly_run("income_boosting_rates")
id3 <- orderly::orderly_run("income_group_time_to_open")


committed1 <- orderly::orderly_commit(id1)
committed2 <- orderly::orderly_commit(id2)
committed3 <- orderly::orderly_commit(id3)



# Load own scenarios
# OW_data <- read.csv("src/run_simulations/vaccine_counterfactuals_OW.csv", header = TRUE,
#                       #na.strings = c("", "NA"),
#                       #strip.white = TRUE,
#                       stringsAsFactors = FALSE)



# the simulations don't take too long to run but we can run these in parallel for ease
library(foreach)
library(doParallel)
library(orderly)
library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)

# set up cluster
# i would not recommend going much above 4 as the simulations use a lot of RAM
n.cores <- 2
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

# now let's create our data frame that control what countries and counterfactuals
# we are simulating
#run_df <- expand.grid("iso3c" = list_iso_4, scenario = c("Increase", "Decrease"))
run_df <- expand.grid("iso3c" = as.character(list_iso_4), scenario = c("New Coverage"),
                      stringsAsFactors = FALSE)

# and now we pass this to our parallel function call that will run the
# run_simulation task for each row in our data
orderly_ids <- foreach(
  i = seq_along(run_df$iso3c),
  .combine = 'c'
) %dopar% {

  #suppressWarnings(
  #suppressMessages(
  tryCatch(
    orderly::orderly_run(
      "run_simulations",
      parameters = list(iso3c = as.character(run_df$iso3c[i]), # must be as character not factor
                        vaccine_counterfactual = as.character(run_df$scenario[i])),
      echo = TRUE
    ),
    error = function(e){NA}
  )
  #))

}

# and stop our cluster
parallel::stopCluster(my.cluster)

# what we have done is run the task run_simulation, which if they run correctly
# will return unique ids for each run
orderly_ids

# the actual runs and model outputs are initially stored in the draft folder
# each named with the id

# we can then look in these and either analyse the outputs or look at the model
# plots generated. we may want to check this by looking at some summary plots
# from each run, which is saved by each task as plots/scenario_plot.pdf

# so let's merge all these into one and save it so we can have a look at it
pdfs <- file.path(
  list.files(file.path(here::here(), "draft/run_simulations"), full.names = TRUE),
  "plots/scenario_plot.pdf")
pdfs <- grep(paste0(orderly_ids, collapse = "|"), pdfs, value = TRUE)

dir.create("analysis/plots")
pdftools::pdf_combine(pdfs, file.path(here::here(), "analysis/plots/scenario_plots.pdf"))

# let's assume these all look great then we commit these to the archive
committed <- lapply(orderly_ids, orderly::orderly_commit)

# we have this separation of draft so that we only commit the file runs we want to
# and we are happy with.

# once we have committed them it may be a good idea to make a store of these ids
saveRDS(orderly_ids, "orderly_ids.rds")

# ---------------------------------------------- #
# 2. Analysis ------------------------------------------------------------
# ---------------------------------------------- #

# now we may want to analyse the raw simulation outputs, which we can read in from
# the archive runs within data/dih_out.rds

# find the file paths and read in the results
res_paths <- file.path("archive/run_simulations", orderly_ids, "data/dih_out.rds")

df <- lapply(res_paths, readRDS)

# one thing to be cautious of is that each of the tasks out includes the baseline
# simulation with no change in vaccination. So we will remove the baseline in
# all outputs except for one per country so we don't double count this later on
for(i in which(duplicated(run_df$iso3c))) {
  df[[i]] <- df[[i]] %>% filter(scenario != "Baseline")
  # df <- data.frame(df) %>%
  #   filter(!scenario == "Baseline") %>%
  #   mutate(date = as.character(date))

}

# now we will make it into one large table
df <- do.call(rbind, df)

# the results are a table of the deaths, infections and hospitalisations for
# eah task run. so here there are two scenarios and two iso3cs run. the outputs
# have one row for each date, and also for each replicate run (the model fits
# are based on 100 draws from the model fitting exercise)

# so to work out the totals for each scenario and country we first need to work these out
# for each replicate before then calculating summary statistics
summary_results <- df %>%
  group_by(iso3c, scenario, replicate) %>%
  summarise(
    across(
      c(deaths, hospitalisations, infections), ~sum(.x, na.rm = TRUE)
    )) %>%
  group_by(iso3c, scenario) %>%
  summarise(
    across(
      c(deaths, hospitalisations, infections), ~median(.x, na.rm=TRUE),
      .names = "{col}_med"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.025, na.rm=TRUE),
      .names = "{col}_025"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.975, na.rm=TRUE),
      .names = "{col}_975"
    ),
    .groups = "drop"
  ) #%>%
# mutate_at(vars(deaths_med, hospitalisations_med, infections_med, deaths_025, hospitalisations_025,
#               infections_025, deaths_975, hospitalisations_975, infections_975), as.numeric)

# let's save this somewhere for use in eventual reprt etc
write.csv(summary_results, "analysis/data_out/summary_outcomes.csv", row.names = FALSE)
summary_results

# great the above show that there are more total deaths, hops, infxs in the Decrease
# scenario (where we decreased the vaccinatin) than the baseline and vice versa for Increase

# and if we wanted to visualise this over time we can do this as well this is similar to the
# plots output in the tasks in plots/scenario_plots.pdf
df %>%
  group_by(iso3c, scenario, replicate) %>%
  mutate(
    across(
      c(deaths, hospitalisations, infections), ~cumsum(.x)
    )) %>%
  group_by(iso3c, scenario, date) %>%
  summarise(
    across(
      c(deaths, hospitalisations, infections), ~median(.x, na.rm=TRUE),
      .names = "{col}_med"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.025, na.rm=TRUE),
      .names = "{col}_025"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.975, na.rm=TRUE),
      .names = "{col}_975"
    ),
    .groups = "drop"
  ) %>%
  ggplot(aes(date, deaths_med, color = scenario)) +
  geom_line() +
  facet_wrap(~iso3c, scales = "free_y") +
  theme_bw()

# Hospitalisations
df %>%
  group_by(iso3c, scenario, replicate) %>%
  mutate(
    across(
      c(deaths, hospitalisations, infections), ~cumsum(.x)
    )) %>%
  group_by(iso3c, scenario, date) %>%
  summarise(
    across(
      c(deaths, hospitalisations, infections), ~median(.x, na.rm=TRUE),
      .names = "{col}_med"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.025, na.rm=TRUE),
      .names = "{col}_025"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.975, na.rm=TRUE),
      .names = "{col}_975"
    ),
    .groups = "drop"
  ) %>%
  ggplot(aes(date, hospitalisations_med, color = scenario)) +
  geom_line() +
  facet_wrap(~iso3c, scales = "free_y") +
  theme_bw()


# infections
df %>%
  group_by(iso3c, scenario, replicate) %>%
  mutate(
    across(
      c(deaths, hospitalisations, infections), ~cumsum(.x)
    )) %>%
  group_by(iso3c, scenario, date) %>%
  summarise(
    across(
      c(deaths, hospitalisations, infections), ~median(.x, na.rm=TRUE),
      .names = "{col}_med"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.025, na.rm=TRUE),
      .names = "{col}_025"
    ),
    across(
      c(deaths, hospitalisations, infections), ~quantile(.x, 0.975, na.rm=TRUE),
      .names = "{col}_975"
    ),
    .groups = "drop"
  ) %>%
  ggplot(aes(date, infections_med, color = scenario)) +
  geom_line() +
  facet_wrap(~iso3c, scales = "free_y") +
  theme_bw()


# ---------------------------------------------- #
# 3. Fin. Next Steps ------------------------------------------------------------
# ---------------------------------------------- #

# hopefully that gives you enough to get started with. next steps would be
# to populate the "src/run_simulations/vaccine_counterfactuals.csv" with
# the vaccine counterfactuals you want to look into and to then repeat the
# simulations for all the countries of interest. Hopefully then it is just a
# matter of deciding how you want to present the results :)
