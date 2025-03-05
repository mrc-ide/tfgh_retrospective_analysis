library(tidyverse)

# ---------------------------------------------- #
# 0. Create your vaccine counterfactuals ---------
# ---------------------------------------------- #

# we first need to create a file inside src/run_simulations which is where
# we store what the vaccine counterfactuals are for the coverage

# to get what the coverage is for a specific country we read in one of the
# fits saved inside src/run_simulations. these are previously run fits of a
# covid-19 transmission model to excess mortality per country using `squire.page`

# e.g. for Angola
fit <- readRDS("src/run_simulations/fits/AGO.rds")

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

# so for angola
vac_df <- get_vacc_coverage(fit)

# let assume we want to create two scenarios with 20% more and less vaccination
low_df <- vac_df %>%
  mutate(primary_doses = primary_doses*0.8,
         booster_doses = booster_doses*0.8,
         scenario = "Decrease")
high_df <- vac_df %>%
  mutate(primary_doses = primary_doses*1.2,
         booster_doses = booster_doses*1.2,
         scenario = "Increase")

# we then combine these, and assign the iso3c and save it
write.csv(
  rbind(low_df, high_df) %>% mutate(iso3c = "AGO"),
  "src/run_simulations/vaccine_counterfactuals.csv",
  row.names = FALSE
)

# and let's do the same for South Africa
zaf <- readRDS("src/run_simulations/fits/ZAF.rds")
zaf_vac_df <- get_vacc_coverage(zaf)

# let assume we want to create two scenarios with 20% more and less vaccination
low_df <- zaf_vac_df %>%
  mutate(primary_doses = primary_doses*0.8,
         booster_doses = booster_doses*0.8,
         scenario = "Decrease")
high_df <- zaf_vac_df %>%
  mutate(primary_doses = primary_doses*1.2,
         booster_doses = booster_doses*1.2,
         scenario = "Increase")

# we then combine these, and add these to the file that already exists
write.csv(
  rbind(
    # we read in as is to ensure it is a character
    read.csv("src/run_simulations/vaccine_counterfactuals.csv", as.is = TRUE),
    rbind(low_df, high_df) %>% mutate(iso3c = "ZAF")
  ),
  "src/run_simulations/vaccine_counterfactuals.csv",
  row.names = FALSE
)

# this file is just an example but I will leave it to you to work out what
# your vaccine scenarios are over time

# ---------------------------------------------- #
# 1. Run our simulations ---------
# ---------------------------------------------- #

# The reason we created this counterfactual file in this directory is because
# this repo is set up using `orderly`, which is a package developed by Imperial
# MRC-GIDA (see mrc-ide on github) to make reproducible analysis simpler

# orderly repos have orederly tasks inside the src/ directory in folders, with
# each folder a task. The files inside this are then processed by orderly and
# run before adding extra infrmation to the output so that we know exactly how
# this task was run (what packages, what files were used, what versions etc)

# for now perhaps do not worry too much about it

# All countries that can be run are in the following file
iso3cs <- readLines("analysis/iso3cs")

# but in this example let's just look at the two we have made counterfactuals for
iso3cs <- c("AGO", "ZAF")

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
n.cores <- 4
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
run_df <- expand.grid("iso3c" = iso3cs, scenario = c("Increase", "Decrease"))

# and now we pass this to our parallel function call that will run the
# run_simulation task for each row in our data
orderly_ids <- foreach(
  i = seq_along(run_df$iso3c),
  .combine = 'c'
) %dopar% {

  suppressWarnings(suppressMessages(
    tryCatch(
      orderly::orderly_run(
        "run_simulations",
        parameters = list(iso3c = as.character(run_df$iso3c[i]), # must be as character not factor
                          vaccine_counterfactual = as.character(run_df$scenario[i])),
        echo = TRUE
      ),
      error = function(e){NA}
    )
  ))

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
res_paths <- file.path("archive/run_simulations/", orderly_ids, "data/dih_out.rds")
df <- lapply(res_paths, readRDS)

# one thing to be cautious of is that each of the tasks out includes the baseline
# simulation with no change in vaccination. So we will remove the baseline in
# all outputs except for one per country so we don't double count this later on
for(i in which(duplicated(run_df$iso3c))) {
  df[[i]] <- df[[i]] %>% filter(scenario != "Baseline")
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
  )

# let's save this somewhere for use in eventual reprt etc
write.csv(summary_results, "analysis/data_out/summary_outcomes.csv")
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

# ---------------------------------------------- #
# 3. Fin. Next Steps ------------------------------------------------------------
# ---------------------------------------------- #

# hopefully that gives you enough to get started with. next steps would be
# to populate the "src/run_simulations/vaccine_counterfactuals.csv" with
# the vaccine counterfactuals you want to look into and to then repeat the
# simulations for all the countries of interest. Hopefully then it is just a
# matter of deciding how you want to present the results :)
