# -------------------------------------------------------------------------- ###
#  Functions for Vaccines
# -------------------------------------------------------------------------- ###

# Update vaccinations in a country model fit with new values for booster model
update_fit_vaccinations <- function(fit, new_values){

  fit$parameters$primary_doses <- new_values$primary_doses
  fit$parameters$booster_doses <- new_values$booster_doses
  fit$parameters$tt_primary_doses <- new_values$tt
  fit$parameters$tt_booster_doses <- new_values$tt
  return(fit)
}

# get vaccine coverage from a model fit
get_vacc_coverage <- function(fit) {

  # work out time of vaccination relevant to start of epidemic
  start_vacc <- fit$parameters$tt_booster_doses[2]
  end_vacc <- tail(fit$parameters$tt_booster_doses, 1)
  tt_doses <- seq(start_vacc, end_vacc, 1)
  tt_doses <- c(0, tt_doses)

  # create our vaccine allocation dataframe
  df <- data.frame(
    "date" = fit$parameters$tt_booster_doses + fit$inputs$start_date,
    "primary_doses" = fit$parameters$primary_doses,
    "booster_doses" = fit$parameters$booster_doses,
    "tt" = tt_doses
  )

  return(df)

}




# -------------------------------------------------------------------------- ###
#  Accessory Functions
# -------------------------------------------------------------------------- ###

quick_format <- function(x, var_select, date_0) {

  d <- nimue:::odin_index(x$model)

  do.call(rbind,lapply(var_select, function(i){
    do.call(rbind, lapply(seq_len(dim(x$output)[3]), function(y) {
      df <- data.frame(y = rowSums(x$output[,d[[i]],y]), compartment = i)
      df$t <- seq_len(nrow(df)) - nrow(df)
      df$replicate <- y
      df$date <- df$t + date_0
      return(df)
    }))
  }))

}

get_deaths_infections_hosps_time <- function(out){
  value <- quick_format(out, c("D", "infections_cumu", "hospitalisation_demand_cumu"), out$inputs$start_date)
  value$date <- as.Date(rownames(value))
  value <- value %>%
    group_by(replicate, compartment) %>%
    arrange(date) %>%
    filter(y > 0) %>%
    transmute(
      y = c(0, diff(y)),
      date = date,
      replicate = replicate
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = compartment, values_from = y) %>%
    rename(deaths = D, infections = infections_cumu, hospitalisations = hospitalisation_demand_cumu)
  value
}
