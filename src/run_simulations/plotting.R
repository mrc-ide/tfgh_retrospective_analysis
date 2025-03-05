vacc_allocation_plot <- function(fit, cf, cf_name, combine = TRUE, end_date) {

  # create our vaccine allocation dataframe
  df1 <- get_vacc_coverage(fit) %>% mutate(scenario = "Baseline")
  df2 <- get_vacc_coverage(cf) %>% mutate(scenario = cf_name)


  # Demonstration Plot of it all together

  # daily primary doses
  gg1 <- rbind(df1, df2) %>%
    mutate(doses = primary_doses) %>%
    ggplot(aes(x = date, doses, color = scenario)) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("black", pals::stepped3()[c(1,5,9,13)])) +
    scale_linetype_manual(name = "", values = c("longdash", rep("solid", 4))) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white", colour = "white"),
          axis.line = element_line()) +
    xlab("Date") +
    ylab("Daily Primary Vaccine Doses")

  # cumulative primary doses
  gg2 <- rbind(df1, df2) %>%
    group_by(scenario) %>%
    mutate(doses = cumsum(primary_doses)) %>%
    ggplot(aes(x = date, doses, color = scenario)) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("black", pals::stepped3()[c(1,5,9,13)])) +
    scale_linetype_manual(name = "", values = c("longdash", rep("solid", 4))) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white", colour = "white"),
          axis.line = element_line()) +
    xlab("Date") +
    ylab("Cumulative Primary Vaccine Doses")

  # daily booster doses
  gg3 <- rbind(df1, df2) %>%
    mutate(doses = booster_doses) %>%
    ggplot(aes(x = date, doses, color = scenario)) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("black", pals::stepped3()[c(1,5,9,13)])) +
    scale_linetype_manual(name = "", values = c("longdash", rep("solid", 4))) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white", colour = "white"),
          axis.line = element_line()) +
    xlab("Date") +
    ylab("Daily Booster Vaccine Doses")


  # cumulative primary doses
  gg4 <- rbind(df1, df2) %>%
    group_by(scenario) %>%
    mutate(doses = cumsum(booster_doses)) %>%
    ggplot(aes(x = date, doses, color = scenario)) +
    geom_line(lwd = 1) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(axis.line = element_line(), legend.position = "top",
          legend.key = element_rect(fill = "white")) +
    scale_color_manual(name = "", values = c("black", pals::stepped3()[c(1,5,9,13)])) +
    scale_linetype_manual(name = "", values = c("longdash", rep("solid", 4))) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)) +
    theme(legend.text = element_text(size = 14),
          panel.grid.major = element_line(),
          legend.key = element_rect(fill = "white", colour = "white"),
          axis.line = element_line()) +
    xlab("Date") +
    ylab("Cumulative Booster Vaccine Doses")

  if(combine) {
    cowplot::plot_grid(gg1, gg2 + theme(legend.position = "none"),
                       gg3, gg4 + theme(legend.position = "none"),
                       ncol = 2, align = "v", rel_heights = c(1,0.8), byrow = FALSE)
  } else {
    list(gg1, gg2, gg3, gg4)
  }
}

# ------------

plot_deaths <- function(res_out){
  baseline <- res_out %>% filter(scenario == "Baseline") %>%
    group_by(replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections), ~cumsum(.x)
      )
    ) %>%
    group_by(date) %>%
    summarise(
      across(
        c(deaths, infections), ~median(.x, na.rm=TRUE),
        .names = "{col}_med"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.025, na.rm=TRUE),
        .names = "{col}_025"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.975, na.rm=TRUE),
        .names = "{col}_975"
      ),
      .groups = "drop"
    )

  scenarios_x <- res_out %>% filter(scenario != "Baseline") %>%
    group_by(scenario, replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections), ~cumsum(.x)
      )
    ) %>%
    group_by(scenario, date) %>%
    summarise(
      across(
        c(deaths, infections), ~median(.x, na.rm=TRUE),
        .names = "{col}_med"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.025, na.rm=TRUE),
        .names = "{col}_025"
      ),
      across(
        c(deaths, infections), ~quantile(.x, 0.975, na.rm=TRUE),
        .names = "{col}_975"
      ),
      .groups = "drop"
    )

  rbind(baseline %>% mutate(scenario = "Baseline", .before = 1),
        scenarios_x) %>%
    ggplot(aes(x = date, y = deaths_med, color = scenario,
               ymin = deaths_025, ymax = deaths_975,
               fill = scenario, group = scenario)) +
    geom_line() +
    geom_ribbon(alpha = 0.1) +
    ggpubr::theme_pubr(base_size = 14) +
    labs(x = "Date", y = "Cumulative Deaths \n(95% quantile and median)")

}

plot_deaths_averted <- function(scenario_df, facet = FALSE){
  baseline <- res_out %>% filter(scenario == "Baseline") %>%
    group_by(replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections, hospitalisations), ~cumsum(.x)
      )
    )

  scenarios_x <- res_out %>% filter(scenario != "Baseline") %>%
    group_by(scenario, replicate) %>%
    arrange(date) %>%
    mutate(
      across(
        c(deaths, infections, hospitalisations), ~cumsum(.x)
      )
    )

  nm_pos <- which(names(baseline) %in% c("deaths", "infections", "hospitalisations"))
  names(baseline)[nm_pos] <- paste0("baseline_", names(baseline)[nm_pos])

  # now calculate deaths averted per scenario against baseline
  scenarios_x <- left_join(scenarios_x, baseline %>% select(-scenario), by = c("date", "replicate")) %>%
    mutate(deaths_averted = baseline_deaths - deaths) %>%
    mutate(infections_averted = baseline_infections - infections) %>%
    mutate(hospitalisations_averted = baseline_hospitalisations - hospitalisations) %>%
    group_by(scenario, date) %>%
    summarise(
      across(
        c(deaths_averted, infections_averted, hospitalisations_averted), ~median(.x, na.rm=TRUE),
        .names = "{col}_med"
      ),
      across(
        c(deaths_averted, infections_averted, hospitalisations_averted), ~quantile(.x, 0.025, na.rm=TRUE),
        .names = "{col}_025"
      ),
      across(
        c(deaths_averted, infections_averted, hospitalisations_averted), ~quantile(.x, 0.25, na.rm=TRUE),
        .names = "{col}_25"
      ),
      across(
        c(deaths_averted, infections_averted, hospitalisations_averted), ~quantile(.x, 0.75, na.rm=TRUE),
        .names = "{col}_75"
      ),
      across(
        c(deaths_averted, infections_averted, hospitalisations_averted), ~quantile(.x, 0.975, na.rm=TRUE),
        .names = "{col}_975"
      ),
      .groups = "drop"
    )

  # make plot
  deaths_averted <- scenarios_x %>%
    ggplot(aes(x = date, y = deaths_averted_med,
               ymin = deaths_averted_025, ymax = deaths_averted_975,
               color = scenario, group = scenario, fill = scenario)) +
    geom_ribbon(alpha = 0.1) +
    geom_line() +
    ggpubr::theme_pubr(base_size = 14) +
    labs(x = "Date", y = "Cumulative Deaths Averted \n(median, IQR, 95% quantile)")

  infections_averted <- scenarios_x %>%
    ggplot(aes(x = date, y = infections_averted_med,
               ymin = infections_averted_025, ymax = infections_averted_975,
               color = scenario, group = scenario, fill = scenario)) +
    geom_ribbon(alpha = 0.1) +
    geom_line() +
    ggpubr::theme_pubr(base_size = 14) +
    labs(x = "Date", y = "Cumulative Infections Averted \n(median, IQR, 95% quantile)")

  hospitalisations_averted <- scenarios_x %>%
    ggplot(aes(x = date, y = hospitalisations_averted_med,
               ymin = hospitalisations_averted_025, ymax = hospitalisations_averted_975,
               color = scenario, group = scenario, fill = scenario)) +
    geom_ribbon(alpha = 0.1) +
    geom_line() +
    ggpubr::theme_pubr(base_size = 14) +
    labs(x = "Date", y = "Cumulative Hospitalisations Averted \n(median, IQR, 95% quantile)")

  leg <- cowplot::get_plot_component(deaths_averted, "guide-box", return_all = TRUE)
  cowplot::plot_grid(leg[[which.max(unlist(lapply(leg, object.size)))]],
                     deaths_averted + theme(legend.position = "none"),
                     hospitalisations_averted + theme(legend.position = "none"),
                     infections_averted + theme(legend.position = "none"),
                     ncol = 1, rel_heights = c(0.5, 1, 1, 1))

}


weekly_death_comp_plot <- function(dih, fit){

  date_0 <- squire.page:::get_data_end_date(fit)
  data <- squire.page:::get_data(fit)
  data$date <- squire.page:::get_dates_greater(fit)
  data$cdeaths <- cumsum(data$deaths)

  dih %>% ggplot(aes(date, deaths*7, group = replicate)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::ylab("Weekly Deaths") +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    ggplot2::xlab("") +
    ggplot2::geom_bar(
      data = data,
      ggplot2::aes(x = .data$date,
                   y = .data$deaths),stat = "identity",
      linetype = "dashed", inherit.aes = FALSE, fill = "#c6a39b"
    ) +
    geom_line(color = "#408da7", alpha = 0.25)  +
    ggtitle(paste0("Baseline Fit: ", fit$parameters$country, " (",data$iso[1], ")"))

}
