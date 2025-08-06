# COVID-19 Retrospective Analysis for CEPI

This is an [`orderly`](https://github.com/vimc/orderly) project.  The directories are:

* `src`: create new reports here
* `archive`: versioned results of running your report
* `data`: copies of data used in the reports

All analysis and outputs are in:

* `analysis`

For people wanting to run the analysis, see `analysis/01_run.Rmd`. Before running this 
install following dependencies:

```
## pkg dependencies for all tasks
install.packages("odin", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
devtools::install_github("mrc-ide/nimue")
devtools::install_github("mrc-ide/squire.page")
install.packages(c("dplyr","readr","purrr","tidyr","geomtextpath","ggpubr","paletteer","pals","slider", "ggpubr", "lubridate", "ggplot2", "forcats"))

# first run dependency tasks
id1 <- orderly::run("generate_vaccine_profiles")
id2 <- orderly::run("income_boosting_rates")
id3 <- orderly::run("income_group_time_to_open")

committed1 <- orderly::orderly_commit(id1)
committed2 <- orderly::orderly_commit(id2)
committed3 <- orderly::orderly_commit(id3)

# then look at 01_run script
```
