# create logger ----
results_folder <- here("Results", cdmName(cdm))
if (!file.exists(results_folder)) {
  dir.create(results_folder, recursive = TRUE)
}
results <- list()
logger_name <- gsub(":| |-", "_", paste0("log_01_001_", Sys.time(), ".txt"))
logger <- create.logger()
logfile(logger) <- here(results_folder, logger_name)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

cdm$person <- cdm$person |>
  filter(
    !is.na(gender_concept_id),
    !is.na(year_of_birth),
    gender_concept_id %in% c(8507,8532)
  )

study_period <- c(as.Date(study_start), as.Date(NA))

# create and export snapshot
info(logger, "RETRIEVING SNAPSHOT")
cli::cli_text("- GETTING CDM SNAPSHOT ({Sys.time()})")
results[["snap"]] <- summariseOmopSnapshot(cdm)
info(logger, "SNAPSHOT COMPLETED")

# summarise observation periods
info(logger, "RETRIEVING OBSERVATION PERIOD SUMMARY")
cli::cli_text("- GETTING OBSERVATION PERIOD SUMMARY ({Sys.time()})")
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)
info(logger, "OBSERVATION PERIOD SUMMARY COMPLETED")

# instantiate necessary cohorts ----
info(logger, "INSTANTIATING STUDY COHORTS")
source(here("Cohorts", "InstantiateCohorts.R"))
info(logger, "STUDY COHORTS INSTANTIATED")

# run analyses ----
if(isTRUE(run_drug_utilisation)){
info(logger, "RUN DRUG UTILISATION")
source(here("Analyses", "drugUtilisation.R"))
info(logger, "DRUG UTILISATION FINISHED")
}

if(isTRUE(run_drug_adherence)){
info(logger, "RUN DRUG ADHERENCE")
source(here("Analyses", "drugAdherence.R"))
info(logger, "DRUG ADHERENCE FINISHED")
}

if(isTRUE(run_characteristics)){
info(logger, "RUN SUMMARISE CHARACTERISTICS")
source(here("Analyses", "characteristics.R"))
info(logger, "SUMMARISE CHARACTERISTICS FINISHED")
}

# export results ----
info(logger, "EXPORTING RESULTS")
result <- omopgenerics::bind(results)
omopgenerics::exportSummarisedResult(result,
                                     minCellCount = min_cell_count,
                                     path = results_folder,
                                     fileName = "results_{cdm_name}_{date}.csv"
)
info(logger, "RESULTS EXPORTED")

info(logger, "STUDY CODE FINISHED")

