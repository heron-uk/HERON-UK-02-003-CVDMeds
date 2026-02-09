### cardio drugs (excl. bb)

info(logger, "INSTANTIATING CARDIOVASCULAR DRUGS COHORT")

mi_drugs_cl <- importCodelist(here("Cohorts", "Primary", "mi_drugs"), type = "csv")

names(mi_drugs_cl) <- paste0(names(mi_drugs_cl), "_mi")

cdm$mi_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = mi_drugs_cl,
  name = "mi_drugs"
)

# collapse records that are within 14 days of each other
cdm$mi_drugs <- cdm$mi_drugs |>
  collapseCohorts(gap = 14,
                  name = "mi_drugs") 

cdm$mi_drugs_first <- cdm$mi_drugs |>
  requireCohortIntersect(
    targetCohortTable = "acute_mi",
    window = c(-30,0),
    name = "mi_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$mi_drugs_after_event <- cdm$mi_drugs |>
  inner_join(cdm$mi_drugs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "mi_drugs_after_event", temporary = FALSE)

drug_count_after <- cdm$mi_drugs_after_event |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100)

cdm$mi_drugs_final <- cdm$mi_drugs_after_event |>
  subsetCohorts(cohortId = drug_count_after$cohort_definition_id,
                name = "mi_drugs_final")

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")

########
info(logger, "INSTANTIATE BETA BLOCKERS AND HF COHORTS")
### beta blockers

bb_id_first <- settings(cdm$mi_drugs_first) |>
  filter(cohort_name == "beta_blockers_mi") |>
  pull(cohort_definition_id)

bb_id <- settings(cdm$mi_drugs_final) |>
  filter(cohort_name == "beta_blockers_mi") |>
  pull(cohort_definition_id)

cdm$beta_blockers_mi_first <- cdm$mi_drugs_first |>
  subsetCohorts(cohortId = bb_id_first,
                name = "beta_blockers_mi_first")

cdm$beta_blockers_after_mi <- cdm$mi_drugs_final |>
  subsetCohorts(cohortId = bb_id,
                name = "beta_blockers_after_mi")


## prior heart failure and no heart failure (bb users)

cdm$beta_blockers_hf_first <- cdm$beta_blockers_mi_first |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf,-1),
    name = "beta_blockers_hf_first"
  )

cdm$beta_blockers_hf <- cdm$beta_blockers_after_mi |>
  inner_join(cdm$beta_blockers_hf_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date),
             by = c("subject_id", "cohort_definition_id")) |>
  compute(name = "beta_blockers_hf", temporary = FALSE)

cdm$beta_blockers_hf <- cdm$beta_blockers_hf |>
  renameCohort(newCohortName = "beta_blockers_mi_prior_heart_failure", 
               cohortId = NULL)

cdm$beta_blockers_no_hf_first <- cdm$beta_blockers_mi_first |>
  requireCohortIntersect(
    targetCohortTable = "heart_failure",
    window = c(-Inf,-1),
    intersections = 0,
    name = "beta_blockers_no_hf_first"
  )

cdm$beta_blockers_no_hf <- cdm$beta_blockers_after_mi |>
  inner_join(cdm$beta_blockers_no_hf_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date),
             by = c("subject_id", "cohort_definition_id")) |>
  compute(name = "beta_blockers_no_hf", temporary = FALSE)

cdm$beta_blockers_no_hf <- cdm$beta_blockers_no_hf |>
  renameCohort(newCohortName = "beta_blockers_mi_no_prior_heart_failure")

info(logger, "INSTANTIATED BETA BLOCKERS AND HF COHORTS")

### combine into one table
  
cdm <- omopgenerics::bind(
  cdm$mi_drugs_final,
  cdm$beta_blockers_hf,
  cdm$beta_blockers_no_hf,
  name = "mi_drugs_final"
)

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")
