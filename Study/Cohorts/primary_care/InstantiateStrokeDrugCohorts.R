### cardio drugs (excl. bb)

info(logger, "INSTANTIATING STROKE DRUGS COHORT")

stroke_drugs_cl <- importCodelist(here("Cohorts", "stroke_drugs"), type = "csv")

names(stroke_drugs_cl) <- paste0(names(stroke_drugs_cl), "_stroke")

cdm$stroke_drugs <- conceptCohort(
  cdm = cdm,
  conceptSet = stroke_drugs_cl,
  name = "stroke_drugs"
)

# collapse records that are within 14 days of each other
cdm$stroke_drugs <- cdm$stroke_drugs |>
  collapseCohorts(gap = 14,
                  name = "stroke_drugs") 

cdm$stroke_drugs_first <- cdm$stroke_drugs |>
  requireCohortIntersect(
    targetCohortTable = "stroke",
    window = c(-30,0),
    name = "stroke_drugs_first"
  ) |>
  requireIsFirstEntry() |>
  requireInDateRange(study_period) |>
  requireAge(ageRange = c(18,150))

cdm$stroke_drugs_after_event <- cdm$stroke_drugs |>
  inner_join(cdm$stroke_drugs_first |> select(subject_id, cohort_definition_id, start_date = cohort_start_date), 
             by = c("subject_id", "cohort_definition_id")) |>
  filter(cohort_start_date >= start_date) |>
  select(-c(start_date)) |>
  compute(name = "stroke_drugs_after_event", temporary = FALSE)

drug_count_after_stroke <- cdm$stroke_drugs_after_event |>
  collect() |>
  group_by(cohort_definition_id) |>
  distinct(subject_id) |>
  tally() |>
  filter(n >= 100)

cdm$stroke_drugs_final <- cdm$stroke_drugs_after_event |>
  subsetCohorts(cohortId = drug_count_after_stroke$cohort_definition_id,
                name = "stroke_drugs_final")

info(logger, "INSTANTIATED STROKE DRUGS COHORT")

## prior afib and no afib (bb users)



### combine into one table
  

info(logger, "INSTANTIATED CARDIOVASCULAR DRUGS COHORT")
