cdm$acute_mi_crm <- cdm$acute_mi_first |>
  requireInDateRange(study_period,
                     name = "acute_mi_crm") |>
  requireAge(ageRange = c(18,150))

# MI
antic_id <- settings(cdm$mi_drugs_final) |>
  filter(cohort_name == "anticoagulants_mi") |>
  pull(cohort_definition_id)


if(length(antic_id) == 1){
cdm$mi_crm_antic <- cdm$acute_mi_crm |>
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = antic_id,
    targetStartDate = "cohort_start_date",
    window = list(c(0, 30)),
    name = "mi_crm_antic"
  ) |>
  PatientProfiles::addCohortIntersectDays(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = antic_id,
    targetDate = "cohort_start_date",
    nameStyle = "{cohort_name}",
    window = list(c(0, 30))
  ) |>
  dplyr::rename("initiate_treatment" = "anticoagulants_mi_0_to_30",
                "days_to_treatment" = "anticoagulants_mi") |>
  PatientProfiles::addDeathDays(
    window = c(0, 30)
  ) |>
  compute(name = "mi_crm_antic", temporary = FALSE) |>
  renameCohort(newCohortName = "anticoagulants_mi")
} else {
  cdm$mi_crm_antic <- omopgenerics::emptyCohortTable(
    name = "mi_crm_antic"
  ) 
}

antih_id <- settings(cdm$mi_drugs_final) |>
  filter(cohort_name == "antihypertensive_mi") |>
  pull(cohort_definition_id)

if(length(antih_id) == 1){
cdm$mi_crm_antih <- cdm$acute_mi_crm |>
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = antih_id,
    targetStartDate = "cohort_start_date",
    window = list(c(0, 30)),
    name = "mi_crm_antih"
  ) |>
  PatientProfiles::addCohortIntersectDays(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = antih_id,
    targetDate = "cohort_start_date",
    nameStyle = "{cohort_name}",
    window = list(c(0, 30))
  ) |>
  dplyr::rename("initiate_treatment" = "antihypertensive_mi_0_to_30",
                "days_to_treatment" = "antihypertensive_mi") |>
  PatientProfiles::addDeathDays(
    window = c(0, 30)
  ) |>
  compute(name = "mi_crm_antih", temporary = FALSE) |>
  renameCohort(newCohortName = "antihypertensives_mi")
} else {
  cdm$mi_crm_antih <- omopgenerics::emptyCohortTable(
    name = "mi_crm_antih"
  ) 
}

antip_id <- settings(cdm$mi_drugs_final) |>
  filter(cohort_name == "antiplatelets_mi") |>
  pull(cohort_definition_id)

if(length(antip_id) == 1){
cdm$mi_crm_antip <- cdm$acute_mi_crm |>
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = antip_id,
    targetStartDate = "cohort_start_date",
    window = list(c(0, 30)),
    name = "mi_crm_antip"
  ) |>
  PatientProfiles::addCohortIntersectDays(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = antip_id,
    targetDate = "cohort_start_date",
    nameStyle = "{cohort_name}",
    window = list(c(0, 30))
  ) |>
  dplyr::rename("initiate_treatment" = "antiplatelets_mi_0_to_30",
                "days_to_treatment" = "antiplatelets_mi") |>
  PatientProfiles::addDeathDays(
    window = c(0, 30)
  ) |>
  compute(name = "mi_crm_antip", temporary = FALSE) |>
  renameCohort(newCohortName = "antiplatelets_mi")

} else {
  cdm$mi_crm_antip <- omopgenerics::emptyCohortTable(
    name = "mi_crm_antip"
  )
}

ll_id <- settings(cdm$mi_drugs_final) |>
  filter(cohort_name == "lipid_lowering_mi") |>
  pull(cohort_definition_id)

if(length(ll_id) == 1){
cdm$mi_crm_ll <- cdm$acute_mi_crm |>
  PatientProfiles::addCohortIntersectFlag(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = ll_id,
    targetStartDate = "cohort_start_date",
    window = list(c(0, 30)),
    name = "mi_crm_ll"
  ) |>
  PatientProfiles::addCohortIntersectDays(
    targetCohortTable = "mi_drugs_final",
    targetCohortId = ll_id,
    targetDate = "cohort_start_date",
    nameStyle = "{cohort_name}",
    window = list(c(0, 30))
  ) |>
  dplyr::rename("initiate_treatment" = "lipid_lowering_mi_0_to_30",
                "days_to_treatment" = "lipid_lowering_mi") |>
  PatientProfiles::addDeathDays(
    window = c(0, 30)
  ) |>
  compute(name = "mi_crm_ll", temporary = FALSE) |>
  renameCohort(newCohortName = "lipid_lowering_mi")
} else {
  cdm$mi_crm_ll <- omopgenerics::emptyCohortTable(
    name = "mi_crm_ll"
  )
}

cdm <- omopgenerics::bind(
  cdm$mi_crm_antic,
  cdm$mi_crm_antih,
  cdm$mi_crm_antip,
  cdm$mi_crm_ll,
  name = "mi_crm"
)
  
  
cdm$mi_crm <- cdm$mi_crm |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "mi_drugs_msm",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150))
  ) |>
  addSES()

x_mi <- cdm$mi_crm |> 
  collect() |>
  mutate(
    days_to_treatment = coalesce(days_to_treatment, 9999),
    days_to_death = coalesce(days_to_death, 9999)
  ) |>
  mutate(event = case_when(
    initiate_treatment == 1 ~ "treatment",
    !is.na(days_to_death) & is.na(days_to_treatment) | days_to_death < days_to_treatment ~ "death",
    TRUE ~ "censor"
  ),
  event = factor(event, levels = c("censor", "treatment", "death")),
  time = pmin(days_to_treatment, days_to_death, na.rm = TRUE)) |>
  mutate(
    sex = factor(sex),
    age_group = factor(age_group),
    ses = factor(ses),
    cohort_name = case_when(
      cohort_definition_id == 1 ~ "Anticoagulants - MI",
      cohort_definition_id == 2 ~ "Antihypertensives - MI",
      cohort_definition_id == 3 ~ "Antiplatelets - MI",
      cohort_definition_id == 4 ~ "Lipid Lowering - MI")
  )

#### Stroke

cdm$ischemic_stroke_crm <- cdm$stroke_first |>
  requireInDateRange(study_period,
                     name = "ischemic_stroke_crm") |>
  requireAge(ageRange = c(18,150))

antic_id_is <- settings(cdm$stroke_drugs_final) |>
  filter(cohort_name == "anticoagulants_stroke") |>
  pull(cohort_definition_id)


if(length(antic_id_is) == 1){
  cdm$stroke_crm_antic <- cdm$ischemic_stroke_crm |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = antic_id_is,
      targetStartDate = "cohort_start_date",
      window = list(c(0, 30)),
      name = "stroke_crm_antic"
    ) |>
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = antic_id_is,
      targetDate = "cohort_start_date",
      nameStyle = "{cohort_name}",
      window = list(c(0, 30))
    ) |>
    dplyr::rename("initiate_treatment" = "anticoagulants_stroke_0_to_30",
                  "days_to_treatment" = "anticoagulants_stroke") |>
    PatientProfiles::addDeathDays(
      window = c(0, 30)
    ) |>
    compute(name = "stroke_crm_antic", temporary = FALSE) |>
    renameCohort(newCohortName = "anticoagulants_stroke")
} else {
  cdm$stroke_crm_antic <- omopgenerics::emptyCohortTable(
    name = "stroke_crm_antic"
  ) 
}

antih_id_is <- settings(cdm$stroke_drugs_final) |>
  filter(cohort_name == "antihypertensive_stroke") |>
  pull(cohort_definition_id)

if(length(antih_id_is) == 1){
  cdm$stroke_crm_antih <- cdm$ischemic_stroke_crm |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = antih_id_is,
      targetStartDate = "cohort_start_date",
      window = list(c(0, 30)),
      name = "stroke_crm_antih"
    ) |>
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = antih_id_is,
      targetDate = "cohort_start_date",
      nameStyle = "{cohort_name}",
      window = list(c(0, 30))
    ) |>
    dplyr::rename("initiate_treatment" = "antihypertensive_stroke_0_to_30",
                  "days_to_treatment" = "antihypertensive_stroke") |>
    PatientProfiles::addDeathDays(
      window = c(0, 30)
    ) |>
    compute(name = "stroke_crm_antih", temporary = FALSE) |>
    renameCohort(newCohortName = "antihypertensives_stroke")
} else {
  cdm$stroke_crm_antih <- omopgenerics::emptyCohortTable(
    name = "stroke_crm_antih"
  ) 
}

antip_id_is <- settings(cdm$stroke_drugs_final) |>
  filter(cohort_name == "antiplatelets_stroke") |>
  pull(cohort_definition_id)

if(length(antip_id_is) == 1){
  cdm$stroke_crm_antip <- cdm$ischemic_stroke_crm |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = antip_id_is,
      targetStartDate = "cohort_start_date",
      window = list(c(0, 30)),
      name = "stroke_crm_antip"
    ) |>
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = antip_id_is,
      targetDate = "cohort_start_date",
      nameStyle = "{cohort_name}",
      window = list(c(0, 30))
    ) |>
    dplyr::rename("initiate_treatment" = "antiplatelets_stroke_0_to_30",
                  "days_to_treatment" = "antiplatelets_stroke") |>
    PatientProfiles::addDeathDays(
      window = c(0, 30)
    ) |>
    compute(name = "stroke_crm_antip", temporary = FALSE) |>
    renameCohort(newCohortName = "antiplatelets_stroke")
  
} else {
  cdm$stroke_crm_antip <- omopgenerics::emptyCohortTable(
    name = "stroke_crm_antip"
  )
}

ll_id_is <- settings(cdm$stroke_drugs_final) |>
  filter(cohort_name == "lipid_lowering_stroke") |>
  pull(cohort_definition_id)

if(length(ll_id_is) == 1){
  cdm$stroke_crm_ll <- cdm$ischemic_stroke_crm |>
    PatientProfiles::addCohortIntersectFlag(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = ll_id_is,
      targetStartDate = "cohort_start_date",
      window = list(c(0, 30)),
      name = "stroke_crm_ll"
    ) |>
    PatientProfiles::addCohortIntersectDays(
      targetCohortTable = "stroke_drugs_final",
      targetCohortId = ll_id_is,
      targetDate = "cohort_start_date",
      nameStyle = "{cohort_name}",
      window = list(c(0, 30))
    ) |>
    dplyr::rename("initiate_treatment" = "lipid_lowering_stroke_0_to_30",
                  "days_to_treatment" = "lipid_lowering_stroke") |>
    PatientProfiles::addDeathDays(
      window = c(0, 30)
    ) |>
    compute(name = "stroke_crm_ll", temporary = FALSE) |>
    renameCohort(newCohortName = "lipid_lowering_stroke")
} else {
  cdm$stroke_crm_ll <- omopgenerics::emptyCohortTable(
    name = "stroke_crm_ll"
  )
}

cdm <- omopgenerics::bind(
  cdm$stroke_crm_antic,
  cdm$stroke_crm_antih,
  cdm$stroke_crm_antip,
  cdm$stroke_crm_ll,
  name = "stroke_crm"
)


cdm$stroke_crm <- cdm$stroke_crm |>
  addDemographics(
    sex = TRUE,
    age = FALSE,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "stroke_drugs_msm",
    ageGroup = list(
      "18 to 39" = c(18, 39),
      "40 to 49" = c(40, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150))
  ) |>
  addSES()

x_stroke <- cdm$stroke_crm |> 
  collect() |>
  mutate(
    days_to_treatment = coalesce(days_to_treatment, 9999),
    days_to_death = coalesce(days_to_death, 9999)
  ) |>
  mutate(event = case_when(
    initiate_treatment == 1 ~ "treatment",
    !is.na(days_to_death) & is.na(days_to_treatment) | days_to_death < days_to_treatment ~ "death",
    TRUE ~ "censor"
  ),
  event = factor(event, levels = c("censor", "treatment", "death")),
  time = pmin(days_to_treatment, days_to_death, na.rm = TRUE)) |>
  mutate(
    sex = factor(sex),
    age_group = factor(age_group),
    ses = factor(ses),
    cohort_name = case_when(
      cohort_definition_id == 1 ~ "Anticoagulants - Stroke",
      cohort_definition_id == 2 ~ "Antihypertensives - Stroke",
      cohort_definition_id == 3 ~ "Antiplatelets - Stroke",
      cohort_definition_id == 4 ~ "Lipid Lowering - Stroke")
  )

x <- rbind(x_mi, x_stroke)

### CR Model

crm_results <- list()

cohorts <- unique(x$cohort_name)

for(coh in cohorts){
  
msdata <- x |>
    filter(cohort_name == coh) |>
  mutate(
    sex = relevel(factor(sex), ref = "Female"),
    age_group = relevel(factor(age_group), ref = "50 to 59"),
    ses = relevel(factor(ses), ref = "5")
  )

cli::cli_inform(c(i = "Fitting CR model for {.pkg {coh}}"))

cif <- tidycmprsk::cuminc(Surv(time, event) ~ 1, data = x)

cif_df <- cif |>
  broom::tidy() |>
  filter(time <= 30) |>
  select(time, outcome, estimate) |>
  pivot_wider(names_from = outcome, values_from = estimate, values_fill = 0) |>
  mutate(
    no_treatment = 1 - treatment - death
  )|>
  pivot_longer(
    cols = c(treatment, death, no_treatment),
    names_to = "state",
    values_to = "prob"
  ) |>
  mutate(cohort_name = coh,
         result_type = "crm_probabilities")

cif_sr <- omopgenerics::transformToSummarisedResult(
  x = cif_df,
  group = c("cohort_name"),
  estimates = c("prob"),
  additional = c("time", "state"),
  settings = c("result_type")
) |>
  mutate(cdm_name = omopgenerics::cdmName(cdm))

crm_results[[paste0("crm_prob_",coh, "_mi")]] <- cif_sr
###

sex_count <- length(unique(msdata$sex))
ses_count <- length(unique(msdata$ses))
age_group_count <- length(unique(msdata$age_group))

if (sex_count < 2 & ses_count < 2 & age_group_count < 2) {
  cli::cli_alert_info("Insufficient levels in strata for cohort {.pkg {coh}}. Skipping adjusted CR model.")
} else {
  
cr_model <- tidycmprsk::crr(
  Surv(time, event) ~ age_group + sex + ses,
  data = msdata,
  failcode = "treatment"
)

tidy_adj_res <- tidy(cr_model) |>
  select("variable_level" = "term", "coef" = "estimate", "se" = "std.error") |>
  mutate(
    cohort_name = coh,
    cdm_name = omopgenerics::cdmName(cdm),
    variable_name = "Competing risk coefficients",
    result_type = "cr_coefficients"
  ) |>
  omopgenerics::transformToSummarisedResult(
    group = c("cohort_name"),
    estimates = c("coef", "se"),
    settings = c("result_type")
  )

crm_results[[paste0("mrm_coef_",coh, "_mi")]] <- tidy_adj_res

}
}
