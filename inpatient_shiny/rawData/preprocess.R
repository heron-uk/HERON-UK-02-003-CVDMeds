# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period", observation_period_ordinal = "all"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_count = list(result_type = "summarise_cohort_count"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_demographics = list(result_type = "summarise_characteristics", variable_name = c("Number records", "Number subjects", "Cohort start date", "Cohort end date", "Ses", "Ethnicity", "Age", "Age group", "Sex", "Prior observation", "Future observation", "Days in cohort", "Days to next record", "Prior ischemic stroke (-30 to -1)", "Prior mi (-30 to -1)")),
  summarise_treatments = list(result_type = "summarise_characteristics", variable_name = "Drug treatment (0, 14)"),
  summarise_procedures = list(result_type = "summarise_characteristics", variable_name = "Procedures (0, 14)")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))

resultChar <- result |>
  omopgenerics::filterSettings(result_type == "summarise_characteristics") |>
  dplyr::mutate(order_id = case_when(
    variable_name == "Number records" ~ 1L,
    variable_name == "Number subjects" ~ 2L,
    variable_name == "Cohort start date" ~ 3L,
    variable_name == "Cohort end date" ~ 4L,
    variable_name == "Sex" ~ 5L,
    variable_name == "Age" ~ 6L,
    variable_name == "Age group" ~ 7L,
    variable_name == "Ethnicity" ~ 8L,
    variable_name == "Ses" ~ 9L,
    variable_name == "Prior observation" ~ 10L,
    variable_name == "Future observation" ~ 11L,
    variable_name == "Days in cohort" ~ 12L,
    variable_name == "Days to next record" ~ 13L,
    variable_name == "Prior ischemic stroke (-30 to -1)" ~ 14L,
    variable_name == "Prior mi (-30 to -1)" ~ 15L,
    variable_name == "Drug treatment (0, 14)" ~ 16L,
    variable_name == "Procedures (0, 14)" ~ 17L,
  )) |>
  omopgenerics::splitStrata() |>
  dplyr::mutate(
    strata = dplyr::case_when(
      age_group != "overall" ~ paste0("Age group: ", age_group),
      ses != "overall" ~ paste0("SES: ", ses),
      sex != "overall" ~ paste0("Sex: ", sex),
      .default = "overall"
    ),
    strata_id = dplyr::case_when(
      age_group != "overall" ~ 2L,
      ses != "overall" ~ 3L,
      sex != "overall" ~ 4L,
      .default = 1L
    )
  ) |>
  dplyr::arrange(cdm_name, group_level, strata_id, strata, order_id) |>
  dplyr::select(!c("order_id", "strata_id")) |>
  omopgenerics::uniteStrata("strata") |>
  dplyr::select(!c("age_group", "ses", "sex"))

result <- omopgenerics::bind(
  result |>
    omopgenerics::filterSettings(result_type != "summarise_characteristics"),
  resultChar
)

data <- prepareResult(result, resultList)

values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
