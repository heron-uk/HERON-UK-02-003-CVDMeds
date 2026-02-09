info(logger, "GET INPATIENT COHORT")

cdm$inpatient_visit <- conceptCohort(
  cdm = cdm,
  conceptSet = list(inpatient = c(9201, 262, 9203)),
  name = "inpatient_visit"
) 

info(logger, "GOT INPATIENT COHORT")