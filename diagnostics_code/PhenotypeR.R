
result <- phenotypeDiagnostics(cdm[["study_cohorts"]], 
                               populationDateRange = as.Date(c("2012-01-01", 
                                                               NA)))

# export the results
exportSummarisedResult(result, 
                      fileName = "results_drugs_{cdm_name}_{date}.csv",
                      path = here::here("Results"), 
                      minCellCount = minCellCount)
