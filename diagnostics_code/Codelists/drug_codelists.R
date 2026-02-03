
# beta blockers -------
beta_blockers <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("acebutolol", "alprenolol", "atenolol", 
           "bisoprolol", "carvedilol", "metoprolol", "nadolol",
           "oxprenolol", "pindolol", "propranolol", "timolol"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(beta_blockers) <- paste0("beta_blocker_", names(beta_blockers))

beta_blockers_all <- beta_blockers %>% 
  unionCodelists()
names(beta_blockers_all) <- "beta_blockers"

beta_blockers <- c(beta_blockers_all, beta_blockers) %>% 
  omopgenerics::newCodelist()

exportCodelist(beta_blockers, path = here::here("Cohorts", "drugs"),
               type = "csv")

# P2Y12 inhibitors -----
p2y12_inhibitors <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("clopidogrel", "ticagrelor", "prasugrel"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(p2y12_inhibitors) <- paste0("p2y12_inhibitors_", names(p2y12_inhibitors))


p2y12_inhibitors_all <- p2y12_inhibitors %>% 
  unionCodelists()
names(p2y12_inhibitors_all) <- "p2y12_inhibitors"

p2y12_inhibitors <- c(p2y12_inhibitors_all, p2y12_inhibitors) %>% 
  omopgenerics::newCodelist()

exportCodelist(p2y12_inhibitors, path = here::here("Cohorts", "drugs"),
               type = "csv")


# aspirin -------
aspirin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("aspirin"),
  nameStyle = "{concept_name}",
  type = "codelist")
exportCodelist(aspirin, path = here::here("Cohorts", "drugs"),
               type = "csv")


# ACEi and ARBs -------
acei_arbs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c(# acei
    "captopril", "enalapril", "fosinopril", 
    "imidapril", "lisinopril", "perindopril", "quinapril", 
    "ramipril", "trandolapril",
    # arbs
    "azilsartan", "candesartan", "eprosartan", "irbesartan",        
    "losartan", "olmesartan", "telmisartan", "valsartan"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(acei_arbs) <- paste0("acei_arbs_", names(acei_arbs))

acei_arbs_all <- acei_arbs %>% 
  unionCodelists()
names(acei_arbs_all) <- "acei_arbs"

acei_arbs <- c(acei_arbs_all, acei_arbs) %>% 
  omopgenerics::newCodelist()

exportCodelist(acei_arbs, path = here::here("Cohorts", "drugs"),
               type = "csv")

# Diuretic (thiazide) ----
thiazide_diuretic <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("indapamide", "chlorthalidone", "metolazone",
           "xipamide", "bendroflumethiazide"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(thiazide_diuretic) <- paste0("thiazide_diuretic_", names(thiazide_diuretic))


thiazide_diuretic_all <- thiazide_diuretic %>% 
  unionCodelists()
names(thiazide_diuretic_all) <- "thiazide_diuretics"

thiazide_diuretic <- c(thiazide_diuretic_all, thiazide_diuretic) %>% 
  omopgenerics::newCodelist()

exportCodelist(thiazide_diuretic, path = here::here("Cohorts", "drugs"),
               type = "csv")

# Calcium-channel blocker ----
calcium_channel_blocker <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("amlodipine", "felodipine", "lacidipine", "lercanidipine", 
           "nicardipine", "nifedipine", "verapamil", "diltiazem"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(calcium_channel_blocker) <- paste0("calcium_channel_blocker_", names(calcium_channel_blocker))

calcium_channel_blocker_all <- calcium_channel_blocker %>% 
  unionCodelists()
names(calcium_channel_blocker_all) <- "calcium_channel_blockers"

calcium_channel_blocker <- c(calcium_channel_blocker_all, calcium_channel_blocker) %>% 
  omopgenerics::newCodelist()

exportCodelist(calcium_channel_blocker, path = here::here("Cohorts", "drugs"),
               type = "csv")

# antihyperensive ----
antihyperensives <- c(acei_arbs_all,
                      thiazide_diuretic_all,
                      calcium_channel_blocker_all) %>% 
  omopgenerics::newCodelist() %>% 
  unionCodelists()
names(antihyperensives) <- "antihyperensives"
exportCodelist(antihyperensives, path = here::here("Cohorts", "drugs"),
               type = "csv")

# statin -------
statin <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("atorvastatin", "rosuvastatin", "simvastatin", 
           "pravastatin", "Fluvastatin"),
  nameStyle = "{concept_name}",
  type = "codelist")
names(statin) <- paste0("statin_", names(statin))

statin_all <- statin %>% 
  unionCodelists()
names(statin_all) <- "statin"

statin <- c(statin_all, statin) %>% 
  omopgenerics::newCodelist()

exportCodelist(statin, path = here::here("Cohorts", "drugs"),
               type = "csv")
