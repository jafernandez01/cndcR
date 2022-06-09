
# load data from Ciampitti et al. (2022) for a case-study on maize
# First, convert kg to Mg, renaming variables, calculate Na from Nupt values, and filter data above 1Mgha-1 until 25d after R1
Data <- readxl::read_xlsx("data-raw/maize_dataset.xlsx", sheet = 1)   %>%
  mutate(W = W_kg_ha*.001, Site_year = paste(Id ,Year, Sites, sep= "_"))   %>%  #
  dplyr::rename(Na = `N(%)`, Samp = `Sampling time`, Nrates = Nrates_kg_ha)   %>%
  mutate(Na = if_else(!is.na(Na), Na, 100*Nupt_kg_ha/W_kg_ha ))   %>%
  filter(W > 1)   %>%
  filter(is.na(Observation)|Observation!="R5")   %>%
  filter(is.na(Observation)|Observation!="R6")   %>%
  separate(Observation, c("Days","Stage"))   %>%
  mutate(Days = as.numeric(Days))   %>%
  filter(Id != "12" | case_when(
    Site_year=="12_1990_Onard" ~ Days <= unique(na.omit(.$Days[.$Stage=="R1" & .$Site_year == "12_1990_Onard"])) + 25 ,
    Site_year=="12_1991_Onard" ~ Days <= unique(na.omit(.$Days[.$Stage=="R1" & .$Site_year == "12_1991_Onard"])) + 25,
    Site_year=="12_1993_Onard" ~ Days <= unique(na.omit(.$Days[.$Stage=="R1" & .$Site_year == "12_1993_Onard"])) + 25,
    TRUE ~ Id == "12"))


# Add number of replicates for the studies and handling conversion from SE and LSD to variance values
Data <- Data  %>%
  mutate(Repetitions = if_else(Id == 12, 3, as.numeric(Repetitions)),
         dfs = if_else(Id == 12, 3-1, as.numeric(Repetitions)-1 ))  %>%
  mutate(t.val = if_else(is.na(LSD_observation), qt(0.975, dfs, lower.tail=TRUE),qt(0.95, dfs, lower.tail=TRUE))) %>%
  mutate(W_LSD = as.numeric(W_LSD), Nconc_LSD = as.numeric(Nconc_LSD), Nupt_LSD = as.numeric(Nupt_LSD),
         Nconc_LSD = if_else(!is.na(Nconc_LSD), Nconc_LSD, 100*Nupt_LSD/W_kg_ha ),
         Na_SE = Nconc_SE_Max - Nconc_SE_min,
         W_SE = W_SE_Max - W_SE_min,
         W_SE = if_else(!is.na(W_SE), W_SE, W_LSD/(t.val *sqrt(2))),
         W_SE = W_SE*.001,
         Na_SE = if_else(!is.na(Na_SE), Na_SE, Nconc_LSD/(t.val *sqrt(2)))) %>%
  mutate(Na_SE = Na_SE*sqrt(Repetitions), W_SE = W_SE*sqrt(Repetitions) )


# load data of reviewed published studies on CNDC
biblioCNDC <- readxl::read_xlsx("data-raw/bibliographic_dataset.xlsx",
                         col_types = c("skip", "text", "numeric", "text",
                                       "text", "text", "numeric", "numeric",
                                       "numeric", "numeric","text", "text",
                                       "text", "text", "numeric","numeric",
                                       "text", "text", "text", "skip")) %>%
  filter(studyType == "CAL")

validationSet <- readxl::read_xlsx("data-raw/maize_validationset.xlsx", sheet = 1)  %>%
  dplyr::mutate(W = .data$W_kg_ha * .001)  %>%
  dplyr::filter(.data$W > 1)

# save the data
#usethis::use_data(biblioCNDC, overwrite = TRUE)
#usethis::use_data(Data, overwrite = TRUE)
#usethis::use_data(validationSet, overwrite = TRUE)
