
setwd(r"(C:\Users\Spring 2023\BUSN41201_BigData\FINAL PROJECT)")

diab_dt <- fread("diabetic_data.csv")

### PRIMARY DATASET CLEAN UP ###

# replacing "?"/"None"/"Unknown/Invalid" with NAs
diab_dt <- as.data.frame(apply(diab_dt, 2, function(x) ifelse(x == "?", NA, x)))
diab_dt <- as.data.frame(apply(diab_dt, 2, function(x) ifelse(x == "NULL", NA, x)))

diab_dt$gender <- 
    ifelse(diab_dt$gender == "Unknown/Invalid", NA, 
           ifelse(diab_dt$gender == "Female", 1, 0))
setnames(diab_dt, "gender", "gender_female")

# remove trailing and leading whitespace
diab_dt <- data.frame(lapply(diab_dt, trimws), stringsAsFactors = FALSE)

# admission_type_id mapping
admission_type_id_map <- c('1'='Urgent',
                           '2'='Urgent',
                           '3'='Non-Urgent',
                           '4'='Non-Urgent',
                           '5'=NA,
                           '6'=NA,
                           '7'='Urgent',
                           '8'=NA)


diab_dt$admission_type_id_char <- admission_type_id_map[as.character(diab_dt$admission_type_id)]

# discharge_disposition_id mapping

discharge_disposition_id_map <- c("1"="Discharge Home",
                                  "2"="Discharge Hospital Facility",
                                  "3"="Discharge Hospital Facility",
                                  "4"="Discharge Hospital Facility",
                                  "5"="Discharge Hospital Facility",
                                  "6"="Discharge Home",
                                  "7"="Discharge Hospital Facility",
                                  "8"="Discharge Home",
                                  "9"="Discharge Hospital Facility",
                                  "10"="Discharge Hospital Facility",
                                  "11"="Expired",
                                  "12"="Discharge Hospital Facility",
                                  "13"="Hospice",
                                  "14"="Hospice",
                                  "15"="Discharge Hospital Facility",
                                  "16"="Discharge Hospital Facility",
                                  "17"="Discharge Hospital Facility",
                                  "18"=NA,
                                  "19"="Expired",
                                  "20"="Expired",
                                  "21"="Expired",
                                  "22"="Discharge Hospital Facility",
                                  "23"="Discharge Hospital Facility",
                                  "24"="Discharge Hospital Facility",
                                  "25"=NA,
                                  "26"=NA,
                                  "27"="Discharge Hospital Facility",
                                  "28"="Discharge Hospital Facility",
                                  "29"="Discharge Hospital Facility",
                                  "30"="Discharge Hospital Facility")


diab_dt$discharge_disposition_id_char <- discharge_disposition_id_map[as.character(diab_dt$discharge_disposition_id)]

# admission_source_id mapping

admission_source_id_map <- c("1"="Non-Emergency",
                                  "2"="Non-Emergency",
                                  "3"="Non-Emergency",
                                  "4"="Non-Emergency",
                                  "5"="Non-Emergency",
                                  "6"="Non-Emergency",
                                  "7"="Emergency",
                                  "8"="Non-Emergency",
                                  "9"=NA,
                                  "10"="Non-Emergency",
                                  "11"="Non-Emergency",
                                  "12"="Non-Emergency",
                                  "13"="Non-Emergency",
                                  "14"="Non-Emergency",
                                  "15"="Non-Emergency",
                                  "17"=NA,
                                  "18"=NA,
                                  "19"="Non-Emergency",
                                  "20"=NA,
                                  "21"=NA,
                                  "22"="Non-Emergency",
                                  "23"="Non-Emergency",
                                  "24"="Non-Emergency",
                                  "25"="Non-Emergency",
                                  "26"="Non-Emergency")

diab_dt$admission_source_id_char <- admission_source_id_map[as.character(diab_dt$admission_source_id)]

# age mapping
age_map <- c("[0-10)"=5,
             "[10-20)" = 15,
             "[20-30)" = 25,
             "[30-40)" = 35,
             "[40-50)" = 45,
             "[50-60)" = 55,
             "[60-70)" = 65,
             "[70-80)" = 75,
             "[80-90)" = 85,
             "[90-100)" = 95)

diab_dt$age_mean <- factor(age_map[as.character(diab_dt$age)])

# creating binary_readmitted outcome variable

diab_dt$readmitted_binary <- ifelse((diab_dt$readmitted == "NO" | diab_dt$readmitted == ">30"),0,1)

diab_dt <- diab_dt[order(diab_dt$encounter_id),]
diab_dt <- diab_dt[!duplicated(diab_dt$patient_nbr),]

diab_dt <- subset(diab_dt, select=-c(acetohexamide,citoglipton,       
                                     examide,glipizide.metformin,glimepiride.pioglitazone,
                                     metformin.pioglitazone,metformin.rosiglitazone,miglitol,tolbutamide,tolazamide,troglitazone))

diab_dt$change <- ifelse(diab_dt$change== "No", 0, 1)

diab_dt$diabetesMed <- ifelse(diab_dt$diabetesMed== "No", 0, 1)

diab_dt$unfavorable_outcome <- ifelse(diab_dt$discharge_disposition_id_char == "Expired" |
                                        diab_dt$discharge_disposition_id_char == "Hospice", 1, 0)

diab_dt$primary_diag_diabetes <- ifelse(diab_dt$diag_1 %like% "^250", 1, 0)

diab_dt$A1Cresult_measured <- ifelse(diab_dt$A1Cresult == "None", 0, 1)


ids_lookup_dt <- fread("IDs_mapping.csv")


### CREATE DATA TABLE OF ONLY FACTORS AND NUMERIC ###
diab_dt_num <- data.table(diab_dt)

diab_dt_num <- mutate(diab_dt_num, 
                      race = as.factor(diab_dt_num$race),
                      gender_female = as.factor(gender_female),
                      admission_type_id_char = as.factor(admission_type_id_char),
                      discharge_disposition_id_char = as.factor(discharge_disposition_id_char),
                      max_glu_serum = as.factor(max_glu_serum),
                      metformin = as.factor(metformin),
                      glimepiride = as.factor(glimepiride),
                      glyburide = as.factor(glyburide),
                      pioglitazone = as.factor(pioglitazone),
                      rosiglitazone = as.factor(rosiglitazone),
                      insulin = as.factor(insulin),
                      change = as.factor(change),
                      diabetesMed = as.factor(diabetesMed))

diab_dt_hot <- one_hot(diab_dt_num, cols = 
                         c("race", "gender_female", "admission_type_id_char","discharge_disposition_id_char",
                           "max_glu_serum","metformin","glimepiride","glyburide","pioglitazone","rosiglitazone","insulin","diabetesMed","age_mean","change"))
                      
char_cols <- sapply(diab_dt_hot, is.character)

diab_dt_hot <- diab_dt_hot[, !char_cols, with=FALSE]

diab_dt_hot <- diab_dt_hot[complete.cases(diab_dt_hot)]

