
library(tidyverse)
library(mice)
library(car)
library(randomForest)

# df <- read.csv("selected_data.csv")
train_in <- read.csv("Train_Inpatientdata-1542865627584.csv")
train_out <- read.csv("Train_Outpatientdata-1542865627584.csv")
train_ben <- read.csv("Train_Beneficiarydata-1542865627584.csv")

test_in <- read.csv("Test_Inpatientdata-1542969243754.csv")
test_out <- read.csv("Test_Outpatientdata-1542969243754.csv")
test_ben <- read.csv("Test_Beneficiarydata-1542969243754.csv")

fraud_prv <- read.csv("Train-1542865627584.csv")


# Convert strings to dates
train_ben$DOB <- as.Date(train_ben$DOB, format="%Y-%m-%d")
train_ben$DOD <- as.Date(train_ben$DOD, format="%Y-%m-%d")
test_ben$DOB <- as.Date(test_ben$DOB, format="%d/%m/%Y")
test_ben$DOD <- as.Date(test_ben$DOD, format="%d/%m/%Y")

train_in$AdmissionDt <- as.Date(train_in$AdmissionDt, format="%d/%m/%Y")
train_in$DischargeDt <- as.Date(train_in$DischargeDt, format="%d/%m/%Y")
test_in$AdmissionDt <- as.Date(test_in$AdmissionDt, format="%Y-%m-%d")
test_in$DischargeDt <- as.Date(test_in$DischargeDt, format="%Y-%m-%d")

# Append data frames
df_claim <- bind_rows(train_in, test_in, train_out, test_out) %>% distinct()
df_ben <- bind_rows(train_ben, test_ben) %>% distinct()


######## Process df_ben
# Create column of Age
df_ben$Age <- ifelse(is.na(df_ben$DOD),
                     floor(as.numeric(difftime(as.Date("2009-12-01"), df_ben$DOB, units = "days"))/365),
                     floor(as.numeric(difftime(df_ben$DOD, df_ben$DOB, units = "days"))/365))

# Adjust columns of diseases to binary values 0-1
cols_sub <- c("ChronicCond_Alzheimer","ChronicCond_Heartfailure", "ChronicCond_KidneyDisease", 
              "ChronicCond_Cancer", "ChronicCond_ObstrPulmonary", "ChronicCond_Depression", 
              "ChronicCond_Diabetes", "ChronicCond_IschemicHeart", "ChronicCond_Osteoporasis", 
              "ChronicCond_rheumatoidarthritis", "ChronicCond_stroke")
df_ben <- df_ben %>% 
  mutate(across(all_of(cols_sub), ~ifelse(. == 2, 0, .))) %>%
  mutate(RenalDiseaseIndicator = ifelse(RenalDiseaseIndicator == "Y", "1", RenalDiseaseIndicator))

# Convert column RenalDiseaseIndicator to numeric
df_ben$RenalDiseaseIndicator <- as.numeric(df_ben$RenalDiseaseIndicator)

# Count diseases per patient
df_ben$Diseases_cnt <- df_ben %>% 
  select(all_of(cols_sub), RenalDiseaseIndicator) %>% rowSums()



# Merge all data frames
df_all <- inner_join(df_claim, df_ben, by = "BeneID") %>% 
  inner_join(fraud_prv, by="Provider")

# Create column of patient days
df_all$PatientDays <- as.numeric(difftime(df_all$DischargeDt, df_all$AdmissionDt, 
                                            units = "days")) + 1


# Count involved physicians for each claim
df_all <- df_all %>% rowwise() %>% 
  mutate(Phys_cnt = n_distinct(c_across(AttendingPhysician:OtherPhysician), na.rm = TRUE))


# Add column of IsDead
df_all$IsDead <- ifelse(is.na(df_all$DOD), 0, 1)

# Check if there were any actions after death
date_columns <- c('AdmissionDt', 'DischargeDt', 'ClaimStartDt', 'ClaimEndDt')

# Convert different date formats to a consistent format
df_all$ClaimEndDt <- ifelse(grepl("/", df_all$ClaimEndDt),
                              as.Date(df_all$ClaimEndDt, format = "%d/%m/%Y"),
                              as.Date(df_all$ClaimEndDt, format="%Y-%m-%d"))
df_all$ClaimEndDt <- as.Date(df_all$ClaimEndDt)

# df_all <- left_join(df_all, select(df_claim, c("ClaimID", "ClaimStartDt")), by = "ClaimID")
# df_all <- df_all %>% select(-one_of("ClaimStartDt.x", "ClaimStartDt.y", "ClaimEndDt_1"))

df_all$ClaimStartDt <- ifelse(grepl("/", df_all$ClaimStartDt.y),
                              as.Date(df_all$ClaimStartDt.y, format = "%d/%m/%Y"),
                              as.Date(df_all$ClaimStartDt.y, format="%Y-%m-%d"))
df_all$ClaimStartDt <- as.Date(df_all$ClaimStartDt)

# Calculate the differences in days for each date column and the DOD
date_differences <- sapply(date_columns, function(col) {
  as.numeric(difftime(df_all[[col]], df_all$DOD, units = "days"))
})

df_all$DeadActions <- ifelse(rowSums(date_differences > 0, na.rm = TRUE) > 0, 1, 0)

# claim days
df_all$ClaimDays <- as.integer(difftime(df_all$ClaimEndDt, df_all$ClaimStartDt, units = "days")) + 1


## Count number of procedures 'ProcedureOfNumber' for each claim
ClmProcedure_vars <- paste0("ClmProcedureCode_", 1:6)
df_all$Proc_cnt <- rowSums(!is.na(df_all[ClmProcedure_vars]))


#Count number of claims, extra reported claims and unique
ClmDiagnosisCode_vars <- paste0("ClmDiagnosisCode_", 1:10)
df_all$Diag_cnt <- rowSums(!is.na(df_all[ClmDiagnosisCode_vars]))
# keep_columns_1 <- c('BeneID', 'ClaimID', 'ClmAdmitDiagnosisCode', 'ClaimsOfNumber', ClmDiagnosisCode_vars)
# df_all$ClaimsOfNumber <- rowSums(!is.na(df_all[, ClmDiagnosisCode_vars]))
# non_unique_ClaimsOfNumber <- df_all[keep_columns_1][df_all$ClaimsOfNumber != uniq(df_all[, ClmDiagnosisCode_vars]), ]
# head(non_unique_ClaimsOfNumber) #got unique claims
# df_all$UniqueClaimsOfNumber <- uniq(df_all[, ClmDiagnosisCode_vars]) #calculate unique claims
# df_all$ExtraClaimsOfNumber <- df_all$ClaimsOfNumber - df_all$UniqueClaimsOfNumber



# person hospitalize or not with DiagnosisGroupCode/ PatientDays
df_all$HospitaliseFlag <- ifelse(!is.na(df_all$PatientDays), 1, 0)


# Drop columns
df_all <- df_all[, !(names(df_all) %in% c(ClmDiagnosisCode_vars,
                                          ClmProcedure_vars,
                                          "DiagnosisGroupCode"))]

# Group data of patients by provider
provider_with_patient <- df_all %>%
  select(Provider, BeneID, Age, RenalDiseaseIndicator, all_of(cols_sub), IsDead, NoOfMonths_PartACov,
         NoOfMonths_PartBCov, IPAnnualReimbursementAmt, IPAnnualDeductibleAmt,
         OPAnnualReimbursementAmt, OPAnnualDeductibleAmt, Diseases_cnt) %>% 
  distinct() %>% 
  group_by(Provider) %>%
  summarize(
    Count_Patients = n_distinct(BeneID),
    Mean_Age = mean(Age),
    Count_Renal = sum(RenalDiseaseIndicator),
    Count_Alzheimer = sum(ChronicCond_Alzheimer),
    Count_Heartfailure = sum(ChronicCond_Heartfailure),
    Count_KidneyDisease = sum(ChronicCond_KidneyDisease),
    Count_Cancer = sum(ChronicCond_Cancer),
    Count_ObstrPulmonary = sum(ChronicCond_ObstrPulmonary),
    Count_Depression = sum(ChronicCond_Depression),
    Count_Diabetes = sum(ChronicCond_Diabetes),
    Count_IschemicHeart = sum(ChronicCond_IschemicHeart),
    Count_Osteoporasis = sum(ChronicCond_Osteoporasis),
    Count_rheumatoidarthritis = sum(ChronicCond_rheumatoidarthritis),
    Count_stroke = sum(ChronicCond_stroke),
    Mean_NoOfDiseases = mean(Diseases_cnt),
    Count_IsDead = sum(IsDead),
    Mean_NoOfMonths_PartACov = mean(NoOfMonths_PartACov),
    Mean_NoOfMonths_PartBCov = mean(NoOfMonths_PartBCov),
    Sum_IPAnnualReimbursementAmt = sum(IPAnnualReimbursementAmt),
    Sum_IPAnnualDeductibleAmt = sum(IPAnnualDeductibleAmt),
    Sum_OPAnnualReimbursementAmt = sum(OPAnnualReimbursementAmt),
    Sum_OPAnnualDeductibleAmt = sum(OPAnnualDeductibleAmt)
  )


# Group data of claims by provider
provider_with_claim <- df_all %>%
  group_by(Provider) %>%
  summarize(
    Count_IP_Claims = sum(HospitaliseFlag),
    Count_OP_CLaims = sum(is.na(PatientDays)),
    Count_Phys = n_distinct(AttendingPhysician, OperatingPhysician, OtherPhysician),
    Mean_PatientDays = mean(PatientDays, na.rm = TRUE),
    Mean_ClaimDays = mean(ClaimDays),
    Mean_NoOfInvolvedPhys = mean(Phys_cnt),
    Sum_InscClaimAmtReimbursed = sum(InscClaimAmtReimbursed),
    Sum_DeductibleAmtPaid = sum(DeductibleAmtPaid),
    Mean_NoOfProcedures = mean(Proc_cnt),
    Mean_NoOfDiagnosis = mean(Diag_cnt)
  )

# Merge data frames of provider
df_provider <- merge(provider_with_patient, provider_with_claim, by = "Provider", all.x = TRUE) %>%
  inner_join(fraud_prv, by="Provider")

# Check missing data to impute (Mean_PatientDays, Sum_DeductibleAmtPaid)
summary(df_provider)

# If a provider has missing data of Mean_PatientDays,
# it means all its claims are from outpatients => Patient Days = 0
df_provider_2 <- df_provider %>% 
  mutate(Mean_PatientDays = ifelse(is.na(Mean_PatientDays), 0, Mean_PatientDays)) %>% 
  # Convert class labels to binary value
  mutate(PotentialFraud = ifelse(PotentialFraud == "Yes", 1, 0))

# Impute missing data of Sum_DeductibleAmtPaid
df_provider_impute <- df_provider_2 %>% 
  select(-Provider) %>% 
  mice(data = ., m=3, method = 'pmm', seed = 500)

# Explore imputed values
df_provider_impute$imp$Sum_DeductibleAmtPaid

# Select second complete dataset (out of 3)
df_provider_complete <- complete(df_provider_impute, 2)

# Logistic regression to detect collinearity
logreg_model <- glm(formula = PotentialFraud ~ ., data = df_provider_complete, family = binomial)
vif <- vif(logreg_model)

# Correlation
cor_matrix <- df_provider_complete %>% select(-PotentialFraud) %>% 
  cor(., method = c("pearson", "kendall", "spearman"))

# Feature importance by Random Forest model
df_provider_complete$PotentialFraud <- as.factor(df_provider_complete$PotentialFraud)
rf_model <- randomForest(PotentialFraud ~ ., data = df_provider_complete)

feature_importance <- importance(rf_model)
varImpPlot(rf_model)


##====

write.csv(df_provider_complete, "selected_data.csv", row.names = FALSE)
write.csv(df_provider, "df_provider_non_imputed.csv", row.names = FALSE)


