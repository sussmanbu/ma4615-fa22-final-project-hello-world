library(tidyverse)
library(dplyr)

## Below is how we load and clean our data set.

## Main Dataset: Medicare Impatient Hospitals - By Providers and Service
## Load Main Dataset 
## Remove dollar signs to make data available

medicare_data <- read_csv(here::here("dataset", "Medicare_Data.csv") , 
                          col_types = cols( Avg_Mdcr_Pymt_Amt=col_number(), 
                                            Avg_Tot_Pymt_Amt=col_number(), 
                                            Avg_Submtd_Cvrd_Chrg=col_number()))

## Create a new global variable with the raw dataset. 
## Filter out the missing values and problems within the datasets.
## Select the data we need for analysis

medicare_data_clean <- medicare_data %>% 
  filter(!grepl('Unknown', medicare_data$Rndrng_Prvdr_RUCA_Desc)) %>%
  drop_na() %>%      
  select(-Rndrng_Prvdr_CCN, 
         -Rndrng_Prvdr_St, 
         -Rndrng_Prvdr_State_FIPS, 
         -Rndrng_Prvdr_Zip5, 
         -Rndrng_Prvdr_RUCA_Desc)

##Store the data for further usage.

write_csv(medicare_data_clean, file = here::here("dataset", "Medicare_Data_clean.csv"))
save(medicare_data_clean, file = here::here("dataset/Medicare_Data.RData"))

## Sub Dataset:  Medicare data listed by 52 groups of states
## Based on the main dataset, a new dataset is created to further analyze the data for each state.

medicare_data_sum<- medicare_data_clean %>%
  group_by(Rndrng_Prvdr_State_Abrvtn) %>%
  summarize(Mean_Discharge = mean(Tot_Dschrgs), 
            Mean_Covered = mean(Avg_Submtd_Cvrd_Chrg), 
            Mean_Total_Payment = mean(Avg_Tot_Pymt_Amt), 
            Mean_Medicare_Payment = mean(Avg_Mdcr_Pymt_Amt))

##Store the data for further usage.

write_csv(medicare_data_sum, file = here::here("dataset", "Medicare_Data_sum.csv"))
save(medicare_data_sum, file = here::here("dataset/Medicare_Data.RData"))

## Dataset 2: Medicare Premiums
## Load dataset 2 with the columns we need for dataset merge process.

premiumA <- read_csv(here::here("dataset", "PremiumA.csv"), 
                     col_types = cols(Total_Premium_Amount =col_number(),  
                                      Reduced_Base_Premium_Amount=col_number(), 
                                      Standard_Base_Premium_Amount = col_number()))

## Create a new global variable for the merged dataset)

medicare_premium_combined <- merge(premiumA,medicare_data_sum)
## Store the dataset for further usage.

write_csv(medicare_premium_combined, file = here::here("dataset", "medicare_premium_combined.csv"))
save(medicare_premium_combined, file = here::here("dataset/Medicare_Data.RData"))

##Dataset 3: GDP by states
GDP <- read_csv(here::here("dataset", "GDP.csv"), 
                col_types = cols_only(GDP=col_number(),  
                                      Rndrng_Prvdr_State_Abrvtn =col_character()))
## Store the dataset for further usage.
write_csv(GDP, file = here::here("dataset", "GDP.csv"))
save(GDP, file = here::here("dataset/GDP.RData"))
##Create a merged dataset
medicare_GDP_combined <- merge(GDP,medicare_data_sum)
write_csv(medicare_GDP_combined, file = here::here("dataset", "medicare_GDP_combined.csv"))
save(medicare_GDP_combined, file = here::here("dataset/medicare_GDP_combined.RData"))

medicare_data19 <- read_csv(here::here("dataset", "medicare-19.csv"))
save(medicare_data19, file = here::here("dataset/medicare-19.RData"))
medicare_data18 <- read_csv(here::here("dataset", "medicare-18.csv"))
save(medicare_data18, file = here::here("dataset/medicare-18.RData"))
medicare_data17 <- read_csv(here::here("dataset", "medicare-17.csv"))
save(medicare_data17, file = here::here("dataset/medicare-17.RData"))
medicare_data16 <- read_csv(here::here("dataset", "medicare-16.csv"))
save(medicare_data16, file = here::here("dataset/medicare-16.RData"))
medicare_data15 <- read_csv(here::here("dataset", "medicare-15.csv"))
save(medicare_data15, file = here::here("dataset/medicare-15.RData"))
medicare_data14 <- read_csv(here::here("dataset", "medicare-14.csv"))
save(medicare_data14, file = here::here("dataset/medicare-14.RData"))

##Dataset 4: Medicare Geographic Variation
Medicare_Geographic_Variation <- read_csv(here::here("dataset", "Medicare_Geographic_Variation.csv"), 
                col_types = cols(IP_MDCR_PYMT_AMT=col_number(),  
                                      IP_MDCR_PYMT_PCT =col_number(),
                                      IP_MDCR_PYMT_PC =col_number(),
                                      IP_MDCR_PYMT_PER_USER=col_number(),
                                      IP_MDCR_STDZD_PYMT_AMT=col_number(),
                                      IP_MDCR_STDZD_PYMT_PCT=col_number(),
                                      IP_MDCR_STDZD_PYMT_PC=col_number(),
                                      IP_MDCR_STDZD_PYMT_PER_USER=col_number(),
                                      BENES_IP_CVRD_STAY_CNT=col_number(),
                                      BENES_IP_PCT=col_number(),
                                      IP_CVRD_STAYS_PER_1000_BENES=col_number(),
                                      IP_CVRD_DAYS_PER_1000_BENES=col_number(),
                                      ACUTE_HOSP_READMSN_CNT=col_number(),
                                      ACUTE_HOSP_READMSN_PCT=col_number()
                                      ))
## Store the dataset for further usage.
write_csv(Medicare_Geographic_Variation, file = here::here("dataset", "Medicare_Geographic_Variation.csv"))
save(Medicare_Geographic_Variation, file = here::here("dataset/Medicare_Geographic_Variation.RData"))


##Dataset 5: GDP by states
GPCI2020 <- read_csv(here::here("dataset", "GPCI2020.csv"), 
                col_types = cols(PW_GPCI =col_number(),  
                                      PE_GPCI =col_number(),
                                      MP_GPCI =col_number()))
## Store the dataset for further usage.
write_csv(GPCI2020, file = here::here("dataset", "GPCI2020.csv"))
save(GPCI2020, file = here::here("dataset/GPCI2020.RData"))










