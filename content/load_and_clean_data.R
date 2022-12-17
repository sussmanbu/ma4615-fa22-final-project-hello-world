library(tidyverse)
library(dplyr)



## Below is how we load and clean our data set.

## Dataset 1: Medicare Impatient Hospitals - By Providers and Service
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
            Mean_Medicare_Payment = mean(Avg_Mdcr_Pymt_Amt),
            Medicare_Coverage = Mean_Medicare_Payment/Mean_Total_Payment)

##Store the data for further usage.

write_csv(medicare_data_sum, file = here::here("dataset", "Medicare_Data_sum.csv"))
save(medicare_data_sum, file = here::here("dataset/Medicare_Data.RData"))

## Dataset 2: Medicare Premiums
## Load dataset 2 with the columns we need for dataset merge process.

premiumA <- read_csv(here::here("dataset", "PremiumA.csv"), 
                     col_types = cols(Total_Premium_Amount =col_number(),  
                                      Reduced_Base_Premium_Amount=col_number(), 
                                      Standard_Base_Premium_Amount = col_number())) %>%
  rename(state = Rndrng_Prvdr_State_Abrvtn) %>%
  mutate(premium_avg = Total_Premium_Amount/Total_Premium_Number_of_Beneficiaries)

## Create a new global variable for the merged dataset)

medicare_premium_combined <- merge(premiumA,medicare_data_sum)
## Store the dataset for further usage.

write_csv(medicare_premium_combined, file = here::here("dataset", "medicare_premium_combined.csv"))
save(medicare_premium_combined, file = here::here("dataset/Medicare_Data.RData"))

##Dataset 3: GDP by states
GDP <- read_csv(here::here("dataset", "GDP.csv"), 
                col_types = cols_only(GDP=col_number(),  
                                      state =col_character()))
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


##Dataset 5: GPCI2018-2020 by states
GPCI2020 <- read_csv(here::here("dataset", "GPCI2020.csv"), 
                col_types = cols_only(state = col_character(),
                                      PW_GPCI =col_number(),  
                                      PE_GPCI =col_number(),
                                      MP_GPCI =col_number()))
GPCI2020 <- GPCI2020 %>% group_by(state) %>% summarise(PW_GPCI=mean(PW_GPCI),PE_GPCI=mean(PE_GPCI),MP_GPCI=mean(MP_GPCI))
GPCI2019 <- read_csv(here::here("dataset", "GPCI2019.csv"), 
                     col_types = cols_only(state = col_character(),
                                           PW_GPCI =col_number(),  
                                           PE_GPCI =col_number(),
                                           MP_GPCI =col_number()))
GPCI2019 <- GPCI2019 %>% group_by(state) %>% summarise(PW_GPCI=mean(PW_GPCI),PE_GPCI=mean(PE_GPCI),MP_GPCI=mean(MP_GPCI)) %>% drop_na()
GPCI2018 <- read_csv(here::here("dataset", "GPCI2018.csv"), 
                     col_types = cols_only(state = col_character(),
                                           PW_GPCI =col_number(),  
                                           PE_GPCI =col_number(),
                                           MP_GPCI =col_number()))
GPCI2018 <- GPCI2018 %>% group_by(state) %>% summarise(PW_GPCI=mean(PW_GPCI),PE_GPCI=mean(PE_GPCI),MP_GPCI=mean(MP_GPCI)) %>% drop_na()

## Store the dataset for further usage.
write_csv(GPCI2020, file = here::here("dataset", "GPCI2020.csv"))
save(GPCI2020, file = here::here("dataset/GPCI2020.RData"))
write_csv(GPCI2019, file = here::here("dataset", "GPCI2019.csv"))
save(GPCI2019, file = here::here("dataset/GPCI2019.RData"))
write_csv(GPCI2018, file = here::here("dataset", "GPCI2018.csv"))
save(GPCI2018, file = here::here("dataset/GPCI2018.RData"))

##Dataset 6: HCC_Readmission_Only
HCC_Readmission <- read_csv(here::here("dataset/Medicare_Geographic_Variation_by_National_State_County.csv"),
                            col_types = cols(BENE_AVG_RISK_SCRE = col_double(),ACUTE_HOSP_READMSN_PCT = col_double()))
HCC_Readmission_clean <- HCC_Readmission %>% filter(BENE_AGE_LVL == 'All' & YEAR == 2020 & BENE_GEO_LVL == 'State') %>% filter(BENE_GEO_DESC!= c('PR','VI'))%>% mutate(state = BENE_GEO_DESC)
HCC_Readmission_Only <- HCC_Readmission_clean %>% select(state, BENE_AVG_RISK_SCRE,ACUTE_HOSP_READMSN_PCT)
##Store the dataset for further usage
write_csv(HCC_Readmission_Only, file = here::here("dataset", "HCC_Readmission_Only.csv"))
save(HCC_Readmission_Only, file = here::here("dataset/HCC_Readmission_Only.RData"))

##Dataset7: Population
population <- read_csv(here::here("dataset","population.csv"))
##Store the dataset for further usage
write_csv(population, file = here::here("dataset", "population.csv"))
save(population, file = here::here("dataset/population.RData"))

##Dataset8: Combined dataset for year 2018-2019
medicare18 <- read_csv(here::here("dataset","medicare-18.csv"),col_types=cols_only(Rndrng_Prvdr_State_Abrvtn=col_character(),Avg_Tot_Pymt_Amt=col_number(),Avg_Mdcr_Pymt_Amt=col_number()))
medicare19 <- read_csv(here::here("dataset","medicare-19.csv"),col_types=cols_only(Rndrng_Prvdr_State_Abrvtn=col_character(),Avg_Tot_Pymt_Amt=col_number(),Avg_Mdcr_Pymt_Amt=col_number()))
medicare_train <- rbind(medicare18,medicare19)
medicare_train <- medicare_train %>% mutate(state=Rndrng_Prvdr_State_Abrvtn) %>% group_by(state)%>% summarize(Medicare_Coverage_Train=mean(Avg_Mdcr_Pymt_Amt/Avg_Tot_Pymt_Amt))

##Dataset9: Combined dataset for year 2020
Avg_LOS <- read_csv(here::here("dataset","Avg_LOS.csv"))
health_status <- read_csv(here::here("dataset", "Health_Status_2020.csv")) %>% rename(state = STATE)
HCC_Readmission_Only <- read_csv(here::here("dataset","HCC_Readmission_Only.csv"))
GPCI2020 <- read_csv(here::here("dataset","GPCI2020.csv"))
GDP <- read_csv(here::here("dataset","GDP.csv"))
population <- read_csv(here::here("dataset","population.csv"),col_types=cols_only(state=col_character(),Year2020=col_number()))
medicare_data_sum <- medicare_data_sum %>% rename(state = Rndrng_Prvdr_State_Abrvtn)
df_list<- list(HCC_Readmission_Only,GPCI2020,GDP,medicare_data_sum,Avg_LOS,population, premiumA, health_status)
Data_Combined <- df_list %>% reduce(inner_join, by='state')
write_csv(Data_Combined, file = here::here("dataset", "Data_Combined.csv"))
save(Data_Combined, file = here::here("dataset/Data_Combined.RData"))

Data_Combined_fit <-Data_Combined %>% select(BENE_AVG_RISK_SCRE:Year2020)
library(GGally)
ggpairs(Data_Combined_fit,upper = list(continuous = wrap("points", alpha = 0.3,size=0.1)),
        lower = list(continuous = wrap('cor', size = 4)))
fit1<-lm(Medicare_Coverage~.,data=Data_Combined_fit)
summary(fit1)
Data_Combined$Medicare_Coverage
plot(fit1)
aic<-step(fit1,direction='both')
view(Data_Combined_fit)

medicare_data_clean<-read_csv(here::here("dataset","medicare_data_clean.csv"),col_types = cols_only(state=col_character(),Avg_Tot_Pymt_Amt=col_number(),Avg_Mdcr_Pymt_Amt=col_number()))
medicare_data_clean<-medicare_data_clean %>% mutate(Medicare_Coverage=Avg_Mdcr_Pymt_Amt/Avg_Tot_Pymt_Amt) %>% select(Medicare_Coverage,Rndrng_Prvdr_State_Abrvtn)
df_list2<-list(medicare_data_clean,GDP,Avg_LOS,HCC_Readmission_Only,GPCI2020,population)
Data_Combined2<-df_list2 %>% reduce(full_join,by='state')
ggpairs(Data_Combined2 %>% select(Medicare_Coverage,GDP:Year2020),upper = list(continuous = wrap("points", alpha = 0.3,size=0.1)),
         lower = list(continuous = wrap('cor', size = 4)))

GPCI_Train<-GPCI_Train %>% rbind(GPCI2018,GPCI2019) %>% group_by(state) %>% summarise(PW_GPCI=mean(PW_GPCI),PE_GPCI=mean(PE_GPCI),MP_GPCI=mean(MP_GPCI))
Train <- merge(GPCI_Train,medicare_train) %>% select(PW_GPCI:Medicare_Coverage_Train)
ggpairs(Train,upper = list(continuous = wrap("points", alpha = 0.3,size=0.1)),
        lower = list(continuous = wrap('cor', size = 4)))

write_csv(Train, file = here::here("dataset", "Training.csv"))
save(Train, file = here::here("dataset/Training.RData"))

##Dataset11: Medicare Coverage and US map
#Run the packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(USAboundaries))

#Create a map for the US
epsg_us <- 2163
us_states <- st_read("C:/Users/Lena/Desktop/MA415/R/ma4615-fa22-final-project-hello-world/dataset/cb_2019_us_state_20m/cb_2019_us_state_20m.shp") %>%
  st_transform(epsg_us)

not_contiguous <-
  c("Guam", "Commonwealth of the Northern Mariana Islands",
    "American Samoa", "Puerto Rico", "United States Virgin Islands")
us_cont <- us_states %>%
  filter(!(NAME %in% not_contiguous)) %>%
  select(STATEFP, STUSPS, NAME)

#Plot a map colored with medicare coverage level
cont_medicare_coverage <- inner_join(us_cont, medicare_data_sum, by = c("STUSPS" = "state"))

write_csv(cont_medicare_coverage, file = here::here("dataset", "cont_medicare_coverage.csv"))
save(cont_medicare_coverage, file = here::here("dataset/cont_medicare_coverage.RData"))
