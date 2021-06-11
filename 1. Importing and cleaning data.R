##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,ggplot2,DescTools,data.table,
               tibble,pbapply,here,tidyverse,readxl)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/Shielding Behavioural Survey/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Set directory
setwd(gitdir)

#Auxiliary function
numbers_only <- function(x) !grepl("\\D", x)

############################
##### Change in health #####
############################

#CH1-B: Has your underlying health condition(s) been affected since early December 2020?

CHRGIS_30jan2021_ch1 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                   sheet = "Table 6.1",range="A19:B25")

CHRGIS_ch1 <- list(CHRGIS_30jan2021_ch1)
names(CHRGIS_ch1) <- c("30 jan 2021")
rm(CHRGIS_30jan2021_ch1)

#Add dates and questions
for (k in 1:length(CHRGIS_ch1)){
  CHRGIS_ch1[[k]]$date <- names(CHRGIS_ch1)[k]
  CHRGIS_ch1[[k]]$`question` <- names(CHRGIS_ch1[[k]])[1]
}

#Append datasets
CHRGIS_ch1_clean <- CHRGIS_ch1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`Has your underlying health condition(s) been affected since early December 2020?3`,
         Percentage=`...2`) %>%
  filter(., level %in% c("Yes","No","Prefer not to say")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA))

#Write
fwrite(CHRGIS_ch1_clean, file = here::here("Clean data","CHRGIS_ch1_clean.csv"), sep = ",")

#CH2-B: Of CEV who indicated their underlying condition had been affected since December 2020,
#Compared to early December 2020, how is your underlying health condition or conditions now? Would you say it is

CHRGIS_30jan2021_ch2 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                   sheet = "Table 6.2",range="A10:B19")

CHRGIS_ch2 <- list(CHRGIS_30jan2021_ch2)
names(CHRGIS_ch2) <- c("30 jan 2021")
rm(CHRGIS_30jan2021_ch2)

#Add dates and questions
for (k in 1:length(CHRGIS_ch2)){
  CHRGIS_ch2[[k]]$date <- names(CHRGIS_ch2)[k]
  CHRGIS_ch2[[k]]$`question` <- names(CHRGIS_ch2[[k]])[1]
}

#Append datasets
CHRGIS_ch2_clean <- CHRGIS_ch2 %>%
  rbindlist(.) %>%
  rename(.,`level`=`Compared to early December 2020, how is your underlying health condition or conditions now? Would you say it is … 3`,
         Percentage=`...2`) %>%
  filter(., level %in% c("Much better now","Slightly better now","Much worse now","Slightly worse now","Prefer not to say","Don't know")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA))

#Write
fwrite(CHRGIS_ch2_clean, file = here::here("Clean data","CHRGIS_ch2_clean.csv"), sep = ",")

#CH1-A: How has your underlying health condition been affected since you received shielding guidance?

SBS_16jul2020_ch1 <- read_excel(paste0(rawdatadir,"16 july/datatablesshieldingbehaviouralsurvey09july16july.xlsx"),
                                sheet = "Table 1.10",range="A5:C12")

SBS_30jun2020_ch1 <- read_excel(paste0(rawdatadir,"30 june/datatablesshieldingbehaviouralsurvey24june30june.xlsx"),
                                sheet = "Table 1.10",range="A5:C12")

SBS_18jun2020_ch1 <- read_excel(paste0(rawdatadir,"18 june/datatablesshieldingbehaviouralsurvey09june18june.xlsx"),
                                sheet = "Table 1.10",range="A5:C12")

SBS_03jun2020_ch1 <- read_excel(paste0(rawdatadir,"3 june/datatablesshieldingbehaviouralsurvey28may03junetitleupdate.xlsx"),
                                sheet = "Table 1.12",range="A5:C12")

SBS_19may2020_ch1 <- read_excel(paste0(rawdatadir,"19 may/datatablesshieldingbehaviouralsurvey1419maytitleupdate.xlsx"),
                                sheet = "Table 1.12",range="A5:C12")

SBS_ch1 <- list(SBS_16jul2020_ch1,SBS_30jun2020_ch1,SBS_18jun2020_ch1,SBS_03jun2020_ch1,SBS_19may2020_ch1)
names(SBS_ch1) <- c("16 july 2020","30 june 2020","18 june 2020","3 june 2020","19 may 2020")
rm(SBS_16jul2020_ch1,SBS_30jun2020_ch1,SBS_18jun2020_ch1,SBS_03jun2020_ch1,SBS_19may2020_ch1)

#Add dates and questions
for (k in 1:length(SBS_ch1)){
  SBS_ch1[[k]]$date <- names(SBS_ch1)[k]
  SBS_ch1[[k]]$`question` <- names(SBS_ch1[[k]])[1]
}

#Append datasets
SBS_ch1_clean <- SBS_ch1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`How has your underlying health condition been affected since you received shielding guidance?`,
         Percentage=`...3`) %>%
  filter(., level %in% c("Condition is better","Condition is slightly better",
                         "Condition is about the same","Condition is slightly worse",
                         "Condition is much worse","Total")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA)) %>%
  select(.,-c("Total"))

#Write
fwrite(SBS_ch1_clean, file = here::here("Clean data","SBS_ch1_clean.csv"), sep = ",")

##########################
##### Access to care #####
##########################

#AC1-B: How has the amount of GP or hospital care for your underlying health condition(s) been affected since last month? 

CHRGIS_27feb2021_ac1 <- read_excel(paste0(rawdatadir,"27 feb 2021/datatablesclinicallyextremelyvulnerable22feb27feb212.xlsx"),
                                   sheet = "Table 6.1",range="A8:B16")

CHRGIS_30jan2021_ac1 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                   sheet = "Table 6.1",range="A8:B16")

CHRGIS_ac1 <- list(CHRGIS_27feb2021_ac1,CHRGIS_30jan2021_ac1)
names(CHRGIS_ac1) <- c("27 feb 2021","30 jan 2021")
rm(CHRGIS_27feb2021_ac1,CHRGIS_30jan2021_ac1)

#Add dates and questions
for (k in 1:length(CHRGIS_ac1)){
  CHRGIS_ac1[[k]]$date <- names(CHRGIS_ac1)[k]
  CHRGIS_ac1[[k]]$`question` <- names(CHRGIS_ac1[[k]])[1]
}

#Append datasets
CHRGIS_ac1_clean <- CHRGIS_ac1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`How has the amount of GP or hospital care for your underlying health condition(s) been affected since last month? Has your care …`,
         Percentage=`...2`) %>%
  filter(., level %in% c("Increased","Continued as normal","Decreased","Stopped","Don't know4")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA))

#Write
fwrite(CHRGIS_ac1_clean, file = here::here("Clean data","CHRGIS_ac1_clean.csv"), sep = ",")

#AC1-A: Are you receiving the same level of GP or hospital care for your underlying health condition since you received shielding guidance?

SBS_16jul2020_ac1 <- read_excel(paste0(rawdatadir,"16 july/datatablesshieldingbehaviouralsurvey09july16july.xlsx"),
                                sheet = "Table 1.9",range="A5:C11")

SBS_30jun2020_ac1 <- read_excel(paste0(rawdatadir,"30 june/datatablesshieldingbehaviouralsurvey24june30june.xlsx"),
                                sheet = "Table 1.9",range="A5:C11")

SBS_18jun2020_ac1 <- read_excel(paste0(rawdatadir,"18 june/datatablesshieldingbehaviouralsurvey09june18june.xlsx"),
                                sheet = "Table 1.9",range="A5:C11")

SBS_03jun2020_ac1 <- read_excel(paste0(rawdatadir,"3 june/datatablesshieldingbehaviouralsurvey28may03junetitleupdate.xlsx"),
                                sheet = "Table 1.11",range="A5:C11")

SBS_19may2020_ac1 <- read_excel(paste0(rawdatadir,"19 may/datatablesshieldingbehaviouralsurvey1419maytitleupdate.xlsx"),
                                sheet = "Table 1.11",range="A5:C11")

SBS_ac1 <- list(SBS_16jul2020_ac1,SBS_30jun2020_ac1,SBS_18jun2020_ac1,SBS_03jun2020_ac1,SBS_19may2020_ac1)
names(SBS_ac1) <- c("16 july 2020","30 june 2020","18 june 2020","3 june 2020","19 may 2020")
rm(SBS_16jul2020_ac1,SBS_30jun2020_ac1,SBS_18jun2020_ac1,SBS_03jun2020_ac1,SBS_19may2020_ac1)

#Add dates and questions
for (k in 1:length(SBS_ac1)){
  SBS_ac1[[k]]$date <- names(SBS_ac1)[k]
  SBS_ac1[[k]]$`question` <- names(SBS_ac1[[k]])[1]
}

#Append datasets
SBS_ac1_clean <- SBS_ac1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`Are you receiving the same level of GP or hospital care for your underlying health condition since you received shielding guidance?`,
         Percentage=`...3`) %>%
  filter(., level %in% c("My care has increased","My care has continued as normal",
                         "I’m no longer accessing some aspects of my care, e.g. tests, scans",
                         "I’m no longer accessing some aspects of my care, eg tests, scans",
                         "I’m not accessing any care","Total")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA)) %>%
  select(.,-c("Total"))

#Write
fwrite(SBS_ac1_clean, file = here::here("Clean data","SBS_ac1_clean.csv"), sep = ",")

#######################################
##### Change in well-being and MH #####
#######################################

#MH1-B: Compared to last month, how is your well-being and mental health now?

CHRGIS_27feb2021_mh1 <- read_excel(paste0(rawdatadir,"27 feb 2021/datatablesclinicallyextremelyvulnerable22feb27feb212.xlsx"),
                                   sheet = "Table 5.1",range="A4:B18")

CHRGIS_30jan2021_mh1 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                sheet = "Table 5.1",range="A4:B17") %>%
  rename(.,Total=`All CEV Total`)

CHRGIS_mh1 <- list(CHRGIS_27feb2021_mh1,CHRGIS_30jan2021_mh1)
names(CHRGIS_mh1) <- c("27 feb 2021","30 jan 2021")
rm(CHRGIS_27feb2021_mh1,CHRGIS_30jan2021_mh1)

#Add dates and questions
for (k in 1:length(CHRGIS_mh1)){
  CHRGIS_mh1[[k]]$date <- names(CHRGIS_mh1)[k]
  CHRGIS_mh1[[k]]$`question` <- as.character(CHRGIS_mh1[[k]][4,1])
}

#Append datasets
CHRGIS_mh1_clean <- CHRGIS_mh1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`...1`) %>%
  filter(., level %in% c("Much better","Slightly better","No difference","Slightly worse","Much worse","Don't know")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Total),Total,NA))

#Write
fwrite(CHRGIS_mh1_clean, file = here::here("Clean data","CHRGIS_mh1_clean.csv"), sep = ",")

#MH1-A: How has your wellbeing and mental health been affected since you received shielding guidance?

SBS_16jul2020_mh1 <- read_excel(paste0(rawdatadir,"16 july/datatablesshieldingbehaviouralsurvey09july16july.xlsx"),
                                sheet = "Table 1.7",range="A5:C11")

SBS_30jun2020_mh1 <- read_excel(paste0(rawdatadir,"30 june/datatablesshieldingbehaviouralsurvey24june30june.xlsx"),
                                sheet = "Table 1.7",range="A5:C11")

SBS_18jun2020_mh1 <- read_excel(paste0(rawdatadir,"18 june/datatablesshieldingbehaviouralsurvey09june18june.xlsx"),
                               sheet = "Table 1.7",range="A5:C11")

SBS_03jun2020_mh1 <- read_excel(paste0(rawdatadir,"3 june/datatablesshieldingbehaviouralsurvey28may03junetitleupdate.xlsx"),
                                sheet = "Table 1.9",range="A5:C11")

SBS_19may2020_mh1 <- read_excel(paste0(rawdatadir,"19 may/datatablesshieldingbehaviouralsurvey1419maytitleupdate.xlsx"),
                                sheet = "Table 1.9",range="A5:C11")

SBS_mh1 <- list(SBS_16jul2020_mh1,SBS_30jun2020_mh1,SBS_18jun2020_mh1,SBS_03jun2020_mh1,SBS_19may2020_mh1)
names(SBS_mh1) <- c("16 july 2020","30 june 2020","18 june 2020","3 june 2020","19 may 2020")
rm(SBS_16jul2020_mh1,SBS_30jun2020_mh1,SBS_18jun2020_mh1,SBS_03jun2020_mh1,SBS_19may2020_mh1)

#Add dates and questions
for (k in 1:length(SBS_mh1)){
  SBS_mh1[[k]]$date <- names(SBS_mh1)[k]
  SBS_mh1[[k]]$`question` <- names(SBS_mh1[[k]])[1]
}

#Append datasets
SBS_mh1_clean <- SBS_mh1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`How has your wellbeing and mental health been affected since you received shielding guidance?`,
         Percentage=`...3`) %>%
  filter(., level %in% c("It's better","No difference","Slightly worse","Much worse","Total")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA))

#Write
fwrite(SBS_mh1_clean, file = here::here("Clean data","SBS_mh1_clean.csv"), sep = ",")

#########################################
##### Treatment for MH difficulties #####
#########################################

#MH2-B: Have you currently or in the past received treatment for mental health difficulties?

CHRGIS_27feb2021_mh2 <- read_excel(paste0(rawdatadir,"27 feb 2021/datatablesclinicallyextremelyvulnerable22feb27feb212.xlsx"),
                                   sheet = "Table 5.1",range="A21:B30")

CHRGIS_30jan2021_mh2 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                   sheet = "Table 5.1",range="A20:B30")

CHRGIS_mh2 <- list(CHRGIS_27feb2021_mh2,CHRGIS_30jan2021_mh2)
names(CHRGIS_mh2) <- c("27 feb 2021","30 jan 2021")
rm(CHRGIS_27feb2021_mh2,CHRGIS_30jan2021_mh2)

#Add dates and questions
for (k in 1:length(CHRGIS_mh2)){
  CHRGIS_mh2[[k]]$date <- names(CHRGIS_mh2)[k]
  CHRGIS_mh2[[k]]$`question` <- names(CHRGIS_mh2[[k]])[1]
}

#Append datasets
CHRGIS_mh2_clean <- CHRGIS_mh2 %>%
  rbindlist(.) %>%
  rename(.,`level`=`Have you currently or in the past received treatment for mental health difficulties?3`,
         Percentage=`...2`) %>%
  filter(., level %in% c("I have never received treatment",
                         "I have received treatment in the past, but do not receive treatment now",
                         "I'm currently receiving treatment for the first time",
                         "I'm currently receiving treatment and I have also received treatment in the past",
                         "Prefer not to say","Don't know")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA))

#Write
fwrite(CHRGIS_mh2_clean, file = here::here("Clean data","CHRGIS_mh2_clean.csv"), sep = ",")

#MH2-A: Are you currently receiving or have you in the past received treatment (medication or talking therapies) for mental health difficulties?

SBS_16jul2020_mh2 <- read_excel(paste0(rawdatadir,"16 july/datatablesshieldingbehaviouralsurvey09july16july.xlsx"),
                                sheet = "Table 1.6",range="A5:C11")

SBS_30jun2020_mh2 <- read_excel(paste0(rawdatadir,"30 june/datatablesshieldingbehaviouralsurvey24june30june.xlsx"),
                                sheet = "Table 1.6",range="A5:C11")

SBS_18jun2020_mh2 <- read_excel(paste0(rawdatadir,"18 june/datatablesshieldingbehaviouralsurvey09june18june.xlsx"),
                                sheet = "Table 1.6",range="A5:C11")

SBS_03jun2020_mh2 <- read_excel(paste0(rawdatadir,"3 june/datatablesshieldingbehaviouralsurvey28may03junetitleupdate.xlsx"),
                                sheet = "Table 1.8",range="A5:C11")

SBS_19may2020_mh2 <- read_excel(paste0(rawdatadir,"19 may/datatablesshieldingbehaviouralsurvey1419maytitleupdate.xlsx"),
                                sheet = "Table 1.8",range="A5:C11")

SBS_mh2 <- list(SBS_16jul2020_mh2,SBS_30jun2020_mh2,SBS_18jun2020_mh2,SBS_03jun2020_mh2,SBS_19may2020_mh2)
names(SBS_mh2) <- c("16 july 2020","30 june 2020","18 june 2020","3 june 2020","19 may 2020")
rm(SBS_16jul2020_mh2,SBS_30jun2020_mh2,SBS_18jun2020_mh2,SBS_03jun2020_mh2,SBS_19may2020_mh2)

#Add dates and questions
for (k in 1:length(SBS_mh2)){
  SBS_mh2[[k]]$date <- names(SBS_mh2)[k]
  SBS_mh2[[k]]$`question` <- names(SBS_mh2[[k]])[1]
  SBS_mh2[[k]]$`level` <- unlist(SBS_mh2[[k]][,1])
  SBS_mh2[[k]] <- SBS_mh2[[k]][,-1]
}

#Append datasets
SBS_mh2_clean <- SBS_mh2 %>%
  rbindlist(.) %>%
  rename(.,Percentage=`...3`) %>%
  filter(., level %in% c("I have never received treatment for mental health problems",
                         "I have received treatment for mental health problems in the past",
                         "I'm currently receiving treatment for mental health problems",
                         "I prefer not to answer this question","Total")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA)) %>%
  select(.,-"Total")

#Write
fwrite(SBS_mh2_clean, file = here::here("Clean data","SBS_mh2_clean.csv"), sep = ",")

#############################
##### Life satisfaction #####
#############################

#MH2-B: Overall, how satisfied are you with your life nowadays?

CHRGIS_27feb2021_mh3 <- read_excel(paste0(rawdatadir,"27 feb 2021/datatablesclinicallyextremelyvulnerable22feb27feb212.xlsx"),
                                   sheet = "Table 5.3",range="A4:F9")

CHRGIS_30jan2021_mh3 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                   sheet = "Table 5.2",range="A4:F9") %>%
  dplyr::rename(.,`Total5`=`All CEV total5`)

CHRGIS_mh3 <- list(CHRGIS_27feb2021_mh3,CHRGIS_30jan2021_mh3)
names(CHRGIS_mh3) <- c("27 feb 2021","30 jan 2021")
rm(CHRGIS_27feb2021_mh3,CHRGIS_30jan2021_mh3)

#Add dates and questions
for (k in 1:length(CHRGIS_mh3)){
  CHRGIS_mh3[[k]]$date <- names(CHRGIS_mh3)[k]
  CHRGIS_mh3[[k]]$`question` <- unlist(CHRGIS_mh3[[k]][4,1])
}

#Append datasets
CHRGIS_mh3_clean <- CHRGIS_mh3 %>%
  rbindlist(.) %>%
  rename(.,level=`...1`,Mean=`Total5`,LCL=`...3`,UCL=`...4`) %>%
  filter(., level %in% c("Overall, how satisfied are you with your life nowadays?6")) %>%
  select(.,-c("...5","...6"))

#Write
fwrite(CHRGIS_mh3_clean, file = here::here("Clean data","CHRGIS_mh3_clean.csv"), sep = ",")

#MH3-A: On a scale of 0 to 10, where 0 is not at all satisfied and 10 is completely satisfied, how satisfied are you with your life nowadays?

SBS_16jul2020_mh3 <- read_excel(paste0(rawdatadir,"16 july/datatablesshieldingbehaviouralsurvey09july16july.xlsx"),
                                sheet = "Table 1.8",range="A1:B6")

SBS_30jun2020_mh3 <- read_excel(paste0(rawdatadir,"30 june/datatablesshieldingbehaviouralsurvey24june30june.xlsx"),
                                sheet = "Table 1.8",range="A1:B6")

SBS_18jun2020_mh3 <- read_excel(paste0(rawdatadir,"18 june/datatablesshieldingbehaviouralsurvey09june18june.xlsx"),
                                sheet = "Table 1.8",range="A1:B6")

SBS_03jun2020_mh3 <- read_excel(paste0(rawdatadir,"3 june/datatablesshieldingbehaviouralsurvey28may03junetitleupdate.xlsx"),
                                sheet = "Table 1.10",range="A1:B6")

SBS_19may2020_mh3 <- read_excel(paste0(rawdatadir,"19 may/datatablesshieldingbehaviouralsurvey1419maytitleupdate.xlsx"),
                                sheet = "Table 1.10",range="A1:B6")

SBS_mh3 <- list(SBS_16jul2020_mh3,SBS_30jun2020_mh3,SBS_18jun2020_mh3,SBS_03jun2020_mh3,SBS_19may2020_mh3)
names(SBS_mh3) <- c("16 july 2020","30 june 2020","18 june 2020","3 june 2020","19 may 2020")
rm(SBS_16jul2020_mh3,SBS_30jun2020_mh3,SBS_18jun2020_mh3,SBS_03jun2020_mh3,SBS_19may2020_mh3)

#Add dates and questions
for (k in 1:length(SBS_mh3)){
  SBS_mh3[[k]]$date <- names(SBS_mh3)[k]
  SBS_mh3[[k]]$`question` <- unlist(SBS_mh3[[k]][4,1])
}

#Append datasets
SBS_mh3_clean <- SBS_mh3 %>%
  rbindlist(.) %>%
  rename(.,level=`SECTION 1: All clinically extremely vulnerable (CEV)`,Mean=`...2`) %>%
  filter(., level %in% c("Mean life satisfaction score"))

#Write
fwrite(SBS_mh3_clean, file = here::here("Clean data","SBS_mh3_clean.csv"), sep = ",")

################
##### Work #####
################

#W1-B: What best describes your current work situation?

CHRGIS_27feb2021_w1 <- read_excel(paste0(rawdatadir,"27 feb 2021/datatablesclinicallyextremelyvulnerable22feb27feb212.xlsx"),
                                   sheet = "Table 7.1",range="A8:B23")

CHRGIS_30jan2021_w1 <- read_excel(paste0(rawdatadir,"30 jan 2021/datatablesclinicallyextremelyvulnerable18jan30jan2110022021142551.xlsx"),
                                   sheet = "Table 7.1",range="A8:B23")

CHRGIS_w1 <- list(CHRGIS_27feb2021_w1,CHRGIS_30jan2021_w1)
names(CHRGIS_w1) <- c("27 feb 2021","30 jan 2021")
rm(CHRGIS_27feb2021_w1,CHRGIS_30jan2021_w1)

#Add dates and questions
for (k in 1:length(CHRGIS_w1)){
  CHRGIS_w1[[k]]$date <- names(CHRGIS_w1)[k]
  CHRGIS_w1[[k]]$`question` <- names(CHRGIS_w1[[k]])[1]
}

#Append datasets
CHRGIS_w1_clean <- CHRGIS_w1 %>%
  rbindlist(.) %>%
  rename(.,`level`=`What best describes your current work situation? Are you in …`,
         Percentage=`...2`) %>%
  filter(., level %in% c("Full-time paid job (31+ hours)",
                         "Part-time paid job (1-30 hours)",
                         "Furlough",
                         "Self-employed",
                         "Receiving company sick pay",
                         "Receiving statutory sick pay",
                         "Student or on government training programme",
                         "Retired",
                         "Disabled or long-term sick",
                         "Unpaid work for a business, community or voluntary",
                         "Unpaid leave from work",
                         "Looking after home and family or homemaker",
                         "Out of work")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA))

#Write
fwrite(CHRGIS_w1_clean, file = here::here("Clean data","CHRGIS_w1_clean.csv"), sep = ",")

#W1-A: Do you normally work?

SBS_16jul2020_w1 <- read_excel(paste0(rawdatadir,"16 july/datatablesshieldingbehaviouralsurvey09july16july.xlsx"),
                                sheet = "Table 1.16",range="A5:C9")

SBS_30jun2020_w1 <- read_excel(paste0(rawdatadir,"30 june/datatablesshieldingbehaviouralsurvey24june30june.xlsx"),
                                sheet = "Table 1.16",range="A5:C9")

SBS_18jun2020_w1 <- read_excel(paste0(rawdatadir,"18 june/datatablesshieldingbehaviouralsurvey09june18june.xlsx"),
                                sheet = "Table 1.16",range="A5:C9")

SBS_03jun2020_w1 <- read_excel(paste0(rawdatadir,"3 june/datatablesshieldingbehaviouralsurvey28may03junetitleupdate.xlsx"),
                                sheet = "Table 1.17",range="A5:C9")

SBS_19may2020_w1 <- read_excel(paste0(rawdatadir,"19 may/datatablesshieldingbehaviouralsurvey1419maytitleupdate.xlsx"),
                                sheet = "Table 1.17",range="A5:C9")

SBS_w1 <- list(SBS_16jul2020_w1,SBS_30jun2020_w1,SBS_18jun2020_w1,SBS_03jun2020_w1,SBS_19may2020_w1)
names(SBS_w1) <- c("16 july 2020","30 june 2020","18 june 2020","3 june 2020","19 may 2020")
rm(SBS_16jul2020_w1,SBS_30jun2020_w1,SBS_18jun2020_w1,SBS_03jun2020_w1,SBS_19may2020_w1)

#Add dates and questions
for (k in 1:length(SBS_w1)){
  SBS_w1[[k]]$date <- names(SBS_w1)[k]
  SBS_w1[[k]]$`question` <- names(SBS_w1[[k]])[1]
}

#Append datasets
SBS_w1_clean <- SBS_w1 %>%
  rbindlist(.) %>%
  rename(.,level=`Do you normally work?`,Percentage=`...3`) %>%
  filter(., level %in% c("Yes","No","Total")) %>%
  mutate(.,pct_clean=ifelse(numbers_only(Percentage),Percentage,NA)) %>%
  select(.,-"Total")

#Write
fwrite(SBS_w1_clean, file = here::here("Clean data","SBS_w1_clean.csv"), sep = ",")