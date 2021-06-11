##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,ggplot2,easycsv,
               gmodels,Rmisc,DescTools,data.table,
               tibble,leaflet,raster,plotly,lubridate,
               here,RColorBrewer,ggthemes,hrbrthemes,
               ggchicklet,tidyverse,showtext)

#Clean up the global environment
rm(list = ls())

#Set directory where inputs are saved
rawdatadir <- "M:/Analytics/Networked Data Lab/Shielding/Shielding Behavioural Survey/"

#Git directory
gitdir <- dirname(rstudioapi::getSourceEditorContext()$path)

#Set directory
setwd(rawdatadir)

##################################################
################### Load clean data ##############
##################################################

fread_folder(paste0(gitdir,"/Clean data/"), extension = "CSV")

######################################################################################
############## Mental health 1: Change in mental health since last month #############
######################################################################################

### COVID High Risk Group Insights Survey (2021)

COVID_HRGS_MH1_data <- CHRGIS_mh1_clean %>%
  filter(.,level != "Don't know") %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("Much worse", "Slightly worse",
                                           "No difference", 
                                           "Slightly better", "Much better")))

COVID_HRGS_MH1 <- COVID_HRGS_MH1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_MH1_data$date)) +
  labs(title="Change in well-being and mental health compared to last month",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_MH1) %>%
  layout(title = list(text = paste0('Change in well-being and mental health compared to last month',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

### Shielding Behavioural Survey (2020)

#Flourish

SBS_mh1_flourish <- SBS_mh1_clean %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  select(.,date,level,pct_clean) %>%
  filter(.,level!="Total") %>%
  pivot_wider(names_from = level, values_from = pct_clean)

fwrite(SBS_mh1_flourish, file = here::here("Clean data","SBS_mh1_flourish.csv"), sep = ",")

#R chart

SBS_MH1_data <- SBS_mh1_clean %>%
  filter(.,!(level %in% c("Total"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("Much worse", "Slightly worse",
                                           "No difference", 
                                           "It's better"))) %>%
  select(.,-c("Total"))

SBS_MH1 <- SBS_MH1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(SBS_MH1_data$date)) +
  labs(title="Change in well-being and mental health since shielding guidance",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(SBS_MH1) %>%
  layout(title = list(text = paste0('Change in well-being and mental health since shielding guidance',
                                    '<br>',
                                    '<sup>',
                                    'Shielding Behavioural Survey (2020)',
                                    '</sup>')))

#########################################################################
############## Mental health 2: Treatment for mental health #############
#########################################################################

### COVID High Risk Group Insights Survey (2021)

COVID_HRGS_MH2_data <- CHRGIS_mh2_clean %>%
  filter(.,!(level %in% c("Don't know","Prefer not to say"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("I'm currently receiving treatment and I have also received treatment in the past",
                                           "I'm currently receiving treatment for the first time",
                                           "I have received treatment in the past, but do not receive treatment now", 
                                           "I have never received treatment")))

COVID_HRGS_MH2 <- COVID_HRGS_MH2_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Treatment")) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_MH2_data$date)) +
  labs(title="Treatment for mental health difficulties",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_MH1) %>%
  layout(title = list(text = paste0('Treatment for mental health difficulties',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

### Shielding Behavioural Survey (2020)

SBS_MH2_data <- SBS_mh2_clean %>%
  filter(.,!(level %in% c("Total","I prefer not to answer this question"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("I'm currently receiving treatment for mental health problems",
                                           "I have received treatment for mental health problems in the past",
                                           "I have never received treatment for mental health problems")))

SBS_MH2 <- SBS_MH2_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "RdYlBu") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(SBS_MH2_data$date)) +
  labs(title="Treatment for mental health difficulties",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(SBS_MH2) %>%
  layout(title = list(text = paste0('Treatment for mental health difficulties',
                                    '<br>',
                                    '<sup>',
                                    'Shielding Behavioural Survey (2020)',
                                    '</sup>')))

###############################################################
############## Mental health 3: Life satisfaction #############
###############################################################

### COVID High Risk Group Insights Survey (2021)

COVID_HRGS_MH3_data <- CHRGIS_mh3_clean %>%
  mutate(.,date=lubridate::dmy(date))

pd <- position_dodge(0.1) # move them .05 to the left and right

COVID_HRGS_MH3 <- COVID_HRGS_MH3_data %>%
  ggplot(., aes(x=date, y=Mean)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_errorbar(aes(ymin=LCL, ymax=UCL), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_MH3_data$date)) +
  labs(title="Life satisfaction",
       x ="Survey date", y = "Mean (with CI)") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_MH3) %>%
  layout(title = list(text = paste0('Mean life satisfaction',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

### Shielding Behavioural Survey (2020)

SBS_MH3_data <- SBS_mh3_clean %>%
  mutate(.,date=lubridate::dmy(date))

pd <- position_dodge(0.1) # move them .05 to the left and right

SBS_HRGS_MH3 <- SBS_MH3_data %>%
  ggplot(., aes(x=date, y=Mean)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(SBS_MH3_data$date)) +
  labs(title="Life satisfaction",
       x ="Survey date", y = "Mean") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(SBS_MH3) %>%
  layout(title = list(text = paste0('Mean life satisfaction',
                                    '<br>',
                                    '<sup>',
                                    'Shielding Behavioural Survey (2020)',
                                    '</sup>')))

#############################################
############## Access to care 1 #############
#############################################

### COVID High Risk Group Insights Survey (2021)

COVID_HRGS_AC1_data <- CHRGIS_ac1_clean %>%
  filter(.,!(level %in% c("Don't know4"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("Stopped",
                                           "Decreased",
                                           "Continued as normal", 
                                           "Increased"))) %>%
  select(.,-c("Percentage"))

COVID_HRGS_AC1 <- COVID_HRGS_AC1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "BrBG") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_AC1_data$date)) +
  labs(title="Amount of GP/hospital care for underlying condition(s) since last month",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_AC1) %>%
  layout(title = list(text = paste0('Amount of GP/hospital care for underlying condition(s) since last month',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

### Shielding Behavioural Survey (2020)

SBS_AC1_data <- SBS_ac1_clean %>%
  mutate(.,level=str_replace_all(level,"â€™","'")) %>%
  mutate(.,level=str_replace_all(level,"eg","e.g.")) %>%
  filter(.,!(level %in% c("Total"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("I'm not accessing any care",
                                           "I'm no longer accessing some aspects of my care, e.g. tests, scans",
                                           "My care has continued as normal",
                                           "My care has increased"))) %>%
  select(.,-c("Percentage"))

SBS_AC1 <- SBS_AC1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "BrBG") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(SBS_AC1_data$date)) +
  labs(title="Amount of GP/hospital care for underlying condition(s) since shielding guidance",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(SBS_AC1) %>%
  layout(title = list(text = paste0('Amount of GP/hospital care for underlying condition(s) since shielding guidance',
                                    '<br>',
                                    '<sup>',
                                    'Shielding Behavioural Survey (2020)',
                                    '</sup>')))

#############################################
############## Change in health #############
#############################################

### COVID High Risk Group Insights Survey (2021)

#Part 1

COVID_HRGS_CH1_data <- CHRGIS_ch1_clean %>%
  filter(.,!(level %in% c("Prefer not to say"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("Yes",
                                           "No"))) %>%
  select(.,-c("Percentage"))

COVID_HRGS_CH1 <- COVID_HRGS_CH1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_CH1_data$date)) +
  labs(title="Change in underlying condition since early Dec 2020",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_CH1) %>%
  layout(title = list(text = paste0('Change in underlying condition since early Dec 2020',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

#Part 2

COVID_HRGS_CH2_data <- CHRGIS_ch2_clean %>%
  filter(.,!(level %in% c("Prefer not to say","Don't know"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("Much worse now",
                                           "Slightly worse now",
                                           "Slightly better now",
                                           "Much better now"))) %>%
  select(.,-c("Percentage"))

COVID_HRGS_CH2 <- COVID_HRGS_CH2_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_CH2_data$date)) +
  labs(title="Change in underlying condition since early Dec 2020 (among those with change)",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_CH2) %>%
  layout(title = list(text = paste0('Change in underlying condition since early Dec 2020 (among those with change)',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

### Shielding Behavioural Survey (2020)

SBS_AC1_data <- SBS_ch1_clean %>%
  filter(.,!(level %in% c("Total"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  mutate(., level = factor(level, levels=c("Condition is much worse",
                                           "Condition is slightly worse",
                                           "Condition is about the same",
                                           "Condition is slightly better",
                                           "Condition is better"))) %>%
  select(.,-c("Percentage"))

SBS_AC1 <- SBS_AC1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "RdYlGn") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(SBS_AC1_data$date)) +
  labs(title="Change in underlying condition since since shielding guidance",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(SBS_AC1) %>%
  layout(title = list(text = paste0('Change in underlying condition since shielding guidance',
                                    '<br>',
                                    '<sup>',
                                    'Shielding Behavioural Survey (2020)',
                                    '</sup>')))

###########################################
############## Work situation #############
###########################################

### COVID High Risk Group Insights Survey (2021)

COVID_HRGS_W1_data <- CHRGIS_w1_clean %>%
  filter(.,!(level %in% c("Don't know4"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  select(.,-c("Percentage"))

COVID_HRGS_W1 <- COVID_HRGS_W1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Situation")) +
  scale_color_tableau(direction = -1) +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(COVID_HRGS_W1_data$date)) +
  labs(title="Current work situation",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(COVID_HRGS_W1) %>%
  layout(title = list(text = paste0('Current work situation',
                                    '<br>',
                                    '<sup>',
                                    'COVID High Risk Group Insights Survey (2021)',
                                    '</sup>')))

### Shielding Behavioural Survey (2020)

SBS_W1_data <- SBS_w1_clean %>%
  filter(.,!(level %in% c("Total"))) %>%
  mutate(.,date=lubridate::dmy(date)) %>%
  select(.,-c("Percentage"))

SBS_W1 <- SBS_W1_data %>%
  ggplot(., aes(fill=level, y=pct_clean, x=date)) + 
  geom_bar(position="stack", stat="identity") +
  theme_ipsum() +
  guides(fill=guide_legend(title="Change")) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_labels = "%b %d %Y", breaks = unique(SBS_W1_data$date)) +
  labs(title="Normally work or not",
       x ="Survey date", y = "% of CEV people") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=11),
        text = element_text(size = 11),
        axis.text = element_text(size =11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size =11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))

ggplotly(SBS_AC1) %>%
  layout(title = list(text = paste0('Normally work or not',
                                    '<br>',
                                    '<sup>',
                                    'Shielding Behavioural Survey (2020)',
                                    '</sup>')))