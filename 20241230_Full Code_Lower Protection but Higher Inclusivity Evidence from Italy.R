# Lower protection but higher inclusivity? The false promise of lower firing costs. Quasi-experimental evidence from Italy. ----------
# FULL CODE -----
# Library Set Up ----

library(plyr) # some basic R commands
library(tidyverse) # broad range of basic R commands (e.g. reading in data)
library(arsenal) # quick tables construction (e.g. via tableby)
library(pander) # tables formatting
library(data.table) # data table creation
library(writexl) # export excel files
library(ggplot2)  # Grphic design
library(ggpubr)
library(ggrepel)
library(car) # check multicollinearity via vif functionILSH <- read_csv2("~/Quantitative Analysis/LoSal_db/ILS_H.TXT")
library(TSstudio) # plotting time series
library(its.analysis) # interrupted time series Analysis
library(ggeffects) 
library(texreg)
library(fixest)
library(kableExtra)
library(did)
library(plm)

library(extrafont)
loadfonts(device = "win")

# LFS Figure -----

rm(list=ls())

## Read In LFs Statistiscs -----
# Sample code. Data freely accesible at https://www.istat.it/en/archivio
# Q12019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Primo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")
# Q22019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Secondo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")
# Q32019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Terzo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")
# Q42019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Quarto_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")

setwd("C:/RData_DPhil/LoSal db/A. Selected_datasets/")


sel_var <- c("SG11", # sesso
             "CLETAQ", # eta quinquennale 
             "REG",  # regione 
             "COND3", # empl status
             "DETIND", # det or indet 
             "PIEPAR", # ft o pt
             "POSPRO", # posizione dettagliata
             "DIPIND" # dipendente 1, or indipendente 2
)

Q12018 <- read.delim2 ("RCFL_Microdati_Anno_2018_Primo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".") %>% 
  select(all_of(sel_var))  
Q22018 <- read.delim2 ("RCFL_Microdati_Anno_2018_Secondo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".") %>% 
  select(all_of(sel_var))
Q32018 <- read.delim2 ("RCFL_Microdati_Anno_2018_Terzo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".") %>% 
  select(all_of(sel_var))
Q42018 <- read.delim2 ("RCFL_Microdati_Anno_2018_Quarto_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".") %>% 
  select(all_of(sel_var))


## Merge and clean environment ---- 
# LF2019 <- rbind(Q12019, Q22019, Q32019, Q42019)
# rm(Q12019, Q22019, Q32019, Q42019)

## Merge and clean environment ---- 

LF2018 <- rbind(Q12018, Q22018, Q32018, Q42018)
rm(Q12018, Q22018, Q32018, Q42018)

## Select, format and filter required Variables ----

LF2018s <- LF2018 %>% mutate(Sex = SG11, # sesso
                             Age = CLETAQ, # eta quinquennale 
                             Region = REG,  # regione 
                             Employment_Status = COND3, # empl status
                             Contract_Type = DETIND, # det or indet 
                             Working_time = PIEPAR, # ft o pt
                             Position = POSPRO)

LF2018s <- LF2018s %>% filter(Age > 1 & Age < 12) %>%
  mutate(
    AgeGroup = ifelse(Age <= 5, "15-34", 
                      ifelse(Age <= 9, "35-54",
                             ifelse(Age <= 11, "55+", NA))),
    GeoArea = ifelse(Region <= 8, "North",
                     ifelse(Region <12, "Central", "South" )),
    Sex = ifelse(Sex == 1, "Male","Female"),
    
    Employment = ifelse(Employment_Status == 3, "Inactive",
                        ifelse(Employment_Status == 2, "Unemployed",
                               ifelse(Employment_Status == 1 & DIPIND == 2, "Ind./Atypical Worker",
                                      ifelse(Employment_Status == 1 & DIPIND == 1 & Contract_Type == 1, 
                                             "FTC",
                                             ifelse(Employment_Status == 1 & DIPIND == 1 & Contract_Type == 2, 
                                                    "OEC", 
                                                    NA))))),
    Working_time = ifelse(Working_time == 1, "Full Time", 
                          ifelse(Working_time == 2, "Part Time", NA)),
    PositionGroup = ifelse(Position <= 3, "White Collar", 
                           ifelse(Position == 4, "Blue Collar", 
                                  ifelse(Position >= 5, "Other", NA)))
  )

summary(LF2018)
summary(LF2018s)

LF2018s$Employment <- factor(LF2018s$Employment,
                             levels = c("Inactive",
                                        "Unemployed",
                                        "Ind./Atypical Worker",
                                        "FTC",
                                        "OEC"))

LF2018s$GeoArea <- factor(LF2018s$GeoArea,
                          levels = c("North",
                                     "Central",
                                     "South"))

LF2018s$PositionGroup <- factor(LF2018s$PositionGroup,
                                levels = c("White Collar",
                                           "Blue Collar",
                                           "Other"))

## PROP TABLES -----

### CUSTOM THEME -----
custom_theme <- theme(
  text = element_text(family = "Century Schoolbook", color = "black", size = 11),
  plot.title = element_text(family = "Century Schoolbook", face = "bold", size = 11),  
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = "grey90"), 
  panel.grid.minor = element_line(color = "grey90"),
  strip.background = element_rect(fill = "white", color = NA),
  strip.text = element_text(family = "Century Schoolbook", color = "black", face = "bold"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.title = element_text(family = "Century Schoolbook", color = "black", size = 9, face = "bold"),
  legend.text = element_text(family = "Century Schoolbook", color = "black", size = 9),
  legend.key = element_rect(fill = "white", color = NA),  # Set legend key background to white
  legend.position = "bottom",
  # plot.title = element_blank()
  # plot.title = element_text(family = "Century Schoolbook", color = "black", size = 10)
)

## SEX ----

dbsex <- as.data.frame(prop.table(table(LF2018s$Employment, LF2018s$Sex)))

ggsex <- ggplot(dbsex,
                aes(x = factor(Var2, labels = c("Female", "Male")), 
                    y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sex", y = "Proportion", fill = "Employment Status") +
  scale_fill_manual(values = c("Inactive" = "grey", 
                               "Unemployed" = "darkgrey", 
                               "Ind./Atypical Worker" = "lightblue", 
                               "FTC" = "blue",
                               "OEC" = "darkblue"),  
                    labels = c("Inactive" = "Inactive", 
                               "Unemployed" = "Unemployed", 
                               "Ind./Atypical Worker" = "Ind./Atypical Worker", 
                               "FTC" = "FTC",
                               "EMPLOEC" = "OEC")) +  
  theme_minimal() +
  ggtitle("Sex") +
  theme(legend.position = "bottom") + custom_theme

ggsex

## AGE GROUP ----

dbag <- as.data.frame(prop.table(table(LF2018s$Employment, LF2018s$AgeGroup)))
dbag

ggag <- ggplot(dbag,
               aes(x = factor(Var2, labels = c("15-34", "35-54", "55+")), 
                   y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age Group", y = "Proportion", fill = "Employment Status") +
  scale_fill_manual(values = c("Inactive" = "grey", 
                               "Unemployed" = "darkgrey", 
                               "Ind./Atypical Worker" = "lightblue", 
                               "FTC" = "blue",
                               "OEC" = "darkblue"),  
                    labels = c("Inactive" = "Inactive", 
                               "Unemployed" = "Unemployed", 
                               "Ind./Atypical Worker" = "Ind./Atypical Worker", 
                               "FTC" = "FTC",
                               "EMPLOEC" = "OEC")) +  
  theme_minimal() +
  ggtitle("Age Group") +
  theme(legend.position = "bottom") + custom_theme

ggag 

## GEO AREA  ----

dbga <- as.data.frame(prop.table(table(LF2018s$Employment, LF2018s$GeoArea)))
dbga

ggga <- ggplot(dbga,
               aes(x = factor(Var2, labels = c("North", "Central", "South")), 
                   y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Geo Area", y = "Proportion", fill = "Employment Status") +
  scale_fill_manual(values = c("Inactive" = "grey", 
                               "Unemployed" = "darkgrey", 
                               "Ind./Atypical Worker" = "lightblue", 
                               "FTC" = "blue",
                               "OEC" = "darkblue"),  
                    labels = c("Inactive" = "Inactive", 
                               "Unemployed" = "Unemployed", 
                               "Ind./Atypical Worker" = "Ind./Atypical Worker", 
                               "FTC" = "FTC",
                               "EMPLOEC" = "OEC")) +  
  theme_minimal() +
  ggtitle("Geo Area") +
  theme(legend.position = "bottom") + custom_theme

ggga

## POSITION  ----

dbpos <- as.data.frame(prop.table(table(LF2018s$Employment, LF2018s$PositionGroup)))
dbpos

ggpos <- ggplot(dbpos[dbpos$Var2 != "Other",],
                aes(x = factor(Var2, labels = c("High Wage", "Low Wage")), 
                    y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Position", y = "Proportion", fill = "Employment Status") +
  scale_fill_manual(values = c("Inactive" = "grey", 
                               "Unemployed" = "darkgrey", 
                               "Ind./Atypical Worker" = "lightblue", 
                               "FTC" = "blue",
                               "OEC" = "darkblue"),  
                    labels = c("Inactive" = "Inactive", 
                               "Unemployed" = "Unemployed", 
                               "Ind./Atypical Worker" = "Ind./Atypical Worker", 
                               "FTC" = "FTC",
                               "EMPLOEC" = "OEC")) +  
  theme_minimal() +
  ggtitle("Position") +
  theme(legend.position = "bottom") + custom_theme

ggpos

#### AGGREGATED GRAPHS ----

LFS2018_plot <- ggarrange(ggsex, ggag, ggga, ggpos,
                          nrow = 1, ncol = 4,
                          common.legend = TRUE, 
                          legend = "bottom")



LFS2018_plot

ggsave("C:/RData_DPhil/Paper Projects/Italy EPL Dualization/Figure1.png", plot = LFS2018_plot, 
       dpi = 300, width = 9, height = 4.5)

# *************** -------------------
# DID DATASET PREP -----
## load merged datasets -----

# Sample code. Actual code should be consistent with writexl location from 20220815_Data_Preparation_vF
setwd("C:/RData_DPhil/LoSal db/A. Selected_datasets/")
getwd()
dbhFE <- read_csv2("dbhFE1.txt")
dbhFE <- dbhFE %>% mutate(cell= paste(cell, Region, Position), # cell= paste(SizeClass, ATECO07,WorkTime, AgeGroup, Sex, Region, Position)
                          cell_T = paste(SizeClass, ATECO07, Region, AgeGroup, Sex),
                          cell_F = paste(SizeClass, ATECO07, Region)) 


# cell include: Province, Sector, FirmSize, Contract Type, Position, Worktime, Gender, Age
# Higgins include: FirmsSize, region, Sector, Contract Type, Age, Gender
# Metadati Anagrafica: Age, Sex, Region + Metadati Imprese: FirmSize, Sector

dbh5 <- dbhFE %>% filter(as.numeric(SizeClass) >= 2 & as.numeric(SizeClass) <= 5) %>% 
  mutate(SizeL=ifelse(as.numeric(SizeClass)>=4,1,0)) %>% 
  # filter(dd == 0) %>%   # include only values until Dignity Decree 
  filter(TimeM >= 61 & TimeM <= 162) %>%  # filter incl only Jan 2009 (post summer 2008 dual reform) to June 2018 (pre-DD)
  mutate(
    Month_Year = as.factor(TimeM),
    cell_num = as.numeric(as.factor(cell)), # numeric cell
    cell_T_num = as.numeric(as.factor(cell_T)),
    cell_F_num = as.numeric(as.factor(cell_F)),
    
    TimeM = TimeM - 61,
    first_treat_SL = as.numeric(ifelse(as.numeric(SizeL) == 1, 
                                       30 , 0)), # first treatmnet
    SC2_t = as.numeric(ifelse(as.numeric(SizeClass) == 2, 30, 0)),
    SC3_t = as.numeric(ifelse(as.numeric(SizeClass) == 3, 30, 0)),
    logOEC = log(OEC+1),
    logFTC = log(FTC+1),
    logTDC = log(OEC+FTC+1))   

dbh5 <- dbh5 %>% mutate(TimeM1 = as.numeric(TimeM))
# dbh5_p <- pdata.frame(dbh5_p, index = c("cell", "TimeM1"))

dbh5 <- dbh5 %>% 
  mutate(TimeY = as.numeric(Year.y),
         groupTY = ifelse(SizeL == 1, 3, 0),
         
         TimeHY = ceiling(as.numeric(TimeM) / 6),
         groupTHY = ifelse(SizeL == 1, 5, 0),
         
         TimeInt = as.numeric(int),
         groupInt1 = ifelse(SizeL == 1 , 1, 0),
         groupInt3 = ifelse(SizeL == 1 , 3, 0),
         
         TimeInt_pre = ifelse(as.numeric(int) > 0, as.numeric(int)+1,
                              ifelse(as.numeric(TimeM) <= 15, 0, 1 )),
         groupInt_pre = ifelse(SizeL == 1 , 2, 0)
  )


## Var Prep Interaction GeoArea ----

dbh5 <- dbh5 %>% mutate(GeoArea= ifelse(Region=="VALLE D'AOSTA"|
                                          Region=="PIEMONTE"|
                                          Region=="LIGURIA"|
                                          Region=="LOMBARDIA"|
                                          Region=="EMILIA ROMAGNA"|
                                          Region=="VENETO"|
                                          Region=="FRIULI VENEZIA GIULIA"|
                                          Region=="TRENTINO ALTO ADIGE", "North",
                                        ifelse(Region=="PUGLIA"|
                                                 Region=="ABRUZZO"|
                                                 Region=="BASILICATA"|
                                                 Region=="CALABRIA"|
                                                 Region=="CAMPANIA"|
                                                 Region=="MOLISE"|
                                                 Region=="SARDEGNA"|
                                                 Region=="SICILIA","South and Islands",
                                               "Central")),
                        Position1 = Position,
                        Position = ifelse(Position == "WhiteCollar" | Position == "High_WC",
                                          "WhiteCollar", "BlueCollar"))

## Re-level factors

dbh5 <- within(dbh5, GeoArea <- relevel(factor(dbh5$GeoArea, ordered = FALSE), ref = "North"))
dbh5 <- within(dbh5, Sex <- relevel(factor(dbh5$Sex, ordered = FALSE), ref = "M"))
dbh5 <- within(dbh5, Position <- relevel(factor(dbh5$Position, ordered = FALSE), ref = "WhiteCollar"))
dbh5 <- within(dbh5, AgeGroup <- relevel(factor(dbh5$AgeGroup, ordered = FALSE), ref = "55+"))


## dbh5_placebo db prep ------

unique(dbhFE$SizeClass)
# AROUND ??

dbh5_placebo <- dbhFE %>% filter(as.numeric(SizeClass) >= 7 & as.numeric(SizeClass) <= 8) %>% 
  mutate(SizeL=ifelse(as.numeric(SizeClass)>=8,1,0)) %>% 
  # filter(dd == 0) %>%   # include only values until Dignity Decree 
  filter(TimeM >= 61 & TimeM <= 162) %>%  # filter incl only Jan 2009 (post summer 2008 dual reform) to June 2018 (pre-DD)
  mutate(
    Month_Year = as.factor(TimeM),
    cell_num = as.numeric(as.factor(cell)), # numeric cell
    cell_T_num = as.numeric(as.factor(cell_T)),
    cell_F_num = as.numeric(as.factor(cell_F)),
    
    TimeM = TimeM - 61,
    first_treat_SL = as.numeric(ifelse(as.numeric(SizeL) == 1, 
                                       30 , 0)), # first treatmnet
    SC2_t = as.numeric(ifelse(as.numeric(SizeClass) == 2, 30, 0)),
    SC3_t = as.numeric(ifelse(as.numeric(SizeClass) == 3, 30, 0)),
    logOEC = log(OEC+1),
    logFTC = log(FTC+1),
    logTDC = log(OEC+FTC+1))   

dbh5_placebo <- dbh5_placebo %>% mutate(TimeM1 = as.numeric(TimeM))
# dbh5_p <- pdata.frame(dbh5_p, index = c("cell", "TimeM1"))

dbh5_placebo <- dbh5_placebo %>% 
  mutate(TimeY = as.numeric(Year.y),
         groupTY = ifelse(SizeL == 1, 3, 0),
         
         TimeHY = ceiling(as.numeric(TimeM) / 6),
         groupTHY = ifelse(SizeL == 1, 5, 0),
         
         TimeInt = as.numeric(int),
         groupInt1 = ifelse(SizeL == 1 , 1, 0),
         groupInt3 = ifelse(SizeL == 1 , 3, 0),
         
         TimeInt_pre = ifelse(as.numeric(int) > 0, as.numeric(int)+1,
                              ifelse(as.numeric(TimeM) <= 15, 0, 1 )),
         groupInt_pre = ifelse(SizeL == 1 , 2, 0)
  )


## Var Prep Interaction GeoArea ----

dbh5_placebo <- dbh5_placebo %>% mutate(GeoArea= ifelse(Region=="VALLE D'AOSTA"|
                                                          Region=="PIEMONTE"|
                                                          Region=="LIGURIA"|
                                                          Region=="LOMBARDIA"|
                                                          Region=="EMILIA ROMAGNA"|
                                                          Region=="VENETO"|
                                                          Region=="FRIULI VENEZIA GIULIA"|
                                                          Region=="TRENTINO ALTO ADIGE", "North",
                                                        ifelse(Region=="PUGLIA"|
                                                                 Region=="ABRUZZO"|
                                                                 Region=="BASILICATA"|
                                                                 Region=="CALABRIA"|
                                                                 Region=="CAMPANIA"|
                                                                 Region=="MOLISE"|
                                                                 Region=="SARDEGNA"|
                                                                 Region=="SICILIA","South and Islands",
                                                               "Central")),
                                        Position1 = Position,
                                        Position = ifelse(Position == "WhiteCollar" | Position == "High_WC",
                                                          "WhiteCollar", "BlueCollar"))

## Level factors ------

dbh5_placebo <- within(dbh5_placebo, GeoArea <- relevel(factor(dbh5_placebo$GeoArea, ordered = FALSE), ref = "North"))
dbh5_placebo <- within(dbh5_placebo, Sex <- relevel(factor(dbh5_placebo$Sex, ordered = FALSE), ref = "M"))
dbh5_placebo <- within(dbh5_placebo, Position <- relevel(factor(dbh5_placebo$Position, ordered = FALSE), ref = "WhiteCollar"))
dbh5_placebo <- within(dbh5_placebo, AgeGroup <- relevel(factor(dbh5_placebo$AgeGroup, ordered = FALSE), ref = "55+"))

# **************** --------------------
# DID MODELS AND ROBUSTNESS TESTS ------
# make sure dbh5 and dbh5_placebo are loaded

## STANDARD MODELS DID -----

LOEC1 <- feols(log(OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region )


summary(LOEC1)


LFTC1 <- feols(log(FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)

summary(LFTC1)

LTDC1 <- feols(log(OEC+FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)

summary(LTDC1)


OecShare_m1 <- feols(OECShare ~
                       fa+jahi+jao+SizeL+ 
                       fa:SizeL+jahi:SizeL +jao:SizeL
                     | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                     , data = dbh5
                     , vcov = ~ cell_T + Region)

summary(OecShare_m1)

## PLACEBO TEST ----

# 31-40 vs 41-50 

LOEC1_p <- feols(log(OEC+1) ~
                   fa+jahi+jao+SizeL+ 
                   fa:SizeL+jahi:SizeL +jao:SizeL
                 | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                 , data = dbh5_placebo
                 , vcov = ~ cell_T + Region )


summary(LOEC1_p)


LFTC1_p <- feols(log(FTC+1) ~
                   fa+jahi+jao+SizeL+ 
                   fa:SizeL+jahi:SizeL +jao:SizeL
                 | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                 , data = dbh5_placebo
                 , vcov = ~ cell_T + Region)

summary(LFTC1_p)

LTDC1_p <- feols(log(OEC+FTC+1) ~
                   fa+jahi+jao+SizeL+ 
                   fa:SizeL+jahi:SizeL +jao:SizeL
                 | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                 , data = dbh5_placebo
                 , vcov = ~ cell_T + Region)

summary(LTDC1_p)


OecShare_m1_p <- feols(OECShare ~
                         fa+jahi+jao+SizeL+ 
                         fa:SizeL+jahi:SizeL +jao:SizeL
                       | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                       , data = dbh5_placebo
                       , vcov = ~ cell_T + Region)

summary(OecShare_m1_p)

## NEWEY WEST ----

LOEC1_NW <- feols(log(OEC+1) ~
                    fa+jahi+jao+SizeL+ 
                    fa:SizeL+jahi:SizeL +jao:SizeL
                  | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                  , data = dbh5
                  , vcov = "NW" 
                  , panel.id = c("cell", "TimeM"))


summary(LOEC1_NW)

LFTC1_NW <- feols(log(FTC+1) ~
                    fa+jahi+jao+SizeL+ 
                    fa:SizeL+jahi:SizeL +jao:SizeL
                  | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                  , data = dbh5
                  , vcov = "NW" 
                  , panel.id = c("cell", "TimeM"))

summary(LFTC1_NW)

LTDC1_NW <- feols(log(OEC+FTC+1) ~
                    fa+jahi+jao+SizeL+ 
                    fa:SizeL+jahi:SizeL +jao:SizeL
                  | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                  , data = dbh5
                  , vcov = "NW" 
                  , panel.id = c("cell", "TimeM"))

summary(LTDC1_NW)


OecShare_m1_NW <- feols(OECShare ~
                          fa+jahi+jao+SizeL+ 
                          fa:SizeL+jahi:SizeL +jao:SizeL
                        | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                        , data = dbh5
                        , vcov = "NW" 
                        , panel.id = c("cell", "TimeM"))


summary(OecShare_m1_NW)

## DRISCOLL KRAAY ----

LOEC1_DK <- feols(log(OEC+1) ~
                    fa+jahi+jao+SizeL+ 
                    fa:SizeL+jahi:SizeL +jao:SizeL
                  | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                  , data = dbh5
                  , vcov = "DK" 
                  , panel.id = c("cell", "TimeM"))


summary(LOEC1_DK)

LFTC1_DK <- feols(log(FTC+1) ~
                    fa+jahi+jao+SizeL+ 
                    fa:SizeL+jahi:SizeL +jao:SizeL
                  | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                  , data = dbh5
                  , vcov = "DK" 
                  , panel.id = c("cell", "TimeM"))

summary(LFTC1_DK)

LTDC1_DK <- feols(log(OEC+FTC+1) ~
                    fa+jahi+jao+SizeL+ 
                    fa:SizeL+jahi:SizeL +jao:SizeL
                  | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                  , data = dbh5
                  , vcov = "DK" 
                  , panel.id = c("cell", "TimeM"))

summary(LTDC1_DK)


OecShare_m1_DK <- feols(OECShare ~
                          fa+jahi+jao+SizeL+ 
                          fa:SizeL+jahi:SizeL +jao:SizeL
                        | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                        , data = dbh5
                        , vcov = "DK" 
                        , panel.id = c("cell", "TimeM"))


summary(OecShare_m1_DK)

# ***************** -----------------

# TRIPLE DID MODELS ------------------

# ***************** ------------

## TOTAL HIRINGS -----

## DIFF IN DIFF -----

LTDC <- feols(log(OEC+FTC+1) ~
                fa+jahi+jao+SizeL+ 
                fa:SizeL+jahi:SizeL +jao:SizeL 
              
              | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
              , data = dbh5
              , vcov = ~ cell_T  )


summary(LTDC)

## TRIPLE DIFF IN DIFF ----
### POSITION - OCCUPATIONAL LEVEL -----

LTDCP <- feols(log(OEC+FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 Position + Position:SizeL + 
                 Position:fa + Position:jahi + Position:jao +
                 fa:SizeL:Position + jahi:SizeL:Position + jao:SizeL:Position
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime
               , data = dbh5
               , vcov = ~ cell_T + Region )

### AGE GROUP -----

LTDCA <- feols(log(FTC+OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 AgeGroup + AgeGroup:SizeL +
                 AgeGroup:fa + AgeGroup:jahi + AgeGroup:jao +
                 fa:SizeL:AgeGroup + jahi:SizeL:AgeGroup + jao:SizeL:AgeGroup
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)


### GEO AREA ------

LTDCG <- feols(log(FTC+OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 GeoArea + GeoArea:SizeL +
                 GeoArea:fa + GeoArea:jahi + GeoArea:jao +
                 fa:SizeL:GeoArea + jahi:SizeL:GeoArea + jao:SizeL:GeoArea
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region )

### SEX -----

LTDCS <- feols(log(FTC+OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 Sex + Sex:SizeL + Sex:fa + Sex:jahi + Sex:jao +
                 Sex:fa + Sex:jahi + Sex:jao +
                 fa:SizeL:Sex + jahi:SizeL:Sex + jao:SizeL:Sex
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)


#### SUMMARY -----
summary(LTDCP)
summary(LTDCA)
summary(LTDCS)
summary(LTDCG)
summary(LTDCW)

# ************************ -----

# OEC HIRINGS -----
## DIFF IN DIFF -----

LOEC <- feols(log(OEC+1) ~
                fa+jahi+jao+SizeL+ 
                fa:SizeL+jahi:SizeL +jao:SizeL 
              
              | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
              , data = dbh5
              , vcov = ~ cell_T  )


summary(LOEC)

## TRIPLE DIFF IN DIFF ----
### POSITION - OCCUPATIONAL LEVEL -----

LOECP <- feols(log(OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 Position + Position:SizeL + 
                 Position:fa + Position:jahi + Position:jao +
                 fa:SizeL:Position + jahi:SizeL:Position + jao:SizeL:Position
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime
               , data = dbh5
               , vcov = ~ cell_T  )

### AGE GROUP -----

LOECA <- feols(log(OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 AgeGroup + AgeGroup:SizeL +
                 AgeGroup:fa + AgeGroup:jahi + AgeGroup:jao +
                 fa:SizeL:AgeGroup + jahi:SizeL:AgeGroup + jao:SizeL:AgeGroup
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)


### GEO AREA ------

LOECG <- feols(log(OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 GeoArea + GeoArea:SizeL +
                 GeoArea:fa + GeoArea:jahi + GeoArea:jao +
                 fa:SizeL:GeoArea + jahi:SizeL:GeoArea + jao:SizeL:GeoArea
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region )

### SEX -----

LOECS <- feols(log(OEC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 Sex + Sex:SizeL + Sex:fa + Sex:jahi + Sex:jao +
                 Sex:fa + Sex:jahi + Sex:jao +
                 fa:SizeL:Sex + jahi:SizeL:Sex + jao:SizeL:Sex
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)


#### SUMMARY -----
summary(LOECP)
summary(LOECA)
summary(LOECS)
summary(LOECG)
summary(LOECW)

# ************************ ----------------

# FTC HIRINGS -----

## DIFF IN DIFF -----

LFTC <- feols(log(FTC+1) ~
                fa+jahi+jao+SizeL+ 
                fa:SizeL+jahi:SizeL +jao:SizeL 
              
              | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
              , data = dbh5
              , vcov = ~ cell_T  )


summary(LFTC)

## TRIPLE DIFF IN DIFF ----
### POSITION - OCCUPATIONAL LEVEL -----

LFTCP <- feols(log(FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 Position + Position:SizeL + 
                 Position:fa + Position:jahi + Position:jao +
                 fa:SizeL:Position + jahi:SizeL:Position + jao:SizeL:Position
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime
               , data = dbh5
               , vcov = ~ cell_T  )

### AGE GROUP -----

LFTCA <- feols(log(FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 AgeGroup + AgeGroup:SizeL +
                 AgeGroup:fa + AgeGroup:jahi + AgeGroup:jao +
                 fa:SizeL:AgeGroup + jahi:SizeL:AgeGroup + jao:SizeL:AgeGroup
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)


### GEO AREA ------

LFTCG <- feols(log(FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 GeoArea + GeoArea:SizeL +
                 GeoArea:fa + GeoArea:jahi + GeoArea:jao +
                 fa:SizeL:GeoArea + jahi:SizeL:GeoArea + jao:SizeL:GeoArea
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region )

### SEX -----

LFTCS <- feols(log(FTC+1) ~
                 fa+jahi+jao+SizeL+ 
                 fa:SizeL+jahi:SizeL +jao:SizeL +
                 
                 Sex + Sex:SizeL + Sex:fa + Sex:jahi + Sex:jao +
                 Sex:fa + Sex:jahi + Sex:jao +
                 fa:SizeL:Sex + jahi:SizeL:Sex + jao:SizeL:Sex
               
               | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
               , data = dbh5
               , vcov = ~ cell_T + Region)


#### SUMMARY -----
summary(LFTCP)
summary(LFTCA)
summary(LFTCS)
summary(LFTCG)
summary(LFTCW)

# ********************************* ------------------

# OEC SHARE -----

OECShare_m1 <- feols(OECShare ~
                       fa+jahi+jao+SizeL+ 
                       fa:SizeL+jahi:SizeL +jao:SizeL 
                     
                     | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                     , data = dbh5
                     , vcov = ~ cell_T + Region  )


summary(OECShare_m1)

## TRIPLE DIFF IN DIFF ----
### POSITION - OCCUPATIONAL LEVEL -----

OECShare_P <- feols(OECShare ~
                      fa+jahi+jao+SizeL+ 
                      fa:SizeL+jahi:SizeL +jao:SizeL +
                      
                      Position + Position:SizeL + 
                      Position:fa + Position:jahi + Position:jao +
                      fa:SizeL:Position + jahi:SizeL:Position + jao:SizeL:Position
                    
                    | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime
                    , data = dbh5
                    , vcov = ~ cell_T + Region )

summary(OECShare_P)

### AGE GROUP -----

OECShare_A <- feols(OECShare ~
                      fa+jahi+jao+SizeL+ 
                      fa:SizeL+jahi:SizeL +jao:SizeL +
                      
                      AgeGroup + AgeGroup:SizeL +
                      AgeGroup:fa + AgeGroup:jahi + AgeGroup:jao +
                      fa:SizeL:AgeGroup + jahi:SizeL:AgeGroup + jao:SizeL:AgeGroup
                    
                    | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                    , data = dbh5
                    , vcov = ~ cell_T + Region)

summary(OECShare_A)

### GEO AREA ------

OECShare_G <- feols(OECShare ~
                      fa+jahi+jao+SizeL+ 
                      fa:SizeL+jahi:SizeL +jao:SizeL +
                      
                      GeoArea + GeoArea:SizeL +
                      GeoArea:fa + GeoArea:jahi + GeoArea:jao +
                      fa:SizeL:GeoArea + jahi:SizeL:GeoArea + jao:SizeL:GeoArea
                    
                    | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                    , data = dbh5
                    , vcov = ~ cell_T + Region )

summary(OECShare_G)

### SEX -----

OECShare_S <- feols(OECShare ~
                      fa+jahi+jao+SizeL+ 
                      fa:SizeL+jahi:SizeL +jao:SizeL +
                      
                      Sex + Sex:SizeL + Sex:fa + Sex:jahi + Sex:jao +
                      Sex:fa + Sex:jahi + Sex:jao +
                      fa:SizeL:Sex + jahi:SizeL:Sex + jao:SizeL:Sex
                    
                    | cell_T + as.factor(Year.y) + as.factor(Month) + WorkTime + Position
                    , data = dbh5
                    , vcov = ~ cell_T + Region)

summary(OECShare_S)

#### SUMMARY -----
summary(OECShare_P)
summary(OECShare_A)
summary(OECShare_S)
summary(OECShare_G)
summary(OECShare_W)

# ************************** ------------------

# ROBUSTNESS TESTS USING DID PACKAGE -----
# DiD by stratification group using Sant'Anna Callway robust DiD method

# DID MODELS -------

# PARALLEL TRENDS TESTING
did_m1 <- att_gt(
  yname = "logOEC",
  tname = "TimeInt_pre",
  idname = "cell_T_num",
  gname = "groupInt_pre",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_m1)

summary(did_m1)
tidy(summary(did_m1))
kable(tidy(did_m1), format = "pipe", digits = 4)


ggdid(did_m1) + 
  labs(title = "OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("2" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5), 
                     labels = c("Pre-Fornero (2010-2011)", 
                                "Pre Fornero (2011-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10))


custom_gg <- labs(title = "OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("2" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5), 
                     labels = c("Pre-Fornero (2010-2011)", 
                                "Pre Fornero (2011-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10))

# SUBGROUPS DID ANALYSIS -----
## SEX ----
### FEMALE -----
unique(dbh5$Sex)
dbh5SF <- dbh5 %>% filter(Sex == "F")

did_mSF <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5SF,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mSF) +
  labs(title = "Female Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

### MALE -----
unique(dbh5$Sex)
dbh5SM <- dbh5 %>% filter(Sex == "M")

did_mSM <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5SM,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mSM) +
  labs(title = "Male Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

## AGE GROUPS ----
### YOUNG -----
unique(dbh5$AgeGroup)
dbh5young <- dbh5 %>% filter(AgeGroup == "<35")

did_mYOUNG <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5young,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mYOUNG) +
  labs(title = "Young (<35 y.o.) Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

### PRIME AGE -----

unique(dbh5$AgeGroup)
dbh5pa <- dbh5 %>% filter(AgeGroup == "35-54")

did_mPA <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5pa,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mPA) +
  labs(title = "Prime Age (35-54 y.o.) Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

### SENIOR -----

unique(dbh5$AgeGroup)
dbh5se <- dbh5 %>% filter(AgeGroup == "55+")

did_mSE <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5se,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mSE) +
  labs(title = "Senior (55+ y.o.) Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )


## GEO AREA ----
### SOUTH -----
unique(dbh5$GeoArea)
dbh5south <- dbh5 %>% filter(GeoArea == "South and Islands")

did_mSOUTH <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5south,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mSOUTH) +
  labs(title = "South Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

### CENTRAL -----
unique(dbh5$GeoArea)
dbh5central <- dbh5 %>% filter(GeoArea == "Central")

did_mCEN <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5central,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mCEN) +
  labs(title = "Central Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

### NORTH -----
unique(dbh5$GeoArea)
dbh5north <- dbh5 %>% filter(GeoArea == "North")

did_mNOR <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y + Position + WorkTime,
  data = dbh5north,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mNOR) +
  labs(title = "North Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

## POSITION ----
### BLUE COLLAR -----
unique(dbh5$Position)
dbh5BC <- dbh5 %>% filter(Position == "BlueCollar")

did_mBC <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y 
  # + Position 
  + WorkTime,
  data = dbh5BC,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mBC) +
  labs(title = "Blue Collar Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

### WHITE COLLAR -----
unique(dbh5$Position)
dbh5WC <- dbh5 %>% filter(Position == "WhiteCollar")

did_mWC <- att_gt(
  yname = "logOEC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y 
  # + Position 
  + WorkTime,
  data = dbh5WC,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mWC) +
  labs(title = "White Collar Only - OEC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

# FTC DID MODELS -----

#### BLUE COLLARS FTC ------

unique(dbh5$Position)
dbh5BC <- dbh5 %>% filter(Position == "BlueCollar")

did_mBC_FTC <- att_gt(
  yname = "logFTC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y 
  # + Position 
  + WorkTime,
  data = dbh5BC,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mBC_FTC) +
  labs(title = "Blue Collar Only - FTC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )



### WHITE COLLAR FTC -----
unique(dbh5$Position)
dbh5WC <- dbh5 %>% filter(Position == "WhiteCollar")

did_mWC_FTC <- att_gt(
  yname = "logFTC",
  tname = "TimeInt",
  idname = "cell_T_num",
  gname = "groupInt1",
  xformla = ~ Month:Year.y 
  # + Position 
  + WorkTime,
  data = dbh5WC,
  panel = T,
  allow_unbalanced_panel = T,
  base_period = "universal",
  anticipation = 0,
  clustervars = c("cell_T_num", "Region")
)

ggdid(did_mWC_FTC) +
  labs(title = "White Collar Only - FTC % Change") +
  facet_wrap(~ group, 
             labeller = labeller(group = c("1" = "Large Firms"))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4), 
                     labels = c("Pre Fornero (2010-2012)",
                                "Post Fornero (2012-2015)", 
                                "Hiring Incentives (2015-2016)",
                                "Jobs Act only (2017-2018)")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   family = "Century Schoolbook",
                                   size = 10),
        text = element_text(family = "Century Schoolbook", size = 10),
        strip.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
  )

# ********************************************** --------------------

# WORDREG TABLES ----

## DID MODELS  ----

DiDmodels <- wordreg(list(LTDC1, LOEC1, LFTC1, OecShare_m1),
                     digits =3,
                     title = "Hiring Flows",
                     custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                     custom.coef.map =
                       list("fa:SizeL" = "Fornero Reform X Large Firms ",
                            "jahi:SizeL" = "Hiring Incentives X Large Firms",
                            "jao:SizeL" = "Jobs Act only X Large Firms"
                       ),
                     custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                     custom.gof.rows = list(
                       "Cell FE" = c("x", "x", "x", "x"),
                       "Year dummies" = c("x", "x", "x", "x"),
                       "Month dummies" = c("x", "x", "x", "x")
                     ),
                     
                     stars = c(0.01, 0.05, 0.1),
                     file = "DiDmodels.docx",
                     doctype = F
)

AIC(LTDC1 , LOEC1, LFTC1, OecShare_m1)
BIC(LTDC1 , LOEC1, LFTC1, OecShare_m1)

## WORKERS FEATURES -----
imodel_worker <- wordreg(list( LOECS, LOECA, LFTCS, LFTCA),
                         digits =3,
                         title = "Hiring Flows: OEC and FTC Hirings",
                         custom.model.names = c("Sex","Age Group", "Sex","Age Group"),
                         custom.coef.map =
                           list(
                             "fa:SizeL:SexF" = "Fornero Reform X Large Firms X Female ",
                             "jao:SizeL:SexF" = "Jobs Act only X Large Firms X Female",
                             "fa:SizeL:AgeGroup<35" = "Fornero Reform X Large Firms X Young",
                             "fa:SizeL:AgeGroup35-54" = "Fornero Reform X Large Firms X Prima Age",
                             "jao:SizeL:AgeGroup<35" = "Jobs Act only X Large Firms X Young",
                             "jao:SizeL:AgeGroup35-54" = "Jobs Act only X Large Firms X Prime Age"),
                         
                         custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                         custom.gof.rows = list(
                           "Cell FE" = c("x", "x", "x", "x"),
                           "Year dummies" = c("x", "x", "x", "x"),
                           "Month dummies" = c("x", "x", "x", "x")),
                         
                         stars = c(0.01, 0.05, 0.1),
                         file = "imodel_worker1.docx",
                         doctype = F)

## JOB FEATURES ------

imodel_job <- wordreg(list(LOECG, LOECP, LOECW, LFTCG, LFTCP, LFTCW),
                      digits =3,
                      title =  "Hiring Flows: Total and OEC Hirings",
                      custom.model.names = c("Geo Area", "Position","Work Time", "Geo Area", "Position","Work Time"),
                      custom.coef.map =
                        list(
                          "fa:SizeL:GeoAreaSouth and Islands" = "Fornero Reform X Large Firms X South and Islands",
                          "fa:SizeL:GeoAreaCentral" = "Fornero Reform X Large Firms X Central Italy",
                          "jao:SizeL:GeoAreaSouth and Islands" = "Jobs Act only X Large South and Islands",
                          "jao:SizeL:GeoAreaCentral" = "Jobs Act only X Large Firms X Central Italy",   
                          
                          "fa:SizeL:PositionBlueCollar" = "Fornero Reform X Large Firms X Blue Collar",
                          "jao:SizeL:PositionBlueCollar" = "Jobs Act only X Large Firms X Blue Collar"
                          )

                      custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                      custom.gof.rows = list(
                        "Cell FE" = c("x", "x", "x", "x", "x", "x" ),
                        "Year dummies" = c("x", "x", "x", "x", "x", "x"),
                        "Month dummies" = c("x", "x", "x", "x", "x", "x")
                      ),
                      
                      stars = c(0.01, 0.05, 0.1),
                      file = "imodel_job1.docx",
                      doctype = F
)


### PLACEBO TEST AROUND 40 ------ 

DiDmodels_p <- wordreg(list(LTDC1_p, LOEC1_p, LFTC1_p, OecShare_m1_p),
                       digits =3,
                       title = "Hiring Flows",
                       custom.model.names = c("Total Hirings","OEC Hrings","FTC Hrings", "OEC Share"),
                       custom.coef.map =
                         list("fa:SizeL" = "Fornero Reform X >40 Employees Firms",
                              "jahi:SizeL" = "Hiring Incentives X >40 Employees Firms",
                              "jao:SizeL" = "Jobs Act only X >40 Employees Firms"
                         ),
                       custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                       custom.gof.rows = list(
                         "Cell FE" = c("x", "x", "x", "x"),
                         "Year dummies" = c("x", "x", "x", "x"),
                         "Month dummies" = c("x", "x", "x", "x")
                       ),
                       
                       stars = c(0.01, 0.05, 0.1),
                       file = "DiDmodels_Placebo.docx",
                       doctype = F
)

### NEWEY WEST ------ 

DiDmodels_NW <- wordreg(list(LTDC1_NW, LOEC1_NW, LFTC1_NW, OecShare_m1_NW),
                        digits =3,
                        title = "Hiring Flows",
                        custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                        custom.coef.map =
                          list("fa:SizeL" = "Fornero Reform X Large Firms",
                               "jahi:SizeL" = "Hiring Incentives X Large Firms",
                               "jao:SizeL" = "Jobs Act only X Large Firms"
                          ),
                        custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                        custom.gof.rows = list(
                          "Cell FE" = c("x", "x", "x", "x"),
                          "Year dummies" = c("x", "x", "x", "x"),
                          "Month dummies" = c("x", "x", "x", "x")
                        ),
                        
                        stars = c(0.01, 0.05, 0.1),
                        file = "DiDmodels_NW.docx",
                        doctype = F
)

### DRISCOLL KRAAY ------ 

DiDmodels_DK <- wordreg(list(LTDC1_DK, LOEC1_DK, LFTC1_DK, OecShare_m1_DK),
                        digits =3,
                        title = "Hiring Flows",
                        custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                        custom.coef.map =
                          list("fa:SizeL" = "Fornero Reform X Large Firms",
                               "jahi:SizeL" = "Hiring Incentives X Large Firms",
                               "jao:SizeL" = "Jobs Act only X Large Firms"
                          ),
                        custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                        custom.gof.rows = list(
                          "Cell FE" = c("x", "x", "x", "x"),
                          "Year dummies" = c("x", "x", "x", "x"),
                          "Month dummies" = c("x", "x", "x", "x")
                        ),
                        
                        stars = c(0.01, 0.05, 0.1),
                        file = "DiDmodels_DK.docx",
                        doctype = F
)

# APPENDIX TRIPLE INTERACTIONS -----

## SEX ----

summary(LTDCS)

DiDmodels_S <- wordreg(list(LTDCS, LOECS, LFTCS, OECShare_S),
                       digits =3,
                       title = "Hiring Flows",
                       custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                       custom.coef.map =
                         list(
                           "fa:SizeL"="Fornero Reform X Large Firms",
                           "jahi:SizeL"="Hiring Incentives X Large Firms",
                           "jao:SizeL"="Jobs Act only X Large Firms",
                           
                           "fa:SizeL:SexF"="Fornero Reform X Large Firms X Female",
                           "jahi:SizeL:SexF"="Hiring Incentives X Large Firms X Female",
                           "jao:SizeL:SexF"="Jobs Act only X Large Firms X Female",
                           
                           "fa:SizeL" = "Fornero Reform X Large Firms ",
                           "jahi:SizeL" = "Hiring Incentives X Large Firms",
                           "jao:SizeL" = "Jobs Act only X Large Firms"
                         ),
                       custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                       custom.gof.rows = list(
                         "Cell FE" = c("x", "x", "x", "x"),
                         "Year dummies" = c("x", "x", "x", "x"),
                         "Month dummies" = c("x", "x", "x", "x")
                       ),
                       
                       stars = c(0.01, 0.05, 0.1),
                       file = "DiDmodels_S.docx",
                       doctype = F
)

c(AIC(LTDCS , LOECS, LFTCS, OECShare_S))
BIC(LTDCS , LOECS, LFTCS, OECShare_S)

## AGE GROUP ----

summary(LTDCA)

DiDmodels_AG <- wordreg(list(LTDCA, LOECA, LFTCA, OECShare_A),
                        digits =3,
                        title = "Hiring Flows",
                        custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                        custom.coef.map =
                          list(
                            "fa:SizeL"="Fornero Reform X Large Firms",
                            "jahi:SizeL"="Hiring Incentives X Large Firms",
                            "jao:SizeL"="Jobs Act only X Large Firms",
                            
                            "fa:SizeL:AgeGroup<35"="Fornero Reform X Large Firms X Young",
                            "jahi:SizeL:AgeGroup<35"="Hiring Incentives X Large Firms X Young",
                            "jao:SizeL:AgeGroup<35"="Jobs Act only X Large Firms X Young",
                            
                            "fa:SizeL:AgeGroup35-54"="Fornero Reform X Large Firms X Prime Age",
                            "jahi:SizeL:AgeGroup35-54"="Hiring Incentives X Large Firms X Prime Age",
                            "jao:SizeL:AgeGroup35-54"="Jobs Act only X Large Firms X Prime Age"
                          ),
                        custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                        custom.gof.rows = list(
                          "Cell FE" = c("x", "x", "x", "x"),
                          "Year dummies" = c("x", "x", "x", "x"),
                          "Month dummies" = c("x", "x", "x", "x")
                        ),
                        
                        stars = c(0.01, 0.05, 0.1),
                        file = "DiDmodels_AG.docx",
                        doctype = F
)

AIC(LTDCA , LOECA, LFTCA, OECShare_A)
BIC(LTDCA , LOECA, LFTCA, OECShare_A)

## GEO AREA ----

summary(LTDCG)

DiDmodels_GA <- wordreg(list(LTDCG, LOECG, LFTCG, OECShare_G),
                        digits =3,
                        title = "Hiring Flows",
                        custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                        custom.coef.map =
                          list(
                            "fa:SizeL"="Fornero Reform X Large Firms",
                            "jahi:SizeL"="Hiring Incentives X Large Firms",
                            "jao:SizeL"="Jobs Act only X Large Firms",
                            
                            "fa:SizeL:GeoAreaSouth and Islands"="Fornero Reform X Large Firms X South and Islands",
                            "jahi:SizeL:GeoAreaSouth and Islands"="Hiring Incentives X Large Firms X South and Islands",
                            "jao:SizeL:GeoAreaSouth and Islands"="Jobs Act only X Large Firms X South and Islands",
                            
                            "fa:SizeL:GeoAreaCentral"="Fornero Reform X Large Firms X Central Italy",
                            "jahi:SizeL:GeoAreaCentral"="Hiring Incentives X Large Firms X Central Italy",
                            "jao:SizeL:GeoAreaCentral"="Jobs Act only X Large Firms X Central Italy"
                            
                          ),
                        custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                        custom.gof.rows = list(
                          "Cell FE" = c("x", "x", "x", "x"),
                          "Year dummies" = c("x", "x", "x", "x"),
                          "Month dummies" = c("x", "x", "x", "x")
                        ),
                        
                        stars = c(0.01, 0.05, 0.1),
                        file = "DiDmodels_GA.docx",
                        doctype = F
)

AIC(LTDCG , LOECG, LFTCG, OECShare_G)
BIC(LTDCG , LOECG, LFTCG, OECShare_G)

## POSITION ----

summary(LTDCP)

DiDmodels_P <- wordreg(list(LTDCP, LOECP, LFTCP, OECShare_P),
                       digits =3,
                       title = "Hiring Flows",
                       custom.model.names = c("Total Hirings","OEC Hirings","FTC Hirings", "OEC Share"),
                       custom.coef.map =
                         list(
                           
                           "fa:SizeL"="Fornero Reform X Large Firms",
                           "jahi:SizeL"="Hiring Incentives X Large Firms",
                           "jao:SizeL"="Jobs Act only X Large Firms",
                           
                           "fa:SizeL:PositionBlueCollar"="Fornero Reform X Large Firms X Blue Collar",
                           "jahi:SizeL:PositionBlueCollar"="Hiring Incentives X Large Firms X Blue Collar",
                           "jao:SizeL:PositionBlueCollar"="Jobs Act only X Large Firms X Blue Collar",
                           
                         ),
                       custom.note = "Standard errors in parentheses *** p < 0.01; ** p < 0.05; * p < 0.1, controls include also DiD non interacted variables policy change time and large firms. Moreover, the model includes cell fixed effects as well as controls for year and month dummies, work time and position. Note that cells are defined as firm size, 2-digit sector code, region, workers' age group and sex. SE are clustered at cell and region level",
                       custom.gof.rows = list(
                         "Cell FE" = c("x", "x", "x", "x"),
                         "Year dummies" = c("x", "x", "x", "x"),
                         "Month dummies" = c("x", "x", "x", "x")
                       ),
                       
                       stars = c(0.01, 0.05, 0.1),
                       file = "DiDmodels_P.docx",
                       doctype = F
)

AIC(LTDCP , LOECP, LFTCP, OECShare_P)
BIC(LTDCP , LOECP, LFTCP, OECShare_P)

# ******************* ---------------

# GGPLOTS -----

## Custom themes ---------

font_import()
y
fonts()
font_add(family = "CMU Sans Serif", regular = "C:/Windows/Fonts/cmunrm.ttf")

custom_theme <- theme(
  text = element_text(family = "Century Schoolbook", color = "black", size = 11),
  plot.title = element_text(family = "Century Schoolbook", face = "bold", size = 11),  
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = "grey90"), 
  panel.grid.minor = element_line(color = "grey90"),
  strip.background = element_rect(fill = "white", color = NA),
  strip.text = element_text(family = "Century Schoolbook", color = "black", face = "bold"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.title = element_text(family = "Century Schoolbook", color = "black", size = 9, face = "bold"),
  legend.text = element_text(family = "Century Schoolbook", color = "black", size = 9),
  legend.key = element_rect(fill = "white", color = NA),  # Set legend key background to white
  legend.position = "bottom",
  # plot.title = element_blank()
  # plot.title = element_text(family = "Century Schoolbook", color = "black", size = 10)
)

fonts()


custom_theme_pres <- theme(
  text = element_text(family = "CMU Sans Serif", color = "black", size = 11),
  plot.title = element_text(family = "CMU Sans Serif", face = "bold", size = 11),  
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = "grey90"), 
  panel.grid.minor = element_line(color = "grey90"),
  strip.background = element_rect(fill = "white", color = NA),
  strip.text = element_text(family = "CMU Sans Serif", color = "black", face = "bold"),
  legend.background = element_rect(fill = "white", color = NA),
  legend.title = element_text(family = "CMU Sans Serif", color = "black", size = 9, face = "bold"),
  legend.text = element_text(family = "CMU Sans Serif", color = "black", size = 9),
  legend.key = element_rect(fill = "white", color = NA),  # Set legend key background to white
  legend.position = "bottom"
)

## SET UP DID -----

LTDC1
LOEC1
LFTC1
OecShare_m1

m_list <- list(LTDC1, LOEC1, LFTC1, OecShare_m1)
vars <- c("Fornero X LF",
          "Hiring Incentives X Large Firms",
          "Jobs Act X LF")

for (i in 1:length(m_list)) { 
  assign(paste("betas", i, sep = ""), coefficients(m_list[[i]])[(length(coefficients(m_list[[i]])) - 2):length(coefficients(m_list[[i]]))])
  
  conf_intervals <- confint(m_list[[i]])[(nrow(confint(m_list[[i]])) - 2):nrow(confint(m_list[[i]])),]
  assign(paste("lb", i, sep = ""), conf_intervals[,1])
  assign(paste("ub", i, sep = ""), conf_intervals[,2])
  
  p_values <- pvalue(m_list[[i]])[(length(pvalue(m_list[[i]])) - 2):length(pvalue(m_list[[i]]))]
  assign(paste("pv", i, sep = ""), p_values)
  assign(paste("d", i, sep = ""), data.frame(paste("betas",i, sep=""),
                                             paste("pv",i, sep=""),
                                             vars,
                                             paste("lb",i, sep=""),
                                             paste("ub",i, sep="")))
}

rm(conf_intervals, p_values)

d1 <- data.frame(betas1,pv1,vars,lb1,ub1)
d2 <- data.frame(betas2,pv2,vars,lb2,ub2)
d3 <- data.frame(betas3,pv3,vars,lb3,ub3)
d4 <- data.frame(betas4,pv4,vars,lb4,ub4)

d1$model <- "Total Hirings"
d2$model <- "OEC Hirings"
d3$model <- "FTC Hirings"
d4$model <- "OEC Share"

colnames(d1) <- c("betas","pv","vars", "lb", "ub", "model")
colnames(d2) <- c("betas","pv","vars", "lb", "ub", "model")
colnames(d3) <- c("betas","pv","vars", "lb", "ub", "model")
colnames(d4) <- c("betas","pv","vars", "lb", "ub", "model")

dDID <- bind_rows(d1, d2, d3, d4)

dDID <- dDID %>% 
  filter(vars != "Hiring Incentives X Large Firms") %>%
  filter(model != "Total Hirings") %>% 
  mutate(model= factor(model,
                       levels = c( # "Total Hirings",
                         "OEC Hirings",
                         "FTC Hirings",
                         "OEC Share")),
         vars = factor(vars,
                       levels = c("Jobs Act X LF",
                                  "Fornero X LF")))


## GGPLOT DID -----

plotDID <- 
  ggplot(dDID, aes(x = betas, y = vars)) +
  geom_point(aes(color = factor(pv < 0.05)), size = 3) +
  geom_errorbarh(aes(xmin = lb, xmax = ub)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Changes in Hiring Flows",
       subtitle = "95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variables") +
  facet_grid(~ factor(model,
                      levels = c("Total Hirings", 
                                 "OEC Hirings",
                                 "FTC Hirings",
                                 "OEC Share"))) +
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  theme(text = element_text(family = "Times New Roman")) +
  labs(color = "P-value < 5%:") +
  scale_color_manual(
    values = c("TRUE" = "lightblue", 
               "FALSE" = "#FF9999"), # Customize colors as needed
    labels = c("False", "True")  # Re-label legend text
  ) +
  custom_theme

plotDID

ggsave("Figure2.png", plot = plotDID, 
       dpi = 300, width = 6, height = 4, )

plotDID_pres <- 
  ggplot(dDID, aes(x = betas, y = vars)) +
  geom_point(aes(color = factor(pv < 0.05)), size = 3) +
  geom_errorbarh(aes(xmin = lb, xmax = ub)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Changes in Hiring Flows",
       subtitle = "95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variables") +
  facet_grid(~ factor(model,
                      levels = c("Total Hirings", 
                                 "OEC Hirings",
                                 "FTC Hirings",
                                 "OEC Share"))) +
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  theme(text = element_text(family = "CMU Sans Serif")) +
  labs(color = "P-value < 5%:") +
  scale_color_manual(
    values = c("TRUE" = "lightblue", 
               "FALSE" = "#FF9999"), # Customize colors as needed
    labels = c("False", "True")  # Re-label legend text
  ) +
  custom_theme_pres

plotDID
plotDID_pres

png("plotDID_pres.png", width = 800, height = 600, res = 150)
plotDID_pres
dev.off()

## SET UP INTERACTIONS -----

im_list <- list(LFTCS, LOECS, LFTCA, LOECA, LFTCP, LOECP, LFTCG, LOECG, LFTCW, LOECW)


ivars <- c(
  rep(c("Fornero Reform X Large Firms X Female",
        "Jobs Act only X Large Firms X Female"),times=2),
  
  rep(c("Fornero Reform X Large Firms X Young",
        "Fornero Reform X Large Firms X Prime Age",
        "Jobs Act only X Large Firms X Young",
        "Jobs Act only X Large Firms X Prime Age"),times=2),
  
  rep(c("Fornero Reform X Large Firms X South and Islands",
        "Fornero Reform X Large Firms X Central Italy",
        "Jobs Act only X Large Firms X South and Islands",
        "Jobs Act only X Large Firms X Central Italy"),times=2),
  
  rep(c("Fornero Reform X Large Firms X Blue Collar",
        "Jobs Act only X Large Firms X Blue Collar"),
      times=2),
  
  rep(c("Fornero Reform X Large Firms X Part Time",
        "Jobs Act only X Large Firms X Part Time"),times=2))


imodel <- c(
  rep(c("Sex Total Hirings"), times=2),
  rep(c("Sex OEC Hirings"), times=2),
  rep(c("Age Group Total Hirings"), times=4),
  rep(c("Age Group OEC Hirings"), times=4),
  rep(c("Geo. Area Total Hirings"), times=4),
  rep(c("Geo. Area OEC Hirings"), times=4),
  
  rep(c("Position Total Hirings"), times = 2),
  rep(c("Position OEC Hirings"), times = 2),
  rep(c("Work Time Total Hirings"), times=2),
  rep(c("Work Time OEC Hirings"), times=2)
)


ioutput <- c(
  rep(c("FTC Hirings"), times=2),
  rep(c("OEC Hirings"), times=2),
  rep(c("FTC Hirings"), times=4),
  rep(c("OEC Hirings"), times=4),
  rep(c("FTC Hirings"), times=4),
  rep(c("OEC Hirings"), times=4),
  
  rep(c("FTC Hirings"), times=2),
  rep(c("OEC Hirings"), times=2),
  rep(c("FTC Hirings"), times=2),
  rep(c("OEC Hirings"), times=2)
)

ioutput <- ioutput %>% 
  factor(levels=c("OEC Hirings",
                  "FTC Hirings"))

interaction <- c(
  rep(c("Female"), times=4),
  rep(c("Young", "Prime Age"), times=4),
  rep(c("Southern Italy", "Central Italy"), times=4),
  
  rep(c("Low Wage"), times = 4),
  rep(c("Part Time"), times=4)
)

interaction <- interaction %>% 
  factor(levels=c("Female",
                  "Prime Age", "Young",
                  "Southern Italy", "Central Italy",
                  
                  "Low Wage",
                  "Part Time"))


intvar <- c(
  rep(c("Fornero X LF X Int.",
        "Jobs Act X LF X Int."),times=2),
  rep(c("Fornero X LF X Int.", 
        "Fornero X LF X Int.",
        "Jobs Act X LF X Int.",
        "Jobs Act X LF X Int."),times=4),
  rep(c("Fornero X LF X Int.",
        "Jobs Act X LF X Int."),times=4)
)

intvar <- intvar %>% 
  factor(levels = c(
    "Jobs Act X LF X Int.",
    "Fornero X LF X Int."))



### Extract Coefficients ----

ibetas <- c(
  coefficients(LFTCS)[c("fa:SizeL:SexF","jao:SizeL:SexF")],
  coefficients(LOECS)[c("fa:SizeL:SexF","jao:SizeL:SexF")],
  
  coefficients(LFTCA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54")],
  coefficients(LOECA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54")],
  
  coefficients(LFTCG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral")],
  coefficients(LOECG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral")],
  
  coefficients(LFTCP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar")],
  coefficients(LOECP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar")],
  
  coefficients(LFTCW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP")],
  coefficients(LOECW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP")]
)

### Extract Upper and Lower Boundaries ----


ilb <-  c(confint(LFTCS)[c("fa:SizeL:SexF","jao:SizeL:SexF"),1],
          confint(LOECS)[c("fa:SizeL:SexF","jao:SizeL:SexF"),1],
          
          confint(LFTCA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54"),1],
          confint(LOECA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54"),1],
          
          confint(LFTCG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral"),1],
          confint(LOECG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral"),1],
          
          confint(LFTCP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar"),1],
          confint(LOECP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar"),1],
          
          confint(LFTCW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP"),1],
          confint(LOECW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP"),1]
)

iub <-  c(confint(LFTCS)[c("fa:SizeL:SexF","jao:SizeL:SexF"),2],
          confint(LOECS)[c("fa:SizeL:SexF","jao:SizeL:SexF"),2],
          
          confint(LFTCA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54"),2],
          confint(LOECA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54"),2],
          
          confint(LFTCG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral"),2],
          confint(LOECG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral"),2],
          
          confint(LFTCP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar"),2],
          confint(LOECP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar"),2],
          
          confint(LFTCW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP"),2],
          confint(LOECW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP"),2]
)

### Extract pvalues -----

ipv <-  c(pvalue(LFTCS)[c("fa:SizeL:SexF","jao:SizeL:SexF")],
          pvalue(LOECS)[c("fa:SizeL:SexF","jao:SizeL:SexF")],
          
          pvalue(LFTCA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54")],
          pvalue(LOECA)[c("fa:SizeL:AgeGroup<35","fa:SizeL:AgeGroup35-54","jao:SizeL:AgeGroup<35","jao:SizeL:AgeGroup35-54")],
          
          pvalue(LFTCG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral")],
          pvalue(LOECG)[c("fa:SizeL:GeoAreaSouth and Islands","fa:SizeL:GeoAreaCentral","jao:SizeL:GeoAreaSouth and Islands","jao:SizeL:GeoAreaCentral")],
          
          pvalue(LFTCP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar")],
          pvalue(LOECP)[c("fa:SizeL:PositionBlueCollar","jao:SizeL:PositionBlueCollar")],
          
          pvalue(LFTCW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP")],
          pvalue(LOECW)[c("fa:SizeL:WorkTimeP","jao:SizeL:WorkTimeP")]
)


### Merge extracted values and build plot  -----

idata <- data.frame(ibetas,ipv,ivars,ilb,iub, imodel, ioutput, interaction, intvar)

## IPLOT SA -----
iplotSA <- 
  ggplot(idata[1:12,], aes(x = ibetas, y = intvar)) +
  geom_point(aes(color = factor(ipv < 0.05)), size = 3) +
  geom_errorbarh(aes(xmin = ilb, xmax = iub)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Changes in Hiring Flows",
       subtitle = "95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variables") +
  facet_grid(rows = vars(ioutput), cols = vars(interaction)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  labs(color = "P-value < 5%:") +
  scale_color_manual(
    values = c("TRUE" = "lightblue", 
               "FALSE" = "#FF9999"), # Customize colors as needed
    labels = c("False", "True")  # Re-label legend text
  ) +
  custom_theme
iplotSA

ggsave("Figure3.png", plot = iplotSA, 
       dpi = 300, width = 6, height = 4, )

AIC(LOEC, LOECP)
BIC(LOEC, LOECP)

iplotSA_pres <- 
  ggplot(idata[1:12,], aes(x = ibetas, y = intvar)) +
  geom_point(aes(color = factor(ipv < 0.05)), size = 3) +
  geom_errorbarh(aes(xmin = ilb, xmax = iub)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Changes in Hiring Flows",
       subtitle = "95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variables") +
  facet_grid(rows = vars(ioutput), cols = vars(interaction)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  labs(color = "P-value < 5%:") +
  scale_color_manual(
    values = c("TRUE" = "lightblue", 
               "FALSE" = "#FF9999"), # Customize colors as needed
    labels = c("False", "True")  # Re-label legend text
  ) +
  custom_theme_pres
iplotSA_pres

png("iplotSA_pres.png", width = 800, height = 600, res = 150)
iplotSA_pres
dev.off()

## IPLOT GW -------

iplotGW <- 
  ggplot(idata[13:24,], aes(x = ibetas, y = intvar)) +
  geom_point(aes(color = factor(ipv < 0.05)), size = 3) +
  geom_errorbarh(aes(xmin = ilb, xmax = iub)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Changes in Hiring Flows",
       subtitle = "95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variables") +
  facet_grid(rows = vars(ioutput), cols = vars(interaction)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  labs(color = "P-value < 5%:") +
  scale_color_manual(
    values = c("TRUE" = "lightblue", 
               "FALSE" = "#FF9999"), # Customize colors as needed
    labels = c("False", "True")  # Re-label legend text
  ) +
  custom_theme
iplotGW

ggsave("Figure4.png", plot = iplotGW, 
       dpi = 300, width = 6, height = 4)


iplotGW_pres <- 
  ggplot(idata[13:24,], aes(x = ibetas, y = intvar)) +
  geom_point(aes(color = factor(ipv < 0.05)), size = 3) +
  geom_errorbarh(aes(xmin = ilb, xmax = iub)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Changes in Hiring Flows",
       subtitle = "95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "Variables") +
  facet_grid(rows = vars(ioutput), cols = vars(interaction)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  labs(color = "P-value < 5%:") +
  scale_color_manual(
    values = c("TRUE" = "lightblue", 
               "FALSE" = "#FF9999"), # Customize colors as needed
    labels = c("False", "True")  # Re-label legend text
  ) +
  custom_theme_pres
iplotGW_pres

png("iplotGW_pres.png", width = 800, height = 600, res = 150)
iplotGW_pres
dev.off()
