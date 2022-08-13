# Set-up -----

library(plyr) # some basic R commands
library(tidyverse) # broad range of basic R commands (e.g. reading in data)
library(lubridate) # floor date function
library(writexl) # export excel files

# Read-in Original Data Sources -----

# This is just a sample code. Actual data access should be requested to segretariatoufficiostatistica@lavoro.gov.it.
Firm2017_2019 <- read_csv("~MATR_LOSAI_2017_2019.TXT", col_names = FALSE, trim_ws = FALSE)
Firm2015_2018 <- read_csv("~MATR_LOSAI_2015_2018.TXT", col_names = FALSE, trim_ws = FALSE)
Firm2005_2016 <- read_csv("~MATR_LOSAI_2005_2016.TXT", col_names = FALSE, trim_ws = FALSE)
Anagrafica <- read_csv("~ANAGRAFICA_2020.TXT", col_names = FALSE, trim_ws = FALSE)
LoSal <- read_csv("~Rapporti_lavoro1985_2019.txt", col_names=FALSE, trim_ws = FALSE)

## Firms 2017-2019 -----

# read, format, and clean
Firm2017_2019 <- Firm2017_2019 %>% 
  separate(X1, c("FirmID", "MotherFirmID", "FirmPosition", "SizeClass", "ATECO07", "Year"), sep = c(8,16,17,25,27))
Firm2017_2019 %>% 
  mutate(across(where(is.character), str_trim))
Firm2017_2019$FirmID <- as.numeric(Firm2017_2019$FirmID)
Firm2017_2019$MotherFirmID <- as.numeric(Firm2017_2019$MotherFirmID)
Firm2017_2019$SizeClass <- as.numeric(Firm2017_2019$SizeClass)
Firm2017_2019$ATECO07 <- as.numeric(Firm2017_2019$ATECO07)
Firm2017_2019$Year <- as.numeric(Firm2017_2019$Year)


## Firms 2015-2018 -----

# read, format, and clean
Firm2015_2018 <- Firm2015_2018 %>% 
  separate(X1, c("FirmID", "MotherFirmID", "FirmPosition", "SizeClass", "ATECO07", "Year"), sep = c(8,16,17,25,27))
Firm2015_2018 %>% 
  mutate(across(where(is.character), str_trim))
Firm2015_2018$FirmID <- as.numeric(Firm2015_2018$FirmID)
Firm2015_2018$MotherFirmID <- as.numeric(Firm2015_2018$MotherFirmID)
Firm2015_2018$SizeClass <- as.numeric(Firm2015_2018$SizeClass)
Firm2015_2018$ATECO07 <- as.numeric(Firm2015_2018$ATECO07)
Firm2015_2018$Year <- as.numeric(Firm2015_2018$Year)

## Firms 2005-2016 -----

# read, format, and clean
Firm2005_2016 <- Firm2005_2016 %>% 
  separate(X1, c("FirmID", "MotherFirmID", "FirmPosition", "SizeClass", "ATECO07", "Year"), sep = c(8,16,17,25,27))
Firm2005_2016 %>% 
  mutate(across(where(is.character), str_trim))
Firm2005_2016$FirmID <- as.numeric(Firm2005_2016$FirmID)
Firm2005_2016$MotherFirmID <- as.numeric(Firm2005_2016$MotherFirmID)
Firm2005_2016$SizeClass <- as.numeric(Firm2005_2016$SizeClass)
Firm2005_2016$ATECO07 <- as.numeric(Firm2005_2016$ATECO07)
Firm2005_2016$Year <- as.numeric(Firm2005_2016$Year)


## Merge Firms Databases ----

FirmsAll <- rbind(Firm2017_2019, Firm2015_2018, Firm2005_2016) # add rows
FirmsAll <- FirmsAll %>% distinct() # remove duplicates

## Read-in Anagrafica ----

# read, format, and clean
Anagrafica <- Anagrafica %>% 
  separate(X1, c("Birth","Sex","Death","SCF","ID_L","ID_LNew","Region"),sep=c(8,9,17,25,33,41,72))
Anagrafica %>% 
  mutate(across(where(is.character), str_trim))
Anagrafica$Birth <- as.numeric(Anagrafica$Birth)
Anagrafica$Death <- as.numeric(Anagrafica$Death)
Anagrafica$SCF <- as.numeric(Anagrafica$SCF)
Anagrafica$ID_L <- as.numeric(Anagrafica$ID_L)
Anagrafica$ID_LNew <- as.numeric(Anagrafica$ID_LNew)
Anagrafica$Region <- trimws(Anagrafica$Region)
Anagrafica <- Anagrafica %>% filter(SCF==1)
Anagrafica <- Anagrafica %>% select(-SCF)

## Read-in LoSal Databases -----

# read, format, and clean
LoSal <- LoSal %>%
  separate(X1, 
           c("WorkerID", 
             "Year", 
             "WorkTime", 
             "Position", 
             "PaidDays", 
             "PaidWeeks", 
             "UsefulWeeks", 
             "ALMP_type", 
             "EndDate", 
             "GrossIncome", 
             "ContractType", 
             "HiringDay", 
             "FiringReason", 
             "HiringReason", 
             "ID_Company"),
           sep=c(8,16,18,30,38,46,54,62,67,75,77,82,85,88,97))


LoSal$WorkerID <- as.numeric(LoSal$WorkerID)
LoSal$Year <- as.numeric(LoSal$Year)
LoSal$PaidDays <- as.numeric(LoSal$PaidDays)
LoSal$PaidWeeks <- as.numeric(LoSal$PaidWeeks)
LoSal$UsefulWeeks <- as.numeric(LoSal$UsefulWeeks)
LoSal$EndDate <- as.numeric(LoSal$EndDate)
LoSal$GrossIncome <- as.numeric(LoSal$GrossIncome)
LoSal$HiringDay <- as.numeric(LoSal$HiringDay)
LoSal$ID_Company <- as.numeric(LoSal$ID_Company)

LoSal$WorkTime <- trimws(LoSal$WorkTime)
LoSal$Position <- trimws(LoSal$Position)
LoSal$ContractType <- trimws(LoSal$ContractType)
LoSal$ALMP_type <- trimws(LoSal$ALMP_type)
LoSal$FiringReason <- trimws(LoSal$FiringReason)
LoSal$HiringReason <- trimws(LoSal$HiringReason)

LoSal <- LoSal %>% filter(Year >= 2005)

## Create aggregated INPS longitudinal sample database ------

# Aggregate and clean environment
ILS <- merge(LoSal, Anagrafica, by.x=c("WorkerID"), by.y=c("ID_LNew"), all.x = TRUE, all.y = FALSE) 
ILS <- merge(ILS, FirmsAll, by.x=c("ID_Company", "Year"), by.y=c("FirmID", "Year") )
rm(LoSal)
rm(Anagrafica)
ILS$ID_CompanyYear <- paste(ILS$ID_Company, ILS$Year) 
FirmsAll$FirmIDYear <- paste(FirmsAll$FirmID, FirmsAll$Year)
ILS <- left_join(ILS, FirmsAll, 
                 by = c("ID_CompanyYear" = "FirmIDYear"))

rm(Firm2017_2019)
rm(Firm2015_2018)
rm(Firm2005_2016)

# Write Integrated Longitudinal Sample - smaple code
write.csv2(ILS, file = "~/Quantitative Analysis/LoSal_db/ILS.TXT", row.names = FALSE)

# Create Hirings Database from INPS longitudinal sample -----
# This is just a sample code. Read code should be consistent with the above written aggregated database
ILS <- read_csv2("~/ILS.TXT")

## Set Dates format ----

ILS$HiringM <- substr(ILS$HiringDay, nchar(ILS$HiringDay)-1, nchar(ILS$HiringDay))
ILS$HiringD <- substr(ILS$HiringDay, 1, nchar(ILS$HiringDay)-nchar(ILS$HiringM))
ILS$HireDate <- paste(ILS$HiringD, ILS$HiringM,ILS$Year.y, sep = "-")
ILS$HireDate <- as.Date(ILS$HireDate,"%d-%m-%Y")

ILS$FiringM <- substr(ILS$EndDate, nchar(ILS$EndDate)-1, nchar(ILS$EndDate))
ILS$FiringD <- substr(ILS$EndDate, 1, nchar(ILS$EndDate)-nchar(ILS$FiringM))
ILS$FireDate <- paste(ILS$FiringD, ILS$FiringM,ILS$Year.y, sep = "-")
ILS$FireDate <- as.Date(ILS$FireDate,"%d-%m-%Y")

ILS$HireMonth <- floor_date(ILS$HireDate, "month")
ILS$HireQuarter <- floor_date(ILS$HireDate, "quarter")

ILS$FireMonth <- floor_date(ILS$FireDate, "month")
ILS$FireQuarter <- floor_date(ILS$FireDate, "quarter")

## Set variables class ----

ILS <- ILS %>% mutate(  WorkTime = as.factor(WorkTime),
                        Position = as.factor(Position),
                        ContractType = as.factor(ContractType),
                        Sex = as.factor(Sex),
                        ALMP_type = as.factor(ALMP_type),
                        FiringReason = as.factor(FiringReason),
                        HiringReason = as.factor(HiringReason),
                        Region = as.factor(Region),
                        FirmPosition = as.factor(FirmPosition),
                        SizeClass = as.numeric(SizeClass)) 


ILS <- ILS %>% mutate(OEC = ifelse(ContractType=="I", 1, 0),
                      FTC = ifelse(ContractType=="D", 1, 0))




## Filter Database ----

# exclude seasonal contracts
ILS <- ILS %>% filter(ContractType == "I" | ContractType == "D")

#exclude PA, Agri workers, Foreign and Domestic Employers
ILS <- ILS %>% filter(ATECO07 != "01" | ATECO07 != "02" | ATECO07 != "03"
                      | ATECO07 != "84"
                      | ATECO07 != "97" | ATECO07 != "98"
                      | ATECO07 != "99")

## Create Age, Firm Size, Contract Trasformations, and Progressive Time Variables ----

# Firm Size and Age
ILS <- ILS %>% mutate(FirmSize = ifelse(SizeClass<=3, "<15 employees", 
                                        ifelse(SizeClass<=8, "16-50 employees",
                                               "50+ employees")),
                      FirmSize = as.factor(FirmSize),
                      
                      FirmSize1 = ifelse(SizeClass<=2, "<10 employees", 
                                         ifelse(SizeClass<=3, "11-15 employees",
                                                ifelse(SizeClass ==4, "16-20 employees",
                                                       ifelse(SizeClass <=8, "21-50 employees",
                                                              "50+ employees")))),
                      FirmSize1 = as.factor(FirmSize1),
                      
                      
                      Age = Year.x-Birth)


# TimeM variable
ILS <- transform(ILS,
                 TimeM = as.numeric(factor(HireMonth)))

# Include Transformations in ILS

ILS$ID_FWY <- (paste(ILS$FirmID, ILS$WorkerID, ILS$Year.x,sep = ""))
ILS <- ILS[with(ILS, order(WorkerID, Year.x, ID_Company, ContractType)), ]

ILS <- ILS %>% mutate(T_OEC =
                        if_else(ID_FWY == dplyr::lag(ID_FWY) 
                                & ContractType != dplyr::lag(ContractType)
                                & ContractType == "I",
                                1,0),
                      TrMonth = ifelse(T_OEC==1, 
                                       floor((365-PaidWeeks*7)/30),"-"),
                      TrDate = ifelse(T_OEC ==1,
                                      paste(1,TrMonth,Year.x,sep = "-"), "-"),
                      TrDate = as.Date(TrDate,"%d-%m-%Y"))

## Clean unnecessary/repeated columns ----
ILSv01 <- ILS %>% select(-c(Year.x, ID_Company, ID_L, ID_CompanyYear,
                            HiringM, HiringD, FiringD, FiringM, ID_FWY, TrMonth, 
                            EndDate, HiringDay))

## Create ILS Hirings Database  ----

ILSH <- ILS %>% filter(!is.na(HireDate) | !is.na(TrDate)) 

ILSH <- ILSH %>% select(-c(Year.x, ID_Company, ID_L, ID_CompanyYear,
                           HiringM, HiringD, FiringD, FiringM, ID_FWY, TrMonth, 
                           EndDate, HiringDay))


# Write Hirings Integrated Longitudinal Sample - sample code
write.csv2(ILSH, file = "~/Quantitative Analysis/LoSal_db/ILShirings.TXT", row.names = FALSE)


# Format and adjust aggregate database ---- 

# This is just a sample code. Read code should be consistent with the above written aggregated database
ILSH <- read_csv2("~/ILShirings.TXT")


## Format variables classes and adjust definition ---
ILSH <- ILSH %>% mutate(WorkTime =as.factor(WorkTime),
                        Position=as.factor(Position),
                        ALMP_type = as.factor(ALMP_type),
                        ContractType = as.factor(ContractType),
                        FiringReason = as.factor(FiringReason),
                        HiringReason = as.factor(HiringReason),
                        Sex = as.factor(Sex),
                        Region = as.factor(Region),
                        FirmID = as.factor(FirmID),
                        MotherFirmID = as.factor(MotherFirmID),
                        FirmPosition = as.factor(FirmPosition),
                        SizeClass= as.factor(SizeClass),
                        ATECO07 = as.factor(ATECO07),
                        FirmSize = as.factor(FirmSize),
                        FirmSize1 = as.factor(FirmSize1),
                        AgeGroup = as.factor(AgeGroup))

ILSH <- ILSH %>% mutate(WorkTime = as.factor(ifelse(WorkTime == "F", "F", "P")))
ILSH <- transform(ILSH,
                  TimeM = ifelse(is.na(TrDate), as.numeric(factor(HireMonth)),
                                 as.numeric(factor(TrDate))))
ILSH <- ILSH %>%  mutate(Month = ifelse(is.na(TrDate), month(HireMonth), month(TrDate)))
ILSH <- ILSH %>% mutate(OEC = ifelse(T_OEC == 1, 0, OEC),
                        TOEC = ifelse(OEC == 1, 1, 
                                      ifelse(T_OEC == 1, 1, 0)))

## Add reforms cutoff points ----

ILSH <- ILSH %>% mutate(pi = ifelse(TimeM < 91, 1,0),
                        fa = ifelse(TimeM < 91, 0,
                                    ifelse(TimeM > 120, 0, 1)),
                        jahi = ifelse(TimeM < 121, 0, 
                                      ifelse(TimeM > 144, 0, 1)),
                        jao = ifelse(TimeM < 145, 0, 
                                     ifelse(TimeM > 162, 0, 1)),
                        dd = ifelse(TimeM < 163, 0, 1))


ILSH <- ILSH %>% mutate(faT = ifelse(fa==1,fa*TimeM-90,0),
                        jahiT = ifelse(jahi==1,jahi*TimeM-120,0),
                        jaoT = ifelse(jao==1,jao*TimeM-144,0),
                        ddT = ifelse(dd==1,dd*TimeM-162,0),
                        int=fa+jahi*2+jao*3+dd*4)


## Clean database to ensure reform relevant categories are included only ----

ILSH <- ILSH %>% filter(Position != "Altro" & Position != "Apprendista" & Position != "Dirigente")
ILSH <- ILSH %>% filter(AgeGroup != "<15")
ILSH <- ILSH %>% filter(ATECO07 != "1" & ATECO07 != "2" & ATECO07 != "3" # agri and fishing
                        & ATECO07 != "84" # PA and Defence
                        & ATECO07 != "97" & ATECO07 != "98" # domestic workers
                        & ATECO07 != "99") # outside Italy


## Create adjusted Position, Age Group and Geo Area Variables ----
ILSH <- ILSH %>%  mutate(
  Position = ifelse(Position=="Operaio", "BlueCollar",
                    ifelse(Position=="Impiegato", "WhiteCollar", "High_WC")),
  AgeGroup = ifelse(Age<=34,"<35",
                    ifelse(Age<=54,"35-54","55+")),
  GeoArea = ifelse(Region=="VALLE D'AOSTA"|
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
                          "Central")))

# define reference level
ILSH <- within(ILSH, AgeGroup <- relevel(factor(ILSH$AgeGroup, ordered = FALSE), ref = "<35"))
ILSH <- within(ILSH, Sex <- relevel(factor(ILSH$Sex, ordered = FALSE), ref = "M"))
ILSH <- within(ILSH, Position <- relevel(factor(ILSH$Position, ordered = FALSE), ref = "BlueCollar"))
ILSH <- within(ILSH, GeoArea <- relevel(factor(ILSH$GeoArea, ordered = FALSE), ref = "Central"))

# Generate Monthly Hirings Database  ----

dbhFE <- ILSH %>%                         # Aggregate data
  group_by(TimeM, Year.y, Month, SizeClass, ATECO07, WorkTime, Position, AgeGroup, Sex, Region, pi, fa, jahi, jao, dd, faT, jahiT,jaoT,ddT) %>% 
  dplyr::summarize(OECShare = sum(OEC)/(sum(OEC)+sum(FTC)),
                   TOECShare = sum(TOEC)/(sum(TOEC)+sum(FTC)),
                   OEC = sum(OEC),
                   TrOEC = sum(T_OEC),
                   TOEC = sum(OEC)+sum(T_OEC),
                   FTC = sum(FTC)) %>% 
  as.data.frame()


## include period factor variable and cell id var ----
dbhFE <- dbhFE %>%  mutate(int=fa+jahi*2+jao*3+dd*4,
                         cell=paste(SizeClass, ATECO07,WorkTime, AgeGroup, Sex))


# Write Monthly Hirings Database allowing for Fixed Effects - sample code
write.csv2(dbhFE, file = "~/dbhFE1.TXT", row.names = FALSE)


