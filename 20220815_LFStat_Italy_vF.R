# Set-up -----

library(plyr) # some basic R commands
library(tidyverse) # broad range of basic R commands (e.g. reading in data)
library(arsenal) # quick tables construction (e.g. via tableby)
library(pander) # tables formatting
library(data.table) # data table creation
library(writexl) # export excel files

library(ggplot2)  # Grphic design
library(ggpubr)
library(ggrepel)
# library(expss)

# Read In LF Statistiscs -----
# Sample code. Data freely accesible at https://www.istat.it/en/archivio
Q12019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Primo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")
Q22019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Secondo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")
Q32019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Terzo_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")
Q42019 <- read.delim2 ("~/RCFL_Microdati_Anno_2019_Quarto_trimestre.TXT",  header=T, sep="	",  quote="",  na.strings = ".")

## Merge and clean environment ---- 
LF2019 <- rbind(Q12019, Q22019, Q32019, Q42019)
rm(Q12019, Q22019, Q32019, Q42019)

## Select, format and filter required Variables ----
LF2019 <- LF2019 %>%  select(REG, ETAM, COND3, SG11,DIPAUT, C1, C9, C20, C27, C28)
LF2019 <- LF2019 %>% rename(Region = REG,
                            Age = ETAM,
                            Condition = COND3,
                            Sex = SG11,
                            ContractType = DIPAUT,
                            WorkType = C1,
                            Position = C9,
                            ContractDuration = C20,
                            WorkTime = C27,
                            Part_Time_Vol = C28) 

LF2019 <- LF2019 %>% filter(Age> 15 & Age <65)

LF2019 <- LF2019 %>% mutate(Condition = factor(Condition, levels = c(1,2,3),
                                               labels = c("Employed", "Unemployed", "Inactive")),
                            Sex = factor(Sex, levels = c(1,2), 
                                         labels = c("Male", "Female")),
                            ContractType = factor(ContractType, levels = c(1,2,3),
                                                  labels = c("D","C","A")),
                            WorkType = factor(WorkType), # dependent employees = 1
                            Position = factor(Position, levels = c(1,2,3,4,5,6),
                                              labels = c("Dirigente", "Quadro", "Impiegato", "Operaio", "Apprendista", "Other")),
                            ContractDuration = factor(ContractDuration, levels = c(1,2),
                                                      labels = c("FTC","OEC")),
                            WorkTime = factor(WorkTime, levels = c(1,2),
                                              labels = c("Full Time","Part Time")),
                            Part_Time_Vol = factor(Part_Time_Vol, levels = c(1,2,3,997),
                                                   labels = c("Voluntary", "Involuntary", "Other", "DK")),
                            Region = factor(Region, levels = c(1:20),
                            labels = c("Piemonte","Valle d'Aosta", "Lombardia", "Trentino Alto Adige", "Veneto",
                                       "Friuli Venezia Giulia", "Liguria", "Emilia Romagna", "Toscana", "Umbria",
                                       "Marche", "Lazio", "Abruzzo", "Molise", "Campania", 
                                       "Puglia", "Basilicata", "Calabria", "Sicilia", "Sardegna")))

LF2019 <- LF2019 %>% mutate(AgeGroup = ifelse(Age<=14, "<15",
                                              ifelse(Age<=34, "15-34",
                                                     ifelse(Age<=54, "35-54","55+"))),
                            GeoArea = ifelse(Region=="Valle d'Aosta"|
                                                 Region=="Piemonte"|
                                                 Region=="Liguria"|
                                                 Region=="Lombardia"|
                                                 Region=="Emilia Romagna"|
                                                 Region=="Veneto"|
                                                 Region=="Friuli Venezia Giulia"|
                                                 Region=="Trentino Alto Adige", "North",
                                               ifelse(Region=="Puglia"|
                                                        Region=="Abruzzo"|
                                                        Region=="Basilicata"|
                                                        Region=="Calabria"|
                                                        Region=="Campania"|
                                                        Region=="Molise"|
                                                        Region=="Sardegna"|
                                                        Region=="Sicilia","South and Islands",
                                                      "Central")),
                            Part_Time_Cond = ifelse(Part_Time_Vol=="Involuntary", "Involuntary","Voluntary"),
                            Position1 = ifelse(Position=="Dirigente", "Manager",
                                                ifelse(Position == "Quadro", "White Collar",
                                                       ifelse(Position == "Impiegato", "White Collar",
                                                              ifelse(Position=="Operaio", "Blue Collar",
                                                                     ifelse(Position=="Apprendista", "Apprentice", "Other"))))))

LF2019$GeoArea <- factor(LF2019$GeoArea, levels = c("North", "Central", "South and Islands"))

## Explore key stats for U by Sex, Age, Geo Area, Position -----

## Data for Figure 4 ----

# create the table using tableby()

t1 <- tableby(
  formula = Condition ~ Sex + AgeGroup + GeoArea, 
  data = LF2019)

# view the table using summary()
summary(object = t1,
        text = T, 
        labelTranslations = c(
        Sex = "Gender*", 
        AgeGroup = "Age Group (Years)",
        GeoArea = "Geographical Area"), 
        digits = 1, 
        test = T, 
        total = F, 
        title = "My descriptive stats table")

# Write on csv file and name it DTable1, such that it contains 4 columns: 
# dimension (for rows categories), variable (for level 2 rows categories),
# status (for column categories), and value as percentage proportion of total by variable 

# Data for Figure 5 ----

# create the table using tableby()

D1$Position1 <- factor(D1$Position1, levels = c("Manager", "White Collar", "Blue Collar", "Apprentice", "Other"))

t2 <- tableby(
  formula = ContractDuration ~ Sex + AgeGroup + GeoArea + Position1 + WorkTime + Part_Time_Cond,
  data = D1)

levels(as.factor(D1$Position1))


# view the table using summary()

summary(object = t2,
        text = T, 
        labelTranslations = c(
          Sex = "Gender*",
          AgeGroup = "Age Group (Years)",
          GeoArea = "Geographical Area",
          Position1 = "Occupational Level",
          WOrkTime = "Working Hours",
          Part_Time_Cond = "Part Time Choice"),
        digits = 0, 
        digits.pct=1,
        test = T, 
        total = F, 
        title = "My descriptive stats table",
        pfootnote=TRUE)

# Write on csv file and name it DTable3, such that it contains 4 columns: 
# dimension (for rows categories), variable (for level 2 rows categories),
# status (for column categories), and value as percentage proportion of total by variable 

 
# LFS Figures ----

## Graphs for Figure 4 ----

# Sample code depending on where the relevant file was created
DT1 <- read_csv2("~/DTable1.csv")
DT1 <- DT1 %>% mutate(Value = as.numeric(Value)*100)

DTA <- DT1 %>% filter(Dimension=="Sex")



A <- ggplot(DTA, aes(fill=Status, y=Value*100, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("Male", "Female")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sex", y = "As %",
       title = "Working Age Pop. by Sex") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

A


DTB <- DT1 %>% filter(Dimension=="Age Group (Years)")

B <- ggplot(DTB, aes(fill=Status, y=Value, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("55+", "35-55", "15-34")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age Group (Years)", y = "As %",
       title = "Working Age Pop. by Age Group") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))
B

DTC <- DT1 %>% filter(Dimension=="Geographical Area")

C <- ggplot(DTC, aes(fill=Status, y=Value, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("Northern Italy", "Central Italy", "Southern Italy")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Geographical Area", y = "As %",
       title = "Working Age Pop. by Geo Area") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))
C

### Aggregated Figure 4 ----

ggarrange(A,B,C,
          nrow =1, ncol = 3,
          common.legend = TRUE, legend="bottom")
  
## Graphs for Figure 5 ----

DT2 <- read_csv2("~/Quantitative Analysis/data/DTable3.csv")
DT2 <- DT2 %>% mutate(Value = as.numeric(Value)*100)

DTA2 <- DT2 %>% filter(Dimension=="Sex")

A2 <- ggplot(DTA2, aes(fill=Status, y=Value*100, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("Male", "Female")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sex", y = "As %",
       title = "Contract Type by Sex") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

A2        

DTB2 <- DT2 %>% filter(Dimension=="Age Group")

B2 <- ggplot(DTB2, aes(fill=Status, y=Value*100, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("55+", "35-54", "15-34")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age Group (Years)", y = "As %",
       title = "Contract Type by Age Group") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

B2        

DTC2 <- DT2 %>% filter(Dimension=="Geographical Area")

C2 <- ggplot(DTC2, aes(fill=Status, y=Value*100, x=Variable)) +
  scale_x_discrete(limits = c("Northern Italy", "Central Italy", "Southern Italy")) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Geographical Area", y = "As %",
       title = "Contract Type by Geo Area") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

C2   

DTD2 <- DT2 %>% filter(Dimension=="Occupational Level")

D2 <- ggplot(DTD2, aes(fill=Status, y=Value*100, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("White Collar", "Blue Collar")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Occupational Level", y = "As %",
       title = "Contract Type by Occupation") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

D2 

DTE2 <- DT2 %>% filter(Dimension=="Work Time")

E2 <- ggplot(DTE2, aes(fill=Status, y=Value*100, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  # scale_x_discrete(limits = c("White Collar", "Blue Collar")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Work Time", y = "As %",
       title = "Contract Type by Work Time") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

E2 

DTF2 <- DT2 %>% filter(Dimension=="Part Time Choice")

F2 <- ggplot(DTF2, aes(fill=Status, y=Value*100, x=Variable)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(limits = c("Voluntary", "Involuntary")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Part Time Choice", y = "As %",
       title = "Contract Type by Part Time Choice") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

F2 

### Aggregated Figure 5 ----

f2 <- ggarrange(A2,B2,C2,D2,E2,F2,
          nrow =2, ncol = 3,
          common.legend = TRUE, legend="bottom",
          align = "h")

annotate_figure(f2, top = text_grob("2019 Italy Dependent Workforce", 
                                      color = "black", size = 14, family="serif"))

