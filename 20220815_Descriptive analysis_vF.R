# Set-up -----

library(plyr) # some basic R commands
library(tidyverse) # broad range of basic R commands (e.g. reading in data)
library(arsenal) # quick tables construction (e.g. via tableby)
library(pander) # tables formatting

library(car) # check multicollinearity via vif functionILSH <- read_csv2("~/Quantitative Analysis/LoSal_db/ILS_H.TXT")
library(TSstudio) # plotting time series
library(its.analysis) # interrupted time series Analysis

library(ggplot2) # plot graphs tables and charts with gg packages
library(ggeffects) 
library(ggpubr)
library(ggrepel)

# Theoretical Model Graphs - function drawing ----

## Case 1-------

d <- 1
r <- 0.2
f <- 1
Cs <- 2.5
Crs <- Cs*r


yTs <- function(x) {ifelse(x < Crs, x*(1-(1-r)^d)/r-f, x*1/r-f-(1-r)^d*Cs)}
yPs <- function(x) {x*(1/r)-Cs}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))

case1 <- p +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = yPs),
        mapping = aes(color = "yPs")) +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = yTs),
        mapping = aes(color = "yTs")) +
  xlim(0,5) +
  scale_color_manual(name = "Functions",
                     values = c("blue", "red"), # Color specification
                     labels = c("OEC Job Offering", "FTC Job Offering"))+
  labs(x = "Worker's Productivity (??)", y = "Returns when job is filled (J)",
       title = "Firms' Job Opening Decisions at F/f<k",
       caption = "Function defined at constant parameters: r=0.2, d=1, F/f=2.5")+
  theme(text = element_text(family = "serif"))+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) 

case1

## Case 2 -------

C <- 5
Cr <- C*r

yT <- function(x) {ifelse(x <= Cr, x*(1-(1-r)^d)/r-f, x/r-f-C*(1-r)^d)}
yP <- function(x) {x/r-C}

case2 <- p +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = yP),
        mapping = aes(color = "yP")) +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = yT),
        mapping = aes(color = "yT")) +
  xlim(0,5) +
  scale_color_manual(name = "Functions",
                     values = c("blue", "red"), # Color specification
                     labels = c("OEC Job Offering", "FTC Job Offering"))+
  labs(x = "Worker's Productivity (??)", y = "Returns when job is filled (J)",
       title = "Firms' Job Opening Decisions at F/f=k",
       caption = "Function defined at constant parameters: r=0.2, d=1, F/f=5")+
  theme(text = element_text(family = "serif"))+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) 
    
case2

## Case 3 -------

Cb <- 7.5
Crb <- Cb*r

yTb <- function(x) {ifelse(x <= Crb, x*(1-(1-r)^d)/r-f, x/r-f-Cb*(1-r)^d)}
yPb <- function(x) {x/r-Cb}

case3 <- p +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = yPb),
        mapping = aes(color = "yPb")) +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = yTb),
        mapping = aes(color = "yTb")) +
  xlim(0,5) +
  scale_color_manual(name = "Functions",
                     values = c("blue", "red"), # Color specification
                     labels = c("OEC Job Offering", "FTC Job Offering"))+
  labs(x = "Worker's Productivity (??)", y = "Returns when job is filled (J)",
       title = "Firms' Job Opening Decisions at F/f=k",
       caption = "Function defined at constant parameters: r=0.2, d=1, F/f=5")+
  theme(text = element_text(family = "serif"))+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = Crb), linetype = "dotted")

case3

### Figure 7 - Arrange graphs and print ----

ggarrange(case1, case2, case3, 
          # + rremove("x.text"), 
          labels = c("A", "B", "C"),
          align = "h",
          widths = c(1.5,1.5,1.5),
          ncol = 1, nrow = 3)


# Theoretical Model Firing Costs ----

FC <- function(x) {ifelse(x > 1, x/x^0.8, 1)}

F1 <- function(x) {ifelse(x > 1,x,1)}
F2 <- function(x) {ifelse(x > 1,x^0.5,1)}


FCG <- p +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = F1),
        mapping = aes(color = "F1")) +
  layer(geom = "path",
        data = data.frame(x = 0),
        stat = "function",
        position=position_dodge(width = 2),
        params = list(fun = F2),
        mapping = aes(color = "F2")) +
  xlim(0,10) +
  scale_color_manual(name = "Functions",
                     values = c("blue", "red"), # Color specification
                     labels = c("OECs' Firing Costs", "FTCs' Firing Costs"))+
  labs(x = "OECs' Firing Costs (F)", y = "EPL Firing Costs",
       title = expression("Firing Cost Evolution at Fixed" ~delta),
       caption = expression("Function defined at constant parameters:"~delta~"=0.5"))+
  theme(text = element_text(family = "serif"))+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))

## Figure 9 - Print ----

FCG

# Read OECD Data on EPL and Empl Share -----

# Sample code. Data available at https://www.oecd.org/els/emp/oecdindicatorsofemploymentprotection.htm
EPLTC <- read_csv2("~/Quantitative Analysis/data/EPLTC.csv")

EPLTC <- EPLTC %>% rename(EPR = `Regular contracts EPL (v3)`,
                          EPT=`Temp contracts EPL (Dep only) (v3)`,
                          FTCW = `Share of Temp`) 
EPLTC <- EPLTC %>%  mutate(FTCW = FTCW/100,
                           EPR = as.numeric(EPR),
                           EPT = as.numeric(EPT))

## Figure 1 - PLOT EPT and EPR 2019 ----

p19e <- EPLTC %>% filter(Year == 2019)

plot2019e <- ggplot(data = p19e, aes(x = EPR, y=EPT, label=Country)) +
  geom_point() +
  geom_label_repel(min.segment.length = 0,max.overlaps = Inf,
                   label.size=0,label.padding = 0.3, label.r=0,
                   family = "serif") +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "EPL for Open-Ended Contracts (0-6)", y = "EPL for Fixed-Term Contracts (0-6)",
       title = "EPL on OECs and FTCs in 2019",
       caption = "Author's elaboration of OECD data. Include EPL on dependent workers, excluding temporary agency workers.")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"))
plot2019e

lm(EPT ~ EPR,
   data = p19e)

cor(p19e$EPT, p19e$EPR)

## Figure 2 - 2019 plot EPL and temp empl ----

p19 <- EPLTC %>% filter(Year == 2019)

plot2019 <- ggplot(data = p19, aes(x = EPR, y=FTCW, label=Country)) +
  geom_point() +
  geom_label_repel(min.segment.length = 0,max.overlaps = Inf,
                   label.size=0,label.padding = 0.3, label.r=0,
                   family = "serif") +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "EPL for Open-Ended Contracts (0-6)", y = "Share of temporary workers (%)",
       title = "EPL on OECs and Temporary Employment in 2019",
       caption = "Author's elaboration of OECD data. Include dependent employees older than 15.")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"))
plot2019


lm(FTCW ~ EPR,
   data = p19) 

cor(p19$EPR, p19$FTCW)

## Figure 3 - Transition rates and temporary contracts ----

levels(EPLTC1$Country)

# Taken from 2018 Eurostat data
TrRates <- c("NA",42.8,37.4,"NA",39.4,27.5, 12.1, 31, 21.9, 43.3, 14.8, 46.9, 30.3, 13.9, 40.1, 54.9)

EPLTC1 <- EPLTC[order(EPLTC$Country, decreasing = F),]

EPLTC1 <- EPLTC1 %>% filter(Year==2018)  %>%
  mutate(TrRates = as.numeric(TrRates))

EPLTC1 <- EPLTC1 %>%  select(FTCW, TrRates, Country)

plot2018t <- ggplot(data = na.omit(EPLTC1), aes(x = TrRates, y=FTCW, label=Country)) +
  geom_point() +
  geom_label_repel(min.segment.length = 0,max.overlaps = Inf,
                   label.size=0,label.padding = 0.3, label.r=0,
                   family = "serif") +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "Transition Rates into OEC (%)", y = "Share of temporary workers (%)",
       title = "Share of temporary contracts and transition rates in 2018",
       caption = "Author's elaboration of OECD and Eurostat data.Share of temporary contracts only dependent workers, and transition rates computed as 3-years averages of the 
       percentage of working-age individuals with temporary contracts who move to a permanent position between two consecutive years")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1))

plot2018t

lm(FTCW ~ as.numeric(TrRates),
   data=na.omit(EPLTC1))

cor(na.omit(EPLTC1)$FTCW, na.omit(EPLTC1)$TrRates)

## Figure 8 - 2019 plot EPT and temp employment ----

p19 <- EPLTC %>% filter(Year == 2019)

plot2019t <- ggplot(data = p19, aes(x = EPT, y=FTCW, label=Country)) +
  geom_point() +
  geom_label_repel(min.segment.length = 0,max.overlaps = Inf,
                   label.size=0,label.padding = 0.3, label.r=0,
                   family = "serif") +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "EPL for Fixed-Term Contracts (0-6)", y = "Share of temporary workers (%)",
       title = "EPL on FTCs and Temporary Employment in 2019",
       caption = "Author's elaboration of OECD data. Include dependent employees older than 15.")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"))
plot2019t

ggarrange(plot2019, plot2019t,
          ncol = 2, nrow = 1)



## EPL evolution in Italy -----

EPLI <- read_csv2("~/Quantitative Analysis/data/EPLI.csv")

plotEPL <- ggplot(data = EPLI, aes(x = Year, y=EPLIndex, group = Group )) +
  geom_step(aes(color=Group))+
  geom_point(aes(color=Group)) +
  labs(x = "Year", y = "EPL Index (0-6)",
       title = "Open-ended and fixed-term contracts EPL evolution in Italy between 2005 and 2019",
       caption = "Author's elaboration of OECD data.EPL index version III computed at the beggining of each year. Following OECD indications, the analysis relies 
       on the latest version of the OECD EPL index providing sufficient historical data. Indeed, version III provides a complete EPL anal-ysis until 2008. 
       Given that no other labour market reforms were implemented between 2005 and 2008, the available values are projected on 2005.
       (*) Includes only dependent workers without temporary agency workers" )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1)) +
  scale_color_manual(name = "EPL Indices", values=c("red", "green", "blue"),
                     labels = c("Delta", "OEC EPL", "FTC EPL*"))
  
plotEPL

## Figure 6 - EPL evolution in Italy ----

EPLI_R <- EPLI %>% filter(Group=="EPR") %>% mutate(EPLIndex = as.numeric(EPLIndex))

plotEPR <- ggplot(data = EPLI_R, aes(x = Year, y=EPLIndex)) +
  geom_line(size = 0.5, linetype=1, colour="blue")+
  labs(x = "Year", y = "EPL for Open-Ended Contracts (0-6)",
       title = "EPL on OECs evolution in Italy (2005-2019)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1)) + 
  scale_y_continuous(limits=c(0, 6))

plotEPR

EPLI_T <- EPLI %>% filter(Group=="EPT") %>% mutate(EPLIndex = as.numeric(EPLIndex))

plotEPT <- ggplot(data = EPLI_T, aes(x = Year, y=EPLIndex)) +
  geom_line(size = 0.5, lineend = "round", colour="red")+
  labs(x = "Year", y = "EPL for Fixed-Term Contracts (0-6)",
       title = "EPL on FTCs evolution in Italy (2005-2019)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1)) +
  scale_y_continuous(limits=c(0, 6))

plotEPT

### Arrange and print figure 6 graphs ----

ggarrange(plotEPR, plotEPT,
          bottom =
          text_grob("Author's elaboration of OECD data.EPL index version III computed at the beggining of each year. 
                    Given that no other labour market reforms were implemented between 2005 and 2008, 2008 figure is projected backward to 2005. 
                    Includes only dependent workers and no temporary agency workers",
                             hjust = 0.5,vjust=-2.5, x=1, size = 10, family="serif"))

# Parallel Trends Assumption Visual Inspection ----

# Sample code. Actual code should be consistent with writexl location from 20220815_Data_Preparation_vF
dbhFE <- read_csv2("~/dbhFE1.TXT")

# Create 4 graphs for each relevant outcome variable ----

dbhPT <- dbhFE %>% select(TimeM,SizeClass, int, OEC, FTC, OECShare, TOEC)

dbhPT <- dbhPT %>% filter(as.numeric(SizeClass) >= 2 & as.numeric(SizeClass) <= 5) %>% 
  mutate(SizeL=ifelse(as.numeric(SizeClass)>=4,1,0))

dbhPT <- dbhPT %>% mutate(SizeClass = as.factor(SizeClass),
                          int = as.factor(int),
                          SizeL = as.factor(SizeL))

dbhPT1 <- dbhPT %>%                         # Aggregate data
  group_by(TimeM, SizeL, int) %>% 
  dplyr::summarize(OECShare = sum(OEC)/(sum(OEC)+sum(FTC)),
                   OEC = sum(OEC),
                   TOEC = sum(OEC)+sum(FTC),
                   FTC = sum(FTC)) %>% 
  as.data.frame()


vl <- c(91,121,145,163)

pta1 <- ggplot(data = dbhPT1, aes(x = TimeM, y=log(TOEC+1) )) +
  geom_line(aes(color = SizeL), size = 1) +
  # geom_point(aes(color=Group)) +
  labs(x = "Monthly Periods (Jan 2005 - Dec 2019)", y = "Hirings Growth | log(y+1)",
       title = "Hirings growth in firms with 6-15 and 16-25 employees Italy between 2005 and 2019",
       
       # caption = "Author's elaboration of INPS data. Includes only dependent workers. Vertical lines reflect rspectively four policy changes: 
       # Fornero Act, temporary hiring incentive on permanent contracts (HI) introducttion, Jobs Act after HI termination, Dignity Decree " 
       
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1),
        legend.position="none") +
  scale_color_manual(name = "Firm Size Class", values=c("light blue", "light green"),
                     labels = c("Control: 6-15 employees", "Treatment: 16-25 employees")) +
  geom_vline(xintercept = vl, linetype="dotted", color = "black", size=0.5) 

pta1

pta2 <- ggplot(data = dbhPT1, aes(x = TimeM, y=log(OEC+1) )) +
  geom_line(aes(color = SizeL), size = 1) +
  # geom_point(aes(color=Group)) +
  labs(x = "Monthly Periods (Jan 2005 - Dec 2019)", y = "OEC Growth | log(y+1)",
       title = "OEC growth in firms with 6-15 and 16-25 employees Italy between 2005 and 2019",
       caption = "Author's elaboration of INPS data. Includes only dependent workers. Vertical lines reflect rspectively four policy changes: 
       Fornero Act, temporary hiring incentive on permanent contracts (HI) introducttion, Jobs Act after HI termination, Dignity Decree " )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1)) +
  scale_color_manual(name = "Firm Size Class", values=c("light blue", "light green"),
                     labels = c("Control: 6-15 employees", "Treatment: 16-25 employees")) +
  geom_vline(xintercept = vl, linetype="dotted", color = "black", size=0.5) 

pta2

pta3 <- ggplot(data = dbhPT1, aes(x = TimeM, y=log(FTC+1) )) +
  geom_line(aes(color = SizeL), size = 1) +
  # geom_point(aes(color=Group)) +
  labs(x = "Monthly Periods (Jan 2005 - Dec 2019)", y = "FTC Growth | log(y+1)",
       title = "FTC growth in firms with 6-15 and 16-25 employees Italy between 2005 and 2019",
       
       # caption = "Author's elaboration of INPS data. Includes only dependent workers. Vertical lines reflect rspectively four policy changes: 
       # Fornero Act, temporary hiring incentive on permanent contracts (HI) introducttion, Jobs Act after HI termination, Dignity Decree " 
       
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1),
        legend.position="none") +
  scale_color_manual(name = "Firm Size Class", values=c("light blue", "light green"),
                     labels = c("Control: 6-15 employees", "Treatment: 16-25 employees")) +
  geom_vline(xintercept = vl, linetype="dotted", color = "black", size=0.5) 

pta3

pta4 <- ggplot(data = dbhPT1, aes(x = TimeM, y=OECShare )) +
  geom_line(aes(color = SizeL), size = 1) +
  # geom_point(aes(color=Group)) +
  labs(x = "Monthly Periods (Jan 2005 - Dec 2019)", y = "OEC Share",
       title = "OEC Share in firms with 6-15 and 16-25 employees Italy between 2005 and 2019",
       
       caption = "Author's elaboration of INPS data. Includes only dependent workers. Vertical lines reflect rspectively four policy changes: 
       Fornero Act, temporary hiring incentive on permanent contracts (HI) introducttion, Jobs Act after HI termination, Dignity Decree " 
       
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(family = "serif"), plot.caption=element_text(hjust = 1)) +
  scale_color_manual(name = "Firm Size Class", values=c("light blue", "light green"),
                     labels = c("Control: 6-15 employees", "Treatment: 16-25 employees")) +
  geom_vline(xintercept = vl, linetype="dotted", color = "black", size=0.5) 

pta4

## Figure 10 - Arrange graphs and print ----

ggarrange(pta1, pta2, pta3, pta4 
          + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          align = "h",
          widths = c(1.7,2.5,1.7,2.5),
          ncol = 2, nrow = 2)






  
