###############################################################################
############### Graphical Analysis of military expenditure data ###############
###############################################################################

##### Setup
setwd("I:/.shortcut-targets-by-id/1fzHWM8MSEMi6f1u8QAUenn3h0-ebcc6K/NATO defense data/PEPSPP")

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(openxlsx)

'%notin%' <- Negate('%in%')

OutFolder <- getwd()

##### Linetype
linetp <- c(
  `Military Expenditure Total` = "solid",
  `Defence Expenditure Total (Eurostat COFOG)` = "longdash",
  `Military Expenditure in arms, equipment, operations and R&D` = "longdash",
  
  `NATO - Defence Expenditure Total (Billions constant 2015 €)` = "solid",
  `NATO - Defence Expenditure per capita (constant 2015 €)` = "solid",
  `NATO - Defence Expenditure without import (Billions constant 2015 €)` = "longdash",
  `NATO - Defence Expenditure without import per capita (constant 2015 €)` = "longdash",
  `NATO - Defence Expenditure without imports share of real GDP (%)` = "longdash",
  `NATO - Defence Expenditure share of real GDP (%)` = "solid",
  `Euro COFOG - Defence total without import (Billions constant 2015 €)` = "longdash",
  `Euro COFOG - Defence Expenditure Total (Billions constant 2015 €)` = "longdash",
  `NATO - Defence Expenditure in arms, equipment, operations and R&D (Billions constant 2015 €)` = "longdash",
  
  `Military Expenditure in personnel` = "solid",
  `NATO - Defence Expenditure in personnel (% total defence expenditure)` = "solid",
  `Military Expenditure in arms and equipments` = "solid",
  `NATO - Defence Expenditure in arms and equipments without imports (Thousands constant 2015 €)` = "longdash",
  
  `Imports of arms and equipments (good & services)` = "twodash"
)

##### Colors
colp <- c(
  "Italy" = "#339900",
  "Germany" = "#FF9900",
  "Spain" = "#CC0000",
  "France" = "#3399FF",
  "NATO EU Members" = "#0000FF",
  "United States of America" = "#3399FF",
  "China"  = "#FFCC33",
  "Rest of the world" = "#339900",
  "USSR/Russia" = "#FF9900",
  
  #####
  "Total General Government Expenditure (COFOG)" = "#3399FF",
  "Total General public services Expenditure (COFOG)" = "#FFCC33",
  "Total Social Protection Expenditure (COFOG)" = "#FF00FF",
  "Total Education Expenditure (COFOG)" = "#0000FF",
  "Total Health Expenditure (COFOG)" = "#FF9900",
  "Total Environmental protection Expenditure (COFOG)" = "#66CC33",
  "Total Defence Expenditure (COFOG)" = "#33CCFF",
  
  #####
  "Military Expenditure in personnel" = "#3399FF",
  "Military Expenditure in arms, equipment, operations and R&D"  = "#FFCC33",
  "Military Expenditure in infrastructures" = "#339900",
  "Military Expenditure in operations, maintenance and other" = "#FF9900",
  
  #####
  "Total Government expenditure" = "#0000FF",
  "Education expenditure" = "#99CCFF",
  "Education" = "#99CCFF",
  "Health expenditure" = "#3399FF",
  "Health" = "#3399FF",
  "Environmental protection expenditure" = "#66CC33",
  "Environment" = "#66CC33",
  
  #####
  "Capital Total Government expenditure" = "#0000FF",
  "Capital Social protection expenditure" = "#FF00FF",
  "Capital Education expenditure" = "#99CCFF",
  "Capital Health expenditure" = "#3399FF",
  "Capital Environmental protection expenditure" = "#66CC33",
  "Capital Military expenditure" = "#33CCFF",
  
  #####
  "Total military expenditure" = "#333333",
  "Arms and equipment expenditure" = "#996633",
  "Arms" = "#996633",
  
  #####
  "GDP" = "#CC0000",
  "GFCF Total" = "#3399FF",
  "Gross Fixed Capital Formation in machinery, equipment and weapon systems" = "#FF9900",
  "Employment" = "#FFCC33"
  
)


##### Data loading
Data <- read_excel("GreenPeace_DefenceExpenditure_Nov2023.xlsx")
Data <- Data %>%
  filter(Year >= 2013 & Year <= 2023,
         Country %notin% c("France"))


####################
##### Figure 1 #####
####################
p1 <- Data %>%
  mutate(
    `Military Expenditure Total` = `NATO - Defence Expenditure (Millions constant 2015 prices national currency)`/1000,
    `Military Expenditure in arms, equipment, operations and R&D` = (`NATO - Defence Expenditure in equipment (Thousands constant 2015 prices national currency)` + `NATO - Defence Expenditure in other (Thousands constant 2015 prices national currency)`)/1000
  ) %>%
  dplyr::select(Country,Year,
         `Military Expenditure Total`,
         `Military Expenditure in arms, equipment, operations and R&D`
  ) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Variable", values_to = "Value") %>%
  filter(Year >= 2013,
         Country %notin% c("NATO EU Members","France")) %>%
  mutate(
    Variable = factor(Variable,
                      levels = c(
                        "Military Expenditure Total",
                        "Military Expenditure in arms, equipment, operations and R&D"
                      ),
                      ordered = T),
    Country = factor(Country,levels = c("Germany","Italy","Spain"),ordered = T)
  ) %>%
  ggplot(mapping = aes(x = Year, col = Country, linetype = Variable)) + 
  geom_line(mapping = aes(y = Value), linewidth = 2) + 
  labs(x = "",
       y = "Billions €, constant prices 2015",
       title = "Figure 1: Total Military Expenditure and Military Expenditure in arms, equipment, operations and R&D",
       subtitle = "Billions €, constant prices 2015.") +
  scale_x_continuous(breaks = c(2008:2024)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 300, by = 5)) + 
  scale_linetype_manual("", values = linetp)+
  scale_color_manual("", values = colp)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25), legend.position = "bottom",
        legend.key.width = unit(3,"cm"),
        plot.title = element_text(face = "bold",size = 16),
        plot.subtitle = element_text(face = "bold",size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) + 
  guides(col=guide_legend(nrow=2,byrow=TRUE),
         linetype=guide_legend(nrow=2,byrow=TRUE))
ggpubr::ggexport(p1,width = 1800, height = 1200, res = 150, filename = "Figure1.png")


####################
##### Figure 2 #####
####################
p2 <- Data %>%
  mutate(
    `NATO - Defence Expenditure without imports share of real GDP (%)` = (`NATO - Defence Expenditure (Millions constant 2015 prices national currency)` - `WMEAT - Imports of arms (good & services) (Millions constant 2015 prices national currency)`)/`GDP (Millions constant 2015 prices national currency)`*100) %>%
  dplyr::select(Country,Year, `NATO - Defence Expenditure share of real GDP (%)`
  ) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Variable", values_to = "Value") %>%
  filter(Year >= 2013) %>%
  mutate(
    Variable = factor(Variable,
                      levels = c("NATO - Defence Expenditure share of real GDP (%)"),
                      ordered = T),
    Country = factor(Country,levels = c("NATO EU Members","Germany","Italy","Spain"),ordered = T)
  ) %>%
  ggplot(mapping = aes(x = Year, col = Country, linetype = Variable)) + 
  geom_line(mapping = aes(y = Value), size = 2) + 
  labs(x = "", y = "% of Real GDP",
       title = "Figure 2: Share (%) of real GDP for Defence Expenditure",
       subtitle = "% of real GDP. Sources: NATO (Military Expenditure) and Eurostat (GDP)") +
  scale_x_continuous(breaks = c(2008:2024)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 2, by = 0.125)) + 
  scale_linetype_manual("", values = linetp)+
  scale_color_manual("", values = colp)+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 25), legend.position = "bottom",
        legend.key.width = unit(3,"cm"),
        plot.title = element_text(face = "bold",size = 18),
        plot.subtitle = element_text(face = "bold",size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) + 
  guides(col=guide_legend(nrow=1,byrow=TRUE),
         linetype = "none")
ggpubr::ggexport(p2,width = 1800, height = 1200, res = 150, filename = "Figure2.png")


####################
##### Figure 3 #####
####################
RateChanges <- Data %>%
  dplyr::select(
    Country,Year,
    `Total Government expenditure` = `Euro COFOG - Capital expenditure in Total General Government Expenditure (Millions constant 2015 prices national currency)`,
    `Education expenditure` = `Euro COFOG - Capital expenditure in Education (Millions constant 2015 prices national currency)`,
    `Health expenditure` = `Euro COFOG - Capital expenditure in Health (Millions constant 2015 prices national currency)`,
    `Environmental protection expenditure` = `Euro COFOG - Capital expenditure in Environmental protection total (Millions constant 2015 prices national currency)`,
    `Arms and equipment expenditure` = `NATO - Defence Expenditure in equipment (Thousands constant 2015 prices national currency)`
  ) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Variable = factor(Variable,
                      levels = c("Total Government expenditure",
                                 "Education expenditure",
                                 "Health expenditure",
                                 "Environmental protection expenditure",
                                 "Arms and equipment expenditure"),
                      ordered = T),
    Country = factor(Country,levels = c("NATO EU Members","Germany","Italy","Spain"),ordered = T)
  ) %>%
  group_by(Country,Variable) %>%
  summarise(`Rate change 2013-2023` = (Value[Year == 2023]/Value[Year == 2013] - 1)*100,
            `Rate change 2018-2023` = (Value[Year == 2023]/Value[Year == 2018] - 1)*100,
            `Rate change 2013-2018` = (Value[Year == 2018]/Value[Year == 2013] - 1)*100) %>%
  ungroup()

p3 <- RateChanges %>%
  ggplot(mapping = aes(fill = Variable, y = `Rate change 2013-2023`, x = Country)) + 
  geom_col(position="dodge", stat="identity") + 
  geom_text(aes(label=ifelse(is.na(`Rate change 2013-2023`), "", round(`Rate change 2013-2023`,0))),
            position=position_dodge(width=0.90), vjust = -0.5) + 
  labs(x = "", y = "%",
       title = "Figure 3: Arms expenditure vs Civilian public capital expenditure (% change in real terms from 2013 to 2023)",
       subtitle = "Sources: NATO (Military expenditure) and Eurostat COFOG (Non-military expenditure)"
  ) +
  scale_y_continuous(breaks = seq(from = -100, to = 300, by = 20)) + 
  scale_fill_manual(values = colp)+
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        plot.title = element_text(face = "bold",size = 16),
        plot.subtitle = element_text(face = "bold",size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE,title = ""))
p3<- ggpubr::annotate_figure(p = p3,
                               bottom = text_grob("Note: for Eurostat variables the last available data is 2022. Values for 2024 are estimated by linearly projecting the trend 2013-2022.",
                                                  size = 12))
ggpubr::ggexport(p3,width = 1800, height = 1200, res = 150, filename = "Figure3.png")




