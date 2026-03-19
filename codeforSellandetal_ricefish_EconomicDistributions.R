### Graph Rice-Fish Yields by Treatment Group ###
### Molly Doruska ###
### Created: July 23, 2025 ###

### packages required ### 
# dplyr, tidyr, ggplot2, readxl, DescTools 

### library packages ### 
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(DescTools)

### import data ###
rice_fish <- read_excel("/Users/eselland/Documents/ND/PROJECT_Rice-Fish/RiceFish_manuscript/Data_tobeshared/ricefishdata_2023.2024.xlsx",sheet="Production.Ecology")
rice_fish<-rice_fish %>% 
  filter(grepl('rice yield', AnalysisInclusion))

### keep just 2023 data ### 
rice_fish <- filter(rice_fish, Year == 2023)

### plot cdfs ### 
rice_fish$Treatment<-factor(rice_fish$Treatment,levels=c("RiceMonoculture","RiceFishCoCulture"),labels=c("Control","Rice Fish"))
myPal <- c('darkgray', 'goldenrod2')
rice_fish$KGrice<-rice_fish$RiceBagHarvest*85 #1 bag is approximately 85 kg of unprocessed rice
rice_fish$KGperHA<-rice_fish$KGrice/(rice_fish$ParcelSize/10000) #kg per hectare, transform ParcelSize from m^2 to hectares
rice_fish$TonneperHA<-rice_fish$KGperHA/1000
cdf<-ggplot(rice_fish, aes(x =TonneperHA, RiceBagHarvest, color = Treatment)) + 
  stat_ecdf(lwd = 1, geom = "line") + scale_color_manual(values = myPal) + 
  theme_bw() + xlab("Rice Yield (metric ton / hectare") + 
  ylab("Cumulative probability") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank())
ggsave("cdfs.pdf", plot = cdf, dpi = 300)
ggsave("cdfs.png", plot = cdf, dpi = 300)

### separate out treatment and control data ###
control <- filter(rice_fish, Treatment == "Control")
treatment <- filter(rice_fish, Treatment == 'Rice Fish')

### test stocahsitic dominance ### 
SomersDelta(rice_fish$TonneperHA, rice_fish$Treatment, direction = "row")
SomersDelta(control$TonneperHA, treatment$TonneperHA, direction = "row")
SomersDelta(control$TonneperHA, treatment$TonneperHA, direction = "column")


