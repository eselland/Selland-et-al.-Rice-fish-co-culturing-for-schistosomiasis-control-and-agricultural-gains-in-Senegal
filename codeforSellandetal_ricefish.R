### Code supporting Rice-fish co-culturing for schistosomiasis control and agricultural gains in Senegal
### Selland et al.

library(glmmTMB)
library(car)
library(rstatix)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(patchwork)
library(emmeans)
library(cowplot)  


############
## Occupational Hazard
hazard<-readxl::read_excel("occuhazard_indivlevel.xlsm",sheet = "occuhazard_indivlevel")
#data<-readxl::read_excel("/Users/eselland/Documents/ND/PROJECT_Rice-Fish/RiceFish_manuscript/Data_tobeshared/occuhazard_indivlevel.xlsm",sheet = "occuhazard_indivlevel")
hazard$landHectRICE.Hlog<-log(hazard$landHectRICE.H+0.1)
hazard$Sm.pres<-as.numeric(hazard$Sm.pres) #4 children weren't tested for S. mansoni, they have NA values
mansoni.prevalence<-glmmTMB(Sm.pres ~ age + landHectRICE.Hlog+ landareaIrri.V +RiverLake + pipedlaundry + KnownTreat + (1 | Village:Household),data=hazard,family=binomial())
Anova(mansoni.prevalence) 

haematobium.prevalence<-glmmTMB(Sh.pres ~ landHectRICE.Hlog + landareaIrri.V * RiverLake + KnownTreat + (1 |Village:Household),data=hazard,family = binomial())
Anova(haematobium.prevalence)

hazard$Sm.eggcount<-round(as.numeric(hazard$Sm.eggcount)) 
hazard.intensitym<-hazard[hazard$Sm.pres=="1",] #remove the non-infected children to test for intensity
mansoni.intensity<-glmmTMB(Sm.eggcount ~ landHectRICE.Hlog + landareaIrri.V * RiverLake + KnownTreat + (1 | Village:Household), data = hazard.intensitym, family=nbinom2())
Anova(mansoni.intensity)

hazard$Sh.eggcount<-round(as.numeric(hazard$Sh.eggcount)) 
hazard.intensityh<-hazard[hazard$Sh.pres=="1",] #remove the non-infected children to test for intensity
haematobium.intensity<-glmmTMB(Sh.eggcount ~ landHectRICE.Hlog + landareaIrri.V + RiverLake + KnownTreat + (1 | Village:Household),data = hazard.intensityh, family=nbinom2())
Anova(haematobium.intensity)

############
## Fish Growth
fish<-readxl::read_excel("ricefishdata_2023.2024.xlsx",sheet = "Production.Ecology")
### experiment 1 - 2023
fish2023<-fish[fish$Year=="2023",]
### Tilapia
#use grepl to select fields used for these analyses
fish2023T<-fish2023 %>% 
  filter(grepl('tilapia growth', AnalysisInclusion))
fish2023T<-fish2023T %>%
  select(Field, InitTilapiaWgt,FinalTilapiaWgt)
fish2023T<-fish2023T %>%
  pivot_longer(!Field, names_to = "BegEnd", values_to = "Wgt")
fish2023T$BegEnd[fish2023T$BegEnd=="InitTilapiaWgt"]<-"Initial"
fish2023T$BegEnd[fish2023T$BegEnd=="FinalTilapiaWgt"]<-"Final"
fish2023T<-fish2023T[complete.cases(fish2023T), ]
fish2023T  %>% 
  mutate(Wgt=as.numeric(Wgt)) %>% 
  t_test(Wgt ~ BegEnd, paired = TRUE,detailed=TRUE) %>%
  add_significance() #signif diff between groups, p=0.016 (one tailed p=0.008)
fish2023T  %>% #Cohen's D (effect size)
  mutate(Wgt=as.numeric(Wgt)) %>%
  cohens_d(Wgt ~ BegEnd, paired = TRUE,hedges.correction = TRUE) #hedges correction for small sample size

### African bonytongue
fish2023AB<-fish2023 %>% 
  filter(grepl('african bonytongue growth', AnalysisInclusion))
fish2023AB<-fish2023AB %>%
  select(Field, InitAB1Wgt,InitAB2Wgt,FinalAB1Wgt,FinalAB2Wgt)
fish2023AB<-fish2023AB %>%
  pivot_longer(!Field, names_to = "BegEnd", values_to = "Wgt")
fish2023AB$BegEnd[fish2023AB$BegEnd=="InitAB1Wgt"]<-"Initial"
fish2023AB$BegEnd[fish2023AB$BegEnd=="InitAB2Wgt"]<-"Initial"
fish2023AB$BegEnd[fish2023AB$BegEnd=="FinalAB1Wgt"]<-"Final"
fish2023AB$BegEnd[fish2023AB$BegEnd=="FinalAB2Wgt"]<-"Final"
fish2023AB$Wgt<-as.numeric(fish2023AB$Wgt)
fish2023AB  %>% #t-test
  t_test(Wgt ~ BegEnd, paired = TRUE,detailed=TRUE) %>%
  add_significance() 
fish2023AB  %>% cohens_d(Wgt ~ BegEnd, paired = TRUE,hedges.correction = TRUE)#Cohen's D (effect size)
### experiment 2 - 2024
### Tilapia
fish2024<-fish[fish$Year=="2024",]
fish2024T<-fish2024 %>% 
  filter(grepl('tilapia growth', AnalysisInclusion))
fish2024T<-fish2024T %>%
  select(Field, InitTilapiaWgt,FinalTilapiaWgt)
fish2024T<-fish2024T %>%
  pivot_longer(!Field, names_to = "BegEnd", values_to = "Wgt")
fish2024T$BegEnd[fish2024T$BegEnd=="InitTilapiaWgt"]<-"Initial"
fish2024T$BegEnd[fish2024T$BegEnd=="FinalTilapiaWgt"]<-"Final"
fish2024T<-fish2024T[complete.cases(fish2024T), ]
fish2024T  %>% 
  mutate(Wgt=as.numeric(Wgt)) %>% 
  t_test(Wgt ~ BegEnd, paired = TRUE,detailed=TRUE) %>%
  add_significance()

### African bonytongue
fish2024AB<-fish2024 %>% 
  filter(grepl('african bonytongue growth', AnalysisInclusion))
fish2024AB<-fish2024AB %>%
  select(Field, InitABWgt,FinalABWgt)
fish2024AB<-fish2024AB %>%
  pivot_longer(!Field, names_to = "BegEnd", values_to = "Wgt")
fish2024AB$BegEnd[fish2024AB$BegEnd=="InitABWgt"]<-"Initial"
fish2024AB$BegEnd[fish2024AB$BegEnd=="FinalABWgt"]<-"Final"
fish2024AB$Wgt<-as.numeric(fish2024AB$Wgt)
fish2024AB  %>% #t-test
  t_test(Wgt ~ BegEnd, paired = FALSE,detailed=TRUE) %>%
  add_significance() 

############
## Snail Intermediate Hosts
### 2023 - field level
snail<-fish %>% 
  filter(grepl('snail abundance', AnalysisInclusion))
snail2023<-snail[snail$Year=="2023",]
snail2023$HeterotisPresence<-0
snail2023$InitAB1Wgt<-as.numeric(snail2023$InitAB1Wgt)
snail2023$HeterotisPresence[!is.na(snail2023$InitAB1Wgt)]<-1
snail2023$InitAB2Wgt<-as.numeric(snail2023$InitAB2Wgt)
snail2023$HeterotisPresence[!is.na(snail2023$InitAB2Wgt)]<-1
snail2023$NumBulinusFound<-as.numeric(snail2023$NumBulinusFound)
snail2023$NumTilapiaHarvested<-as.numeric(snail2023$NumTilapiaHarvested)
bulinus.tg<-glmmTMB(NumBulinusFound~as.factor(HeterotisPresence)+NumTilapiaHarvested+(1|Village),data=snail2023,family=nbinom2,na.action=na.omit)
Anova(bulinus.tg) #Bulinus truncatus/globosus, one tailed p value 0.012
snail2023$NumLymnaeFound<-as.numeric(snail2023$NumLymnaeFound)
lymnaea<-glmmTMB(NumLymnaeFound~as.factor(HeterotisPresence)+NumTilapiaHarvested+(1|Village),data=snail2023,family=nbinom2,na.action=na.omit)
Anova(lymnaea) #Lymnaea spp., one tailed p value 0.085
snail2023$NumBulFSFound<-as.numeric(snail2023$NumBulFSFound)
bulinus.fs<-glmmTMB(NumBulFSFound~as.factor(HeterotisPresence)+NumTilapiaHarvested+(1|Village),data=snail2023,family=nbinom2,na.action=na.omit)
Anova(bulinus.fs) #Bulinus forskalii/senegalensis,one tailed p value 0.34

### 2024 - sweep level
sweepeco<-readxl::read_excel("ricefishdata_2023.2024.xlsx",sheet = "Production.Ecology.Sweep")
sweepeco$Depth<-as.numeric(sweepeco$Depth)
Anova(glmmTMB(NumBulinusFound~Treatment+Depth+(1|Village/FieldID),sweepeco,family=nbinom2())) #bulinus truncatus/globosus
sweepeco$NumBulFSFound<-as.numeric(sweepeco$NumBulFSFound)
Anova(glmmTMB(NumBulFSFound~Treatment+Depth+(1|Village/FieldID),sweepeco,family=nbinom2())) #bulinus forskalii/senegalensis


############
## Insects - 2024 only, sweep level
sweepeco$NumInsectFound<-as.numeric(sweepeco$NumInsectFound)
sweepeco$Depth<-as.numeric(sweepeco$Depth)
insect.abundance<-glmmTMB(NumInsectFound~Treatment+Depth+(1|Village/FieldID),data=sweepeco,family=nbinom2())
Anova(insect.abundance)

############
## Nutrients
soil<-readxl::read_excel("ricefishdata_2023.2024.xlsx",sheet = "Soil.Water")
### soil carbon
soilcarbon<-glmmTMB(SoilCarbon~Treatment+(1|Village/Field),soil,family=gaussian())
Anova(soilcarbon)

### soil nitrogen
soilnitrogen<-glmmTMB(SoilNitrogen~Treatment+(1|Village/Field),soil,family=gaussian())
Anova(soilnitrogen)

### soil available phosphorus
soilphosphorus<-glmmTMB(SoilPhosphorus~Treatment+(1|Village/Field),soil,family=gaussian())
Anova(soilphosphorus)

### water phosphorus
soil$WaterSRPConcentration.log<-log(soil$WaterSRPConcentration)
waterphosphorus<-glmmTMB(WaterSRPConcentration.log~Treatment+(1|Village/Field),soil,family=gaussian())
Anova(waterphosphorus)

### water ammonia
soil$WaterAmmoniaConcentration.log<-log(soil$WaterAmmoniaConcentration)
wateramonia<-glmmTMB(WaterAmmoniaConcentration.log~Treatment+(1|Village/Field),soil,family=gaussian())
Anova(wateramonia)

############
## Rice Yields
### 2023
rice2023<-fish2023 %>% 
  filter(grepl('rice yield', AnalysisInclusion))
rice2023$KGrice<-rice2023$RiceBagHarvest*85 #1 bag is approximately 85 kg of unprocessed rice
rice2023$KGperHA<-rice2023$KGrice/(rice2023$ParcelSize/10000) #kg per hectare, transform ParcelSize from m^2 to hectares
rice2023  %>% #t-test 
  t_test(KGperHA ~ Treatment, paired = TRUE,detailed=TRUE) %>%
  add_significance()
rice2023  %>% cohens_d(KGperHA ~ Treatment, paired = TRUE) #Cohen's D (effect size)

### 2024
fish2024<-fish[fish$Year=="2024",]
rice2024<-fish2024 %>% 
  filter(grepl('rice yield', AnalysisInclusion))
rice2024$KGrice<-rice2024$RiceBagHarvest*85 #1 bag is approximately 85 kg of unprocessed rice
rice2024$KGperHA<-rice2024$KGrice/(rice2024$ParcelSize/10000) #kg per hectare,transform ParcelSize from m^2 to hectares
rice2024  %>% #t-test 
  t_test(KGperHA ~ Treatment, paired = FALSE,detailed=TRUE) %>%
  add_significance()
rice2024  %>% cohens_d(KGperHA ~ Treatment, paired = FALSE) #Cohen's D (effect size)



############
## Figures
## Occupational Hazard
### Figure 1A
mansonidf <- ggpredict(mansoni.prevalence, terms = c("landHectRICE.Hlog [all]"))
haemdf <- ggpredict(haematobium.prevalence, terms = c("landHectRICE.Hlog [all]"))

ylabel <- expression(atop(bold("Predicted child likelihood of"),
                          bolditalic(Schistosoma) ~ bold("spp. infection")))  

combinedplot<-ggplot() + 
  geom_line(data=haemdf,aes(x, predicted,linetype="haem", color="haem"),color="#0072B2") +
  geom_ribbon(data=haemdf,aes(x, predicted,ymin=conf.low, ymax=conf.high, fill="haem"), alpha=0.2,fill="#0072B2") +
  geom_line(data=mansonidf,aes(x, predicted,linetype="mansoni", color="mansoni"),color="goldenrod3") +
  geom_ribbon(data=mansonidf,aes(x, predicted,ymin=conf.low, ymax=conf.high, fill="mansoni"), alpha=0.2,fill="goldenrod3")+
  theme_academia()+ylim(0,1)+
  labs(
    x = "log Hectares of Rice Farmed by Household",
    y = ylabel,
    title = "",
    color = ""
  )+
  scale_color_manual(values=c("haem"="#0072B2", "mansoni"="goldenrod3"),
                     labels=c("haem"=expression(italic(S.~haematobium)),
                              "mansoni"=expression(italic(S.~mansoni)))) +
  scale_fill_manual(values=c("haem"="#0072B2", "mansoni"="goldenrod3"),
                    labels=c("haem"=expression(italic(S.~haematobium)),
                             "mansoni"=expression(italic(S.~mansoni)))) +
  scale_linetype_manual(values=c("haem"="solid", "mansoni"="solid"),
                        labels=c("haem"=expression(italic(S.~haematobium)),
                                 "mansoni"=expression(italic(S.~mansoni)))) +
  theme(plot.margin = unit(c(0.3,4,0.1,0.3),"cm"),legend.position = c(0.75,0.18),axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),legend.title = element_blank())+annotate("text",x=1, y = 0.3,label="****",size=6,color="goldenrod3")+
  (xlab("Log hectares of rice farmed by household"))+theme(plot.margin = margin(5, 2, 5, 2))

### Figure 1B
ylabel <- expression(atop(bold("Predicted egg output from"),
                          bolditalic(Schistosoma) ~ bold("spp. infection")))  
mansonidf_in <- ggpredict(mansoni.intensity, terms = c("landHectRICE.Hlog [all]"))
haemdf_in <- ggpredict(haematobium.intensity, terms = c("landHectRICE.Hlog [all]"))

combinedplot_in<-ggplot() + 
  geom_line(data=haemdf_in,aes(x, predicted,linetype=group, color=group),color="#0072B2") +
  geom_ribbon(data=haemdf_in,aes(x, predicted,ymin=conf.low, ymax=conf.high, fill=group), alpha=0.2,fill="#0072B2") +
  geom_line(data=mansonidf_in,aes(x, predicted,linetype=group, color=group),color="goldenrod3") +
  geom_ribbon(data=mansonidf_in,aes(x, predicted,ymin=conf.low, ymax=conf.high, fill=group), alpha=0.2,fill="goldenrod3")+
  theme_academia()+
  labs(
    x = "log Hectares of Rice Farmed by Household",
    y = ylabel,
    title = "",
    color = ""
  )+
  theme(plot.margin = unit(c(0.3,4,0.1,0.3),"cm"),legend.position = "none",axis.text.y = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  geom_rug(data=ROmerge16.16,aes(landHectRICE.Hlog), sides = "b",alpha = 0.4,linewidth=0.8,color="black")+
  (xlab("Log hectares of rice farmed by household"))+annotate("text",x=0.9, y = 95,label="*",size=6,color="#0072B2")+theme(plot.margin = margin(5, 2, 5, 2))

# Figure 1
spacer <- ggplot() + theme_void()
ggarrange(combinedplot,spacer,combinedplot_in,labels = c("A","B"),ncol=3,nrow=1,widths = c(1,0.1, 1))


### Figure 2A
rice2023$Treatment<-factor(rice2023$Treatment,levels = c("RiceMonoculture","RiceFishCoCulture"),labels = c("Rice Monoculture","Rice-Fish Co-Culture"))
riceyieldplot<-ggplot(rice2023, aes(x = Treatment, y = KGperHA,fill=Treatment))+
  geom_boxplot()+
  geom_point(aes(x =Treatment, y = KGperHA),color="black") +
  geom_line(aes(x  =Treatment, y = KGperHA, group = Pairing),linetype = 3)+
  scale_fill_manual(values=c("grey","goldenrod3"))+
  xlab("")+
  ylab("Rice Yield (kg/ha)")+
  theme_academia()  +  
  theme(legend.position = "none")+
  annotate("text", x = 1.5, y = 10000.3, label = "***",size=6)

### Figure 2B
rice2024$Treatment<-factor(rice2024$Treatment,levels = c("RiceMonoculture","RiceFishCoCulture"),labels = c("Rice Monoculture","Rice-Fish Co-Culture"))
riceyieldyr2<-ggplot(rice2024, aes(x = Treatment, y = KGperHA,fill=Treatment))+
  geom_boxplot()+
  geom_point(aes(x =Treatment, y = KGperHA),color="black") +
  scale_fill_manual(values=c("grey","goldenrod3"))+
  xlab("")+
  ylab("Rice Yield (kg/ha)")+
  theme_academia()  +  
  theme(legend.position = "none")

### Figure 2C
ylabel2 <- expression(atop(bold("Average")~bolditalic(O.~niloticus),
                           bold("weight (g)")))  
fish2023T$BegEnd<-factor(fish2023T$BegEnd,levels = c("Initial","Final"),labels = c("Stocking","Harvest"))
tilapiaplot<-ggplot(fish2023T, aes(x = BegEnd, y = as.numeric(Wgt),fill=BegEnd))+
  geom_boxplot()+
  geom_point(aes(x =BegEnd, y = as.numeric(Wgt)),color="black") +
  geom_line(aes(x  =BegEnd, y = as.numeric(Wgt), group = Field),linetype = 3)+
  scale_fill_manual(values=c("grey","#0072B2"))+
  xlab("")+
  ylab(ylabel)+
  theme_academia()  +  
  theme(legend.position = "none",plot.margin = unit(c(0.3,0.1,4,0.3),"cm"))+ylim(0,170)+
  annotate("text",x=1.5, y = 160,label="***",size=6)+ylab(ylabel2)+scale_fill_manual(values=c("lightcyan","#0072B2"))


### Figure 2D
ylabel2 <- expression(atop(bold("Individual")~bolditalic(H.~niloticus),
                           bold("weight (g)")))  
fish2023AB$BegEnd<-factor(fish2023AB$BegEnd,levels = c("Initial","Final"),labels = c("Stocking","Harvest"))
heterotisplot<-ggplot(fish2023AB, aes(x = BegEnd, y = as.numeric(Wgt),fill=BegEnd))+
  geom_boxplot()+
  geom_point(aes(x =BegEnd, y = as.numeric(Wgt)),color="black") +
  scale_fill_manual(values=c("grey","#0072B2"))+
  xlab("")+
  ylab(ylabel)+
  theme_academia()  +  
  theme(legend.position = "none",plot.margin = unit(c(0.3,0.1,4,0.3),"cm"))+
  annotate("text",x=1.5, y = 2050,label="****",size=6)+ylim(100,2300)+ylab(ylabel2)+scale_fill_manual(values=c("lightcyan","#0072B2"))

# Figure 2

riceyieldyr2 <- riceyieldyr2 + theme(legend.position = "none")+theme(legend.title = element_blank()) 
riceyieldplot<-riceyieldplot+
  theme(legend.title = element_blank()) +theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
tilapiaplot3<-tilapiaplot+
  theme(legend.title = element_blank()) +theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
heterotisplot3<-heterotisplot+
  theme(legend.title = element_blank()) +theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))

riceyieldplot + riceyieldyr2 +tilapiaplot3+heterotisplot3+
  plot_layout(design = layout)+plot_annotation(tag_levels = c("A", "B","C","D","E"))+
  plot_layout(axis_titles ="collect",guides = "collect")



### Figure 3A
ylabel <- expression(atop(bold("Estimated marginal log"),
                          bold("mean snail abundance")))  
emm1=emmeans(bulinus.tg,pairwise~HeterotisPresence)
Buldf<-emm1$emmeans %>% #plot estimated marginal means
  as.data.frame()
emmL<-emmeans(lymnaea,pairwise~HeterotisPresence)
Lymndf<-emmL$emmeans %>%
  as.data.frame()
emmF<-emmeans(bulinus.fs,pairwise~HeterotisPresence)
Foskdf<-emmF$emmeans %>%
  as.data.frame()
#combine dfs for easy plotting
Lymndf$snail<-"Lymn"
Buldf$snail<-"Bul"
Foskdf$snail<-"Fosk"
df <- merge(Lymndf, Buldf, all = TRUE)
df<-merge(df,Foskdf,all=TRUE)
df$HeterotisPresence<-as.character(df$HeterotisPresence)
df$HeterotisPresence[df$HeterotisPresence=="0"]<-"H. niloticus absence"
df$HeterotisPresence[df$HeterotisPresence=="1"]<-"H. niloticus presence"

snailpairs<-tribble( #pull information from t tests to make a df for plotting
  ~group1,~group2,~p.adj,~snail,
  "H. niloticus absence","H. niloticus presence","0.0118","Bul",
  "H. niloticus absence","H. niloticus presence","0.0847","Lymn",
  "H. niloticus absence","H. niloticus presence","n.s.","Fosk") 
snailpairs <- snailpairs %>%
  mutate(y.position = c(3.6, 3.8, 4),xmin=c(0.83,1,1.16),xmax=c(1.83,2,2.16))
snailpairs$p.adj[snailpairs$p.adj=="0.0118"]<-"**"
snailpairs$p.adj[snailpairs$p.adj=="0.0847"]<-"*"
snailpairs3<-snailpairs2[snailpairs2$snail!="Fosk",]
snailpairsns<-snailpairs2[snailpairs2$snail=="Fosk",]
df$snail<-factor(df$snail,levels = c("Bul","Lymn","Fosk"))
snailpredation<-ggplot(df,aes(x=Hetero,y=emmean,group=snail,shape=snail)) + 
  geom_point(position=position_dodge(width = 0.5),size=2.2) + 
  scale_shape_manual(values=c(15,17,18),labels=c(expression(italic(B. ~truncatus/globosus)), 
                                                 expression(italic(Lymnaea)~"spp."), 
                                                 expression(italic(B. ~forskalii/senegalensis))))+
  geom_errorbar(aes(x = Hetero, ymin =asymp.LCL,  ymax = asymp.UCL,width = 0.2), position = position_dodge(width = 0.5),
    alpha = 1, show.legend=FALSE)+  geom_point(color="black", size=1,position=position_dodge(width=0.5)) +
  xlab("")+
  ylab(ylabel)+
  theme_academia()  +  
  ylim(-2.9,7)+
  scale_x_discrete(labels = c(
    expression(italic(H. ~niloticus)~"Absence"),
    expression(italic(H. ~niloticus)~"Presence")))+
  theme(legend.position = "bottom",legend.title=element_blank())+
  stat_pvalue_manual(snailpairs3, step.increase = 0.125,
                     label = "p.adj",size = 6.5,y.position = 4)+
  stat_pvalue_manual(snailpairsns, step.increase = 0.15,
                     label = "p.adj",size = 3.5,y.position = 5.93)

### Figure 3B
ylabel <- expression(atop(bold("Estimated marginal log"),
                          bold("mean insect abundance")))  
insectdf<-as.data.frame(emmeans(insect.abundance,~Treatment,method="pairwise")) 
insectdf$Treatment<-factor(insectdf$Treatment,levels = c("RiceMonoculture","RiceFishCoCulture"))
insectplot<-ggplot() +
  geom_errorbar(
    data = insectdf,
    aes(
      x = Treatment, 
      ymin =asymp.LCL,  
      ymax = asymp.UCL,
      width = 0.2
    ), position = position_dodge(width = 0.3),
    alpha = 1, 
    show.legend=FALSE
  )+geom_point(
    data = insectdf, 
    aes(x = Treatment, y = emmean),
    show.legend=TRUE,
    position = position_dodge(width = 0.3)
  )+theme_academia() +
  labs(
    x = "",
    y = ylabel,
    title = ""
  )+scale_x_discrete(labels = c("Rice Monoculture", "Rice-Fish Co-Culture"))+
  theme(legend.title=element_blank(),strip.text = element_text(size = 5),
        legend.position = "")+ylim(0.4,2.8)+
  annotate("text", x = 1.5, y = 2.7, label = "***",size=6)
### Figure 3C
ylabel <- expression(atop(bold("Predicted soil"),
                          bold("oxidized carbon (%)")))  
carb_pred <- ggpredict(soilcarbon, terms = c("Treatment"))
carb_pred$x<-factor(carb_pred$x,levels = c("RiceMonoculture","RiceFishCoCulture"))
carbonplot<-ggplot(carb_pred,aes(x,predicted))+geom_point()+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width = 0.5))+theme_academia()+
  labs(
    x = "",
    y = ylabel,
    title = "",
    color = "Village"
  )+theme(strip.text = element_text(size = 10),plot.margin = unit(c(0.3,0.1,1,0.3),"cm"),
          legend.position = "bottom")+
  annotate("text", x = 1.5, y = 1.1, label = "**",size=6)+theme(axis.text.x = element_text(angle = 0, hjust = 0.7,vjust=0.9))+
  ylim(0,1.2)

### Figure 3D
ylabel <- expression(atop(bold("Predicted soil"),
                          bold("total nitrogen (%)")))  
nit_pred<-ggpredict(soilnitrogen,terms=c("Treatment"))
nit_pred$x<-factor(nit_pred$x,levels = c("RiceMonoculture","RiceFishCoCulture"))
nitroplot<-ggplot(nit_pred,aes(x,predicted))+geom_point()+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width = 0.5))+theme_academia()+
  labs(
    x = "",
    y = ylabel,
    title = "",
    color = "Village"
  )+theme(strip.text = element_text(size = 10),plot.margin = unit(c(0.3,0.1,1,0.3),"cm"),
          legend.position = "bottom")+
  annotate("text", x = 1.5, y = 0.105, label = "***",size=6)+theme(axis.text.x = element_text(angle = 0, hjust = 0.7,vjust=0.9))+
  ylim(0.025,0.115)

# Figure 3
legend_snail <- get_legend(snailpredation + theme(legend.position = "bottom"))
snailpredation_nolegend <- snailpredation + theme(legend.position = "none")+theme(plot.margin = unit(c(0,0.1,0,0.1),"cm"))
insectplot<-insectplot+theme(plot.margin = unit(c(0,0.1,0,0.1),"cm"))
carbonplot<-carbonplot+theme(plot.margin = unit(c(0,0.1,0,0.1),"cm"))
nitroplot<-nitroplot+theme(plot.margin = unit(c(0,0.1,0,0.1),"cm"))
layout <- c(
  area(t = 1, l = 1, b = 2, r = 2),
  area(t = 1, l = 3, b = 2, r = 4),
  area(t = 3, l = 1, b = 4, r = 2),
  area(t = 3, l = 3, b = 4, r = 4)
)
(snailpredation_nolegend) + (insectplot)+(carbonplot)+(nitroplot)+
  plot_layout(design = layout)+plot_annotation(tag_levels = c("A", "B","C","D","E"))+
  plot_layout(axis_titles ="collect",guides = "collect") & theme(legend.position = 'bottom')





