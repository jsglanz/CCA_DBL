####pH profiles
#load data
pH_long<-Oct2020 #"mVprofiles" sheet

#Load packages
library(dplyr)
library(tidyr) 
library(plotrix)
library(plyr)
library(ggplot2)

###Need to change data from long to wide but first need to tidy 
pH_long$Height<-as.factor(pH_long$`HeightAboveSurf(mm)`) #need to make height factor so it will go wide

#select columns that will be used 
View(phl)
phl<-pH_long%>%select(3:8,15,19)

#Need to filter by light/dark because sample id's are shared between light conditions and id's must be unique when spreading
lp_long<-phl%>% filter(`Light/Dark`=="light")
dp_long<-phl%>% filter(`Light/Dark`=="dark")

#Spread data from long to wide; spread(name of long dataframe, column to spread into multiple columns, name of column that will become cells under new height columns)
dp_wide<- spread(dp_long,Height,pHdev)

lp_wide<- spread(lp_long,Height,pHdev)

#select only heights common to all samples (since additional heights were measured with the first few samples)
View(lp_wide)
dp_wide<-dp_wide%>% select(1:7,9,11:13,15:23)
lp_wide<-lp_wide%>% select(1:7,9,11:13,15:18,20:24)

####Obtain pooled means and se's across all samples for each light condition, flow speed, replicate type (branch (B) vs interstitial space (IS)), and invert presence
#Filter dark and light data by flow speed and rep type
dpbs<-dp_wide%>% filter(Flow=="0", `Branch/IS`=="B")
dpiss<-dp_wide%>% filter(Flow=="0", `Branch/IS`=="IS")

dpbl<-dp_wide%>% filter(Flow=="2", `Branch/IS`=="B")
dpisl<-dp_wide%>% filter(Flow=="2", `Branch/IS`=="IS")

dpbh<-dp_wide%>% filter(Flow=="15", `Branch/IS`=="B")
dpish<-dp_wide%>% filter(Flow=="15", `Branch/IS`=="IS")

lpbs<-lp_wide%>% filter(Flow=="0", `Branch/IS`=="B")
lpiss<-lp_wide%>% filter(Flow=="0", `Branch/IS`=="IS")

lpbl<-lp_wide%>% filter(Flow=="2", `Branch/IS`=="B")
lpisl<-lp_wide%>% filter(Flow=="2", `Branch/IS`=="IS")

lpbh<-lp_wide%>% filter(Flow=="15", `Branch/IS`=="B")
lpish<-lp_wide%>% filter(Flow=="15", `Branch/IS`=="IS")

#Calculate mean and se by invertebrate presence for each filtered dataset above
lpbs.m<-aggregate(lpbs[, (7:20)], list(lpbs$Epi), mean, na.rm=TRUE)
lpbs.se<-aggregate(lpbs[, 7:20], list(lpbs$Epi), std.error, na.rm=TRUE)

#Change column names 
colnames(lpbs.m)[1]<-'epi'
colnames(lpbs.se)[1]<-'epi'

#convert into long format
lpbs.long<-lpbs.m %>%
  gather(Height, Mean, 2:15)

lpbs.se.long<-lpbs.se %>% gather(Height, se, 2:15)

#combine so se is in same df as mean
lpbs.r<-cbind(lpbs.long,lpbs.se.long$se)

#rename columns
colnames(lpbs.r)[3]<-'Mean' 
colnames(lpbs.r)[4]<-'se'

#save data to csv
write.csv(lpbs.r,file="lpbs.csv", row.names = TRUE)

#load PooledpHDevMeansSE
Oct20<-PooledpHDevMeansSE

#Filter by flow speed and rep type
bh<-Oct20%>% filter(Flow=="15", IsB=="B")
ish<-Oct20%>% filter(Flow=="15", IsB=="IS")
bl<-Oct20%>% filter(Flow=="2", IsB=="B")
isl<-Oct20%>% filter(Flow=="2", IsB=="IS")
bs<-Oct20%>% filter(Flow=="0", IsB=="B")
iss<-Oct20%>% filter(Flow=="0", IsB=="IS")

#Plot profiles
library("wesanderson")#for wes anderson color palette

#Interstitial space, flow = 0
ggplot(iss, aes(x =    Mean,
               y = Height,
               color= Epi,
               shape= LD)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7, height=0.1) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(-0.15, 0.35)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Deviation from Ambient pH")+
  labs(color = "Invert Presence",shape="Light/Dark")

#Interstitial space, flow = Low
ggplot(isl, aes(x =    Mean,
                y = Height,
                color= Epi,
                shape= LD)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7, height=0.1) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(-0.05, 0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Deviation from Ambient pH")+
  labs(color = "Invert Presence",shape="Light/Dark")

#Interstitial space, flow = High
ggplot(ish, aes(x =    Mean,
                y = Height,
                color= Epi,
                shape= LD)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7, height=0.1) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(-0.01, 0.025)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Deviation from Ambient pH")+
  labs(color = "Invert Presence",shape="Light/Dark")

#Branch, flow = 0
ggplot(bs, aes(x =    Mean,
                y = Height,
                color= Epi,
                shape= LD)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7, height=0.1) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(-0.05, 0.1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Deviation from Ambient pH")+
  labs(color = "Invert Presence",shape="Light/Dark")

#Branch, flow = Low
ggplot(bl, aes(x =    Mean,
                y = Height,
                color= Epi,
                shape= LD)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7, height=0.1) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(-0.01, 0.015)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Deviation from Ambient pH")+
  labs(color = "Invert Presence",shape="Light/Dark")

#Branch, flow = High
ggplot(bh, aes(x =    Mean,
                y = Height,
                color= Epi,
                shape= LD)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7, height=0.1) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(-0.0025, 0.005)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Deviation from Ambient pH")+
  labs(color = "Invert Presence",shape="Light/Dark")
