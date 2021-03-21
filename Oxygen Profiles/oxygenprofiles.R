#Oxygen profiles
#load data
oxy_long<-Oct2020 #RawO2Profiles sheet

#Load packages
library(dplyr)
library(tidyr) 
library(plotrix)
library(plyr)
library(ggplot2)

###Need to change data from long to wide but first need to tidy 
oxy_long$Height<-as.factor(oxy_long$`x(mm)`) #need to make height factor so it will go wide
oxy_long<-oxy_long[,-c(10:11)]#remove se column (since we'll calc new ones for the new means) and extra column

oxy_long<-oxy_long[,-3] #remove unused id column 
oxy_long<-oxy_long[,-7] #remove unused x(mm) 

View(oxy_long)

#Need to filter by light/dark because sample id's are shared between light conditions and id's must be unique when spreading
lo_long<-oxy_long%>% filter(`Light/Dark`=="Light")
do_long<-oxy_long%>% filter(`Light/Dark`=="Dark")

#Spread data from long to wide; spread(name of long dataframe, column to spread into multiple columns, name of column that will become cells under new height columns)
lo_wide<- spread(lo_long,Height,Mean)
do_wide<- spread(do_long,Height,Mean)
View(do_wide)

#remove extraneous rows from dataframe
do_long<-do_long[-c(83:91),]#
do_long<-do_long[-c(172:173),]
do_long<-do_long[-c(190:193),]

#remove NA column(s)
lo_wide<-lo_wide[,-25]
do_wide<-do_wide[,-c(39:41)]

#select only heights common to all samples (since additional heights were measured with the first few samples)
do_wide<- do_wide %>% select(1:11,13:14,16:17,19,21,23,25,28,30,33,35,38)


###Obtain pooled means and se's across all samples for each light condition, flow speed, replicate type (branch (B) vs interstitial space (IS)), and invert presence
#Filter dark and light data by flow speed and rep type
dobs<-do_wide%>% filter(`FlowSpeed(cm/s)`=="0", `B/IS`=="B")
lobs<-lo_wide%>% filter(`FlowSpeed(cm/s)`=="0", `B/IS`=="B")

dobl<-do_wide%>% filter(`FlowSpeed(cm/s)`=="2", `B/IS`=="B")
lobl<-lo_wide%>% filter(`FlowSpeed(cm/s)`=="2", `B/IS`=="B")

dobh<-do_wide%>% filter(`FlowSpeed(cm/s)`=="15", `B/IS`=="B")
lobh<-lo_wide%>% filter(`FlowSpeed(cm/s)`=="15", `B/IS`=="B")

doiss<-do_wide%>% filter(`FlowSpeed(cm/s)`=="0", `B/IS`=="IS")
loiss<-lo_wide%>% filter(`FlowSpeed(cm/s)`=="0", `B/IS`=="IS")

doisl<-do_wide%>% filter(`FlowSpeed(cm/s)`=="2", `B/IS`=="IS")
loisl<-lo_wide%>% filter(`FlowSpeed(cm/s)`=="2", `B/IS`=="IS")

doish<-do_wide%>% filter(`FlowSpeed(cm/s)`=="15", `B/IS`=="IS")
loish<-lo_wide%>% filter(`FlowSpeed(cm/s)`=="15", `B/IS`=="IS")


#Calculate mean and se by invertebrate presence for each filtered dataset above
dobs.m<-aggregate(dobs[, (7:24)], list(dobs$`With/WithoutEpi`), mean, na.rm=TRUE) 
dobs.se<-aggregate(dobs[, 7:24], list(dobs$`With/WithoutEpi`), std.error, na.rm=TRUE)

#Change column names 
colnames(dobs.m)[1]<-'epi'
colnames(dobs.se)[1]<-'epi'

#convert into long format
dobs.long<-dobs.m %>%
  gather(Height, Mean, 2:19)

dobs.se.long<-dobs.se %>% gather(Height, se, 2:19)

#combine so se is in same df as mean
lobs.r<-cbind(dobs.long,dobs.se.long$se)

#rename columns
colnames(dobs.r)[3]<-'Mean' 
colnames(dobs.r)[4]<-'se'

#save data to csv
write.csv(dobs.r,file="loiss.csv", row.names = TRUE)

###Load pooled O2 Means and SE data
Oct20<-PooledO2MeansSE

#Filter by flow speed and rep type
bh<-Oct20%>% filter(Flow=="High", IsB=="B")
ish<-Oct20%>% filter(Flow=="High", IsB=="IS")
bl<-Oct20%>% filter(Flow=="Low", IsB=="B")
isl<-Oct20%>% filter(Flow=="Low", IsB=="IS")
bs<-Oct20%>% filter(Flow=="Static", IsB=="B")
iss<-Oct20%>% filter(Flow=="Static", IsB=="IS")

#Plot profiles
library("wesanderson")#for wes anderson color palette

#Interstitial space, flow=0
p<-ggplot(iss, aes(x =    Mean,
                   y = Height,
                   color= Epi,
                   shape=LD
)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(40, 250)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Oxygen (%Ambient)")+
  labs(color = "Invert Presence", shape="Light/Dark")

p#check xlim (40, 250)

#Add dashed line segments to denote where DBL met ambient seawater
#wes_palette("Royal2")#"#9A8822", "#F5CDB4" aka brown, light pink
p+geom_segment(aes(x=40, xend=95,y=9.66,yend=9.66), linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=40, xend=95, y=7.88, yend=7.88),linetype="dashed",color= "#F5CDB4")+
  geom_segment(aes(x=105, xend=250, y=7.9, yend=7.9),linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=105, xend=250, y=9.93, yend=9.93),linetype="dashed",color="#F5CDB4")


#Interstitial space, flow= Low
p<-ggplot(isl, aes(x =    Mean,
                   y = Height,
                   color= Epi,
                   shape=LD
)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(80, 125)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Oxygen (%Ambient)")+
  labs(color = "Invert Presence", shape="Light/Dark")

p#check xlim (80, 130 )

#Add dashed line segments to denote where DBL met ambient seawater
#wes_palette("Royal2")#"#9A8822", "#F5CDB4" aka brown, light pink
p+geom_segment(aes(x=80, xend=95,y=2.78,yend=2.78), linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=80, xend=95, y=3, yend=3),linetype="dashed",color= "#F5CDB4")+
  geom_segment(aes(x=105, xend=125, y=4.15, yend=4.15),linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=105, xend=125, y=3.34, yend=3.34),linetype="dashed",color="#F5CDB4")

#Interstitial space, flow= High
p<-ggplot(ish, aes(x =    Mean,
                   y = Height,
                   color= Epi,
                   shape=LD
)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(95, 110)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Oxygen (%Ambient)")+
  labs(color = "Invert Presence", shape="Light/Dark")

p#check xlim (95, 110)

#Add dashed line segments to denote where DBL met ambient seawater
#wes_palette("Royal2")#"#9A8822", "#F5CDB4" aka brown, light pink
p+geom_segment(aes(x=95, xend=98,y=.55,yend=.55), linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=95, xend=98, y=.36, yend=.36),linetype="dashed",color= "#F5CDB4")+
  geom_segment(aes(x=102, xend=110, y=.2, yend=.2),linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=102, xend=110, y=.68, yend=.68),linetype="dashed",color="#F5CDB4")

#Branches, flow=0
p<-ggplot(bs, aes(x =    Mean,
                   y = Height,
                   color= Epi,
                   shape=LD
)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(65, 200)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Oxygen (%Ambient)")+
  labs(color = "Invert Presence", shape="Light/Dark")

p#check xlim ( 65, 200 )

#Add dashed line segments to denote where DBL met ambient seawater
#wes_palette("Royal2")#"#9A8822", "#F5CDB4" aka brown, light pink
p+geom_segment(aes(x=65, xend=95,y=2.87,yend=2.87), linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=65, xend=95, y=2.43, yend=2.43),linetype="dashed",color= "#F5CDB4")+
  geom_segment(aes(x=105, xend=200, y=3.38, yend=3.38),linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=105, xend=200, y=3.21, yend=3.21),linetype="dashed",color="#F5CDB4")


#Interstitial space, flow= Low
p<-ggplot(bl, aes(x =    Mean,
                   y = Height,
                   color= Epi,
                   shape=LD
)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(95, 110)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Oxygen (%Ambient)")+
  labs(color = "Invert Presence", shape="Light/Dark")

p#check xlim (95, 110 )

#Add dashed line segments to denote where DBL met ambient seawater
#wes_palette("Royal2")#"#9A8822", "#F5CDB4" aka brown, light pink
p+geom_segment(aes(x=95, xend=98,y=.27,yend=.27), linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=95, xend=98, y=0.42, yend=0.42),linetype="dashed",color= "#F5CDB4")+
  geom_segment(aes(x=102, xend=110, y=.9, yend=.9),linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=102, xend=110, y=.82, yend=.82),linetype="dashed",color="#F5CDB4")

#Interstitial space, flow= High
p<-ggplot(bh, aes(x =    Mean,
                   y = Height,
                   color= Epi,
                   shape=LD
)) +
  geom_errorbarh(aes(xmin=Mean-se,
                     xmax=Mean+se),
                 size=0.7) +
  geom_point(size=2) +
  scale_color_manual(values = wes_palette(n=2, "Royal2")) + 
  xlim(98, 104)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title = element_text(face = "bold")) +
  ylab("Distance from Algal Surface (mm)")+
  xlab("Oxygen (%Ambient)")+
  labs(color = "Invert Presence", shape="Light/Dark")

p#check xlim (98, 104)

#Add dashed line segments to denote where DBL met ambient seawater
#wes_palette("Royal2")#"#9A8822", "#F5CDB4" aka brown, light pink
p+geom_segment(aes(x=98, xend=99.5,y=.062,yend=.062), linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=98, xend=99.5, y=.056, yend=.056),linetype="dashed",color= "#F5CDB4")+
  geom_segment(aes(x=100.5, xend=104, y=.081, yend=.081),linetype="dashed",color="#9A8822")+
  geom_segment(aes(x=100.5, xend=104, y=.081, yend=.081),linetype="dashed",color="#F5CDB4")

