
## At the moment, all other code should be run; currently figuring out how to best organize code so that there are the fewest number of concise files.

### MAIN FIGURES


## FIGURE 1 - Nitrogen: Focal species cover, C:N, habitat volume, orthoptera abundance
plotBL$N <- factor(plotBL$N,levels = c("Nitrogen", "Control"))
plotBL$P <- factor(plotBL$P,levels = c("Phosphorous", "Control"))

coverBLN <- ggplot(plotBL, aes(x=Species, y= total, fill = N))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius")))+
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), 
                    labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic() + ylab("% Cover")+ labs(fill = "") + ylim(0, 50) + 
  theme(legend.key.size = unit(0.5, 'cm'), axis.title.x = element_blank(),legend.text= element_text(size=7)) +
  theme(legend.position = c(0.75, 0.9), legend.title = element_blank()) + annotate("text", x=0.55, y=48.5, label= "A", fontface = "bold", size = 6)

cnratios$N <- factor(cnratios$N,levels = c("Nitrogen", "Control"))
cnratios$P <- factor(cnratios$P,levels = c("Phosphorous", "Control"))

ratioN <- ggplot(cnratios, aes(x=Species, y= ratio, fill = N))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius")))+
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), 
                    labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic() + ylab("C:N") + ylim(10,60) + 
  theme(axis.title.x = element_blank())+
  theme(legend.position = "none") + annotate("text", x=0.55, y=58, label= "B", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(49, 23), xmin = c(0.7, 1.7), xmax = c(1.3, 2.3),
    annotation = c("*", "*"), tip_length = .03, col = "black")

structureplot <- structurez %>%
  filter(Region != "StrArea_H1")%>%
  filter(Region != "StrArea_H2")%>%
  filter(Region != "StrArea_H3")%>%
  filter(Region != "StrArea_H4")%>%
  filter(Region != "StrArea_H5")

structureplot$N <- factor(structureplot$N,levels = c("Nitrogen", "Control"))
structureplot$P <- factor(structureplot$P,levels = c("Phosphorous", "Control"))
structureplot$Region <- factor(structureplot$Region,levels = c("StrTotal", "bottom", "top"))

strplotNN <- ggplot(structureplot, aes(x=Region, y= meanarea, fill = N))+
  geom_boxplot() + scale_x_discrete(labels=(c("Total", "0 - 23.71 cm", "23.72 - 71.12 cm")))+
  scale_fill_manual(values=c("salmon1", "darkolivegreen4")) + 
  theme_classic() + ylab("Habitat Volume")+ xlab("Height Class") + labs(fill = "") + ylim(0,1700) + 
  theme(axis.text.x= element_text(angle = 20, vjust = 1, hjust=1)) +
  theme(legend.position = "none") +
  annotate("text", x=0.65, y=1680, label= "C", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(1555, 1160), xmin = c(0.7, 1.7), xmax = c(1.3, 2.3),
    annotation = c("***", "***"), tip_length = .03, col = "black") + theme(legend.position = "none")

RinZPlotN <- ggplot(ringsseason, aes(x=N, y= plotR, fill = N))+
  geom_boxplot() + scale_x_discrete(labels=(c("Nitrogen", "No Nitrogen")))+
  scale_fill_manual(values=c("salmon1", "darkolivegreen4")) + 
  theme_classic() + ylab("Orthoptera Abundance (#/m^2)") + xlab("Treatment") + labs(fill = "") + ylim(0,3) + 
  theme(legend.position = "none", axis.text.x= element_text(angle = 27, vjust = 1, hjust=1)) +
  annotate("text", x=0.55, y=2.95, label= "D", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(2.6), xmin = c(0.85), xmax = c(2.15),
    annotation = c("*"), tip_length = .03, col = "black") + theme(legend.position = "none")

Nitrogendirect <- ggarrange(coverBLN, ratioN, ncol = 2, nrow = 1)
Ndirect <- annotate_figure(Nitrogendirect, bottom = text_grob("Focal Species", size = 11))
Nitrogendother <- ggarrange(strplotNN, RinZPlotN, ncol = 2, nrow = 1)
Nblahhh <- ggarrange(Ndirect, Nitrogendother, ncol = 1, nrow = 2)
ggsave("Figure1Thesis.pdf", height = 6, width = 6)





## FIGURE 2 - Nitrogen: plot-level LTR and LTR per capita
LTRdone$N <- factor(LTRdone$N,levels = c("Nitrogen", "Control"))
LTRdone$P <- factor(LTRdone$P,levels = c("Phosphorous", "Control"))

TotalLDN <- ggplot(LTRdone, aes(x=Species, y= plotLTR, fill = N))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius"))) +
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), 
                    labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic() + ylab("LTR (%)")+ labs(fill = "") + ylim(0, 12.5) + 
  theme(axis.title.x = element_blank(), legend.text= element_text(size=7)) +
  theme(legend.position = c(0.25, 0.65), legend.title = element_blank()) + annotate("text", x=0.55, y=12.1, label= "A", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(3.3, 12), xmin = c(0.7, 1.7), xmax = c(1.3, 2.3),
    annotation = c("*", "*"), tip_length = .03, col = "black")

PercLDN <- ggplot(LTRdone, aes(x=Species, y= LTRper, fill = N))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius"))) +
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), 
                    labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic() + ylab("LTR per capita (%)")+ xlab("Species") + labs(fill = "") + ylim(0, 0.46) + 
  theme(legend.text= element_text(size=7)) +
  theme(legend.position = "none") + annotate("text", x=0.55, y=0.45, label= "B", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(0.425), xmin = c(1.7), xmax = c(2.3),
    annotation = c("*"), tip_length = .03, col = "black")

nitld <- ggarrange(TotalLDN, PercLDN, ncol=1, nrow=2)
ggsave("Figure2Thesis.pdf", height = 6, width = 3)





## FIGURE 3 - Phosphorus: Focal species cover, habitat volume, orthoptera abundance
coverBLP <- ggplot(plotBL, aes(x=Species, y= total, fill = P))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius")))+
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen"), 
                    labels= c("Phosphorus", "No Phosphorus")) + 
  theme_classic() + ylab("% Cover")+ labs(fill = "") + ylim(0, 35) + 
  theme(axis.text.x= element_text(angle = 22, vjust = 1, hjust=1)) +
  theme(legend.position = "none") + annotate("text", x=0.55, y=35, label= "A", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(23), xmin = c(0.7), xmax = c(1.3),
    annotation = c("***"), tip_length = .03, col = "black")

structureplot$N <- factor(structureplot$N,levels = c("Nitrogen", "Control"))
structureplot$P <- factor(structureplot$P,levels = c("Phosphorous", "Control"))
structureplot$Region <- factor(structureplot$Region,levels = c("StrTotal", "bottom", "top"))

strplotPP <- ggplot(structureplot, aes(x=Region, y= meanarea, fill = P))+
  geom_boxplot() + scale_x_discrete(labels=(c("Total", "0 - 23.71 cm", "23.72 - 71.12 cm")))+
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen")) + 
  theme_classic() + ylab("Habitat Volume")+ xlab("Height Class") + labs(fill = "") + ylim(0,1700) + 
  theme(axis.text.x= element_text(angle = 14, vjust = 1, hjust=1)) +
  theme(legend.key.size = unit(0.5, 'cm'), legend.text= element_text(size=7), legend.position = c(0.7, 0.9), legend.title = element_blank()) +
  annotate("text", x=0.65, y=1680, label= "B", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(1150), xmin = c(1.7), xmax = c(2.3),
    annotation = c("**"), tip_length = .03, col = "black")

RinZPlotP <- ggplot(ringsseason, aes(x=P, y= plotR, fill = P))+
  geom_boxplot() + scale_x_discrete(labels=(c("Phosphorus", "No Phosphorus")))+
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen")) + 
  theme_classic() + ylab("Orthoptera Abundance (#/m^2)") + xlab("Treatment") + labs(fill = "") + ylim(0, 2.5) + theme(legend.position = "none", axis.text.x= element_text(angle = 16, vjust = 1, hjust=1)) +
  annotate("text", x=0.55, y=2.45, label= "C", fontface = "bold", size = 6) 


Pblahhh <- ggarrange(coverBLP, strplotPP, RinZPlotP, ncol = 3, nrow = 1)
ggsave("Figure3Thesis.pdf", height = 3, width = 7)





## FIGURE 4 - Phosphorus: plot-level LTR and LTR per capita
TotalLDP <- ggplot(LTRdone, aes(x=Species, y= plotLTR, fill = P))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius"))) +
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen"), 
                    labels= c("Phosphorus", "No Phosphorus")) + 
  theme_classic() + ylab("LTR (%)")+ labs(fill = "") +ylim(0, 13) +
  theme(axis.title.x = element_blank(), legend.text= element_text(size=7)) + 
  theme(legend.position = c(0.28, 0.65), legend.title = element_blank()) + annotate("text", x=0.55, y=12.5, label= "A", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(3.25), xmin = c(0.7), xmax = c( 1.3),
    annotation = c("**"), tip_length = .03, col = "black")

PercLDP <- ggplot(LTRdone, aes(x=Species, y= LTRper, fill = P))+
  geom_boxplot() + scale_x_discrete(labels=expression(italic("B. carinatus"), italic("L. latifolius"))) +
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen"), 
                    labels= c("Phosphorus", "No Phosphorus")) + 
  theme_classic() + ylab("LTR per capita (%)")+ xlab("Species")+ labs(fill = "") +ylim(0, 0.46) +
  theme(legend.text= element_text(size=7)) + 
  theme(legend.position = "none") + annotate("text", x=0.55, y=0.45, label= "B", fontface = "bold", size = 6) +
  geom_signif(
    y_position = c(0.11), xmin = c(0.7), xmax = c( 1.3),
    annotation = c("***"), tip_length = .03, col = "black")

phosld <- ggarrange(TotalLDP, PercLDP, ncol=1, nrow=2)
ggsave("Figure4Thesis.pdf", height = 6, width = 3)






## FIGURE 5 - Phosphorus: Plant functional group cover and trophic group abundance
Pcov2021 <- ggplot(covertotals, aes(x=Group, y= plotC, fill = P))+
  geom_boxplot() + scale_x_discrete(labels =c("Grass" = "Grass", "Graminoid" = "Sedge", "Phlox" = "Phlox (Forb)", 
                                              "Forb" = "Other Forb", "Legume" = "Legume")) + 
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen"), labels= c("Phosphorus", "No Phosphorus")) + 
  theme_classic() + ylab("% Cover")+ xlab("Functional Group") + labs(fill = "") + theme(legend.position = "none") +
  theme(legend.text= element_text(size=8)) + 
  ylim(0,90)  + 
  theme(axis.text.x = element_text(angle = 30, hjust=1)) + geom_signif(
    y_position = c(68, 88, 53), xmin = c(0.7, 2.7, 3.7), xmax = c(1.3, 3.3, 4.3),
    annotation = c("***", "**", "*"), tip_length = .03) + annotate("text", x=0.8, y=90, label= "A", fontface = "bold", size = 6)

ggsave("Figure5AThesis.pdf", height = 4, width = 3)

inverts <- read_csv("invertSEMdata.csv")
Bugz <- inverts %>%
  pivot_longer(
    cols = c(8:12),
    names_to = "Guild",
    values_to = "Abundance"
  )

Bugz$N <- factor(Bugz$N,levels = c("Nitrogen", "Control"))
Bugz$P <- factor(Bugz$P,levels = c("Phosphorous", "Control"))
Bugz$Guild <- factor(Bugz$Guild,levels = c("SapH", "LeafH", "PollH", "Preds", "Pars"))

Pbugs <- ggplot(Bugz, aes(x=Guild, y= Abundance, fill = P))+
  geom_boxplot() + scale_x_discrete(labels= c("Suck (H)", "Chew (H)", "Pollen/Nectar (H)", "Predator", "Parasitoid")) +
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen"), 
                    labels= c("Phosphorus", "No Phosphorus")) + 
  theme_classic() + ylab("Abundance")+ xlab("Trophic Group") + labs(fill = "") + ylim(0, 415) + 
  theme(axis.text.x = element_text(angle = 20, hjust=1), legend.position = c(0.7, 0.7), legend.title = element_blank()) + 
  annotate("text", x=0.8, y=415, label= "B", fontface = "bold", size = 5)+ 
  geom_signif(
    y_position = c(135, 44), xmin = c(1.7, 4.7), xmax = c(2.3, 5.3),
    annotation = c("*", "*"), tip_length = .01, col = "black")

ggsave("Figure5BThesis.pdf", height = 4, width = 3)






## SUPPLEMENTAL


## FIGURE S3 - Nitrogen: Plant functional group cover and trophic group abundance
Ncov2021 <- ggplot(covertotals, aes(x=Group, y= plotC, fill = N))+
  geom_boxplot() + scale_x_discrete(labels =c("Grass" = "Grass", "Graminoid" = "Sedge", "Phlox" = "Phlox (Forb)", 
                                              "Forb" = "Other Forb", "Legume" = "Legume")) + 
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic()  + ylab("% Cover")+ xlab("Functional Group") + labs(fill = "") + 
  theme(legend.text= element_text(size=8))  + theme(legend.position = "none") + 
  ylim(0,90) + 
  theme(axis.text.x = element_text(angle = 30, hjust=1)) + geom_signif(
    y_position = c(68, 68, 55), xmin = c(0.7, 1.7, 3.7), xmax = c(1.3, 2.3, 4.3),
    annotation = c("**", "***", "**"), tip_length = .03) + annotate("text", x=0.8, y=90, label= "A", fontface = "bold", size = 5)

ggsave("FigureS3AThesis.pdf", height = 4, width = 3)

Nbugs <- ggplot(Bugz, aes(x=Guild, y= Abundance, fill = N))+
  geom_boxplot() + scale_x_discrete(labels= c("Suck (H)", "Chew (H)", "Pollen/Nectar (H)", "Predator", "Parasitoid")) +
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), 
                    labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic() + ylab("Abundance")+ xlab("Trophic Group") + labs(fill = "") + ylim(0, 415) + 
  theme(axis.text.x = element_text(angle = 20, hjust=1), legend.position = c(0.7, 0.7), legend.title = element_blank()) + 
  annotate("text", x=0.8, y=415, label= "B", fontface = "bold", size = 5) + 
  geom_signif(
    y_position = c(80), xmin = c(2.7), xmax = c(3.3),
    annotation = c("***"), tip_length = .01, col = "black")

ggsave("FigureS3BThesis.pdf", height = 4, width = 3)





## FIGURE S4 - Nitrogen: Plant species percent cover
pastey <- cover2021means
pastey$unID <- paste(pastey$Species, pastey$Group)

pastey$N <- factor(pastey$N,levels = c("Nitrogen", "Control"))
pastey$P <- factor(pastey$P,levels = c("Phosphorous", "Control"))

pasteyN <- ggplot(pastey, aes(x=N, y=means, fill = N))+
  geom_boxplot() + facet_wrap(~ unID, scales = "free") + 
  scale_fill_manual(values=c("salmon1", "darkolivegreen4"), labels= c("Nitrogen", "No Nitrogen")) + 
  theme_classic() + labs(fill = "") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),legend.position = "none", axis.text.x = element_blank())

annotate_figure(pasteyN, left = text_grob("% Cover", rot = 90, vjust = 1, size = 18),
                bottom = text_grob("Treatment", size = 18))
ggsave("FigureS4Thesis.pdf", height = 8, width = 12)





## FIGURE S5 - Both nutrients: Long term change
#Nitrogen
ForbLTN <- bunchgrassall %>%
  filter(functional_group == "FORB") %>%
  group_by(year_trt, N)%>%
  summarize(mean= mean(means), SE = calcSE(means))

ForbLTNLINE <- ggplot(ForbLTN, aes(x=year_trt, y= mean, color = N))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = N))+ 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Nitrogen", "No Nitrogen"), values=c("salmon1", "darkolivegreen4"))+ 
  scale_fill_manual(values=c("lightsalmon", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())  +
  ylim(0, 60) + ggtitle("OTHER FORB") + annotate("text", x=0.5, y=56, label= "G", fontface = "bold", size = 6)


GrassLTN <- bunchgrassall %>%
  filter(functional_group == "GRASS") %>%
  group_by(year_trt, N)%>%
  summarize(mean= mean(means), SE = calcSE(means))

GrassLTNLINE <- ggplot(GrassLTN, aes(x=year_trt, y= mean, color = N)) +
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = N))+ theme_classic() + 
  ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Nitrogen", "No Nitrogen"), values=c("salmon1", "darkolivegreen4"))+ 
  scale_fill_manual(values=c("lightsalmon", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2)  + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  ylim(15, 90) + ggtitle("GRASS") + annotate("text", x=0.5, y=86, label= "A", fontface = "bold", size = 6)


LegumeLTN <- bunchgrassall %>%
  filter(functional_group == "LEGUME") %>%
  group_by(year_trt, N)%>%
  summarize(mean= mean(means), SE = calcSE(means))

LegumeLTNLINE <- ggplot(LegumeLTN, aes(x=year_trt, y= mean, color = N))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = N))+ 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Nitrogen", "No Nitrogen"), values=c("salmon1", "darkolivegreen4"))+ 
  scale_fill_manual(values=c("lightsalmon", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  ylim(0, 40) + ggtitle("LEGUME") + annotate("text", x=0.5, y=35, label= "I", fontface = "bold", size = 6)


GraminoidLTN <- bunchgrassall %>%
  filter(functional_group == "GRAMINOID") %>%
  group_by(year_trt, N)%>%
  summarize(mean= mean(means), SE = calcSE(means))

GraminoidLTNLINE <- ggplot(GraminoidLTN, aes(x=year_trt, y= mean, color = N))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = N))+ 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Nitrogen", "No Nitrogen"), values=c("salmon1", "darkolivegreen4"))+ 
  scale_fill_manual(values=c("lightsalmon", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  ylim(15, 70) + ggtitle("SEDGE") + annotate("text", x=0.5, y=66, label= "C", fontface = "bold", size = 6)


PHLOXTN <- bunchgrassall %>%
  filter(functional_group == "PHLOX") %>%
  group_by(year_trt, N)%>%
  summarize(mean= mean(means), SE = calcSE(means))

PHLOXLTNLINE <- ggplot(PHLOXTN, aes(x=year_trt, y= mean, color = N))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = N))+ 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Nitrogen", "No Nitrogen"), values=c("salmon1", "darkolivegreen4"))+ 
  scale_fill_manual(values=c("lightsalmon", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  ylim(0, 80) + ggtitle("PHLOX (FORB)") + annotate("text", x=0.4, y=77, label= "E", fontface = "bold", size = 6)


## Phosphorous
ForbLTP <- bunchgrassall %>%
  filter(functional_group == "FORB") %>%
  group_by(year_trt, P)%>%
  summarize(mean= mean(means), SE = calcSE(means))

ForbLTPLINE <- ggplot(ForbLTP, aes(x=year_trt, y= mean, color = P))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = P)) + 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Phosphorous", "No Phosphorous"), values=c("lightskyblue3", "darkolivegreen"))+ 
  scale_fill_manual(values=c("lightskyblue1", "darkseagreen3")) + 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank())+
  ylim(0, 60) + annotate("text", x=0.5, y=56, label= "H", fontface = "bold", size = 6) + ggtitle("") 


GrassLTP <- bunchgrassall %>%
  filter(functional_group == "GRASS") %>%
  group_by(year_trt, P)%>%
  summarize(mean= mean(means), SE = calcSE(means))

GrassLTPLINE <- ggplot(GrassLTP, aes(x=year_trt, y= mean, color = P)) +
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = P))+ 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Phosphorous", "No Phosphorous"), values=c("lightskyblue3", "darkolivegreen")) + 
  scale_fill_manual(values=c("lightskyblue1", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2)  + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  ylim(15, 90) + annotate("text", x=0.5, y=86, label= "B", fontface = "bold", size = 6) + ggtitle("") 


LegumeLTP <- bunchgrassall %>%
  filter(functional_group == "LEGUME") %>%
  group_by(year_trt, P)%>%
  summarize(mean= mean(means), SE = calcSE(means))

LegumeLTPLINE <- ggplot(LegumeLTP, aes(x=year_trt, y= mean, color = P))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = P))+ 
  theme_classic() + ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Phosphorous", "No Phosphorous"), values=c("lightskyblue3", "darkolivegreen"))+ 
  scale_fill_manual(values=c("lightskyblue1", "darkseagreen3"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12))+ 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  ylim(0, 40) + annotate("text", x=0.5, y=35, label= "J", fontface = "bold", size = 6) + ggtitle("") 


GraminoidLTP <- bunchgrassall %>%
  filter(functional_group == "GRAMINOID") %>%
  group_by(year_trt, P)%>%
  summarize(mean= mean(means), SE = calcSE(means))

GraminoidLTPLINE <- ggplot(GraminoidLTP, aes(x=year_trt, y= mean, color = P))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = P))+ theme_classic() + 
  ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Phosphorous", "No Phosphorous"), values=c("lightskyblue3", "darkolivegreen"))+ 
  scale_fill_manual(values=c("lightskyblue1", "darkseagreen3")) + 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  ylim(15, 70) + annotate("text", x=0.5, y=66, label= "D", fontface = "bold", size = 6) + ggtitle("") 


PHLOXLTP <- bunchgrassall %>%
  filter(functional_group == "PHLOX") %>%
  group_by(year_trt, P)%>%
  summarize(mean= mean(means), SE = calcSE(means))

PHLOXLTPLINE <- ggplot(PHLOXLTP, aes(x=year_trt, y= mean, color = P))+
  geom_line()+ geom_smooth(method = lm, se = 0.85, aes(fill = P))+ theme_classic() + 
  ylab("Cover") + xlab("Years since treatment start") + 
  scale_color_manual(name= "Treatment", labels=c("Phosphorus", "No Phosphorus"), values=c("lightskyblue3", "darkolivegreen"))+ 
  scale_fill_manual(values=c("lightskyblue1", "darkseagreen3")) + 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank()) + 
  ylim(0, 80) + annotate("text", x=0.4, y=77, label= "F", fontface = "bold", size = 6) + ggtitle("") 

P_legend <- get_legend(PHLOXLTNLINE)
N_legend <- get_legend(PHLOXLTPLINE)


Nplot <- ggarrange(GrassLTNLINE, GrassLTPLINE, GraminoidLTNLINE, GraminoidLTPLINE, PHLOXLTNLINE, PHLOXLTPLINE, ForbLTNLINE, ForbLTPLINE, LegumeLTNLINE, LegumeLTPLINE, ncol=2, nrow=5, legend.grob = N_legend, legend = "right")
annotate_figure(Nplot, left = text_grob("% Cover", rot = 90, vjust = 1, size = 14, face = "bold"),
                bottom = text_grob("Years Since Treatment Start", size = 14, face = "bold"))
ggsave("FigureS5Thesis.pdf", height = 7.5, width = 6)





## FIGURE S6 - Phosphorus: Plant species percent cover
pasteyP <- ggplot(pastey, aes(x=P, y=means, fill = P))+
  geom_boxplot() + facet_wrap(~ unID, scales = "free") + 
  scale_fill_manual(values=c("lightskyblue3", "darkolivegreen"), labels= c("Phosphorus", "No Phosphorus")) + 
  theme_classic() + labs(fill = "") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),legend.position = "none", axis.text.x = element_blank())

annotate_figure(pasteyP, left = text_grob("% Cover", rot = 90, vjust = 1, size = 18),
                bottom = text_grob("Treatment", size = 18))
ggsave("FigureS6Thesis.pdf", height = 8, width = 12)

