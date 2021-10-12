#masters drought paper


library(ggplot2)
library(readr)
library(patchwork)

Height <- read_csv("C:/Users/etelford.IC.003/Dropbox/Masters/Masters/Height.csv")
Height$IndividualID <- as.factor(paste(as.character(Height$Tag),as.character(Height$Pot),sep="_")) 
Height$Treatment <- factor(Height$Treatment, levels=c('4%','8%','16%'))
Height$Species <- factor(Height$Species, levels=c('VE','VS'))
H3<-subset(Height, Harvest == "3")
VS<-subset(H3, Species=="VS")
VE<-subset(H3, Species=="VE")

# New facet label names for supp variable
supp.labs <- c("V. erioloba", "V. sieberiana")
names(supp.labs) <- c("VE", "VS")



P2_height <- ggplot(data = Height,
                       aes(x = Week, 
                           y = Height,
                           color = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_minimal(base_size = 17) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + facet_grid(.~ Species, labeller = labeller(Species = supp.labs))+
  labs(x = "Week", y = "Height (mm)")

P2_height+ scale_x_continuous(breaks = seq(1,15 , by = 2))

masters_LabHarvest <- read_csv("C:/Users/etelford.IC.003/Dropbox/Masters/Masters/masters_LabHarvest.csv")
LabHarvest <- masters_LabHarvest

LabHarvest$Treatment <- factor(LabHarvest$Treatment, levels=c('4%','8%','16%'))
LabHarvest$Species<-factor(LabHarvest$Species, levels=c('VE','VS'))
LabHarvest$Harvest<-as.factor(LabHarvest$Harvest)
LabHarvest$Tag<-as.factor(LabHarvest$Tag)
H3<-subset(LabHarvest, Harvest=="3")
VS_H3<-subset(H3, Species=="VS")
LabHarvest_VS<-subset(LabHarvest, Species=="VS")
LabHarvest_VE<-subset(LabHarvest, Species=="VE")


P3_below <- ggplot(data = H3,
                    aes(x = Treatment, 
                        y = BG_DW,
                        fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))  +
  guides(fill = "none")+  facet_grid(.~ Species, labeller = labeller(Species = supp.labs)) +
  labs(x = "Treatment", y = "Below biomass (g)")
P3_below

P3_above <- ggplot(data = H3,
                   aes(x = Treatment, 
                       y = AG_DW,
                       fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))  +
  facet_grid(.~ Species, labeller = labeller(Species = supp.labs)) +
  labs(x = "Treatment", y = "Above biomass (g)")
P3_above

P3_r_s <- ggplot(data = H3,
                   aes(x = Treatment, 
                       y = R_S,
                       fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))  +
  guides(fill = "none")+  facet_grid(.~ Species, labeller = labeller(Species = supp.labs)) +
  labs(x = "Treatment", y = "Above: Below")

patchwork <- P3_below + P3_above + P3_r_s+ guide_area() + plot_layout(guides = "collect")
patchwork + plot_annotation(tag_levels = 'a')

#nodules Figure 4

P4_nod_1 <- ggplot(data = LabHarvest_VS,
                   aes(x = Harvest, 
                       y = Nodules_W,
                       fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 17) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))   +
  labs(x = "Harvest", y = "Nodule biomass (g)") +
  facet_grid(.~ Treatment)



P4_nod_2 <- ggplot(data = LabHarvest_VS,
                    aes(x = Nodules_W, 
                        y = BG_DW ,
                        color = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_minimal(base_size = 17) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Nodule biomass (g)", y = "Below biomass (g)")
  
patchwork <- P4_nod_1 + P4_nod_2+ guide_area() + plot_layout(guides = "collect")
patchwork + plot_annotation(tag_levels = 'a')
