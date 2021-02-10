# Plot and merge probability distributions with frequency and boxplots with geom density 
# for WatchMore (MPN), WatchMoreSprayLess (MPR), Reduce (R) and Normal (N)

# Adjusting font styles while using font_import()
# loadfonts(device"win") for windows
extrafont::font_import()
y
extrafont::loadfonts(device="win")
extrafont::fonts()

windowsFonts(Times=windowsFont("TT Times New Roman"))


data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_plot_MPN<-dplyr::select(data, starts_with("cashflow_MPN")) %>%
  stack(drop=FALSE)
data_plot_MPN$values <- as.numeric(data_plot_MPN$values)
plot_MPN <- ggplot(data_plot_MPN, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("Watch"), values = ("blue3") , guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "blue3" ) +
  geom_abline(intercept = 0) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-240000, 340000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -150000, y = 0.75, label = 'atop(bold("Watch"))', size = 12, parse = TRUE, family="Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x=element_text(color = "white", margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white",margin = unit(c(0, 1, 0, 0), "mm")),
        axis.text.x=element_text(color = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32),
        axis.text = element_text(size=32),
        axis.title = element_text(size=32,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Frequency") 

data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_plot_MPR<-dplyr::select(data, starts_with("cashflow_MPR")) %>%
  stack(drop=FALSE)
data_plot_MPR$values <- as.numeric(data_plot_MPR$values) 
plot_MPR <- ggplot(data_plot_MPR, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("WatchReduce"), values = c("green3"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "green3" ) +
  geom_abline(intercept = 0) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-240000, 340000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -150000, y = 0.75, label = 'atop(bold("WatchReduce"))', size = 12, parse = TRUE, family = "Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x=element_text(color = "white", margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white", margin = unit(c(0, 1, 0, 0), "mm")),
        axis.text.x=element_text(color = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32),
        axis.text = element_text(size=32),
        axis.title = element_text(size=32,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Frequency") 

data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_plot_R<-dplyr::select(data, starts_with("cashflow_R")) %>%
  stack(drop=FALSE)
data_plot_R$values <- as.numeric(data_plot_R$values) 
plot_R <- ggplot(data_plot_R, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("Reduce"), values = ("red3"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "red3" ) +
  geom_abline(intercept = 0) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-240000, 340000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -150000, y = 0.75, label = 'atop(bold("Reduce"))', size = 12, parse = TRUE, family = "Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x = element_text(color = "white", margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white", margin = unit(c(0, 1, 0, 0), "mm")),
        axis.text.x=element_text(color = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32),
        axis.text = element_text(size=32),
        axis.title = element_text(size=32,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Frequency") 

data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_plot_N<-dplyr::select(data, starts_with("cashflow_N")) %>%
  stack(drop=FALSE)
data_plot_N$values <- as.numeric(data_plot_N$values) 
plot_N <- ggplot(data_plot_N, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("Normal"), values = ("magenta3"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "magenta3" ) +
  geom_abline(intercept = 0) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-240000, 340000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -150000, y = 0.75, label = 'atop(bold("Normal"))', size = 12, parse = TRUE, family = "Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x=element_text(margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white", margin = unit(c(0, 1, 0, 0), "mm")),
        legend.title = element_blank(),
        axis.ticks.x=element_line(),
        legend.position = "none",
        legend.text = element_text(size=32),
        axis.text = element_text(size=32),
        axis.title = element_text(size=32,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Frequency") 

#### Merge probability distribution plots of Watch, WatchReduce, Reduce and Normal ####
dir.create("./Probability_distributions")

merge_plot_of_geom_density_4 <- ggarrange(plot_MPN, plot_MPR, plot_R, plot_N, ncol = 1, nrow = 4)

merge_plot_of_geom_density_4 <- annotate_figure(merge_plot_of_geom_density_4,
                                                left = text_grob("Scaled density", color = "black", rot = 90,
                                                                 face = "bold", size =32, family = "Times"))

ggsave("./Probability_distributions/merge_plot_of_geom_density_4.png", merge_plot_of_geom_density_4, dpi = 800,  device = "png", width = 12, height = 15)

#### Plot and merge probability distributions with frequency and boxplots for WatchMore, WatchMoreSprayLess and SprayLess ####

data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_WatchMore<-dplyr::select(data, starts_with("comp_MPN_N")) %>%
  stack(drop=FALSE)
data_WatchMore$values <- as.numeric(data_WatchMore$values) 
plot_WatchMore <- ggplot(data_WatchMore, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("WatchMore"), values = ("blue4"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "blue4" ) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-204000, 120000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -120000, y = 0.75, label = 'atop(bold("WatchMore"))', size = 12, parse = TRUE, family = "Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x=element_text(color = "white",margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white", margin = unit(c(0, 1, 0, 0), "mm")),
        axis.text.x=element_text(color = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=34),
        axis.text = element_text(size=34),
        axis.title = element_text(size=34,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Fre") 


data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_WatchMoreSprayLess<-dplyr::select(data, starts_with("comp_MPR_N")) %>%
  stack(drop=FALSE)
data_WatchMoreSprayLess$values <- as.numeric(data_WatchMoreSprayLess$values) 
plot_WatchMoreSprayLess <- ggplot(data_WatchMoreSprayLess, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("WatchMoreSprayLess"), values = ("green4"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "green4" ) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-204000, 120000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -120000, y = 0.75, label = 'atop(bold("WatchMoreSprayLess"))', size = 12, parse = TRUE, family = "Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x=element_text(color = "white", margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white", margin = unit(c(0, 1, 0, 0), "mm")),
        axis.text.x=element_text(color = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=34),
        axis.text = element_text(size=34),
        axis.title = element_text(size=34,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Fre") 


data<-read.csv("Results_low_prophy/mcSimulationResults.csv")
data_SprayLess<-dplyr::select(data, starts_with("comp_R_N")) %>%
  stack(drop=FALSE)
data_SprayLess$values <- as.numeric(data_SprayLess$values) 
plot_SprayLess <- ggplot(data_SprayLess, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("SprayLess"), values = ("red4"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.1), width = 0.1, fill = "red4" ) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""), limits = c(-204000, 120000)) +
  ylim(breaks = c(0,1)) +
  annotate("text", x = -120000, y = 0.75, label = 'atop(bold("SprayLess"))', size = 12, parse = TRUE, family = "Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title.x=element_text(margin = unit(c(9, 0, 2, 0), "mm")),
        axis.title.y = element_text(color = "white", margin = unit(c(0, 1, 0, 0), "mm")),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=34),
        axis.text = element_text(size=34),
        axis.title = element_text(size=34,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  xlab("Net benefit (Partial Farm Budget; Euros)")+ 
  ylab("Fre")

#### Merge probability distribution plots of WatchMore, WatchMoreSprayLess and SprayLess ####

merge_plot_of_geom_density_3 <- ggarrange(plot_WatchMore, plot_WatchMoreSprayLess, plot_SprayLess, ncol = 1, nrow = 3)

merge_plot_of_geom_density_3 <- annotate_figure(merge_plot_of_geom_density_3,
                                                left = text_grob("Scaled density", color = "black", rot = 90,
                                                                 face = "bold", size =32, family = "Times"))

ggsave("./Probability_distributions/merge_plot_of_geom_density_3.png", merge_plot_of_geom_density_3, dpi = 800, device = "png", width = 12, height = 15)

#### Create and merge horizontal barplot for EVPI and VIP of SprayLess ####  

EVPI_data_R_N<-read.csv("EVPI_tables_low_prophy/EVPI_table_comp_R_N.csv", header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$EVPI_dont),] %>%
  subset(.,EVPI_dont>0)
# head(.,10)

colnames(EVPI_data_R_N)[colnames(EVPI_data_R_N)=="EVPI_dont"] <- "EVPI"
colnames(EVPI_data_R_N)[colnames(EVPI_data_R_N)=="variable"] <- "Variable"
EVPI_data_R_N$Variable <- as.character(EVPI_data_R_N$Variable) 
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "infection_risk"] <- "Infection risk"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "number_yearly_prophy_application_R"] <- "Reduce: Preventive applications"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "fungus_probability_R"] <- "Reduce: Fungus occurrence"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "value_of_saleable_Calluna"] <- "Sellable heather"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "threshold_big_area_more_staff"] <- "Threshold more staff"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "detection_factor_R"] <- "Reduce: Detection rate"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "detection_factor_MP"] <- "Watch: Detection rate"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "detection_factor_N"] <- "Normal: Detection rate"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "number_yearly_prophy_application_N"] <- "Normal: Preventive applications"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "fungus_fight_effect_N"] <- "Normal: Effect of applications"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "fungus_fight_effect_R"] <- "Reduce: Effect of applications"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "fungus_fight_effect_MP"] <- "Watch: Effect of applications"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "price_premium_sustainable"] <- "Reduce: More sustainable Calluna"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "savings_due_to_MP"] <- "Savings monitoring plan"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "costs_monitoring_plan_per_ha"] <- "Watch: Monitoring costs"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "amount_of_samples_MP"] <- "Watch: Number of samples"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "chance_higher_price_sustainable"] <- "Reduce: Chance of for higher price"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "costs_more_staff"] <- "Costs for more staff"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "costs_normal_fertilizer"] <- "Normal: Fertilizer costs"
EVPI_data_R_N$Variable[EVPI_data_R_N$Variable == "costs_monitoring_per_ha_month"] <- "Normal: Monitoring costs"

EVPI_data_R_N <- EVPI_data_R_N[order(EVPI_data_R_N$EVPI), ] 
EVPI_data_R_N$Variable <- factor(EVPI_data_R_N$Variable, levels = EVPI_data_R_N$Variable)
EVPI_data_R_N$EVPI <- round(EVPI_data_R_N$EVPI, digits= 2) %>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

EVPI_barplot_R_N <- ggplot(EVPI_data_R_N, aes(Variable, EVPI, fill = EVPI, label = EVPI)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  #  geom_text(aes(y=max(EVPI+(y=max(EVPI/5))), label=sprintf("%0.2f", round(EVPI, digits = 2))), size = 4.8, color="black", hjust = 1)+ #or max as EVPI
  ggtitle("SprayLess: Information Value" , subtitle = "Result probably negative") +
  #  geom_hline(yintercept = max(EVPI_data_R_N$EVPI)) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  scale_fill_gradient(low = "ivory2", high = "purple") +
  scale_y_continuous(limits = c(0, 2500)) +
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(color = "red", size = 15, face = "bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) 

#ggsave("EVPI_barplot_R_N.png", device = "png",  width = 20, height = 20, units = "cm")

VIP_table_comp_R_N <- read.csv("./Results_low_prophy/comp_R_N_pls_results.csv",
                               header = TRUE) %>%
  
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8) 
#  subset(.,VIP>0.8)

colnames(VIP_table_comp_R_N)[colnames(VIP_table_comp_R_N)=="X"] <- "Variable"

VIP_table_comp_R_N$Variable <- as.character(VIP_table_comp_R_N$Variable) 
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "infection_risk"] <- "Infection risk"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "number_yearly_prophy_application_R"] <- "Reduce: Preventive applications"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "fungus_probability_R"] <- "Reduce: Fungus occurrence"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "value_of_saleable_Calluna"] <- "Sellable heather"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "threshold_big_area_more_staff"] <- "Threshold more staff"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "detection_factor_R"] <- "Reduce: Detection rate"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "detection_factor_MP"] <- "Watch: Detection rate"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "detection_factor_N"] <- "Normal: Detection rate"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "number_yearly_prophy_application_N"] <- "Normal: Preventive applications"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "fungus_fight_effect_N"] <- "Normal: Effect of applications"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "fungus_fight_effect_R"] <- "Reduce: Effect of applications"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "fungus_fight_effect_MP"] <- "Watch: Effect of applications"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "price_premium_sustainable"] <- "Reduce: More sustainable Calluna"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "savings_due_to_MP"] <- "Savings monitoring plan"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "costs_monitoring_plan_per_ha"] <- "Watch: Monitoring costs"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "amount_of_samples_MP"] <- "Watch: Number of samples"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "chance_higher_price_sustainable"] <- "Reduce: Chance of for higher price"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "costs_more_staff"] <- "Costs for more staff"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "costs_normal_fertilizer"] <- "Normal: Fertilizer costs"
VIP_table_comp_R_N$Variable[VIP_table_comp_R_N$Variable == "costs_monitoring_per_ha_month"] <- "Normal: Monitoring costs"

VIP_table_comp_R_N <- VIP_table_comp_R_N[order(VIP_table_comp_R_N$VIP), ]  # sort
VIP_table_comp_R_N$Variable <- factor(VIP_table_comp_R_N$Variable, levels = VIP_table_comp_R_N$Variable)
VIP_table_comp_R_N$VIP <- round(VIP_table_comp_R_N$VIP, digits= 2)%>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

VIP_table_comp_R_N$color <- ifelse(VIP_table_comp_R_N$Coefficient>0, "forestgreen", "firebrick3")
VIP_table_comp_R_N$color <- as.character(VIP_table_comp_R_N$color)

VIP_barplot_comp_R_N <- ggplot(VIP_table_comp_R_N, aes(Variable, VIP, label = VIP, fill = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_table_comp_R_N$color) +
  #  geom_text(aes(y=max(VIP+(y=max(VIP/5))), label=sprintf("%0.2f", round(VIP, digits = 2))), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle("Subtitle", subtitle = "SprayLess: Variable Importance") +
  #  geom_hline(yintercept = max(VIP_table_comp_R_N$VIP)) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  scale_y_continuous(limits = c(0, 5)) +
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(color = "black", size = 15, face = "bold"),
        plot.title = element_text(color = "white", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())
#ggsave("VIP_barplot_comp_R_N.png", device = "png",  width = 20, height = 20, units = "cm")


#### Create and merge horizontal barplot for EVPI and VIP of WatchMore #### 

EVPI_data_MPN_N<-read.csv("EVPI_tables_low_prophy/EVPI_table_comp_MPN_N.csv", header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$EVPI_do),] %>%
  subset(.,EVPI_do>0)
# head(.,10)

colnames(EVPI_data_MPN_N)[colnames(EVPI_data_MPN_N)=="variable"] <- "Variable"
colnames(EVPI_data_MPN_N)[colnames(EVPI_data_MPN_N)=="EVPI_do"] <- "EVPI"

EVPI_data_MPN_N$Variable <- as.character(EVPI_data_MPN_N$Variable) 
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "infection_risk"] <- "Infection risk"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "number_yearly_prophy_application_R"] <- "Reduce: Preventive applications"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "fungus_probability_R"] <- "Reduce: Fungus occurrence"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "value_of_saleable_Calluna"] <- "Sellable heather"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "threshold_big_area_more_staff"] <- "Threshold more staff"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "detection_factor_R"] <- "Reduce: Detection rate"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "detection_factor_MP"] <- "Watch: Detection rate"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "detection_factor_N"] <- "Normal: Detection rate"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "number_yearly_prophy_application_N"] <- "Normal: Preventive applications"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "fungus_fight_effect_N"] <- "Normal: Effect of applications"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "fungus_fight_effect_R"] <- "Reduce: Effect of applications"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "fungus_fight_effect_MP"] <- "Watch: Effect of applications"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "price_premium_sustainable"] <- "Reduce: More sustainable Calluna"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "savings_due_to_MP"] <- "Savings monitoring plan"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "costs_monitoring_plan_per_ha"] <- "Watch: Monitoring costs"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "amount_of_samples_MP"] <- "Watch: Number of samples"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "chance_higher_price_sustainable"] <- "Reduce: Chance of for higher price"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "costs_more_staff"] <- "Costs for more staff"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "costs_normal_fertilizer"] <- "Normal: Fertilizer costs"
EVPI_data_MPN_N$Variable[EVPI_data_MPN_N$Variable == "costs_monitoring_per_ha_month"] <- "Normal: Monitoring costs"

EVPI_data_MPN_N <- EVPI_data_MPN_N[order(EVPI_data_MPN_N$EVPI), ] 
EVPI_data_MPN_N$Variable <- factor(EVPI_data_MPN_N$Variable, levels = EVPI_data_MPN_N$Variable)
EVPI_data_MPN_N$EVPI <- round(EVPI_data_MPN_N$EVPI, digits= 2) %>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

EVPI_barplot_MPN_N <- ggplot(EVPI_data_MPN_N, aes(Variable, EVPI, fill = EVPI, label = EVPI)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  #  geom_text(aes(y=max(EVPI+(y=max(EVPI/5))), label=sprintf("%0.2f", round(EVPI, digits = 2))), size = 4.8, color="black", hjust = 1)+ #or max as EVPI
  ggtitle("WatchMore: Information Value", subtitle = "Result probably positive") +
  #  geom_hline(yintercept = max(EVPI_data_MPN_N$EVPI)) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  scale_fill_gradient(low = "ivory2", high = "purple") +
  scale_y_continuous(limits = c(0, 2500)) +
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(color = "darkgreen", size = 15, face = "bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#ggsave("EVPI_barplot_MPN_N.png", device = "png",  width = 20, height = 20, units = "cm")


VIP_table_comp_MPN_N <- read.csv("./Results_low_prophy/comp_MPN_N_pls_results.csv",
                                 header = TRUE) %>%
  
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8)
# subset(.,VIP>0.8)

colnames(VIP_table_comp_MPN_N)[colnames(VIP_table_comp_MPN_N)=="X"] <- "Variable"

VIP_table_comp_MPN_N$Variable <- as.character(VIP_table_comp_MPN_N$Variable) 
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "infection_risk"] <- "Infection risk"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "number_yearly_prophy_application_R"] <- "Reduce: Preventive applications"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "fungus_probability_R"] <- "Reduce: Fungus occurrence"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "value_of_saleable_Calluna"] <- "Sellable heather"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "threshold_big_area_more_staff"] <- "Threshold more staff"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "detection_factor_R"] <- "Reduce: Detection rate"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "detection_factor_MP"] <- "Watch: Detection rate"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "detection_factor_N"] <- "Normal: Detection rate"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "number_yearly_prophy_application_N"] <- "Normal: Preventive applications"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "fungus_fight_effect_N"] <- "Normal: Effect of applications"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "fungus_fight_effect_R"] <- "Reduce: Effect of applications"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "fungus_fight_effect_MP"] <- "Watch: Effect of applications"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "price_premium_sustainable"] <- "Reduce: More sustainable Calluna"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "savings_due_to_MP"] <- "Savings monitoring plan"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "costs_monitoring_plan_per_ha"] <- "Watch: Monitoring costs"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "amount_of_samples_MP"] <- "Watch: Number of samples"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "chance_higher_price_sustainable"] <- "Reduce: Chance of for higher price"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "costs_more_staff"] <- "Costs for more staff"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "costs_normal_fertilizer"] <- "Normal: Fertilizer costs"
VIP_table_comp_MPN_N$Variable[VIP_table_comp_MPN_N$Variable == "costs_monitoring_per_ha_month"] <- "Normal: Monitoring costs"

VIP_table_comp_MPN_N <- VIP_table_comp_MPN_N[order(VIP_table_comp_MPN_N$VIP), ]
VIP_table_comp_MPN_N$Variable <- factor(VIP_table_comp_MPN_N$Variable, levels = VIP_table_comp_MPN_N$Variable)
VIP_table_comp_MPN_N$VIP <- round(VIP_table_comp_MPN_N$VIP, digits= 2)%>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

VIP_table_comp_MPN_N$color <- ifelse(VIP_table_comp_MPN_N$Coefficient>0, "forestgreen", "firebrick3")
VIP_table_comp_MPN_N$color <- as.character(VIP_table_comp_MPN_N$color)

VIP_barplot_comp_MPN_N <- ggplot(VIP_table_comp_MPN_N, aes(Variable, VIP, fill = VIP, label = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_table_comp_MPN_N$color) +
  #  geom_text(aes(y=max(VIP+(y=max(VIP/5))), label=sprintf("%0.2f", round(VIP, digits = 2))), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle("Subtitle", subtitle = "WatchMore: Variable Importance") +
  #  geom_hline(yintercept = max(VIP_table_comp_MPN_N$VIP)) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  scale_y_continuous(limits = c(0, 5)) +
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(color = "black", size = 15, face = "bold"),
        plot.title = element_text(color = "white", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#ggsave("VIP_barplot_comp_MPN_N.png", device = "png",  width = 20, height = 20, units = "cm")


#### Create and merge horizontal barplot for EVPI and VIP of WatchMoreSprayLess #### 

EVPI_data_MPR_N<-read.csv("EVPI_tables_low_prophy/EVPI_table_comp_MPR_N.csv", header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$EVPI_dont),] %>%
  subset(.,EVPI_dont>0)
# head(.,10)

colnames(EVPI_data_MPR_N)[colnames(EVPI_data_MPR_N)=="variable"] <- "Variable"
colnames(EVPI_data_MPR_N)[colnames(EVPI_data_MPR_N)=="EVPI_dont"] <- "EVPI"
EVPI_data_MPR_N$Variable <- as.character(EVPI_data_MPR_N$Variable) 
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "infection_risk"] <- "Infection risk"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "number_yearly_prophy_application_R"] <- "Reduce: Preventive applications"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "fungus_probability_R"] <- "Reduce: Fungus occurrence"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "value_of_saleable_Calluna"] <- "Sellable heather"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "threshold_big_area_more_staff"] <- "Threshold more staff"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "detection_factor_R"] <- "Reduce: Detection rate"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "detection_factor_MP"] <- "Watch: Detection rate"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "detection_factor_N"] <- "Normal: Detection rate"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "number_yearly_prophy_application_N"] <- "Normal: Preventive applications"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "fungus_fight_effect_N"] <- "Normal: Effect of applications"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "fungus_fight_effect_R"] <- "Reduce: Effect of applications"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "fungus_fight_effect_MP"] <- "Watch: Effect of applications"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "price_premium_sustainable"] <- "Reduce: More sustainable Calluna"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "savings_due_to_MP"] <- "Savings monitoring plan"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "costs_monitoring_plan_per_ha"] <- "Watch: Monitoring costs"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "amount_of_samples_MP"] <- "Watch: Number of samples"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "chance_higher_price_sustainable"] <- "Reduce: Chance of for higher price"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "costs_more_staff"] <- "Costs for more staff"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "costs_normal_fertilizer"] <- "Normal: Fertilizer costs"
EVPI_data_MPR_N$Variable[EVPI_data_MPR_N$Variable == "costs_monitoring_per_ha_month"] <- "Normal: Monitoring costs"

EVPI_data_MPR_N <- EVPI_data_MPR_N[order(EVPI_data_MPR_N$EVPI), ] 
EVPI_data_MPR_N$Variable <- factor(EVPI_data_MPR_N$Variable, levels = EVPI_data_MPR_N$Variable)
EVPI_data_MPR_N$EVPI <- round(EVPI_data_MPR_N$EVPI, digits= 2) %>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

EVPI_barplot_MPR_N <- ggplot(EVPI_data_MPR_N, aes(Variable, EVPI, fill = EVPI, label = EVPI)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  #  geom_text(aes(y=max(EVPI+(y=max(EVPI/5))), label=sprintf("%0.2f", round(EVPI, digits = 2))), size = 4.8, color="black", hjust = 1)+ #or max as EVPI, or numbers 
  ggtitle("WatchMoreSprayLess: Information Value", subtitle = "Result probably negative") +
  theme_bw()+theme(panel.grid=element_blank())+
  #  geom_hline(yintercept = max(EVPI_data_MPR_N$EVPI)) +
  coord_flip()+
  scale_fill_gradient(low = "ivory2", high = "purple") +
  scale_y_continuous(limits = c(0, 2500)) +
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(color = "red", size = 15, face = "bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#ggsave("EVPI_barplot_MPR_N.png", device = "png",  width = 20, height = 20, units = "cm")


VIP_table_comp_MPR_N <- read.csv("./Results_low_prophy/comp_MPR_N_pls_results.csv",
                                 header = TRUE) %>%
  
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  head(., 8)
# subset(.,VIP>0.8)

colnames(VIP_table_comp_MPR_N)[colnames(VIP_table_comp_MPR_N)=="X"] <- "Variable"

VIP_table_comp_MPR_N$Variable <- as.character(VIP_table_comp_MPR_N$Variable) 
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "infection_risk"] <- "Infection risk"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "number_yearly_prophy_application_R"] <- "Reduce: Preventive applications"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "fungus_probability_R"] <- "Reduce: Fungus occurrence"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "value_of_saleable_Calluna"] <- "Sellable heather"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "threshold_big_area_more_staff"] <- "Threshold more staff"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "detection_factor_R"] <- "Reduce: Detection rate"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "detection_factor_MP"] <- "Watch: Detection rate"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "detection_factor_N"] <- "Normal: Detection rate"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "number_yearly_prophy_application_N"] <- "Normal: Preventive applications"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "fungus_fight_effect_N"] <- "Normal: Effect of applications"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "fungus_fight_effect_R"] <- "Reduce: Effect of applications"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "fungus_fight_effect_MP"] <- "Watch: Effect of applications"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "price_premium_sustainable"] <- "Reduce: More sustainable Calluna"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "savings_due_to_MP"] <- "Savings monitoring plan"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "costs_monitoring_plan_per_ha"] <- "Watch: Monitoring costs"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "amount_of_samples_MP"] <- "Watch: Number of samples"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "chance_higher_price_sustainable"] <- "Reduce: Chance of for higher price"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "costs_more_staff"] <- "Costs for more staff"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "costs_normal_fertilizer"] <- "Normal: Fertilizer costs"
VIP_table_comp_MPR_N$Variable[VIP_table_comp_MPR_N$Variable == "costs_monitoring_per_ha_month"] <- "Normal: Monitoring costs"

VIP_table_comp_MPR_N <- VIP_table_comp_MPR_N[order(VIP_table_comp_MPR_N$VIP), ] 
VIP_table_comp_MPR_N$Variable <- factor(VIP_table_comp_MPR_N$Variable, levels = VIP_table_comp_MPR_N$Variable)
VIP_table_comp_MPR_N$VIP <- round(VIP_table_comp_MPR_N$VIP, digits= 2)%>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

VIP_table_comp_MPR_N$color <- ifelse(VIP_table_comp_MPR_N$Coefficient>0, "forestgreen", "firebrick3")
VIP_table_comp_MPR_N$color <- as.character(VIP_table_comp_MPR_N$color)

VIP_barplot_comp_MPR_N <- ggplot(VIP_table_comp_MPR_N, aes(Variable, VIP, fill = VIP, label = VIP)) +
  geom_bar(width = 1, stat = "identity", color = "black", fill = VIP_table_comp_MPR_N$color) +
  #  geom_text(aes(y=max(VIP+(y=max(VIP/5))), label=sprintf("%0.2f", round(VIP, digits = 2))), size = 4.8, color="black", hjust = 1)+ #or max as VIP
  ggtitle("Subtitle", subtitle = "WatchMoreSprayLess: Variable Importance") +
  #  geom_hline(yintercept = max(VIP_table_comp_MPR_N$VIP)) +
  theme_bw()+theme(panel.grid=element_blank())+
  coord_flip()+
  scale_y_continuous(limits = c(0, 5)) +
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(color = "black", size = 15, face = "bold"),
        plot.title = element_text(color = "white", size = 15, face = "bold"),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none",
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

#ggsave("VIP_barplot_comp_MPR_N.png", device = "png",  width = 20, height = 20, units = "cm")


#### Merge EVPI and VIP of WatchMore, WatchMoreSprayLess and SprayLess ####
dir.create("./EVPI_and_VIP_plots")

EVPI_and_VIP_R_N <- ggarrange(EVPI_barplot_R_N, VIP_barplot_comp_R_N, ncol = 2, nrow = 1)

EVPI_and_VIP_MPN_N <- ggarrange(EVPI_barplot_MPN_N, VIP_barplot_comp_MPN_N, ncol = 2, nrow = 1)

EVPI_and_VIP_MPR_N <- ggarrange(EVPI_barplot_MPR_N, VIP_barplot_comp_MPR_N, ncol = 2, nrow = 1)

All_EVPI_and_VIP <- ggarrange(EVPI_and_VIP_MPN_N, EVPI_and_VIP_MPR_N, EVPI_and_VIP_R_N, ncol = 1, nrow = 3)
ggsave("./EVPI_and_VIP_plots/All_EVPI_and_VIP.png", All_EVPI_and_VIP, device = "png", width = 45, dpi = 800, height = 40, units =  "cm")

#### End of code ####


#colnames(VIP_table_comp_R_N)[colnames(VIP_table_comp_R_N)=="Variable"] <- "Variable_VIP"
#colnames(EVPI_data_R_N)[colnames(EVPI_data_R_N)=="Variable"] <- "Variable_EVPI"

R_N <- data.frame(VIP_table_comp_R_N, EVPI_data_R_N, stringsAsFactors = TRUE)

R_N <- merge(VIP_table_comp_R_N, EVPI_data_R_N, by = "Variable", all.x = TRUE)
R_N$EVPI[is.na(R_N$EVPI)] <- 0


EVPI_data_R_N
q <-  ggplot(data = R_N, aes(x = R_N$Variable, y = R_N$EVPI, fill = R_N$EVPI))+
  geom_bar(width = 1, stat = "identity", color = "black") +
  scale_fill_gradient(low = "ivory2", high = "purple") +  
  ggtitle("Nothing", subtitle ="Information Value") +
  theme_bw()+theme(panel.grid=element_blank())+ 
  ylab("EVPI")+ 
  xlab(NULL)+
  scale_y_continuous(limits = c(0, 2400), breaks = c(0,1000,2000)) +
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15), 
        axis.text.y = element_blank(),
        axis.line.y.left = element_blank(),
        #  axis.ticks.y = element_line(),
        #    panel.grid=element_blank(),
        #    panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        #    axis.line = element_line(colour = "black"),
        #    legend.title = element_blank(),
        #   legend.text = element_blank(),
        legend.position = "none") +
  coord_flip()


VIP_table_comp_R_N
p <- ggplot(R_N,aes(x=Variable,y=VIP, alpha = VIP))+
  geom_bar(width = 1,aes(fill=color),stat ="identity", color = "black")+ 
  ggtitle("Nothing", subtitle ="Variable Importance") +
  ylab("VIP")+
  xlab(NULL)+
  theme_bw()+theme(panel.grid=element_blank())+
  scale_fill_manual(values = c("firebrick3","forestgreen") )+
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        legend.position = "none",
        #    axis.title.y =element_text(color="black", size=15), 
        # axis.text.y = element_blank(),
        axis.ticks =  element_line(),
        #    axis.line.y.right = element_line(),
        #    panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_line(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        #   axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  scale_y_reverse(lim = c(5, 0)) +
  coord_flip()

g.mid <- ggplot(R_N,aes(x=1,y=R_N$Variable))+geom_text(aes(label=R_N$Variable))+
  geom_segment(aes(x=0,xend=0,yend=R_N$Variable))+
  geom_segment(aes(x=0,xend=0,yend=R_N$Variable))+
  ggtitle("SprayLess", subtitle = "Result probably negative") +
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5,color = "red", size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5, color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.line.y = element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=15, color=NA),
        axis.ticks.x=element_line(size=15, color=NA),
        plot.margin = unit(c(1,0,1,0), "mm"))

gg1 <- ggplot_gtable(ggplot_build(p))
gg2 <- ggplot_gtable(ggplot_build(q))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

R_N_plot <- cowplot::plot_grid(gg1,gg.mid,gg2, ncol = 3, align = "h")

MPR_N <- data.frame(VIP_table_comp_MPR_N, EVPI_data_MPR_N, stringsAsFactors = TRUE)

MPR_N <- merge(VIP_table_comp_MPR_N, EVPI_data_MPR_N, by = "Variable", all.x = TRUE)
MPR_N$EVPI[is.na(MPR_N$EVPI)] <- 0

EVPI_data_MPR_N
q <-  ggplot(data = MPR_N, aes(x = MPR_N$Variable, y = MPR_N$EVPI, fill = MPR_N$EVPI))+
  geom_bar(width = 1, stat = "identity", color = "black") +
  scale_fill_gradient(low = "ivory2", high = "purple") +  
  ggtitle("Nothing" , subtitle =" Information Value") +
  theme_bw()+theme(panel.grid=element_blank())+ 
  ylab("EVPI")+ 
  xlab(NULL)+
  scale_y_continuous(limits = c(0, 2400), breaks = c(0,1000,2000)) +
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15), 
        axis.text.y = element_blank(),
        axis.line.y.left = element_blank(),
        #  axis.ticks.y = element_line(),
        #    panel.grid=element_blank(),
        #    panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        #    axis.line = element_line(colour = "black"),
        #    legend.title = element_blank(),
        #   legend.text = element_blank(),
        legend.position = "none") +
  coord_flip()

VIP_table_comp_MPR_N
p <- ggplot(MPR_N,aes(x=Variable,y=VIP, alpha = VIP))+
  geom_bar(width = 1,aes(fill=color),stat ="identity", color = "black")+ 
  ggtitle("Nothing", subtitle = "Variable Importance") +
  ylab("VIP")+
  xlab(NULL)+
  theme_bw()+theme(panel.grid=element_blank())+
  scale_fill_manual(values = c("firebrick3","forestgreen"))+
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        legend.position = "none",
        #    axis.title.y =element_text(color="black", size=15), 
        # axis.text.y = element_blank(),
        axis.ticks =  element_line(),
        #    axis.line.y.right = element_line(),
        #    panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_line(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        #   axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  scale_y_reverse(lim = c(5, 0)) +
  coord_flip()

g.mid <- ggplot(MPR_N,aes(x=1,y=MPR_N$Variable))+geom_text(aes(label=MPR_N$Variable))+
  geom_segment(aes(x=0,xend=0,yend=MPR_N$Variable))+
  geom_segment(aes(x=0,xend=0,yend=MPR_N$Variable))+
  ggtitle("WatchMoreSprayLess", subtitle = "Result probably negative") +
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5,color = "red", size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5, color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.line.y = element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=15, color=NA),
        axis.ticks.x=element_line(size=15, color=NA),
        plot.margin = unit(c(1,0,1,0), "mm"))

gg1 <- ggplot_gtable(ggplot_build(p))
gg2 <- ggplot_gtable(ggplot_build(q))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

MPR_N_plot <- cowplot::plot_grid(gg1,gg.mid,gg2, ncol = 3, align = "h")

#MPN_N <- data.frame(VIP_table_comp_MPN_N, EVPI_data_MPN_N, stringsAsFactors = TRUE)

MPN_N <- merge(VIP_table_comp_MPN_N, EVPI_data_MPN_N, by = "Variable", all.x = TRUE)
MPN_N$EVPI[is.na(MPN_N$EVPI)] <- 0

EVPI_data_MPN_N
q <-  ggplot(data = MPN_N, aes(x = MPN_N$Variable, y = MPN_N$EVPI, fill = MPN_N$EVPI))+
  geom_bar(width = 1, stat = "identity", color = "black") +
  scale_fill_gradient(low = "ivory2", high = "purple") +  
  ggtitle("Nothing", subtitle = "Information Value") +
  theme_bw()+theme(panel.grid=element_blank())+ 
  ylab("EVPI")+ 
  xlab(NULL)+
  scale_y_continuous(limits = c(0, 2400), breaks = c(0,1000,2000)) +
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15), 
        axis.text.y = element_blank(),
        axis.line.y.left = element_blank(),
        #  axis.ticks.y = element_line(),
        #    panel.grid=element_blank(),
        #    panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        #    axis.line = element_line(colour = "black"),
        #    legend.title = element_blank(),
        #   legend.text = element_blank(),
        legend.position = "none") +
  coord_flip()

VIP_table_comp_MPN_N
p <- ggplot(MPN_N,aes(x=Variable,y=VIP, alpha = VIP))+
  geom_bar(width = 1,aes(fill=color),stat ="identity", color = "black")+ 
  ggtitle("Nothing", subtitle = "Variable Importance") +
  ylab("VIP")+
  xlab(NULL)+
  theme_bw()+theme(panel.grid=element_blank())+
  scale_fill_manual(values = c("firebrick3","forestgreen"))+
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        legend.position = "none",
        #    axis.title.y =element_text(color="black", size=15), 
        # axis.text.y = element_blank(),
        axis.ticks =  element_line(),
        #    axis.line.y.right = element_line(),
        #    panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_line(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        #   axis.line = element_line(colour = "black"),
        panel.background = element_blank()) +
  scale_y_reverse(lim = c(5, 0)) +
  coord_flip()

g.mid <- ggplot(MPN_N,aes(x=1,y=MPN_N$Variable))+geom_text(aes(label=MPN_N$Variable))+
  geom_segment(aes(x=0,xend=0,yend=MPN_N$Variable))+
  geom_segment(aes(x=0,xend=0,yend=MPN_N$Variable))+
  ggtitle("WatchMore", subtitle = "Result probably positive") +
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5,color = "darkgreen", size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5, color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y.left = element_line(),
        axis.ticks.y.right = element_blank(),
        axis.line.y = element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=15, color=NA),
        axis.ticks.x=element_line(size=15, color=NA),
        plot.margin = unit(c(1,0,1,0), "mm"))

gg1 <- ggplot_gtable(ggplot_build(p))
gg2 <- ggplot_gtable(ggplot_build(q))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

MPN_N_plot <- cowplot::plot_grid(gg1,gg.mid,gg2, ncol = 3, align = "h")

All_cowplot <- cowplot::plot_grid(MPN_N_plot, MPR_N_plot, R_N_plot, ncol = 1, nrow = 3)

ggsave("./EVPI_and_VIP_plots/All_cowplot.png", All_cowplot, device = "png", width = 20, dpi = 800, height = 22, units =  "cm")
