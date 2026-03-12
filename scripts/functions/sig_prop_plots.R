sig_prop_plots <- function()
{
library(dplyr)
library(ggplot2)
library(lemon)

data <- read.csv("data/figures/treatment_proportions.csv", header=T) %>% 
  mutate(Total_Negative_Proportion = -Total_Negative_Proportion)
fire <- data[data$Treatment == "Prescribed Fire", ]


firepcom <- ggplot(fire, aes(x =species, y = Total_Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#FB8072") +
  geom_bar(aes(y = Total_Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(Treatment ~ .) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
       axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_blank()
  ) + 
  ggtitle("Prescribed Fire")


cowplot::save_plot("data/figures/sig_props_fire.png", firepcom, base_height = 10, base_width = 10)


clear <- data[data$Treatment == "Clearcut", ]


clearpcom <- ggplot(clear, aes(x =species, y = Total_Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#984ea3") +
  geom_bar(aes(y = Total_Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(Treatment ~ .) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
       axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_blank()
  ) + 
  ggtitle("Clearcut")


cowplot::save_plot("data/figures/sig_props_clearcut.png", clearpcom, base_height = 10, base_width = 10)





mast <- data[data$Treatment == "Mastication", ]


mastpcom <- ggplot(mast, aes(x =species, y = Total_Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#80b1d3") +
  geom_bar(aes(y = Total_Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(Treatment ~ .) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) + 
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
    axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_blank()
  ) + 
  ggtitle("Mastication")


cowplot::save_plot("data/figures/sig_props_Mastication.png", mastpcom, base_height = 10, base_width = 10)


thin <- data[data$Treatment == "Thinning", ]


thinpcom <- ggplot(thin, aes(x =species, y = Total_Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#b3de69") +
  geom_bar(aes(y = Total_Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(Treatment ~ .) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) + 
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
    axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_blank()
  ) + 
  ggtitle("Thinning")


cowplot::save_plot("data/figures/sig_props_Thinning.png", thinpcom, base_height = 10, base_width = 10)

#### Recent vs old

#pull treatment species with recent_Negative_proportion in subset
recentneg <- data[, c("Treatment", "species", "recent_Negative_Proportion")]
  recentneg <- recentneg %>% 
    rename(Negative_Proportion = recent_Negative_Proportion)
recentneg$group <- "recent"
#repeat for recent_Positive_Proportion
recentpos <- data[, c("Treatment", "species", "recent_Positive_Proportion")] 
recentpos <- recentpos %>% 
  rename(Positive_Proportion = recent_Positive_Proportion)
recentpos$group <- "recent"

#merge recentneg and recentpos by Treatment, species, and group
recentdat <- merge(recentneg, recentpos, by = c("Treatment", "species", "group"), all = TRUE)

#repeat above for old_Negative_Proportion and old_Positive_Proportion
oldneg <- data[, c("Treatment", "species", "old_Negative_Proportion")]
oldneg <- oldneg %>% 
  rename(Negative_Proportion = old_Negative_Proportion)
oldneg$group <- "old"
oldpos <- data[, c("Treatment", "species", "old_Positive_Proportion")]
oldpos <- oldpos %>% 
  rename(Positive_Proportion = old_Positive_Proportion)
oldpos$group <- "old"
olddat <- merge(oldneg, oldpos, by = c("Treatment", "species", "group"), all = TRUE)

timedat <- rbind(recentdat, olddat)%>% 
  mutate(Negative_Proportion= -Negative_Proportion)


firetime <- timedat[timedat$Treatment == "Prescribed Fire", ]

#subset firetime by group is old
oldfire <- subset(firetime, group == "old")
oldfire$group <- "Old"

recentfire <- subset(firetime, group == "recent")
recentfire$group <- "Recent"



fireoldplot <- ggplot(oldfire, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#FB8072") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
       axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_text(size=12)
  ) + 
  ggtitle("")





firerecentplot <- ggplot(recentfire, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#FB8072") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
    axis.line = element_blank(),
    title = element_text(size=16),# No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_text(size=12)
  ) + 
  ggtitle("Prescribed Fire")

### grid arrange the two plots together
firetimepcom <- gridExtra::grid.arrange(firerecentplot,fireoldplot, ncol=2)

cowplot::save_plot("data/figures/sig_props_fire_time.png", firetimepcom, base_height = 12, base_width = 10)


### Repeat lines 132 to 194 for clearcut
cleartime <- timedat[timedat$Treatment == "Clearcut", ]
#subset cleartime by group is old
oldclear <- subset(cleartime, group == "old")
recentclear <- subset(cleartime, group == "recent")
oldclear$group <- "Old"
recentclear$group <- "Recent"


clearoldplot <- ggplot(oldclear, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#984ea3") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
    axis.text = element_text(size = 12), # Increase axis text size
       axis.line = element_blank(), # No axis lines
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_text(size=12)
  ) + 
  ggtitle("")
clearrecentplot <- ggplot(recentclear, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#984ea3") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
    axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12),
    title = element_text(size=16),# Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_text(size=12))  +
  ggtitle("Clearcut")
cleartimepcom <- gridExtra::grid.arrange(clearrecentplot,clearoldplot, ncol=2)
cowplot::save_plot("data/figures/sig_props_clearcut_time.png", cleartimepcom, base_height = 12, base_width = 10)

### Repeat for thinning
thintime <- timedat[timedat$Treatment == "Thinning", ]

#subset thintime by group is old
oldthin <- subset(thintime, group == "old")
recentthin <- subset(thintime, group == "recent")
oldthin$group <- "Old"
recentthin$group <- "Recent"

thinoldplot <- ggplot(oldthin, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#b3de69") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
       axis.line = element_blank(), # No axis lines
    plot.background = element_rect(fill = NA, colour = NA), 
    axis.text = element_text(size = 12), # Increase axis text size
    legend.position = "none",strip.text = element_text(size=12)
  ) + 
  ggtitle("")
thinrecentplot <- ggplot(recentthin, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#b3de69") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position ="dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
    axis.line = element_blank(), # No axis lines
    title = element_text(size=16),
    plot.background = element_rect(fill = NA, colour = NA),
    axis.text = element_text(size = 12), # Increase axis text size
    legend.position = "none",strip.text = element_text(size=12)) +
  ggtitle("Thinning")
thintimepcom <- gridExtra::grid.arrange(thinrecentplot,thinoldplot, ncol=2)
cowplot::save_plot("data/figures/sig_props_thinning_time.png", thintimepcom, base_height = 12, base_width = 10)

### Repeat for mastication
masttime <- timedat[timedat$Treatment == "Mastication", ]

#subset masttime by group is old
oldmast <- subset(masttime, group == "old")
recentmast <- subset(masttime, group == "recent")
oldmast$group <- "Old"
recentmast$group <- "Recent"
mastoldplot <- ggplot(oldmast, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#80b1d3") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects",
       fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    axis.title = element_blank(), # No axis titles
       axis.line = element_blank(), # No axis lines
    axis.text = element_text(size = 12), # Increase axis text size
    plot.background = element_rect(fill = NA, colour = NA), 
    legend.position = "none",strip.text = element_text(size=12)) + 
  ggtitle("")
mastrecentplot <- ggplot(recentmast, aes(x =species, y = Positive_Proportion)) +
  geom_bar(stat = "identity", position = "dodge",fill="#80b1d3") +
  geom_bar(aes(y = Negative_Proportion), stat = "identity", position = "dodge") +
  coord_flip() +
  facet_grid(. ~ group) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "", y = "Proportion of Significant Effects", fill = "Model") + ylim(-1,1) +
  theme(
    panel.background = element_rect(fill = NA, colour = NA), # No panel background
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), # Black border around the panel
    panel.grid.major = element_blank(), # No major gridlines
    panel.grid.minor = element_blank(), # No minor gridlines
    title = element_text(size=16),
    axis.title = element_blank(), # No axis titles
    axis.line = element_blank(), # No axis lines
    plot.background = element_rect(fill = NA, colour = NA), 
    axis.text = element_text(size = 12), # Increase axis text size
    legend.position = "none",strip.text = element_text(size=12)) +
  ggtitle("Mastication")
masttimepcom <- gridExtra::grid.arrange(mastrecentplot,mastoldplot, ncol=2)
cowplot::save_plot("data/figures/sig_props_mastication_time.png", masttimepcom, base_height = 12, base_width = 10)

}
