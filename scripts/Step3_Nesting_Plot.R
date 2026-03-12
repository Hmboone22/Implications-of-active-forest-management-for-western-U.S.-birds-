#Step 3 - run after step1 and step2 to make nesting association figure 


# Load libraries
library(dplyr)
library(ggplot2)
library(ggdist)
library(tidyr)
library(gridExtra)
library(grid)
library(data.table)
theme_set(theme_ggdist())

setwd("C:/Submission_Materials/data/")


data <- fread("figures/all_treatment_smooth_estimates_allspecies.csv")
spdata <- fread("Bird_nesting_information.csv")

#merge data and spdata by species, scientific_name and common_name

data <- merge(data, spdata, by = c("species"), all.x = TRUE)


#remove duplicates per species, timescale, treatment, treatment_binned
data <- data %>%
  distinct(species, .by, treatment, .estimate,recent,old, .keep_all = TRUE)


#filter where signtest = 1
data <- data %>% filter(signtest == 1)

penguins_binned <- data %>%
  mutate(treatment_binned = cut(
    treatment_proportion,
    breaks = c(0,0.1,0.2,0.3, 0.4,0.5, 0.6,0.7, 0.8,0.9, 1.0), # Define your custom bin breaks
    right = TRUE,
    labels = c("10","20", "30","40", "50","60", "70","80", "90","100") # Assign a label to each bin
  )) %>%
  # Filter out any NA values from the new column
  filter(!is.na(treatment_binned))

#penguins_binned$treatment_binned <- recode(penguins_binned$treatment_binned, "0-0.1" = 0.1, "0.1-0.2" = 0.2, "0.2-0.3" = 0.3, "0.3-0.4" = 0.4, "0.4-0.5" = 0.5, "0.5-0.6" = 0.6, "0.6-0.7" = 0.7, "0.7-0.8" = 0.8, "0.8-0.9" = 0.9, "0.9-1.0" = 1.0)
penguins_binned$treatment_binned <- as.numeric(as.character(penguins_binned$treatment_binned))


#subset penguins_binned where clearcut treatment and treatment_proportion less than equal to 0.2
clearcut_data <- penguins_binned %>%
  filter(treatment == "Clearcut" & treatment_proportion <= 0.2)
#subset penguins_binned where thinning treatment, .b7=recent and treatment_proportion less than equal to 0.6
thinning_data <- penguins_binned %>%
  filter(treatment == "Thinning" & .by == "recent" & treatment_proportion <= 0.6)
thinning2_data <- penguins_binned %>%
  filter(treatment == "Thinning" & .by == "old" & treatment_proportion <= 0.5)
#subset penguins_binned where mastication treatment, .by=recent and treatment_proportion less than equal to 0.6
mastication_data <- penguins_binned %>%
  filter(treatment == "Mastication" & .by == "recent" & treatment_proportion <= 0.9)
mastication2_data <- penguins_binned %>%
  filter(treatment == "Mastication" & .by == "old" & treatment_proportion <= 0.5)
#subset penguins_binned where prescribed fire treatment, .by=recent and treatment_proportion less than equal to 0.4
prescribedfire_data <- penguins_binned %>%
  filter(treatment == "Prescribed Fire" & treatment_proportion <= 0.7)

#subsets combine above subsets into 1 dataframe
subset_data <- rbind(clearcut_data, thinning_data, thinning2_data, mastication_data, mastication2_data, prescribedfire_data)


total_subsetsig <- subset_data %>%
  group_by(treatment, treatment_binned, .by, species, Nesting) %>%
  summarise(
    total_pos_test = sum(pos_test == 1),
    total_neg_test = sum(neg_test == 1),
    total_sig_test = sum(issignificant == 1)
  ) %>%
  ungroup()

#divide pos_test by total sig_test to get proportion of pos_test
total_subsetsig <- total_subsetsig %>%
  mutate(sig_dif = total_pos_test - total_neg_test)


#combined .by and Habitat into one column
#total_subsetsig <- total_subsetsig %>%
#  unite("Nesting", c(".by", "Nesting"), sep = "_", remove = FALSE)


#switch old and recent's order
total_subsetsig$.by <- factor(total_subsetsig$.by, levels = c("recent", "old"))

#reorder update_habitat levels to shrubland woodland, open forest, semi-open forest, Dense forest
#total_subsetsig$Updated_Habitat <- factor(total_subsetsig$Updated_Habitat, levels = c("Shrubland", "Woodland", "Open forest", "Semi-open forest", "Dense forest"))

total_subsetsig$.by <- recode(total_subsetsig$.by, "recent" = "Recent", "old" = "Old")

total_subsetsig$treatment <- recode(total_subsetsig$treatment, "Prescribed Fire" = "Prescribed fire")
#stat slab interval plot
hab_palette <- c("Prescribed fire" = "#FB8072", "Thinning" = "#B3DE69", "Clearcut" = "#984ea3", "Mastication" = "#80B1D3","Shrubland" = "#E69F00", "Woodland" = "#56B4E9", "Open forest" = "#009E73", "Semi-open forest" = "#984ea3", "Dense forest" = "#0072B2")


#colorblind palette for updated_habitat
cb_palette <- c("Tree" = "#E69F00", "Cavity" = "#56B4E9", "Shrub" = "#984ea3", "Ground" = "#0072B2")



#times totalsig sig_dif by 10
total_subsetsig$sig_dif_10 <- total_subsetsig$sig_dif * 10

#colorblind palette for updated_habitat
cb_palette <- c("Tree" = "#E69F00", "Cavity" = "#56B4E9", "Shrub" = "#984ea3", "Ground" = "#0072B2")


#make points the average sig_dif_10 for each treatment_binned and Updated_Habitat
avg_totalsig <- total_subsetsig %>%
  group_by(treatment_binned,treatment, Nesting, .by) %>%
  summarise(
    avg_sig_dif_10 = mean(sig_dif_10)
  ) %>%
  ungroup()

#reorder levels of treatment to be Prescribed fire, Mastication, Thinning, Clearcut
avg_totalsig$treatment <- factor(avg_totalsig$treatment, levels = c("Prescribed fire", "Mastication", "Thinning", "Clearcut"))  


#plot a line graph with treatment_binned as x axis average sig dif is y axis fill= Updated_Habitat and facet by .by
ggplot(avg_totalsig, aes(x=treatment_binned, y=avg_sig_dif_10, group=treatment, color=Nesting)) +
  geom_line(aes(group=Nesting), size=0.6) +
  geom_point(size=4, aes(fill=Nesting), shape=21, stroke=0.5, alpha=0.6) +
  facet_grid(treatment~.by) +
  theme(legend.position = "top", axis.ticks.x = element_line(), 
        axis.text=element_text(size=13),
        axis.title=element_text(size=14),
        strip.text = element_text(size=14),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        panel.border = element_rect(colour = "grey", fill=NA, size=0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey") + 
  xlab("Treatment percentage") +
  ylab("Response difference (positive - negative)") +
  labs(color="Nesting", fill="Nesting") +
  scale_color_manual(values=cb_palette) +
  scale_fill_manual(values=cb_palette) +
  guides(color="none")



ggsave("/figures/Ebird_Nesting_Line_Plot_Average.png", width = 8, height = 7, units = "in", dpi = 300)
### to repeat the above with median not average ###


#### behavior category trends ####

setwd("D:/Colorado Job/Birds_Fuel_Type/figures")
data <- fread("all_treatment_smooth_estimates_allspecies_10km.csv")
spdata <- fread("D:/Colorado Job/Birds_Fuel_Type/Writing drafts/Species_List_Count_Trait_Info.csv")

#merge data and spdata by species, scientific_name and common_name

data <- merge(data, spdata, by = c("species", "scientific_name", "common_name"), all.x = TRUE)


#remove duplicates per species, timescale, treatment, treatment_binned
data <- data %>%
  distinct(species, .by, treatment, .estimate,recent,old, .keep_all = TRUE)


#change Semi-open to Semi-open forest
data$Updated_Habitat <- recode(data$Updated_Habitat, "Semi-open" = "Semi-open forest")

#group together aerial dive, aerial forager, hovering behaviors into just "Aerial"
data$Behavior <- recode(data$Behavior, "Aerial Dive" = "Aerial", "Aerial Forager" = "Aerial", "Hovering" = "Aerial")

#filter where signtest = 1
data <- data %>% filter(signtest == 1)

penguins_binned <- data %>%
  mutate(treatment_binned = cut(
    treatment_proportion,
    breaks = c(0,0.1,0.2,0.3, 0.4,0.5, 0.6,0.7, 0.8,0.9, 1.0), # Define your custom bin breaks
    right = TRUE,
    labels = c("0-0.1","0.1-0.2", "0.2-0.3","0.3-0.4", "0.4-0.5","0.5-0.6", "0.6-0.7","0.7-0.8", "0.8-0.9","0.9-1.0") # Assign a label to each bin
  )) %>%
  # Filter out any NA values from the new column
  filter(!is.na(treatment_binned))

penguins_binned$treatment_binned <- recode(penguins_binned$treatment_binned, "0-0.1" = 0.1, "0.1-0.2" = 0.2, "0.2-0.3" = 0.3, "0.3-0.4" = 0.4, "0.4-0.5" = 0.5, "0.5-0.6" = 0.6, "0.6-0.7" = 0.7, "0.7-0.8" = 0.8, "0.8-0.9" = 0.9, "0.9-1.0" = 1.0)
penguins_binned$treatment_binned <- as.numeric(as.character(penguins_binned$treatment_binned))


#subset penguins_binned where clearcut treatment and treatment_proportion less than equal to 0.2
clearcut_data <- penguins_binned %>%
  filter(treatment == "Clearcut" & treatment_proportion <= 0.2)
#subset penguins_binned where thinning treatment, .b7=recent and treatment_proportion less than equal to 0.6
thinning_data <- penguins_binned %>%
  filter(treatment == "Thinning" & .by == "recent" & treatment_proportion <= 0.6)
thinning2_data <- penguins_binned %>%
  filter(treatment == "Thinning" & .by == "old" & treatment_proportion <= 0.5)
#subset penguins_binned where mastication treatment, .by=recent and treatment_proportion less than equal to 0.6
mastication_data <- penguins_binned %>%
  filter(treatment == "Mastication" & .by == "recent" & treatment_proportion <= 0.9)
mastication2_data <- penguins_binned %>%
  filter(treatment == "Mastication" & .by == "old" & treatment_proportion <= 0.5)
#subset penguins_binned where prescribed fire treatment, .by=recent and treatment_proportion less than equal to 0.4
prescribedfire_data <- penguins_binned %>%
  filter(treatment == "Prescribed Fire" & treatment_proportion <= 0.7)

#subsets combine above subsets into 1 dataframe
subset_data <- rbind(clearcut_data, thinning_data, thinning2_data, mastication_data, mastication2_data, prescribedfire_data)


total_subsetsig <- subset_data %>%
  group_by(treatment, treatment_binned, .by, species, Behavior) %>%
  summarise(
    total_pos_test = sum(pos_test == 1),
    total_neg_test = sum(neg_test == 1),
    total_sig_test = sum(issignificant == 1)
  ) %>%
  ungroup()

#divide pos_test by total sig_test to get proportion of pos_test
total_subsetsig <- total_subsetsig %>%
  mutate(sig_dif = total_pos_test - total_neg_test)


#combined .by and Habitat into one column
#total_subsetsig <- total_subsetsig %>%
#  unite("Nesting", c(".by", "Nesting"), sep = "_", remove = FALSE)


#switch old and recent's order
total_subsetsig$.by <- factor(total_subsetsig$.by, levels = c("recent", "old"))

#reorder update_habitat levels to shrubland woodland, open forest, semi-open forest, Dense forest
#total_subsetsig$Updated_Habitat <- factor(total_subsetsig$Updated_Habitat, levels = c("Shrubland", "Woodland", "Open forest", "Semi-open forest", "Dense forest"))

total_subsetsig$.by <- recode(total_subsetsig$.by, "recent" = "Recent", "old" = "Old")

total_subsetsig$treatment <- recode(total_subsetsig$treatment, "Prescribed Fire" = "Prescribed fire")
#stat slab interval plot
hab_palette <- c("Prescribed fire" = "#FB8072", "Thinning" = "#B3DE69", "Clearcut" = "#984ea3", "Mastication" = "#80B1D3","Shrubland" = "#E69F00", "Woodland" = "#56B4E9", "Open forest" = "#009E73", "Semi-open forest" = "#984ea3", "Dense forest" = "#0072B2")


#colorblind palette for updated_habitat
#cb_palette <- c("Ground Forager" = "#E69F00", "Bark Forager" = "#56B4E9", "Foliage Gleaner" = "#009E73", "Hovering" = "#984ea3", "Aerial Dive" = "#0072B2","Aerial Forager" = "#ef8a62","Flycatching" = "#91cf60")
#colorblind palette for updated_habitat
cb_palette <- c("Ground Forager" = "#E69F00", "Bark Forager" = "#56B4E9", "Foliage Gleaner" = "#009E73", "Aerial" = "#984ea3","Flycatching" = "#91cf60")




#times totalsig sig_dif by 10
total_subsetsig$sig_dif_10 <- total_subsetsig$sig_dif * 10

#plot a line graph with treatment_binned as x axis average sig dif is y axis fill= Updated_Habitat and facet by .by
ggplot(total_subsetsig, aes(x=treatment_binned, y=sig_dif_10, color=Behavior)) +
  geom_line(aes(group=Behavior), size=1) +
  geom_point(size=2, aes(fill=Behavior), shape=21, color="black", stroke=0.5) +
  facet_grid(.~.by) +
  theme(legend.position = "top", axis.ticks.x = element_line(), axis.title.y=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        panel.border = element_rect(colour = "grey", fill=NA, size=0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey") + 
  xlab("Treatment proportion (binned)") +
  ylab("Difference in significant associations") +
  labs(color="Behavior", fill="Behavior") +
  scale_color_manual(values=cb_palette) +
  scale_fill_manual(values=cb_palette) +
  guides(color="none")
#colorblind palette for updated_habitat

#make points the average sig_dif_10 for each treatment_binned and Updated_Habitat
avg_totalsig <- total_subsetsig %>%
  group_by(treatment_binned,treatment, Behavior, .by) %>%
  summarise(
    avg_sig_dif_10 = mean(sig_dif_10)
  ) %>%
  ungroup()
#plot a line graph with treatment_binned as x axis average sig dif is y axis fill= Updated_Habitat and facet by .by
ggplot(avg_totalsig, aes(x=treatment_binned, y=avg_sig_dif_10, group=treatment, color=Behavior)) +
  geom_line(aes(group=Behavior), size=0.6) +
  geom_point(size=4, aes(fill=Behavior), shape=21, stroke=0.5, alpha=0.6) +
  facet_grid(treatment~.by) +
  theme(legend.position = "top", axis.ticks.x = element_line(), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        panel.border = element_rect(colour = "grey", fill=NA, size=0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "grey") + 
  xlab("Treatment proportion (binned)") +
  ylab("Average difference in significant associations") +
  labs(color="Behavior", fill="Behavior") +
  scale_color_manual(values=cb_palette) +
  scale_fill_manual(values=cb_palette) +
  guides(color="none")
ggsave("Ebird_Foraging_Line_Plot_Average.png", width = 12, height = 8, units = "in", dpi = 300)







behavior <- read.csv("E:/Colorado Job/Birds_Fuel_Type/data/species_nesting_foraging_behavior.csv")
behavior$Behavior <- recode(behavior$Behavior, "Aerial Dive" = "Aerial", "Aerial Forager" = "Aerial", "Hovering" = "Aerial")

#calculate number of species in each behavior
behavior_counts <- behavior %>%
  group_by(Behavior) %>%
  summarise(
    species_count = n_distinct(species)
  ) %>%
  ungroup()

#calculate number of species in each behavior
nesting_counts <- behavior %>%
  group_by(Nesting) %>%
  summarise(
    species_count = n_distinct(species)
  ) %>%
  ungroup()
