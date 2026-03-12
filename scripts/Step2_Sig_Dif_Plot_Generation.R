#### STEP 2: After Step1_treatment_analysis_dash code is complete

## generate figure 3 plots for paper 

#### Make sig_difference plot 
# Load libraries
library(dplyr)
library(ggplot2)
library(ggdist)
library(tidyr)
library(gridExtra)
library(grid)

theme_set(theme_ggdist())

setwd("C:/Submission_Materials/data/figures")

data <- read.csv("all_treatment_smooth_estimates_allspecies.csv")

## pull data where only pos_test  or neg_test equal to 1

#remove duplicates per species, timescale, treatment, treatment_binned
data <- data %>%
  distinct(species, .by, treatment, .estimate,recent,old, .keep_all = TRUE)


#### Calculate total number of pos_test, neg_test, sig_test for each treatment per each bin, .by, and treatment
totalsig <- data %>%
  group_by(treatment, treatment_proportion, .by, species) %>%
  summarise(
    total_pos_test = sum(pos_test == 1),
    total_neg_test = sum(neg_test == 1),
    total_sig_test = sum(issignificant == 1)
  ) %>%
  ungroup()

#divide pos_test by total sig_test to get proportion of pos_test
totalsig <- totalsig %>%
  mutate(sig_dif = total_pos_test - total_neg_test)

### calculate average sig_dif per treatment, bin, and .by
avg_sig_dif <- totalsig %>%
  group_by(treatment, treatment_proportion, .by) %>%
  summarise(avg_sig_dif = mean(sig_dif, na.rm = TRUE)) %>%
  ungroup()

#Calculate standard error of sig_dif per treatment, bin, and .by
se_sig_dif <- totalsig %>%
  group_by(treatment, treatment_proportion, .by) %>%
  summarise(se_sig_dif = sd(sig_dif, na.rm = TRUE) / sqrt(n())) %>%
  ungroup()
#merge avg_sig_dif and se_sig_dif into totalsig
totalsig <- merge(totalsig, avg_sig_dif, by = c("treatment", "treatment_proportion", ".by"))
totalsig <- merge(totalsig, se_sig_dif, by = c("treatment", "treatment_proportion", ".by"))




#make totalsig treatment_proportion a percentage
totalsig$treatment_proportion <- totalsig$treatment_proportion * 100



#subset totalsig based on where 95% of checklists occur in site - ignore if you don't need this step
clearcut_data <- totalsig %>%
  filter(treatment == "Clearcut" & treatment_proportion <= 20)
#subset penguins_binned where thinning treatment, .b7=recent and treatment_proportion less than equal to 0.6
thinning_data <- totalsig %>%
  filter(treatment == "Thinning" & .by == "recent" & treatment_proportion <= 60)
thinning2_data <- totalsig %>%
  filter(treatment == "Thinning" & .by == "old" & treatment_proportion <= 50)
#subset penguins_binned where mastication treatment, .by=recent and treatment_proportion less than equal to 0.6
mastication_data <- totalsig %>%
  filter(treatment == "Mastication" & .by == "recent" & treatment_proportion <= 90)
mastication2_data <- totalsig %>%
  filter(treatment == "Mastication" & .by == "old" & treatment_proportion <= 50)
#subset penguins_binned where prescribed fire treatment, .by=recent and treatment_proportion less than equal to 0.4
prescribedfire_data <- totalsig %>%
  filter(treatment == "Prescribed Fire" & treatment_proportion <= 70)





#subsets combine above subsets into 1 dataframe
subset_data <- rbind(clearcut_data, thinning_data, thinning2_data, mastication_data, mastication2_data, prescribedfire_data)


#reorder old and recent in .by
subset_data$.by <- factor(subset_data$.by, levels = c("recent", "old"))
#capitalize first letter of .by levels
levels(subset_data$.by) <- c("Recent", "Old")


#Change Prescribed Fire to Prescribed fire
subset_data$treatment <- recode(subset_data$treatment, "Prescribed Fire" = "Prescribed fire")

#change level orders to be prescribed fire, mastication. thinning, clearcut
subset_data$treatment <- factor(subset_data$treatment, levels = c("Prescribed fire", "Mastication", "Thinning", "Clearcut"))  



ggplot(subset_data, aes(x = treatment_proportion, y = sig_dif, group = .by, color = .by, fill = .by)) +
  #geom_line() +
  geom_smooth(method = "loess", se = T, alpha = 0.2) +
  #geom_ribbon(aes(ymin = mean_sig_dif - se_sig_dif, ymax = mean_sig_dif + se_sig_dif), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Treated landscape percentage",
    y = "Response difference (positive - negative)",
    color = "Time since treatment",
    fill = "Time since treatment"
  ) + facet_wrap(~ treatment) +
  scale_color_manual(values = c("Recent" = "#000000", "Old" = "#999999")) +
  scale_fill_manual(values = c("Recent" = "#000000", "Old" = "#999999")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14)
        )


#save plot as png in figures folder
ggsave("data/figures/Sig_Dif_Plot.png", width = 8, height = 6, dpi = 300)


