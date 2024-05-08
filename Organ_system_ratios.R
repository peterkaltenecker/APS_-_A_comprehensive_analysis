library(readr)
library(tidyverse)
library(scales)
library(ggstats)


###############
# data import #
###############

df_APS <- read_csv("final_data_pyExport.xls")

disease_key <- read_delim("disease_key_updated.csv", delim = ";", 
                          escape_double = FALSE, trim_ws = TRUE)


#####################
# data manipulation #
#####################

# organ system information for 2nd, 3rd and 4th manifestations
secondMan_organ <- df_APS %>% 
  filter(first_manifestation != -1) %>%    # to remove unknown rows
  mutate(disease_code = second_manifestation, 
         manifestation = "second") %>%
  left_join(disease_key, by = "disease_code") %>%
  select(first_manifestation, manifestation, abbreviaton, organ_system)

thirdMan_organ <- df_APS %>% 
  filter(first_manifestation != -1 &     # to remove unknown rows
           assoc_disease_no == 3) %>%    # to select the right combinations
  mutate(disease_code = third_manifestation, 
         manifestation = "third") %>%
  left_join(disease_key, by = "disease_code") %>%
  select(first_manifestation, manifestation, abbreviaton, organ_system)

fourthMan_organ <- df_APS %>% 
  filter(first_manifestation != -1 &     # to remove unknown rows
           assoc_disease_no == 4) %>%    # to select the right combinations
  mutate(disease_code = fourth_manifestation, 
         manifestation = "fourth") %>%
  left_join(disease_key, by = "disease_code") %>%
  select(first_manifestation, manifestation, abbreviaton, organ_system)

# to unite the above dfs
df_organSystem <- rbind(secondMan_organ, thirdMan_organ, fourthMan_organ)


###############################################################
# ratio of organ system involvement after first manifestation #
###############################################################

# stacked bar chart
diseases_OS = c(0, 1, 2, 3, 5)
disease_order = c(3, 2, 1, 5, 0)

df_organSystem %>% 
  filter(first_manifestation %in% diseases_OS) %>%
  ggplot(aes(x = factor(first_manifestation, levels = disease_order), 
             fill = organ_system, by = first_manifestation)) +
  geom_bar(position = "fill", alpha = 0.8, width = 0.75) +
  geom_text(stat = "prop", position = position_fill(0.5), size = 3, 
            angle = 45) +
  scale_y_continuous(expand = c(0.015, 0.015), labels = percent) +
  scale_x_discrete(labels = c("0" = "HT", "1" = "GD", "2" = "AD", 
                              "3" = "CeD", "5" = "T1D"), 
                   position = "top") +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(title= "Ratio of subsequent organ system involvement \n following the five most frequent first manifestations", 
       x = "First manifestation", 
       y = "Percentage") +
  guides(fill=guide_legend(title="Organ system")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_line(color = "white"),
        panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
        panel.grid.major.y = element_line(color = "white"),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 11),
        #axis.title.x = element_text(color = "white"),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        panel.border = element_rect(linewidth = 0.4, color = "black"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(color = "black"), 
        legend.position = "left", 
        aspect.ratio = 0.6)

ggsave("organProps_forSankey.tiff", dpi = 300)
ggsave("organProps_forSankey.pdf", dpi = 300)


##############################################
# lollipop plot for visualising significance #
##############################################

# creating a df for the plot
X = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
Y1 = c(0, 0, 1, 0, 1, 2, 0, 1, 2, 3)
Y2 = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)

df_lolPop <- data.frame(X, Y1, Y2)

# lollipop plot
df_lolPop %>% 
  ggplot() +
  geom_segment(aes(x = X, xend = X, y = Y1, yend = Y2), 
               color = "black", linewidth = 1) +
  geom_point(aes(x = X, y = Y1), color = "black", size = 2.7) +
  geom_point(aes(x = X, y = Y2), color = "black", size = 2.7) +
  scale_y_reverse(breaks = c(0, 1, 2, 3, 4), 
                  labels = c("0" = "HT", "1" = "T1D", "2" = "GD", 
                             "3" = "AD", "4" = "CeD")) +
  scale_x_discrete(expand = c(0.06, 0.06)) +
  labs(y = "First manifestation", 
       x = "Significance") +
  theme_light() +
  theme(axis.text.x = element_text(color = "white"), 
        axis.text.y = element_text(size = 15.5, color = "black"), 
        axis.title.x = element_text(size = 15.5), 
        axis.title.y = element_text(size = 15.5), 
        axis.ticks = element_line(color = "white"),
        panel.grid.major.x = element_line(color = "white"), 
        panel.grid.minor.y = element_line(color = "white"),
        panel.grid.major.y = element_line(color = "gray"),
        panel.border = element_rect(color = "white"), 
        aspect.ratio = 1)

ggsave("lollipop_forSignif.tiff", dpi = 300)
ggsave("lollipop_forSignif.pdf", dpi = 300)
