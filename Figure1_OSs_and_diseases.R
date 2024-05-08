library(readr)
library(tidyverse)
library(ggthemes)

###############
# data import #
###############

df_APS <- read_csv("final_data_pyExport.xls")

disease_key <- read_delim("disease_key_updated.csv", delim = ";", 
                          escape_double = FALSE, trim_ws = TRUE)


#####################
# data manipulation #
#####################

# creating a new table to include additional information
# it starts with reorganising the data by manifestations
firstMan <- df_APS %>% 
  mutate(manifestation = "1st",
         disease_code = first_manifestation) %>% 
  select(manifestation, disease_code) %>% 
  left_join(disease_key, by = "disease_code") %>% 
  filter(disease != 'unknown')

secondMan <- df_APS %>% 
  mutate(manifestation = "2nd",
         disease_code = second_manifestation) %>% 
  select(manifestation, disease_code) %>% 
  left_join(disease_key, by = "disease_code") %>% 
  filter(disease != 'unknown')

thirdMan <- df_APS %>% 
  mutate(manifestation = "3rd",
         disease_code = third_manifestation) %>% 
  select(manifestation, disease_code) %>% 
  left_join(disease_key, by = "disease_code") %>% 
  filter(disease != 'unknown')

fourthMan <- df_APS %>% 
  mutate(manifestation = "4th",
         disease_code = fourth_manifestation) %>% 
  select(manifestation, disease_code) %>% 
  left_join(disease_key, by = "disease_code") %>% 
  filter(disease != 'unknown')

# concatenating the above df-s
df_allMan = rbind(firstMan, secondMan, thirdMan, fourthMan)


#####################################################################
# ratio of organ and organ system involvement by each manifestation #
#####################################################################

# plot for the organ systems
OS_levels <- c("Endocrine", "Gastrointestinal", "Skin", "Nervous system", 
               "Hematopoetic", "Systemic")   # for ordering the facets

manif_labels <- c(expression("1"^st), expression("2"^nd), expression("3"^rd), 
                  expression("4"^th))   # for superscripts

df_allMan %>% 
  filter(organ_system != "Not autoimmun") %>% 
  ggplot() + 
  geom_bar(aes(manifestation, fill = organ), alpha = 0.8) +
  scale_fill_discrete() +
  scale_y_continuous(limits = c(-0.5, 248.5), expand = c(0, 0)) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd", "4th"), 
                   labels = manif_labels) +
  facet_wrap(~factor(organ_system, levels = OS_levels), 
             nrow = 2, ncol = 5, 
             scales='free_x') +
  labs(y = "Number of patients",
       x = "Manifestations", 
       fill = "Organ") +
  theme_light() +
  theme(legend.position = c(0.6,0.2), 
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8.5), 
        panel.grid.minor = element_line(color = "white"), 
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"), 
        panel.grid.major.x = element_line(color = "white"), 
        axis.text = element_text(size = 8.5, color = "black"), 
        axis.title = element_text(size = 8.5), 
        axis.ticks = element_line(color = "black", linewidth = 0.2), 
        strip.text = element_text(color = "black", size = 9.5), 
        panel.border = element_rect(linewidth = 0.4, color = "black"), 
        strip.background = element_rect(fill = "white"), 
        aspect.ratio = 1)

ggsave("organ_systems.tiff", dpi = 300)


###################
# diseases in APS #
###################

# 1. total number of cases per manifestation
df_allMan %>% 
  ggplot() +
  geom_bar(aes(manifestation, fill = manifestation), alpha = 0.8) +
  scale_fill_tableau() +
  scale_y_continuous(limits = c(-0.5, 390.5), expand = c(0, 0)) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd", "4th"), 
                   labels = manif_labels) +
  labs(title= "Total number of cases \n per manifestation", 
       y = "Number of patients",
       x = "Manifestations") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        legend.position = "NULL",
        panel.grid.minor = element_line(color = "white"),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "white"),
        axis.text = element_text(size = 17.5, color = "black"),
        axis.title = element_text(size = 17.5),
        axis.ticks = element_line(color = "black", linewidth = 0.4),
        panel.border = element_rect(linewidth = 0.8, color = "black"), 
        aspect.ratio = 1)

ggsave("total_cases_manifs.tiff", dpi = 300)

# 2. total number of cases per disease
# to figure out the frequency of diseases/order of the columns
df_allMan %>% 
  group_by(abbreviaton) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  print(n = 28)

# the actual plot
disease_order <- c("HT", "GD", "T1D", "CeD", "RA", "AD", "Vit", "AIG", "SS", 
                   "SLE", "PsO", "UC", "CD", "POF", "Alo", "AIH", "MG", "PBC", 
                   "MS", "PM", "SSc", "AIHA", "LH", "PAPS", "hypoPT", "CMC", 
                   "ITP", "PSC")   # for ordering the columns

df_allMan %>%
  ggplot() +
  geom_bar(aes(x = factor(abbreviaton, levels = disease_order), 
               fill = manifestation), alpha = 0.8) +
  scale_fill_tableau(labels = manif_labels) +
  scale_y_continuous(limits = c(-0.5, 390.5), expand = c(0, 0)) +
  labs(title= "Total number of cases per disease", 
       y = "Number of patients", 
       fill = "Manifestations") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 13.5), 
        legend.position = c(0.74,0.88), 
        legend.direction = 'horizontal', 
        legend.key.size = unit(0.65, "cm"),
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11.5), 
        panel.grid.minor = element_line(color = "white"),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "white"),
        axis.text = element_text(size = 11.5, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 11.5),
        axis.title.x = element_text(color = "white"),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        panel.border = element_rect(linewidth = 0.55, color = "black"), 
        aspect.ratio = 0.265)

ggsave("total_cases_diseases.tiff", dpi = 300)

# 3. the ten most frequent diseases
disease_order2 <- c("Hashimoto's thyroiditis", "Graves' disease", 
                   "Diabetes mellitus type 1", "Coeliac disease", 
                   "Rheumatoid arthritis", "Addison's disease", 
                   "Vitiligo", "Autoimmune gastritis", 
                   "Sjogren's syndrome", "Systemic lupus erythematosus")

df_allMan %>% 
  filter(disease == "Hashimoto's thyroiditis" | disease == "Graves' disease" | 
           disease == "Diabetes mellitus type 1" | 
           disease == "Coeliac disease" | disease == "Rheumatoid arthritis" | 
           disease == "Addison's disease" | disease == "Vitiligo" | 
           disease == "Autoimmune gastritis" | disease == "Sjogren's syndrome" |
           disease == "Systemic lupus erythematosus") %>% 
  ggplot() + 
  geom_bar(aes(manifestation, fill = manifestation), alpha = 0.8) +
  scale_fill_tableau() +
  scale_y_continuous(limits = c(-0.5, 145.5), expand = c(0, 0)) +
  scale_x_discrete(limits = c("1st", "2nd", "3rd", "4th"), 
                   labels = manif_labels) +
  facet_wrap(~factor(disease, levels = disease_order2), 
             nrow = 2, ncol = 5, scales='free_x', 
             labeller = label_wrap_gen(multi_line = TRUE)) +
  labs(y = "Number of patients",
       x = "Manifestations") +
  theme_light() +
  theme(legend.position = "NULL",
        panel.grid.minor = element_line(color = "white"),
        panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
        panel.grid.major.x = element_line(color = "white"),
        axis.text = element_text(size = 8.5, color = "black"),
        axis.title = element_text(size = 8.5),
        axis.ticks = element_line(color = "black", linewidth = 0.2),
        strip.text = element_text(color = "black", size = 9.5),
        panel.border = element_rect(linewidth = 0.4, color = "black"),
        strip.background = element_rect(fill = "white"),
        aspect.ratio = 1)

ggsave("most_freq_diseases.tiff", dpi = 300)
