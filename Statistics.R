library(readr)
library(tidyverse)
library(FSA)


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


########################
# statistical analyses #
########################

# 1. age difference between sexes (in total)
# normality tests for different sexes
men <- df_APS %>% 
  filter(age_at_disease_onset != -1 & gender == 1)
men <- men$age_at_disease_onset

shapiro.test(men)
print(paste0("n= ", length(men)))

women <- df_APS %>% 
  filter(age_at_disease_onset != -1 & gender == 2)
women <- women$age_at_disease_onset

shapiro.test(women)
print(paste0("n= ", length(women)))

# Wilcoxon rank sum test
wilcox.test(men, women)


# 2. age at disease onset
# normality tests for each disease
diseases <- c(0, 1, 2, 3, 5, 6, 8, 9, 11, 13)

for (dis in diseases) {
  # to create a selection
  selection <- df_APS %>% 
    filter(age_at_disease_onset != -1 & first_manifestation == dis)
  selection <- selection$age_at_disease_onset
  
  # stats
  print(paste0("Disease: ", dis))
  print(paste0("n=", length(selection)))
  print(shapiro.test(selection))
}

# Kruskal-Wallis test
forKW <- df_APS %>% 
  filter(age_at_disease_onset != -1 & first_manifestation %in% diseases) %>% 
  select(first_manifestation, age_at_disease_onset)

kruskal.test(age_at_disease_onset ~ first_manifestation, 
             data = forKW)

# Dunn's multiple comparison
forKW$first_manifestation <- as.factor(forKW$first_manifestation)
# dunnTest() requires the above variable to be a factor

dunnTest(age_at_disease_onset ~ first_manifestation, 
         data = forKW, 
         method="bonferroni")


# 3. age at disease onset by sexes
# normality tests
sexes <- c(1, 2)

for (sex in sexes) {
  for (dis in diseases) {
    # to create a selection
    selection <- df_APS %>% 
      filter(age_at_disease_onset != -1 & 
               gender == sex & 
               first_manifestation == dis)
    selection <- selection$age_at_disease_onset
    
    # stats
    print(paste0("Disease: ", dis, " , Sex: ", sex))
    print(paste0("n=", length(selection)))
    if (length(selection) < 3) {
      print("Sample size is too small, Shapiro test is not possible.")
    } else {
      print(shapiro.test(selection))
    }
  }
}

# sexes are compared within each disease separately
# pairing diseases with the proper tests (based on their distribution)
diseases_A <- c(2, 5) # for Mann-Whitney-Wilcoxon tests
diseases_B <- c(0, 1, 9, 11) # for T-tests

# Wilcoxon rank sum tests
for (dis in diseases_A) {
  # selection for men
  men <- df_APS %>% 
    filter(age_at_disease_onset != -1 & 
             gender == 1 & 
             first_manifestation == dis)
  men <- men$age_at_disease_onset
  
  # selection for women
  women <- df_APS %>% 
    filter(age_at_disease_onset != -1 & 
             gender == 2 & 
             first_manifestation == dis)
  women <- women$age_at_disease_onset
  
  # stats
  print(paste0("Disease: ", dis))
  print(wilcox.test(men, women, exact = F))
}

# T tests
for (dis in diseases_B) {
  # disease selection
  forTtest <- df_APS %>% 
    filter(age_at_disease_onset != -1 & 
             first_manifestation == dis) %>% 
    select(gender, age_at_disease_onset)
  
  # stats
  print(paste0("Disease: ", dis))
  print(t.test(age_at_disease_onset ~ gender, data = forTtest))
}


# 4. ratio of organ system development after first manifestation
diseases_OS = c(0, 1, 2, 3, 5)

forKW <- df_organSystem %>% 
  filter(first_manifestation %in% diseases_OS) %>% 
  mutate(organ_system_num = 
           case_when(organ_system == "Endocrine" ~ 0,
                     organ_system == "Gastrointestinal" ~ 1,
                     organ_system == "Hematopoetic" ~ 2,
                     organ_system == "Nervous system" ~ 3,
                     organ_system == "Skin" ~ 4,
                     organ_system == "Systemic" ~ 5,)) %>% 
  select(first_manifestation, organ_system_num)

# Kruskal-Wallis test
kruskal.test(organ_system_num ~ first_manifestation, 
             data = forKW)

# Dunn's multiple comparison
forKW$first_manifestation <- as.factor(forKW$first_manifestation)

dunnTest(organ_system_num ~ first_manifestation, 
         data = forKW, 
         method="bonferroni")
