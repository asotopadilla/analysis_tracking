#Time Experiment Analysis

target_variable <- "time_to_start_moving" #which variable do you want to analyse in the model
sep <- ',' #specify file separator
auditory <-c("FC", "FL", "FS")
visual <- c("FC", "FL", "FS")
naming <- "stimulus_id" #naming convention used for file. I.e. stimulus_genotype_id or stimulus_id


# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, broom, tools, fBasics, ART)

# Find where files to be analyzed live
file_load <- file.choose()
setwd(dirname(file_load))
file_save <- paste0(basename(file_path_sans_ext(file_load)), "_anova_results.txt")

# load file and clean it
naming <- str_split(naming, "_") %>% unlist()

df <- read.csv(file_load, sep=sep, stringsAsFactors = FALSE) %>%
  dplyr::filter(phase_type == "trial") %>%
  separate(video, naming, "_", remove = FALSE, extra = "drop")


sf <- df %>%
  dplyr::filter(start_in_pole==1) %>%
  mutate(check=ifelse(first_to_safe==first_to_closest, 1, 0)) %>%
  group_by(first_to_safe, first_to_closest) %>%
  summarise(n=n(), mean=mean(check, na.rm=TRUE)) %>%
  group_by(mean) %>%
  mutate(n2=sum(n), n_pcnt=100*(n/n2))


# Anovas an Tukeys
df_model <- df [c(naming, "phase", target_variable)]
names(df_model) <- c(naming, "phase", "target")
df_model <- df_model %>%
  {if ("stimulus" %in% naming && "genotype" %in% naming) mutate(., group = paste0(stimulus, "_", genotype))
   else if ("stimulus" %in% naming && !("genotype" %in% naming)) mutate(., group = stimulus, genotype = "gen")
   else if (!("stimulus" %in% naming) && "genotype" %in% naming) mutate(., group = genotype, stimulus = "stim")
   else mutate(., group = "other")} %>%
  mutate(phase=as.factor(phase),
         group_phase = paste0(group, "_", phase),
         experiment = case_when(stimulus %in% auditory ~ "auditory",
                                stimulus %in% visual ~ "visual",
                                TRUE ~ "other"))

# Start printing console output to file
options(tibble.print_max = Inf)
con <- file(file_save)
sink(con, append=TRUE)
sink(con, append=TRUE, type="output")

## 1. Within stimulus: CS-NL phase 1 vs CS-NL phase 2 vs CS-NL phase 3 (one anova per group)
aov_1 <- lapply(unique(df_model$group), function(x) {
  anova <- aov(target ~ group_phase, df_model %>% dplyr::filter(group==x))
  art <- aligned.rank.transform(target ~ group_phase, df_model %>% dplyr::filter(group==x), perform.aov = TRUE)
  posthoc <- TukeyHSD(x=anova, 'group_phase',  conf.level=0.95)
  result <- list(tidy(anova),
                 as.tibble(art$significance),
                 tidy(posthoc) %>%
                   mutate(comparison2=gsub("[^[:digit:]-]", "", comparison)) %>%
                   separate(comparison2, c("phase_1", "phase_2"), remove=FALSE) %>%
                   dplyr::filter(as.numeric(phase_2)==(as.numeric(phase_1)-2)) %>%
                   ungroup() %>%
                   arrange(as.numeric(phase_2), as.numeric(phase_1)) %>%
                   select(-comparison2, -phase_1, -phase_2)
                 )
  names(result) <- c(paste0(x, " Anova"), paste0(x, " ART"), paste0(x, " Tukey"))
  return(result)
})

print("#### 1. Within stimulus: CS-NL phase 1 vs CS-NL phase 2 vs CS-NL phase 3 (one anova per group) ####")
aov_1
print("####################################################################################################")

## 2. Between genotypes: CS-NL vs OR-NL (one anova per stimulus)
aov_2 <- lapply(unique(df_model$stimulus), function(x) {
  anova <- aov(target ~ genotype, df_model %>% dplyr::filter(stimulus==x))
  art <- aligned.rank.transform(target ~ genotype, df_model %>% dplyr::filter(stimulus==x), perform.aov = TRUE)
  posthoc <- TukeyHSD(x=anova, 'genotype',  conf.level=0.95)
  result <- list(tidy(anova), as.tibble(art$significance), tidy(posthoc))
  names(result) <- c(paste0(x, " Anova"), paste0(x, " ART"), paste0(x, " Tukey"))
  return(result)
})
print("#### 2. Between genotypes: CS-NL vs OR-NL (one anova per stimulus) ####")
aov_2
print("####################################################################################################")

## 3. Between stimulus: CS-NL (light) vs CS-NT (sound) (one anova per genotype)
aov_3 <- lapply(unique(df_model$genotype), function(x) {
  anova <- aov(target ~ stimulus, df_model %>% dplyr::filter(genotype==x))
  art <- aligned.rank.transform(target ~ stimulus, df_model %>% dplyr::filter(genotype==x), perform.aov = TRUE)
  posthoc <- TukeyHSD(x=anova, 'stimulus',  conf.level=0.95)
  result <- list(tidy(anova), as.tibble(art$significance), tidy(posthoc))
  names(result) <- c(paste0(x, " Anova"), paste0(x, " ART"), paste0(x, " Tukey"))
  return(result)
})
print("#### 3. Between stimulus: CS-NL (light) vs CS-NT (sound) (one anova per genotype) ####")
aov_3
print("####################################################################################################")

## 4. Within experiment: NL_CS vs OL_CS vs TL_CS (one anova per genotype)
aov_4 <- lapply(unique(df_model$genotype), function(x) {
  anova <- aov(target ~ experiment, df_model %>% dplyr::filter(genotype==x))
  art <- aligned.rank.transform(target ~ experiment, df_model %>% dplyr::filter(genotype==x), perform.aov = TRUE)
  posthoc <- TukeyHSD(x=anova, 'experiment',  conf.level=0.95)
  result <- list(tidy(anova), as.tibble(art$significance), tidy(posthoc))
  names(result) <- c(paste0(x, " Anova"), paste0(x, " ART"), paste0(x, " Tukey"))
  return(result)
})
print("#### 4. Within experiment: NL_CS vs OL_CS vs TL_CS (one anova per genotype) ####")
aov_4
print("####################################################################################################")

## 5. Within genotype
aov_5 <- lapply(1, function(x) {
  anova <- aov(target ~ genotype, df_model)
  art <- aligned.rank.transform(target ~ genotype, df_model, perform.aov = TRUE)
  posthoc <- TukeyHSD(x=anova, 'genotype',  conf.level=0.95)
  result <- list(tidy(anova), as.tibble(art$significance), tidy(posthoc))
  names(result) <- c(paste0("Anova"), paste0("ART"), paste0("Tukey"))
  return(result)
})

print("#### 5. Within genotype ####")
aov_5 
print("####################################################################################################")

## 6. Within stimulus (one anova per genotype and phase)
grps<-unique(df_model %>% select(phase, genotype))

aov_6 <- lapply(1:NROW(grps), function(x) {
  anova <- aov(target ~ stimulus, df_model %>% dplyr::filter(phase==grps$phase[x], genotype==grps$genotype[x]))
  art <- aligned.rank.transform(target ~ stimulus, df_model %>% dplyr::filter(phase==grps$phase[x], genotype==grps$genotype[x]), perform.aov = TRUE)
  posthoc <- TukeyHSD(x=anova, 'stimulus',  conf.level=0.95)
  result <- list(tidy(anova), as.tibble(art$significance), tidy(posthoc))
  names(result) <- c(paste0(grps$phase[x], " ", grps$genotype[x], " Anova"), paste0(grps$phase[x], " ", grps$genotype[x], " ART"), paste0(grps$phase[x], " ", grps$genotype[x], " Tukey"))
  return(result)
})
print("#### 6. Within stimulus (one anova per genotype and phase) ####")
aov_6
print("####################################################################################################")


## 7. D'Agostino Pearson normality test

df_normal <- df %>%
  select(stimulus, genotype, time_to_start_moving, time_in_pole_initial, time_in_safe_initial, time_in_safe_total,
         time_outside_safe_after_reaching, time_to_safe, dist_to_safe, speed_to_safe) %>%
  gather(var, val, -stimulus, -genotype) %>%
  na.omit()

grps <- df_normal %>% select(var) %>% unique()

dago <- lapply(unique(df_normal$var), function(x){
  dago <- dagoTest(df_normal %>% dplyr::filter(var==x) %>% .$val)
  result <- list((dago))
  names(result) <- c(paste0(x, " D'Agostino Pearson"))
  return(result)
})

print("#### 7. D'Agostino Pearson normality test ####")
dago
print("####################################################################################################")


# Resume printing console output to console
sink()
sink(type="message")
options(tibble.print_max = 10)

print(paste0("All done! Results file saved to: ", dirname(file_load), "/", file_save))
