#Time Experiment Analysis

target_variable <- "time_to_safe" #which variable do you want to analyse in the model
sep <- ',' #specify file separator
naming <- "stimulus_genotype_id" #naming convention used for file. I.e. stimulus_genotype_id or stimulus_id


# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, broom, tools, fBasics, ART, nlme, lsmeans)

# Find where files to be analyzed live
file_load <- file.choose()
setwd(dirname(file_load))
file_save <- paste0(basename(file_path_sans_ext(file_load)), "_anova_results.txt")

# load file and clean it
naming <- str_split(naming, "_") %>% unlist()

df <- read.csv(file_load, sep=sep, stringsAsFactors = FALSE) %>%
  dplyr::filter(phase_type == "trial") %>%
  separate(video, naming, "_", remove = FALSE, extra = "drop")

df_model <- df[c(naming, "phase", target_variable)]
names(df_model) <- c(naming, "phase", "target")
df_model <- df_model %>%
  mutate(phase = as.factor(phase),
         id = as.factor(id))
  
# Start printing console output to file
options(tibble.print_max = Inf)
con <- file(file_save)
sink(con, append=TRUE)
sink(con, append=TRUE, type="output")

## 1. Anova and Tukey per genotype
results <- lapply(unique(df_model$genotype), function(x) {
  art <- aligned.rank.transform(target ~ stimulus * phase, df_model %>% dplyr::filter(genotype==x), perform.aov = FALSE)
  lme_model <- lme(fixed = target ~ stimulus * phase, random=~1|id, na.action = na.omit, data = art$aligned %>% dplyr::filter(genotype==x))
  posthoc <- lsmeans(lme_model, pairwise~stimulus, adjust="tukey")
  result <- list(anova(lme_model), summary(posthoc))
  names(result) <- c(paste0(x, " Anova"), paste0(x, " Tukey"))
  return(result)
})

print("#### 1. Anova and Tukey(one per genotype) ####")
results
print("####################################################################################################")


## 2. D'Agostino Pearson normality test

df_normal <- df %>%
  dplyr::select(stimulus, genotype, time_to_start_moving, time_in_pole_initial, time_in_safe_initial, time_in_safe_total,
         time_outside_safe_after_reaching, time_to_safe, dist_to_safe, speed_to_safe) %>%
  gather(var, val, -stimulus, -genotype) %>%
  na.omit()

grps <- df_normal %>% dplyr::select(var) %>% unique()

dago <- lapply(unique(df_normal$var), function(x){
  dago <- dagoTest(df_normal %>% dplyr::filter(var==x) %>% .$val)
  result <- list((dago))
  names(result) <- c(paste0(x, " D'Agostino Pearson"))
  return(result)
})

print("#### 2. D'Agostino Pearson normality test ####")
dago
print("####################################################################################################")


# Resume printing console output to console
sink()
sink(type="message")
options(tibble.print_max = 10)

print(paste0("All done! Results file saved to: ", dirname(file_load), "/", file_save))
