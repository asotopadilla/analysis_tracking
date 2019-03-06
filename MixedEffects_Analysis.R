# Mixed Effects Model Analysis

target_variable <- "time_to_safe" #which variable do you want to analyse in the model
sep <- ',' #specify file separator

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, lme4, lmerTest, broom)

# Find where files to be analyzed live
file_load <- file.choose()
setwd(dirname(file_load))

# load file and clean it
df <- read.csv(file_load, sep=sep, stringsAsFactors = FALSE) %>%
  filter(phase_type == "trial") %>%
  separate(video, c("stimulus", "genotype", "id"), "_", remove = FALSE, extra = "drop")

# LME
df_model <- df [c("stimulus", "genotype", "id", target_variable)]
names(df_model) <- c("stimulus", "genotype", "id", "target")
df_model <- df_model %>%
  mutate(target=ifelse(is.na(target), 0, target))

lme = lmer(target ~ stimulus*genotype + (1|id),
               data = df_model)

summary(lme)
anova(lme)

# Tukey's
anova <- aov(target ~ stimulus + genotype, df_model)
summary(anova)
posthoc_stim <- TukeyHSD(x=anova, 'stimulus',  conf.level=0.95)
posthoc_gen <- TukeyHSD(x=anova, 'genotype',  conf.level=0.95)
plot(posthoc_stim)
plot(posthoc_gen)

