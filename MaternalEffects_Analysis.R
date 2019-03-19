# Maternal Effects Model Analysis

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, lme4, lmerTest, gmodels)

# Find where files to be analyzed live
file_load <- file.choose()
setwd(dirname(file_load))

# Start printing console output to file
options(tibble.print_max = Inf)
con <- file("mother_results.txt")
sink(con, append=TRUE)
sink(con, append=TRUE, type="output")

# Survival
print("#################### Survival ####################")

## Load and clean data
df_fitness <- read.csv(file.path("fitness_data.csv"), stringsAsFactors = FALSE, na.strings = c("NA", "#VALUE!", "#DIV/0!")) %>%
  select(Var1, F1.survival.m, F1.survival.mm, F2.survival.m, F2.survival.mm, egg_f1_m, egg_f1_mm, egg_f2_m, egg_f2_mm) %>%
  gather(condition, survival, -Var1, -egg_f1_m, -egg_f1_mm, -egg_f2_m, -egg_f2_mm) %>%
  separate(condition, into = c("generation", "metric", "condition")) %>%
  mutate(mother=gsub("[^A-Za-z]", "", Var1),
         mother_id=Var1,
         offspring=case_when(mother=="C" & condition=="m"~"C",
                             mother=="C" & condition=="mm"~"H",
                             mother=="H" & condition=="m"~"H",
                             mother=="H" & condition=="mm"~"C"),
         match=ifelse(condition=="m", 1, 0),
         egg=case_when(generation=="F1" & condition=="m" ~ egg_f1_m,
                       generation=="F1" & condition=="mm" ~ egg_f1_mm,
                       generation=="F2" & condition=="m" ~ egg_f2_m,
                       generation=="F2" & condition=="mm" ~ egg_f2_mm)) %>%
  select(mother_id, mother, offspring, match, egg, generation, survival) %>%
  na.omit()

## Model
### Generation 1
print("########## Generation 1 ##########")
lme = lmer(survival ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_fitness %>% filter(generation=="F1"))

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(survival ~ mother, data=df_fitness %>% filter(generation=="F1")) 
wilcox.test(survival ~ offspring, data=df_fitness %>% filter(generation=="F1")) 
wilcox.test(survival ~ match, data=df_fitness %>% filter(generation=="F1")) 

print("# Data Summary")
df_fitness  %>%
  filter(generation=="F1") %>%
  group_by(mother, offspring) %>%
  summarise(survival_mean=mean(survival),
            survival_ci_error=ci(survival)[4],
            survival_std=sd(survival))

### Generation 2
print("########## Generation 2 ##########")
lme = lmer(survival ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_fitness %>% filter(generation=="F2"))

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(survival ~ mother, data=df_fitness %>% filter(generation=="F2")) 
wilcox.test(survival ~ offspring, data=df_fitness %>% filter(generation=="F2")) 
wilcox.test(survival ~ match, data=df_fitness %>% filter(generation=="F2")) 

print("# Data Summary")
df_fitness  %>%
  filter(generation=="F2") %>%
  group_by(mother, offspring) %>%
  summarise(survival_mean=mean(survival),
            survival_ci_error=ci(survival)[4],
            survival_std=sd(survival))

# Eggs
print("#################### Eggs ####################")

## Model
### Generation 1
print("########## Generation 1 ##########")
lme = lmer(egg ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_fitness %>% filter(generation=="F1"))

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(egg ~ mother, data=df_fitness %>% filter(generation=="F1")) 
wilcox.test(egg ~ offspring, data=df_fitness %>% filter(generation=="F1")) 
wilcox.test(egg ~ match, data=df_fitness %>% filter(generation=="F1")) 

print("# Data Summary")
df_fitness  %>%
  filter(generation=="F1") %>%
  group_by(mother, offspring) %>%
  summarise(egg_mean=mean(egg),
            egg_ci_error=ci(egg)[4],
            egg_std=sd(egg))

### Generation 2
print("########## Generation 2 ##########")
lme = lmer(egg ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_fitness %>% filter(generation=="F2"))

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(egg ~ mother, data=df_fitness %>% filter(generation=="F2")) 
wilcox.test(egg ~ offspring, data=df_fitness %>% filter(generation=="F2")) 
wilcox.test(egg ~ match, data=df_fitness %>% filter(generation=="F2")) 

print("# Data Summary")
df_fitness  %>%
  filter(generation=="F2") %>%
  group_by(mother, offspring) %>%
  summarise(egg_mean=mean(egg),
            egg_ci_error=ci(egg)[4],
            egg_std=sd(egg))

# Recovery
print("#################### Recovery ####################")

## Load and clean data
df_recovery <- read.csv(file.path("recovery_data.csv"), stringsAsFactors = FALSE, na.strings = c("NA", "#VALUE!", "#DIV/0!")) %>%
  select(mother_id, rec_c_m, rec_c_mm, rec_h_m, rec_h_mm, climb_m, climb_mm) %>%
  rename(climb_o_m=climb_m,
         climb_o_mm=climb_mm) %>%
  gather(condition, value, -mother_id) %>%
  separate(condition, into = c("metric", "temperature", "condition")) %>%
  mutate(mother=gsub("[^A-Za-z]", "", mother_id),
         offspring=case_when(mother=="C" & condition=="m"~"C",
                             mother=="C" & condition=="mm"~"H",
                             mother=="H" & condition=="m"~"H",
                             mother=="H" & condition=="mm"~"C"),
         match=ifelse(condition=="m", 1, 0)) %>%
  na.omit() %>%
  select(mother_id, mother, offspring, match, metric, temperature, value)

## Model
### Recovery Cold
print("########## Recovery Cold ##########")

lme = lmer(recovery ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_recovery %>% filter(metric=="rec", temperature=="c") %>% rename(recovery=value))

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(recovery ~ mother, data=df_recovery %>% filter(metric=="rec", temperature=="c") %>% rename(recovery=value)) 
wilcox.test(recovery ~ offspring, data=df_recovery %>% filter(metric=="rec", temperature=="c") %>% rename(recovery=value)) 
wilcox.test(recovery ~ match, data=df_recovery %>% filter(metric=="rec", temperature=="c") %>% rename(recovery=value)) 

print("# Data Summary")
df_recovery %>%
  filter(metric=="rec", temperature=="c") %>%
  rename(recovery=value) %>%
  group_by(mother, offspring) %>%
  summarise(recovery_mean=mean(recovery),
            recovery_ci_error=ci(recovery)[4],
            recovery_std=sd(recovery))

### Recovery Hot
print("########## Recovery Hot ##########")

lme = lmer(recovery ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_recovery %>% filter(metric=="rec", temperature=="h") %>% rename(recovery=value))

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(recovery ~ mother, data=df_recovery %>% filter(metric=="rec", temperature=="h") %>% rename(recovery=value)) 
wilcox.test(recovery ~ offspring, data=df_recovery %>% filter(metric=="rec", temperature=="h") %>% rename(recovery=value)) 
wilcox.test(recovery ~ match, data=df_recovery %>% filter(metric=="rec", temperature=="h") %>% rename(recovery=value)) 

print("# Data Summary")
df_recovery %>%
  filter(metric=="rec", temperature=="h") %>%
  rename(recovery=value) %>%
  group_by(mother, offspring) %>%
  summarise(recovery_mean=mean(recovery),
            recovery_ci_error=ci(recovery)[4],
            recovery_std=sd(recovery))

### Climb
print("########## Climb ##########")

lme = lmer(recovery ~ mother + offspring + mother*offspring + (1|mother_id),
           data = df_recovery %>% filter(metric=="climb") %>% rename(recovery=value))

print("# Model Summary")
anova(lme)

wilcox.test(recovery ~ mother, data=df_recovery %>% filter(metric=="climb") %>% rename(recovery=value)) 
wilcox.test(recovery ~ offspring, data=df_recovery %>% filter(metric=="climb") %>% rename(recovery=value)) 
wilcox.test(recovery ~ match, data=df_recovery %>% filter(metric=="climb") %>% rename(recovery=value)) 

print("# Data Summary")
df_recovery %>%
  filter(metric=="climb") %>%
  rename(recovery=value) %>%
  group_by(mother, offspring) %>%
  summarise(recovery_mean=mean(recovery),
            recovery_ci_error=ci(recovery)[4],
            recovery_std=sd(recovery))

# Tempbox
print("#################### Tempbox ####################")

## Load and clean data
df_tempbox <- read.csv(file.path("tempbox_data.csv"), stringsAsFactors = FALSE, na.strings = c("NA", "#VALUE!", "#DIV/0!")) %>%
  select(mother_id, Speed_m, Speed_mm, Temp_m, Temp_mm, Sex_m, Sex_mm) %>%
  gather(condition, speed, -mother_id, -Temp_m, -Temp_mm, -Sex_m, -Sex_mm) %>%
  separate(condition, into = c("metric", "condition")) %>%
  mutate(mother=gsub("[^A-Za-z]", "", mother_id),
         offspring=case_when(mother=="C" & condition=="m"~"C",
                             mother=="C" & condition=="mm"~"H",
                             mother=="H" & condition=="m"~"H",
                             mother=="H" & condition=="mm"~"C"),
         temperature=ifelse(condition=="m", Temp_m, Temp_mm),
         sex=ifelse(condition=="m", Sex_m, Sex_mm),
         match=ifelse(condition=="m", 1, 0)) %>%
  na.omit() %>%
  select(mother_id, mother, offspring, match, temperature, sex, speed)

## Model
### Temperature
print("########## Temperature ##########")

lme = lmer(speed ~ mother + offspring + temperature + mother*offspring + mother*offspring*temperature + (1|mother_id),
           data = df_tempbox)

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(speed ~ mother, data=df_tempbox) 
wilcox.test(speed ~ offspring, data=df_tempbox) 
wilcox.test(speed ~ match, data=df_tempbox) 

print("# Data Summary")
df_tempbox %>%
  group_by(mother, offspring, temperature) %>%
  summarise(speed_mean=mean(speed),
            speed_ci_error=ci(speed)[4],
            speed_std=sd(speed))

### Temperature and Sex
print("########## Temperature and Sex ##########")

lme = lmer(speed ~ mother + offspring + temperature + sex + mother*offspring + mother*offspring*temperature*sex + (1|mother_id),
           data = df_tempbox)

print("# Model Summary")
anova(lme)

print("# Wilcox Test")
wilcox.test(speed ~ mother, data=df_tempbox) 
wilcox.test(speed ~ offspring, data=df_tempbox) 
wilcox.test(speed ~ match, data=df_tempbox) 
wilcox.test(speed ~ sex, data=df_tempbox) 

print("# Data Summary")
df_tempbox %>%
  group_by(mother, offspring, temperature, sex) %>%
  summarise(speed_mean=mean(speed),
            speed_ci_error=ci(speed)[4],
            speed_std=sd(speed))

# Resume printing console output to console
sink()
sink(type="message")
options(tibble.print_max = 10)

print(paste0("All done! Results file saved to: ", getwd(), " mother_results.txt"))

