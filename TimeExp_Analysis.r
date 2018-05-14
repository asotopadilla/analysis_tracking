# Time Experiment analysis

######## Input Parameters #########
numphases <- 60 #number of phases in experiment
phaseduration <- 1800 #duration of phase in frames
onlytrials <- TRUE #set to TRUE if only trial output wanted, FALSE if trials and poles output
fps <- 30 #video fps
width_px <- 777 #arena width in pixels
sep="," #specify file separator

######## Output Variables #########
# time_in_pole_initial - Time in pole before getting out. Only counts is start in the pole position.
# time_in_safe_initial - Time in safe first time entered. Only counts if not starting in safe tile already.
# time_in_safe_total - Total time in safe tile.
# time_outside_safe_after_reaching - Time outside safe tile after reaching it.
# time_to_safe - Time before entering the safe tile for the first time. Only counts if not starting in safe tile already.
# start_in_pole - Fly starte in pole position (1) or not (0)
# first_to_safe - First tile fly went to is safe (1) or not (0). Only counts when pole is middle tile.
###################################

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse)

# Function to take blink leds and transform them to constant leds
led_extend <- function(led, phaseduration){
  i<-1
  led_extended <- rep(0, length(led))
  while (i <= length(led)) {
    if (led[i]==1 & led[i+1]==0){
      if (i+phaseduration<=length(led)){
        led_extended[i:(i+phaseduration-1)]<-1
        i<-i+phaseduration
      } else {
        led_extended[i:length(led)]<-1
        break()
      }
    } else {
      i<-i+1
    }
  }
  return(led_extended)
}

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

if (!dir.exists(file.path(dir, "reults"))) dir.create(file.path(dir, "results"))

# Get all .csv files in chosen directory
files <- list.files(pattern = glob2rx("*.csv"))

# Bind them into one data frame and calculate variables
df <- (lapply(files, function(x) read.csv(x, sep=sep, stringsAsFactors = FALSE)))
names(df) <- files
df <- do.call(rbind, df) %>%
  rename(x=X0_x, y=X0_y) %>%
  mutate(video=row.names(.),
         video=gsub("\\.csv\\..*","",video),
         video=gsub("_output","",video),
         pole=gsub("^.*_", "", video),
         video=gsub("_[^_]*$", "", video),
         led=gsub("^.*_", "", video),
         video=gsub("_[^_]*$", "", video)) %>%
  arrange(video, frame_idx) %>%
  group_by(video, led) %>%
  mutate(led_1_status=ifelse(led=="2", led_extend(led_1_status, phaseduration), led_1_status),
         led_2_status=ifelse(led=="2", led_extend(led_2_status, phaseduration), led_2_status)) %>%
  group_by(video) %>%
  mutate(phase=ifelse(led_1_status!=lag(led_1_status) | led_2_status!=lag(led_2_status), 1, 0),
         phase=ifelse(row_number()==1, 1, phase),
         phase=cumsum(phase),
         phase_type=ifelse(led_1_status==0 & led_2_status==0, "pole", "trial")) %>%
  filter(phase<=numphases) %>%
  group_by(video, phase) %>%
  mutate(rm_phase=ifelse(phase==1 | phase>numphases, 1, 0),
         rm_frame=ifelse(row_number()<(n()-phaseduration), 1, 0)) %>%
  filter(!(rm_phase==1 & rm_frame==1)) %>%
  select(-rm_phase, -rm_frame, -led) %>%
  ungroup() %>%
  mutate(safe_location=case_when(led_1_status==0 & led_2_status==0 ~ "P",
                                 led_1_status==1 & led_2_status==0 ~ "L",
                                 led_1_status==0 & led_2_status==1 ~ "R"),
         pole_l=case_when(pole=="L" ~ 0,
                          pole=="M" ~ width_px/3,
                          pole=="R" ~ 2*width_px/3),
         pole_r=case_when(pole=="L" ~ width_px/3,
                          pole=="M" ~ 2*width_px/3,
                          pole=="R" ~ width_px),
         safe_l=case_when(safe_location=="L" & pole %in% c("M", "R") ~ 0,
                          safe_location=="L" & pole %in% c("L") ~ width_px/3,
                          safe_location=="R" & pole %in% c("L", "M") ~ 2*width_px/3,
                          safe_location=="R" & pole %in% c("R") ~ width_px/3,
                          safe_location=="P" ~ pole_l),
         safe_r=case_when(safe_location=="L" & pole %in% c("M", "R") ~ width_px/3,
                          safe_location=="L" & pole %in% c("L") ~ 2*width_px/3,
                          safe_location=="R" & pole %in% c("L", "M") ~ width_px,
                          safe_location=="R" & pole %in% c("R") ~ 2*width_px/3,
                          safe_location=="P" ~ pole_r),
         fly_location=case_when(x>=pole_l & x<=pole_r ~ "pole",
                                x>=safe_l & x<=safe_r ~ "safe",
                                TRUE ~ "unsafe")) %>%
  group_by(video, phase) %>%
  mutate(step_num=ifelse(fly_location!=lag(fly_location), 1, 0),
         step_num=ifelse(row_number()==1, 1, step_num),
         step_num=cumsum(step_num)) %>%
  group_by(video, phase, phase_type, pole, safe_location, fly_location, step_num) %>%
  summarise(num_frames=n()) %>%
  group_by(video, phase, fly_location) %>%
  arrange(video, phase, fly_location, step_num) %>%
  mutate(location_num=row_number()) %>%
  group_by(video, phase) %>%
  mutate(total_frames=sum(num_frames)) %>%
  ungroup() %>%
  arrange(video, phase, step_num, fly_location) %>%
  select(video, phase, phase_type, pole, safe_location, fly_location, step_num, location_num, num_frames, total_frames)

## Run up to here and check variable df for a per phase, per location summary

df_out <- df %>%
  group_by(video, phase) %>%
  mutate(time_in_pole_initial=ifelse(step_num==1 & fly_location=="pole", num_frames, NA),
         time_in_safe_initial=ifelse(step_num>1 & location_num==1 & fly_location=="safe", num_frames, NA),
         time_in_safe_total=ifelse(fly_location=="safe", num_frames, NA),
         reach_safe=ifelse(!is.na(time_in_safe_initial), step_num, NA),
         reach_safe=max(reach_safe, na.rm=TRUE),
         reach_safe=ifelse(is.infinite(reach_safe), 0, reach_safe),
         time_outside_safe_after_reaching=ifelse(reach_safe>0 & fly_location!="safe" & step_num>reach_safe, num_frames, NA),
         time_to_safe=ifelse(reach_safe>0 & fly_location!="safe" & step_num<reach_safe, num_frames, NA),
         start_in_pole=ifelse(step_num==1 & fly_location=="pole", 1, 0),
         first_to_safe=ifelse(step_num==2 & fly_location=="safe", 1, 0),
         safe_location=case_when(pole=="M" ~ as.character(NA),
                                 pole=="L" & safe_location=="L" ~ "short",
                                 pole=="L" & safe_location=="R" ~ "long",
                                 pole=="R" & safe_location=="L" ~ "long",
                                 pole=="R" & safe_location=="R" ~ "short",
                                 TRUE ~ as.character(NA))) %>%
  select(-c(fly_location, step_num, location_num, num_frames, total_frames, reach_safe)) %>%
  group_by(video, phase, phase_type, pole, safe_location) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>%
  mutate(first_to_safe=ifelse(pole!="M", NA, first_to_safe)) %>%
  ungroup() %>%
  mutate_at(vars(contains("time")), funs(round(./fps, 2))) %>%
  rename(pole_location=pole)

# If onlytrials is true, remove pole phases from data
if (onlytrials){
  df_out <- filter(df_out, phase_type=="trial")
  }

# Save combines videos and variables output
write.table(df_out, "results/time_experiment_analysis_combined.csv", row.names = FALSE, sep=",")

# Save one file per variable with videos as columns
for (i in 1:(NCOL(df_out)-5)) {
  df_sep <- df_out[, c(1, 2, 3, 5, i+5)] %>%
    spread(video, names(df_out)[i+5])
    
    write.table(df_sep, paste0("results/time_experiment_analysis_", names(df_out)[i+5], ".csv"), row.names = FALSE, sep=",")
    
    rm(df_sep, i)
}










