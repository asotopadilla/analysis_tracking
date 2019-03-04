# Time Experiment analysis

######## Input Parameters #########
numphases <- 60 #number of phases in experiment
phaseduration <- 1800 #duration of phase in frames
minphaseduration <- 1780 #Fix phases with length lower than this number of frames. This fixed problems with phases where LED will register as off for a few frames
dead_distance <- 20 #Max number of pixels a fly moves in a phase to consider it dead
maxspeed <- 15 #maximum speed in pixels/frame fly can move before considering it a jump
num_phases_for_dead <- 2 #Number of phases to consider fly dead if it doesn't move
fps <- 30 #video fps
width_px <- 777 #arena width in pixels
width_cm <- 7.5 #arena width in centimeters
sep="," #specify file separator

only_trials <- TRUE #set to TRUE if only trial output wanted, FALSE if trials and poles output
remove_not_in_pole <- TRUE #Set to TRUE to change values for flies that don't start in pole to NA
remove_dead <- TRUE #Set to true to change values from dead flies to NA

######## Output Variables #########
# video - Video name
# phase - Phase number
# phase_type - Indicates whether phase is a Pole or Trial
# pole_location - Which tile is the pole
# safe_location - Location of the safe tile in the current phase
# start_position - Which tile does fly start phase in
# time_in_pole_initial - Time in pole before getting out. Only counts is start in the pole position
# time_in_safe_initial - Time in safe first time entered. Only counts if not starting in safe tile already
# time_in_safe_total - Total time in safe tile
# time_outside_safe_after_reaching - Time outside safe tile after reaching it
# time_to_safe - Time before entering the safe tile for the first time. Only counts if not starting in safe tile already
# start_in_pole - Fly starte in pole position (1) or not (0)
# first_to_safe - First tile fly went to is safe (1) or not (0). Only counts when pole is middle tile
# first_to_closest - First tile fly went to is closest it was to in start of phase (1) or not (0). Only counts is start in the pole position
# speed_to_safe - means speed to reach safe tile in cm/s
# dead_fly - (1) if fly died in the experiment or (0) otherwise
###################################

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, zoo)

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

#Cartesian distance function
cart_dist <- function(x1, x2, y1, y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)
}

seq_grp <- function(x){
  grp<-rle(x) %>%
    do.call("cbind", .) %>%
    as.data.frame() %>%
    mutate(values2=cumsum(values),
           values=ifelse(values!=0, values2, 0)) %>%
    select(-values2)
  
  return(rep(grp$values, grp$lengths))
}

tocms <- fps*width_cm/width_px
tocm <- width_cm/width_px

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
         phase_type=ifelse(led_1_status==0 & led_2_status==0, "pole", "trial"))

phase_fix <- df %>%
  group_by(video, phase_type, phase) %>%
  summarise(n=n()) %>%
  arrange(video, phase) %>%
  group_by(video) %>%
  mutate(phase_fix=ifelse(n<=minphaseduration & (lag(n)<=minphaseduration | lead(n)<=minphaseduration), 1, 0),
         phase_num=seq_grp(phase_fix)) %>%
  group_by(video, phase_num) %>%
  mutate(phase_new=ifelse(phase_fix==0, phase, min(phase))) %>%
  group_by(video, phase_new) %>%
  cbind(., phase_fixed=group_indices(.)) %>%
  group_by(video) %>%
  mutate(phase_fixed=phase_fixed-min(phase_fixed)+1) %>%
  group_by(video, phase_fixed) %>%
  mutate(phase_type=ifelse(phase==phase_new | is.na(phase_new), phase_type, NA),
         phase_type=na.locf(phase_type)) %>%
  ungroup() %>%
  select(video, phase, phase_type, phase_fixed)

df <- df %>%
  select(-phase_type) %>%
  left_join(., phase_fix, by=c("video", "phase")) %>%
  select(-phase) %>%
  rename(phase=phase_fixed) %>%
  filter(phase<=numphases) %>%
  group_by(video, phase) %>%
  mutate(rm_phase=ifelse(phase==1 | phase>numphases | (phase==numphases & row_number()>=phaseduration), 1, 0),
         rm_frame=ifelse(row_number()<(n()-phaseduration) | (phase==numphases & row_number()>=phaseduration), 1, 0),
         led_1_status=ifelse(max(led_1_status==1), 1, 0),
         led_2_status=ifelse(max(led_2_status==1), 1, 0)) %>%
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
                                TRUE ~ "unsafe"),
         fly_tile=case_when(x < width_px/3 ~ "L",
                            x > 2*width_px/3 ~ "R",
                            TRUE ~ "M")) %>%
  group_by(video, phase) %>%
  mutate(closest_tile=case_when(row_number()==1 & (x <= width_px/3 | x >= 2*width_px/3) ~ "M",
                                row_number()==1 & x > width_px/3 & x < width_px/2 ~ "L",
                                row_number()==1 & x < 2*width_px/3 & x > width_px/2 ~ "R",
                                TRUE ~ as.character(NA)),
         closest_tile=na.locf(closest_tile, na.rm = FALSE),
         step_num=ifelse(fly_location!=lag(fly_location), 1, 0),
         step_num=ifelse(row_number()==1, 1, step_num),
         step_num=cumsum(step_num),
         dist=cart_dist(x, lag(x), y, lag(y)),
         dist=ifelse(dist > maxspeed, NA, dist)) %>%
  group_by(video, phase, phase_type, pole, safe_location, fly_location, closest_tile, step_num) %>%
  mutate(fly_tile=ifelse(row_number()==1, fly_tile, NA),
         fly_tile=na.locf(fly_tile, na.rm = FALSE)) %>%
  group_by(video, phase, phase_type, pole, safe_location, fly_location, fly_tile, closest_tile, step_num) %>%
  summarise(num_frames=n(), dist=sum(dist, na.rm = TRUE)) %>%
  group_by(video, phase, fly_location) %>%
  arrange(video, phase, fly_location, step_num) %>%
  mutate(location_num=row_number()) %>%
  group_by(video, phase) %>%
  mutate(total_frames=sum(num_frames),
         start_position=ifelse(step_num==1, fly_location, NA),
         start_position=max(start_position, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(video, phase, step_num, fly_location) %>%
  select(video, phase, phase_type, pole, safe_location, start_position, closest_tile, fly_location,
         fly_tile, step_num, location_num, num_frames, total_frames, dist)

rm(phase_fix)

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
         first_to_closest=ifelse(step_num==2 & fly_tile==closest_tile, 1, 0),
         safe_location=case_when(pole=="M" & safe_location=="L" ~ "left",
                                 pole=="M" & safe_location=="R" ~ "right",
                                 pole=="L" & safe_location=="L" ~ "short",
                                 pole=="L" & safe_location=="R" ~ "long",
                                 pole=="R" & safe_location=="L" ~ "long",
                                 pole=="R" & safe_location=="R" ~ "short",
                                 safe_location=="P" ~ "pole",
                                 TRUE ~ safe_location),
         dist_to_safe=ifelse(step_num<reach_safe, dist, 0)) %>%
  select(-c(fly_location, fly_tile, closest_tile, step_num, location_num, num_frames, total_frames, reach_safe)) %>%
  group_by(video, phase, phase_type, pole, safe_location, start_position) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>%
  mutate(first_to_safe=ifelse(pole!="M", NA, first_to_safe)) %>%
  mutate_at(vars(time_to_safe, time_outside_safe_after_reaching),
            funs(ifelse(time_in_safe_total==0, NA, .))) %>%
  mutate(speed_to_safe = (dist_to_safe/time_to_safe)*tocms) %>%
  group_by(video) %>%
  arrange(video, phase) %>%
  mutate(dead=ifelse(dist<=dead_distance & lag(dist)<=dead_distance, 1, 0),
         dead_grp=seq_grp(dead)) %>%
  group_by(video, dead_grp) %>%
  mutate(dead_time=ifelse(dead==1, n(), 0)) %>%
  group_by(video) %>%
  mutate(dead_fly=ifelse(max(dead_time)>=num_phases_for_dead, 1, 0))

# Check df_out to see the dead fly variables

df_out <- df_out %>%
  select(-c(dist, dist_to_safe, dead, dead_grp, dead_time)) %>%
  ungroup() %>%
  mutate_at(vars(contains("time")), funs(round(./fps, 2))) %>%
  rename(pole_location=pole)

# Save file with list of videos with dead flies
df_dead <- df_out %>%
  filter(dead_fly==1) %>%
  select(video) %>%
  unique()

write.table(df_dead, "results/dead_fly_videos.csv", row.names = FALSE, sep=",")

# If only_trials is true, remove pole phases from data
if (only_trials){
  df_out <- filter(df_out, phase_type=="trial")
  }

# If remove_not_in_pole is TRUE, set results for flies that don't start in pole to NA
if (remove_not_in_pole==TRUE){
  df_out <- df_out %>%
    mutate_at(vars(time_in_safe_initial, time_in_safe_total, time_outside_safe_after_reaching, time_to_safe, first_to_safe, first_to_closest),
              funs(ifelse(phase_type=="trial" & start_position=="pole", ., NA)))
}

# If remove_dead is TRUE, set results for flies that are dead to NA
if (remove_not_in_pole==TRUE){
  df_out <- df_out %>%
    mutate_at(vars(start_position, time_in_pole_initial, time_in_safe_initial, time_in_safe_total,
                   time_outside_safe_after_reaching, time_to_safe, start_in_pole, first_to_safe, first_to_closest),
              funs(ifelse(dead_fly==1, NA, .)))
}
  
# Save combined videos and variables output
write.table(df_out, "results/time_experiment_analysis_combined.csv", row.names = FALSE, sep=",")

# Save one file per variable with videos as columns
df_out <- df_out %>%
  mutate(safe_location=ifelse(safe_location %in% c("left", "right"), "leftright", safe_location))

safe_locs <- unique(df_out$safe_location)

for (i in seq_along(safe_locs)) {
  for (j in 1:(NCOL(df_out)-6)) {
    df_sep <- df_out %>%
      filter(safe_location==safe_locs[i]) %>%
      select(1, 2, 3, j+5) %>%
      spread(video, names(df_out)[j+5])
    
    write.table(df_sep, paste0("results/", safe_locs[i], "_time_experiment_analysis_", names(df_out)[j+5], ".csv"), row.names = FALSE, sep=",")
    
    rm(df_sep, j)
  }
}

