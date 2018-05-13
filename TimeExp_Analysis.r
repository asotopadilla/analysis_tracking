# Time Experiment analysis

######## Parameters #########
minspeed <- 5 #minimum speed in pixels/frame to count number of frames spent moving
maxspeed <- 30 #maximum speed in pixels/frame fly can move before considering it a jump
numphases <- 60 #number of phases in experiment
phaseduration <- 1800 #duration of phase in frames
onlytrials <- TRUE #set to TRUE if only trial output wanted, FALSE if trials and poles output
fps <- 30 #video fps
width_px <- 777 #arena width in pixels (767 Box1; 777 Box2)
width_cm <- 7.5 #arena width in centimeters
sep="," #specify file separator

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply, gmodels)

#define functions
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
files <- list.files(pattern = "*.csv")

# Bind them into one data frame with a variable indication which video it comes from
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
  mutate(pole_l=case_when(pole=="L" ~ 0,
                          pole=="M" ~ width_px/3,
                          pole=="R" ~ 2*width_px/3),
         pole_r=case_when(pole=="L" ~ width_px/3,
                          pole=="M" ~ 2*width_px/3,
                          pole=="R" ~ width_px),
         safe_l=case_when(led_1_status==1 & led_2_status==0 & pole %in% c("M", "R") ~ 0,
                          led_1_status==1 & led_2_status==0 & pole %in% c("L") ~ width_px/3,
                          led_1_status==0 & led_2_status==1 & pole %in% c("L", "M") ~ 2*width_px/3,
                          led_1_status==0 & led_2_status==1 & pole %in% c("R") ~ width_px/3,
                          led_1_status==0 & led_2_status==0 ~ pole_l),
         safe_r=case_when(led_1_status==1 & led_2_status==0 & pole %in% c("M", "R") ~ width_px/3,
                          led_1_status==1 & led_2_status==0 & pole %in% c("L") ~ 2*width_px/3,
                          led_1_status==0 & led_2_status==1 & pole %in% c("L", "M") ~ width_px,
                          led_1_status==0 & led_2_status==1 & pole %in% c("R") ~ 2*width_px/3,
                          led_1_status==0 & led_2_status==0 ~ pole_r),
         fly_location=case_when(x>=pole_l & x<=pole_r ~ "pole",
                                x>=safe_l & x<=safe_r ~ "safe",
                                TRUE ~ "unsafe"),
         speed=cart_dist(x, lag(x), y, lag(y))) %>%
  group_by(video, phase) %>%
  mutate(step_num=ifelse(fly_location!=lag(fly_location), 1, 0),
         step_num=ifelse(row_number()==1, 1, step_num),
         step_num=cumsum(step_num)) %>%
  group_by(video, phase, phase_type, fly_location, step_num) %>%
  summarise(num_frames=n()) %>%
  group_by(video, phase, fly_location) %>%
  arrange(video, phase, fly_location, step_num) %>%
  mutate(location_num=row_number()) %>%
  group_by(video, phase) %>%
  mutate(total_frames=sum(num_frames)) %>%
  ungroup() %>%
  arrange(video, phase, step_num, fly_location) %>%
  select(video, phase, phase_type, fly_location, step_num, location_num, num_frames, total_frames)

## Run up to here and check variable df for a per phase, per location summary

df_out <- df %>%
  group_by(video, phase, phase_type) %>%
  mutate(time_in_pole_initial=ifelse(step_num==1 & fly_location=="pole", num_frames, NA),
         time_in_safe_initial=ifelse(step_num>1 & location_num==1 & fly_location=="safe", num_frames, NA),
         time_in_safe_total=ifelse(fly_location=="safe", num_frames, NA),
         reach_safe=ifelse(!is.na(time_in_safe_initial), step_num, NA),
         reach_safe=max(reach_safe, na.rm=TRUE),
         reach_safe=ifelse(is.infinite(reach_safe), 0, reach_safe),
         time_outside_safe_after_reaching=ifelse(reach_safe>0 & fly_location!="safe" & step_num>reach_safe, num_frames, NA),
         time_to_safe=ifelse(reach_safe>0 & fly_location!="safe" & step_num<reach_safe, num_frames, NA),
         start_in_pole=ifelse(step_num==1 & fly_location=="pole", 1, 0),
         first_to_safe=ifelse(step_num==2 & fly_location=="safe", 1, 0)) %>%
  select(-c(fly_location, step_num, location_num, num_frames, total_frames, reach_safe)) %>%
  summarise_all(funs(sum(., na.rm=TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(contains("time")), funs(round(./fps, 2)))

if (onlytrials){
  df_out <- filter(df_out, phase_type=="trial")
  }
  
write.table(df_out, "results/time_experiment_analysis_combined.csv", row.names = FALSE, sep=",")

for (i in 1:(NCOL(df_out)-3)) {
  df <- df_out[, c(1, 2, 3, i+3)] %>%
    spread(video, names(df_out)[i+3])
    
    write.table(df, paste0("results/time_experiment_analysis_", names(df_out)[i+3], ".csv"), row.names = FALSE, sep=",")
    
    rm(df)
}










