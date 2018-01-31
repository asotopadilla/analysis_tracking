# Group dynamic analysis

######## Parameters #########
minspeed <- 5 #minimum speed in pixels/frame to count number of frames spent moving
maxspeed <- 30 #maximum speed in pixels/frame fly can move before considering it a jump
fps <- 30 #video fps
width_px <- 777 #arena width in pixels
width_cm <- 7 #arena width in centimeters

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

tocms <- fps*width_cm/width_px
tocm <- width_cm/width_px

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

if (!dir.exists(file.path(dir, "reults"))) dir.create(file.path(dir, "results"))

# Get all .csv files in chosen directory
files <- list.files(pattern = "*.csv")

# Bind them into one data frame with a variable indication which video it comes from
df <- (lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
names(df) <- files
df <- do.call(rbind, df) %>%
  mutate(video=row.names(.),
         video=gsub("\\_output.*","",video)) %>%
  filter(led_1_status==1 | led_2_status==1)  %>%
  arrange(video, frame_idx) %>%
  group_by(video) %>%
  mutate(phase=ifelse(led_1_status!=lag(led_1_status),1, 0),
         phase=ifelse(row_number()==1, 1, phase),
         phase=cumsum(phase)) %>%
  ungroup() %>%
  select(-led_1_status, -led_2_status) %>%
  gather(Metric, Value, -video, -phase, -frame_idx) %>%
  mutate(Coord=ifelse(grepl("_x", Metric), "x", "y")) %>%
  select(-Metric) %>%
  unique() %>%
  spread(Coord, Value) %>%
  arrange(video, phase, frame_idx)

df_speed <- df %>%
  arrange(video, phase, frame_idx) %>%
  group_by(video) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         move=ifelse(dist>=minspeed, 1, 0),
         dist=dist*tocms) %>%
  group_by(video, phase) %>%
  summarise(speed_mean=mean(dist, na.rm=TRUE),
            speed_median=median(dist, na.rm=TRUE),
            speed_min=min(dist, na.rm=TRUE),
            speed_max=max(dist, na.rm=TRUE),
            speed_var=var(dist, na.rm=TRUE),
            speed_sd=sd(dist, na.rm=TRUE),
            speed_ci_mean=ci(t(dist), na.rm=TRUE)[1],
            speed_ci_lower=ci(t(dist), na.rm=TRUE)[2],
            speed_ci_upper=ci(t(dist), na.rm=TRUE)[3],
            speed_ci_stderror=ci(t(dist), na.rm=TRUE)[4]) %>%
  ungroup()

df_seconds_moving <- df %>%
  arrange(video, phase, frame_idx) %>%
  group_by(video) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         move=ifelse(dist>=minspeed & dist<=maxspeed, 1, 0)) %>%
  group_by(video, phase) %>%
  summarise(seconds_moving=sum(move, na.rm = TRUE)/fps) %>%
  ungroup()

df_out <- left_join(df_seconds_moving, df_speed, by = c("video", "phase"))

rm(df, df_speed, df_seconds_moving)

write.table(df_out, "results/singlefly_analysis_combined.csv", row.names = FALSE, sep=",")

for (i in 1:(NCOL(df_out)-2)) {
  df <- df_out[, c(1, 2, i+2)] %>%
    spread(video, names(df_out)[i+2])
    
    write.table(df, paste0("results/singlefly_analysis_", names(df_out)[i+2], ".csv"), row.names = FALSE, sep=",")
    
    rm(df)
}









