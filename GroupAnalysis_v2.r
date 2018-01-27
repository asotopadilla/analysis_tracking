# Group dynamic analysis

######## Parameters #########
mindist <- 10 #minimum distance in pixels to consider flie clustered
maxdist <- 20 #maximum distance in pixels to consider flies clustered
mintime <- 5 #minumum number of frames to consider the flies clustered
minspeed <- 2 #minimum speed i pixels/frame to count number of frames spent moving
  
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

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

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
  mutate(Coord=ifelse(grepl("_x", Metric), "x", "y"),
         fly=gsub("[^\\d]+", "", Metric, perl=TRUE)) %>%
  select(-Metric) %>%
  unique() %>%
  spread(Coord, Value) %>%
  arrange(video, phase, frame_idx, fly)

df_dists <- df %>%
  left_join(.,
            .[c("video", "frame_idx", "phase", "fly", "x", "y")],
            by = c("video", "phase", "frame_idx")) %>%
  filter(fly.x<fly.y) %>%
  mutate(dist=cart_dist(x.x, x.y, y.x, y.y)) %>%
  mutate_at(vars(fly.x, fly.y), funs(as.integer(.)+1)) %>%
  filter(dist>=mindist) %>%
  arrange(video, fly.x, fly.y, frame_idx) %>%
  group_by(video, phase) %>%
  mutate(grp=ifelse(dist>=mindist & dist<=maxdist, 1, 0),
         grp2=seq_grp(grp)) %>%
  group_by(video, phase, fly.x, fly.y, grp2) %>%
  mutate(grp_final=ifelse(n()>=mintime & grp2!=0, grp2, 0),
         n=n(),
         grp_n=ifelse(grp_final!=0 & row_number()==1, n(), NA)) %>%
  group_by(video, phase, fly.x) %>%
  mutate(min_dist=ifelse(row_number()==1, min(dist), NA),
         max_dist=ifelse(row_number()==1, max(dist), NA)) %>%
  group_by(video, phase) %>%
  summarise(encounters_num=NROW(unique(grp_final))-1,
            encounters_mean_length=mean(grp_n, na.rm=TRUE),
            encounters_median_length=median(grp_n, na.rm=TRUE),
            encounters_min_length=min(grp_n, na.rm=TRUE),
            encounters_max_length=max(grp_n, na.rm=TRUE),
            encounters_var_length=var(grp_n, na.rm=TRUE),
            encounters_sd_length=sd(grp_n, na.rm=TRUE),
            encounters_ci_mean_length=ci(t(grp_n), na.rm=TRUE)[1],
            encounters_ci_lower_length=ci(t(grp_n), na.rm=TRUE)[2],
            encounters_ci_upper_length=ci(t(grp_n), na.rm=TRUE)[3],
            encounters_ci_stderror_length=ci(t(grp_n), na.rm=TRUE)[4],
            dist_mean=mean(dist),
            dist_median=median(dist),
            dist_min=min(dist),
            dist_max=max(dist),
            dist_var=var(dist),
            dist_sd=sd(dist),
            dist_ci_mean=ci(t(dist))[1],
            dist_ci_lower=ci(t(dist))[2],
            dist_ci_upper=ci(t(dist))[3],
            dist_ci_stderror=ci(t(dist))[4],
            shortest_dist_mean=mean(min_dist, na.rm=TRUE),
            shortest_dist_median=median(min_dist, na.rm=TRUE),
            shortest_dist_min=min(min_dist, na.rm=TRUE),
            shortest_dist_max=max(min_dist, na.rm=TRUE),
            shortest_dist_var=var(min_dist, na.rm=TRUE),
            shortest_dist_sd=sd(min_dist, na.rm=TRUE),
            shortest_dist_ci_mean=ci(t(min_dist), na.rm=TRUE)[1],
            shortest_dist_ci_lower=ci(t(min_dist), na.rm=TRUE)[2],
            shortest_dist_ci_upper=ci(t(min_dist), na.rm=TRUE)[3],
            shortest_dist_ci_stderror=ci(t(min_dist), na.rm=TRUE)[4],
            longest_dist_mean=mean(max_dist, na.rm=TRUE),
            longest_dist_median=median(max_dist, na.rm=TRUE),
            longest_dist_min=min(max_dist, na.rm=TRUE),
            longest_dist_max=max(max_dist, na.rm=TRUE),
            longest_dist_var=var(max_dist, na.rm=TRUE),
            longest_dist_sd=sd(max_dist, na.rm=TRUE),
            longest_dist_ci_mean=ci(t(max_dist), na.rm=TRUE)[1],
            longest_dist_ci_lower=ci(t(max_dist), na.rm=TRUE)[2],
            longest_dist_ci_upper=ci(t(max_dist), na.rm=TRUE)[3],
            longest_dist_ci_stderror=ci(t(max_dist), na.rm=TRUE)[4]
            ) %>%
  ungroup() %>%
  mutate_at(vars(-video), funs(ifelse(is.nan(.) | is.infinite(.), NA, .)))

df_speed <- df %>%
  arrange(video, fly, phase, frame_idx) %>%
  group_by(video, fly) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         move=ifelse(dist>=minspeed, 1, 0)) %>%
  group_by(video, phase, fly) %>%
  summarise(frames_moving=sum(move, na.rm = TRUE)) %>%
  group_by(video, phase) %>%
  summarise(frames_moving_mean=mean(frames_moving),
            frames_moving_median=median(frames_moving),
            frames_moving_min=min(frames_moving),
            frames_moving_max=max(frames_moving),
            frames_moving_var=var(frames_moving),
            frames_moving_sd=sd(frames_moving),
            frames_moving_ci_mean=ci(t(frames_moving))[1],
            frames_moving_ci_lower=ci(t(frames_moving))[2],
            frames_moving_ci_upper=ci(t(frames_moving))[3],
            frames_moving_ci_stderror=ci(t(frames_moving))[4]) %>%
  ungroup()

df_out <- left_join(df_dists, df_speed, by = c("video", "phase"))

rm(df, df_dists, df_speed)

write.table(df_out, "group_analysis.csv", row.names = FALSE)






