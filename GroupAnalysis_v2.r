# Group dynamic analysis

######## Parameters #########
mindist <- 25 #minimum distance in pixels to consider flie clustered
maxdist <- 40 #maximum distance in pixels to consider flies clustered
mintime <- 15 #minumum number of frames to consider the flies clustered
minspeed <- 5 #minimum speed in pixels/frame to count number of frames spent moving
maxspeed <- 15 #maximum speed in pixels/frame fly can move before considering it a jump
minbouttime <- 15 #minimum number of frames to consider a bout
fps <- 30 #video fps
width_px <- 767 #arena width in pixels (777 Box 2; 730 dark Box2; 767 Box 1)
width_cm <- 7.5 #arena width in centimeters
height_cm <- 2.5 #arena height in centimeters
border_size <- 30 #width of border in pixels
focus_tile <- NA #choose tile to look at for speed (1 - Left, 2 - Middle, 3 - Right, NA - All)
sep="," #specify file separator

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply, gmodels, corrplot, ggcorrplot)

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
height_px <- height_cm*width_px/width_cm

if (is.na(focus_tile)) {
  xfilter <- c(0, width_px)
} else if (focus_tile==1) {
  xfilter <- c(0, width_px/3)
} else if (focus_tile==2){
  xfilter <- c(width_px/3, 2*width_px/3)
} else if (focus_tile==3) {
  xfilter <- c(2*width_px/3, width_px)
}

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

if (!dir.exists(file.path(dir, "results"))) dir.create(file.path(dir, "results"))

# Get all .csv files in chosen directory
files <- list.files(pattern = glob2rx("*.csv"))

# Bind them into one data frame with a variable indication which video it comes from
df <- (lapply(files, function(x) read.csv(x, sep=sep, stringsAsFactors = FALSE)))
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
  arrange(video, phase, frame_idx, fly) %>%
  mutate(filter=paste(video, phase, frame_idx, fly, sep = "_"))

df_filter <- df %>%
  mutate(frame_idx=frame_idx+1,
         frame_idx=ifelse(frame_idx<=0, NA, frame_idx)) %>%
  rename(prev_frame=frame_idx, fly_prev=fly, x_prev=x, y_prev=y) %>%
  left_join(df,
            .,
            by = c("video", "phase", "frame_idx"="prev_frame")) %>%
  mutate(dist=cart_dist(x, x_prev, y, y_prev)) %>%
  group_by(frame_idx, video, phase, fly) %>%
  arrange(fly, video, frame_idx) %>%
  filter(dist==min(dist) | is.na(dist)) %>%
  group_by(video, phase, fly) %>%
  mutate(different_match=ifelse(fly!=fly_prev, 1, 0),
         different_match=ifelse(lag(different_match)==1 | lead(different_match)==1, 1, different_match)) %>%
  filter(different_match==1) %>%
  ungroup()

df_dists <- df %>%
  filter(x>=xfilter[1] & x<=xfilter[2]) %>%
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
         grp=ifelse(filter %in% df_filter$filter, 0, grp),
         grp=seq_grp(grp)) %>%
  group_by(video, phase, fly.x, fly.y, grp) %>%
  mutate(grp_final=ifelse(n()>=mintime & grp!=0, grp, 0),
         n=n(),
         grp_n=ifelse(grp_final!=0 & row_number()==1, n(), NA)/fps) %>%
  group_by(video, phase, fly.x) %>%
  mutate(dist=ifelse(filter %in% df_filter$filter, NA, dist*tocm),
         min_dist=ifelse(row_number()==1, min(dist, na.rm=TRUE), NA),
         max_dist=ifelse(row_number()==1, max(dist, na.rm=TRUE), NA)) %>%
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
            dist_mean=mean(dist, na.rm=TRUE),
            dist_median=median(dist, na.rm=TRUE),
            dist_min=min(dist, na.rm=TRUE),
            dist_max=max(dist, na.rm=TRUE),
            dist_var=var(dist, na.rm=TRUE),
            dist_sd=sd(dist, na.rm=TRUE),
            dist_ci_mean=ci(t(dist), na.rm=TRUE)[1],
            dist_ci_lower=ci(t(dist), na.rm=TRUE)[2],
            dist_ci_upper=ci(t(dist), na.rm=TRUE)[3],
            dist_ci_stderror=ci(t(dist), na.rm=TRUE)[4],
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
  mutate_at(vars(-video), funs(ifelse(is.nan(.) | is.infinite(.), NA, .))) %>%
  mutate_at(vars(encounters_min_length, encounters_num),
            funs(ifelse(is.na(.), 0, .)))

df_speed <- df %>%
  filter(x>=xfilter[1] & x<=xfilter[2]) %>%
  arrange(video, fly, phase, frame_idx) %>%
  group_by(video, fly) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         dist=ifelse(filter %in% df_filter$filter | dist>=maxspeed, NA, dist),
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
  filter(x>=xfilter[1] & x<=xfilter[2]) %>%
  arrange(video, fly, phase, frame_idx) %>%
  group_by(video, fly) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         dist=ifelse(filter %in% df_filter$filter, NA, dist),
         move=ifelse(dist>=minspeed & dist<=maxspeed, 1, 0)) %>%
  group_by(video, phase, fly) %>%
  summarise(seconds_moving=sum(move, na.rm = TRUE)/fps) %>%
  group_by(video, phase) %>%
  summarise(seconds_moving_mean=mean(seconds_moving, na.rm=TRUE),
            seconds_moving_median=median(seconds_moving, na.rm=TRUE),
            seconds_moving_min=min(seconds_moving, na.rm=TRUE),
            seconds_moving_max=max(seconds_moving, na.rm=TRUE),
            seconds_moving_var=var(seconds_moving, na.rm=TRUE),
            seconds_moving_sd=sd(seconds_moving, na.rm=TRUE),
            seconds_moving_ci_mean=ci(t(seconds_moving), na.rm=TRUE)[1],
            seconds_moving_ci_lower=ci(t(seconds_moving), na.rm=TRUE)[2],
            seconds_moving_ci_upper=ci(t(seconds_moving), na.rm=TRUE)[3],
            seconds_moving_ci_stderror=ci(t(seconds_moving), na.rm=TRUE)[4]) %>%
  ungroup()

df_bouts <- df %>%
  filter(x>=xfilter[1] & x<=xfilter[2]) %>%
  arrange(video, phase, fly, frame_idx) %>%
  group_by(video, phase, fly) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         bout=ifelse(dist>=minspeed & dist<=maxspeed, 1, 0),
         bout=ifelse(is.na(bout), 0, bout),
         bout_grp=seq_grp(bout)) %>%
  group_by(video, phase, fly, bout_grp) %>%
  mutate(bout_time=ifelse(n()>=minbouttime & bout_grp!=0, n(), NA),
         bouts=ifelse(is.na(bout_time), 0, bout_grp),
         bout_speed=ifelse(is.na(bout_time), NA, dist)) %>%
  group_by(video, phase, fly) %>%
  summarise(mean_num_bouts=NROW(unique(bouts))-1,
            mean_mean_bout_time=mean(bout_time, na.rm = TRUE),
            mean_min_bout_time=min(bout_time, na.rm = TRUE),
            mean_max_bout_time=max(bout_time, na.rm = TRUE),
            mean_mean_bout_speed=mean(bout_speed, na.rm = TRUE),
            mean_min_bout_speed=min(bout_speed, na.rm = TRUE),
            mean_max_bout_speed=max(bout_speed, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(mean_mean_bout_time, mean_min_bout_time, mean_max_bout_time),
            funs(ifelse(is.infinite(.), NA, ./fps))) %>%
  mutate_at(vars(mean_mean_bout_speed, mean_min_bout_speed, mean_max_bout_speed),
            funs(ifelse(is.infinite(.), NA, .*tocms))) %>%
  select(-fly) %>%
  group_by(video, phase) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>%
  ungroup() %>%
  mutate_at(vars(mean_num_bouts,
                 mean_mean_bout_time, mean_min_bout_time, mean_max_bout_time,
                 mean_mean_bout_speed, mean_min_bout_speed, mean_max_bout_speed),
            funs(ifelse(is.na(.), 0, .)))

df_borders <- df %>%
  mutate(border = case_when(x <= border_size ~ "border_left",
                            x >= width_px - border_size ~ "border_right",
                            y <= border_size ~ "border_top",
                            y >= height_px - border_size ~ "border_bottom",
                            TRUE ~ "out")) %>%
  filter(border != "out") %>%
  group_by(video, phase, fly, border) %>%
  summarise(n=n()/fps) %>%
  group_by(video, phase, fly) %>%
  mutate(border_total = sum(n)) %>%
  spread(border, n) %>%
  group_by(video, phase) %>%
  select(-fly) %>%
  summarise_all(funs(mean(., na.rm = TRUE),
                     median(., na.rm = TRUE),
                     min(., na.rm = TRUE), max(., na.rm = TRUE),
                     var(., na.rm = TRUE),
                     sd(., na.rm = TRUE),
                     ci_mean=ci(t(.), na.rm=TRUE)[1],
                     ci_lower=ci(t(.), na.rm=TRUE)[2],
                     ci_upper=ci(t(.), na.rm=TRUE)[3],
                     ci_stderror=ci(t(.), na.rm=TRUE)[4])) %>%
  mutate_all(funs(ifelse(is.nan(.) | is.infinite(.), NA, .))) %>%
  ungroup()

df_out <- left_join(df_dists, df_speed, by = c("video", "phase")) %>%
  left_join(., df_seconds_moving, by = c("video", "phase")) %>%
  left_join(., df_bouts, by = c("video", "phase")) %>%
  left_join(., df_borders, by = c("video", "phase"))

rm(df, df_filter, df_dists, df_speed, df_seconds_moving, df_bouts, df_borders)

write.table(df_out, "results/group_analysis_combined.csv", row.names = FALSE, sep=",")

for (i in 1:(NCOL(df_out)-2)) {
  df <- df_out[, c(1, 2, i+2)] %>%
    spread(video, names(df_out)[i+2])
  
  write.table(df, paste0("results/group_analysis_", names(df_out)[i+2], ".csv"), row.names = FALSE, sep=",")
  
  rm(df)
}

###### Graph to fine tune parameters ######

col_name <- "encounters_num"
group_names <- c("TC_Grp_3_f", "TC_Grp_3_m")

df_plot <- df_out %>%
  select(one_of(c("video", "phase", col_name))) %>%
  filter(str_detect(video, paste(group_names, collapse = "|"))) %>%
  mutate(fly=gsub(paste(group_names, collapse = "|"), "",  video),
         video=stri_replace_last_fixed(video, fly, "")) %>%
  select(-fly)
colnames(df_plot) <- c("video", "phase", "value")

ggplot(df_plot, aes(x=phase, y=value, color=video)) +
  geom_point() +
  geom_smooth(method = 'loess')

###### end of graph ###### 

