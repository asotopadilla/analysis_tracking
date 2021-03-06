# Group dynamic analysis

######## Parameters #########
minspeed <- 5 #minimum speed in pixels/frame to count number of frames spent moving
maxspeed <- 30 #maximum speed in pixels/frame fly can move before considering it a jump
minbouttime <- 15 #minimum number of frames to consider a bout
fps <- 30 #video fps
width_px <- 767 #arena width in pixels (767 Box1; 777 Box2; 730 flies in dark Box 2)
width_cm <- 7.5 #arena width in centimeters
height_cm <- 2.5 #arena height in centimeters
border_size <- 30 #width of border in pixels
focus_tile <- NA #choose tile to look at for speed (1 - Left, 2 - Middle, 3 - Right, NA - All)
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

if (!dir.exists(file.path(dir, "reults"))) dir.create(file.path(dir, "results"))

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
  mutate(Coord=ifelse(grepl("_x", Metric), "x", "y")) %>%
  select(-Metric) %>%
  unique() %>%
  spread(Coord, Value) %>%
  arrange(video, phase, frame_idx)

df_speed <- df %>%
  filter(x>=xfilter[1] & x<=xfilter[2]) %>%
  arrange(video, phase, frame_idx) %>%
  group_by(video) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         dist=ifelse(dist>=maxspeed, NA, dist),
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
  arrange(video, phase, frame_idx) %>%
  group_by(video) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         move=ifelse(dist>=minspeed & dist<=maxspeed, 1, 0)) %>%
  group_by(video, phase) %>%
  summarise(seconds_moving=sum(move, na.rm = TRUE)/fps) %>%
  ungroup()

df_bouts <- df %>%
  filter(x>=xfilter[1] & x<=xfilter[2]) %>%
  arrange(video, phase, frame_idx) %>%
  group_by(video, phase) %>%
  mutate(dist=cart_dist(x, lag(x), y, lag(y)),
         bout=ifelse(dist>=minspeed & dist<=maxspeed, 1, 0),
         bout=ifelse(is.na(bout), 0, bout),
         bout_grp=seq_grp(bout)) %>%
  group_by(video, phase, bout_grp) %>%
  mutate(bout_time=ifelse(n()>=minbouttime & bout_grp!=0, n(), NA),
         bouts=ifelse(is.na(bout_time), 0, bout_grp),
         bout_speed=ifelse(is.na(bout_time), NA, dist)) %>%
  group_by(video, phase) %>%
  summarise(num_bouts=NROW(unique(bouts))-1,
            mean_bout_time=mean(bout_time, na.rm = TRUE),
            min_bout_time=min(bout_time, na.rm = TRUE),
            max_bout_time=max(bout_time, na.rm = TRUE),
            mean_bout_speed=mean(bout_speed, na.rm = TRUE),
            min_bout_speed=min(bout_speed, na.rm = TRUE),
            max_bout_speed=max(bout_speed, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(mean_bout_time, min_bout_time, max_bout_time),
            funs(ifelse(is.infinite(.), NA, ./fps))) %>%
  mutate_at(vars(mean_bout_speed, min_bout_speed, max_bout_speed),
            funs(ifelse(is.infinite(.), NA, .*tocms))) %>%
  mutate_at(vars(num_bouts,
                 mean_bout_time, min_bout_time, max_bout_time,
                 mean_bout_speed, min_bout_speed, max_bout_speed),
            funs(ifelse(is.na(.), 0, .)))

df_borders <- df %>%
  mutate(border = case_when(x <= border_size ~ "border_left",
                            x >= width_px - border_size ~ "border_right",
                            y <= border_size ~ "border_top",
                            y >= height_px - border_size ~ "border_bottom",
                            TRUE ~ "out")) %>%
  filter(border != "out") %>%
  group_by(video, phase, border) %>%
  summarise(n=n()/fps) %>%
  group_by(video, phase) %>%
  mutate(border_total = sum(n)) %>%
  spread(border, n)

df_out <- left_join(df_seconds_moving, df_speed, by = c("video", "phase")) %>%
  left_join(., df_bouts, by = c("video", "phase")) %>%
  left_join(., df_borders, by = c("video", "phase"))

rm(df, df_speed, df_seconds_moving, df_bouts, df_borders)

write.table(df_out, "results/singlefly_analysis_combined.csv", row.names = FALSE, sep=",")

for (i in 1:(NCOL(df_out)-2)) {
  df <- df_out[, c(1, 2, i+2)] %>%
    spread(video, names(df_out)[i+2])
    
    write.table(df, paste0("results/singlefly_analysis_", names(df_out)[i+2], ".csv"), row.names = FALSE, sep=",")
    
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





