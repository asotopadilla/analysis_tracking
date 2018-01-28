# Group dynamic analysis

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply, FSA)

#define functions
cart_dist <- function(x1, x2, y1, y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)
}

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

# Get all .csv files in chosen directory
files <- list.files(pattern = "*t.csv")

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

df1 <- df %>%
  mutate(prev_frame=frame_idx-1,
         prev_frame=ifelse(prev_frame<=0, NA, prev_frame)) %>%
  left_join(.,
            .[c("video", "phase", "fly", "x", "y", "prev_frame")] %>%
              rename(fly_prev=fly, x_prev=x, y_prev=y),
            by = c("video", "phase", "frame_idx"="prev_frame")) %>%
  mutate(dist=cart_dist(x, x_prev, y, y_prev)) %>%
  select(-prev_frame) %>%
  group_by(frame_idx, video, phase, fly) %>%
  arrange(fly, video, frame_idx) %>%
  filter(dist==min(dist) | is.na(dist)) %>%
  mutate(mutilple_matches=ifelse(n()>1, 1, 0),
         different_match=ifelse(fly!=fly_prev, 1, 0))

df2 <- df1 %>%
  select(-x_prev, -y_prev) %>%
  filter(fly==2) %>%
  group_by(video, fly) %>%
  mutate(remove=cumsum(different_match),
         fly_close=ifelse(different_match, fly_prev, NA),
         fly_close=na.locf(fly_close, na.rm = FALSE)) %>%
  left_join(.,
            df %>% rename(fly2=fly, x2=x, y2=y),
            by = c("video", "phase", "frame_idx")) %>%
  filter(fly2==fly_close) %>%
  mutate(dist2=cart_dist(x, x2, y, y2)) %>%
  select(-x2, -y2)
  



write.table(df1, "fly_dists.csv", row.names = FALSE)

