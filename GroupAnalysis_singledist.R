# Get dist of single frame

### Run this chunck first ###
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo)

dir <- dirname(file.choose())
setwd(dir)

files <- list.files(pattern = "*.csv")

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
  arrange(video, phase, frame_idx, fly) %>%
  group_by(video, phase, frame_idx)
### ###

# Change frame_idx and video to select whichever frame for a video you want to calculate the distance for
df_single <- df %>%
  filter(frame_idx==73, video=="TC_Grp_6_f_2")

dist(data.frame(df_single$x, df_single$y))



