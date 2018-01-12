# Group dynamic analysis

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply)

#define functions
cart_dist <- function(x1, x2, y1, y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)
}

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

# Get all .csv files in chosen directory
files <- list.files(pattern = "*ted.csv")

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
  arrange(video, phase, frame_idx, fly) %>%
  left_join(.,
            .[c("video", "frame_idx", "phase", "fly", "x", "y")],
            by = c("video", "phase", "frame_idx")) %>%
  filter(fly.x<fly.y) %>%
  mutate(dist=cart_dist(x.x, x.y, y.x, y.y)) %>%
  mutate_at(vars(fly.x, fly.y), funs(as.integer(.)+1)) %>%
  unite("pair", c("fly.x", "fly.y"), sep="_") %>%
  select(video, frame_idx, phase, pair, dist) %>%
  spread(pair, dist)

write.table(df, "group_pair_dists.csv", row.names = FALSE)

