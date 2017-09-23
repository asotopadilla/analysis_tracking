# Group dynamic analysis

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo)

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
         video=gsub("\\_output.*","",video))

# Cacultate variables per phase
df <- df %>%
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
  group_by(video, phase, frame_idx) %>%
  summarise(mean_dist=mean(dist(data.frame(x,y))),
         meadian_dist=median(dist(data.frame(x,y))),
         mean_shortest_dist=dist(data.frame(x,y), upper = TRUE) %>%
           as.matrix() %>%
           as.data.frame() %>%
           mutate_all(funs(ifelse(.==0, NA, .))) %>%
           summarise_all(funs(min(., na.rm=TRUE))) %>%
           as.numeric() %>% mean(),
         median_shortest_dist=dist(data.frame(x,y), upper = TRUE) %>%
           as.matrix() %>%
           as.data.frame() %>%
           mutate_all(funs(ifelse(.==0, NA, .))) %>%
           summarise_all(funs(min(., na.rm=TRUE))) %>%
           as.numeric() %>% median(),
         mean_longest_dist=dist(data.frame(x,y), upper = TRUE) %>%
           as.matrix() %>%
           as.data.frame() %>%
           mutate_all(funs(ifelse(.==0, NA, .))) %>%
           summarise_all(funs(max(., na.rm=TRUE))) %>%
           as.numeric() %>% mean(),
         median_longest_dist=dist(data.frame(x,y), upper = TRUE) %>%
           as.matrix() %>%
           as.data.frame() %>%
           mutate_all(funs(ifelse(.==0, NA, .))) %>%
           summarise_all(funs(max(., na.rm=TRUE))) %>%
           as.numeric() %>% median()
         ) %>%
  group_by(video, phase) %>%
  summarise(mean_dist=mean(mean_dist),
            meadian_dist=median(meadian_dist),
            mean_shortest_dist=mean(mean_shortest_dist),
            median_shortest_dist=median(median_shortest_dist),
            mean_longest_dist=mean(mean_longest_dist),
            median_longest_dist=median(median_longest_dist)
            ) %>%
  ungroup()

write.table(df, "group_analysis.csv", row.names = FALSE)






