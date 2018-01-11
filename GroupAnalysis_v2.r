# Group dynamic analysis

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply)

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

grps <- unique(df[, c("video", "phase", "frame_idx")])

system.time(
df2 <- pblapply(1:1000, function(x){
  left_join(grps[x,], df, by = c("video", "phase", "frame_idx")) %>%
    select(x, y) %>%
    dist() %>% 
    as.matrix() %>%
    as.data.frame() %>%
    mutate(fly_1=row_number()) %>%
    gather(fly_2, dist, -fly_1) %>%
    filter(fly_1<fly_2) %>%
    arrange(fly_1, fly_2) %>%
    unite("pair", c("fly_1", "fly_2"), sep="_") %>%
    spread(pair, dist) %>%
    cbind(grps[x,], .)
}) %>%
  do.call("rbind", .)
)


write.table(df, "group_analysis.csv", row.names = FALSE)






