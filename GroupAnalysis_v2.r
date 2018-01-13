# Group dynamic analysis

######## Parameters #########
mindist <- 10
maxdist <- 20
mintime <- 5
  
# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply, gmodels)

#define functions
cart_dist <- function(x1, x2, y1, y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)
}

x<-df1$grp

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
  arrange(video, phase, frame_idx, fly) %>%
  left_join(.,
            .[c("video", "frame_idx", "phase", "fly", "x", "y")],
            by = c("video", "phase", "frame_idx")) %>%
  filter(fly.x<fly.y) %>%
  mutate(dist=cart_dist(x.x, x.y, y.x, y.y)) %>%
  mutate_at(vars(fly.x, fly.y), funs(as.integer(.)+1)) %>%
  filter(dist>=mindist) %>%
  arrange(video, fly.x, fly.y, frame_idx) %>%
  group_by(video, phase, fly.x, fly.y) %>%
  mutate(grp=ifelse(dist>=mindist & dist<=maxdist, 1, 0),
         grp=seq_grp(grp)) %>%
  group_by(video, phase, fly.x, fly.y, grp) %>%
  mutate(grp_final=ifelse(n()>=mintime & grp!=0, grp, 0)) %>%
  group_by(video, phase) %>%
  summarise(num_encounters=NROW(unique(grp_final))-1,
            mean_dist=mean(dist),
            meadian_dist=median(dist),
            min_dist=min(dist),
            max_dist=max(dist),
            var_dist=var(dist),
            sd_dist=sd(dist),
            mean_shortest_dist=mean(min(dist)),
            median_shortest_dist=median(min(dist)),
            mean_longes_dist=mean(max(dist)),
            median_longest_dist=median(max(dist)),
            ci_mean_dist=ci(t(dist))[1],
            ci_lower_dist=ci(t(dist))[2],
            ci_upper_dist=ci(t(dist))[3],
            ci_stderror_dist=ci(t(dist))[4]
            ) %>%
  ungroup()

write.table(df, "group_analysis.csv", row.names = FALSE)






