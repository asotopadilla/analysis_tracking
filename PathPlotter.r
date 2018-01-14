# Path plotter

######## Parameters #########
phases <- c(1, 3, 5:10) ### phases to plot
arena <- c(777, 243)
maxdist <- 10000 ### max distance for a step to be plotted. Steps between frames which are larger than this distance will not be plotted. Set to a very large number to ignore it.

# Load required packages
if (!require(pacman)) install.packages(pacman)
pacman::p_load(tidyverse, here, stringr, zoo, pbapply, gmodels, ggthemes)

#define functions
cart_dist <- function(x1, x2, y1, y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)
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
  group_by(video, phase, fly) %>%
  mutate(prevx=lag(x),
         prevy=lag(y),
         dist=cart_dist(x, prevx, y, prevy),
         frame_idx=frame_idx-min(frame_idx),
         frame_idx=frame_idx/max(frame_idx)) %>%
  filter(phase %in% phases)

graphs <- df %>%
  select(video, phase, fly) %>%
  unique()

for (i in 1:NROW(graphs)) {
  print(i)
  plot_df <- right_join(df, graphs[i,], by = c("video", "phase", "fly")) %>% filter(dist<=maxdist)
  ggplot(plot_df, aes(x=x, y=y, color=frame_idx, fill=frame_idx, alpha=frame_idx, size=frame_idx*0.01)) +
    geom_vline(xintercept = c(arena[1]/3, 2*arena[1]/3), color="gray") +
    geom_point() +
    geom_segment(aes(x = prevx, y = prevy, xend = x, yend = y)) +
    ggtitle(paste0("Video: ", graphs[i, "video"], " / Phase: ", graphs[i, "phase"], " / FLy: ", graphs[i, "fly"])) +
    scale_size_continuous(range = c(0.1, 2)) +
    xlim(0, arena[1]) +
    ylim(0, arena[2]) +
    xlab("") +
    ylab("") +
    theme_base() +
    theme(legend.position = "none")
  ggsave(filename=paste0(graphs[i, "video"], "_", graphs[i, "phase"], "_", graphs[i, "fly"], ".png"), width = arena[1]*0.5, height = arena[2]*0.5, units = "mm")
  rm(plot_df)
}









