pacman::p_load(devtools)
install_github("AppliedDataSciencePartners/xgboostExplainer")

library(xgboostExplainer)

pacman::p_load(tidyverse, xgboost, caret, stringi)

# Find where files to be analyzed live
dir <- dirname(file.choose())
setwd(dir)

# Get all .csv files in chosen directory
files <- list.files(pattern = glob2rx("*.csv"))

df <- (lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

single <- df[[2]] %>% select(video, phase, speed_mean, seconds_moving, num_bouts, mean_bout_time, border_total)
group <- df[[1]] %>% select(video, phase, speed_mean, seconds_moving_mean, mean_num_bouts, mean_mean_bout_time, border_total_mean)
colnames(group) <- colnames(single)

group_names <- c("TCD_1_1_f", "TCD_1_1_m", "TCD_1_3_f", "TCD_1_3_m", "TCD_3_1_f", "TCD_3_1_m", "TCD_3_3_f", "TCD_3_3_m")
vars <- c("phase", "speed_mean", "seconds_moving", "num_bouts", "mean_bout_time", "border_total")

df <- rbind(single, group) %>%
  arrange(video, phase) %>%
  mutate(fly=gsub(paste(group_names, collapse = "|"), "",  video),
         video=stri_replace_last_fixed(video, fly, "")) %>%
  select(-fly) %>%
  mutate(target = group_indices(., video)-1)

rm(group, single)

set.seed(100)

index <- createDataPartition(df$target, p=0.75, list=FALSE)
train <- df[ index,]
test <- df[-index,]

xgb.train.data = xgb.DMatrix(data.matrix(train[, vars]),
                             label = train$target,
                             missing = NA)

xgb.test.data <- xgb.DMatrix(data.matrix(test[, vars]),
                             missing = NA)

xgb.model <- xgboost(param = list(objective = "multi:softmax"),
                     num_class = length(group_names),
                     data = xgb.train.data,
                     nrounds = 1500,
                     early_stopping_rounds = 100,
                     metrics='rmse')

#### Xgb importance
col_names <- attr(xgb.train.data, ".Dimnames")[[2]]
imp <- xgb.importance(col_names, xgb.model)
xgb.plot.importance(imp)

#### The XGBoost Explainer

explainer <- buildExplainer(xgb.model, xgb.train.data, type="multi:softmax")

pred.breakdown <- explainPredictions(xgb.model, explainer, xgb.test.data)

i <- 5
test[i,]

showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test[, vars]) , i, type = "multi:softmax") +
  xlab("") +
  theme_bw()

####### IMPACT AGAINST VARIABLE VALUE
ggplot(test, aes(x=test$speed_mean, y=pred.breakdown$speed_mean)) +
  geom_point() +
  ylab("Impact on log-odds") +
  theme_bw()
