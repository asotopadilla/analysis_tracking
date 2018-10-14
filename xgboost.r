xgb.train.data = xgb.DMatrix(data.matrix(train[, vars]),
                             label = train$rp,
                             missing = NA)

xgb.test.data <- xgb.DMatrix(data.matrix(test[, vars]),
                             missing = NA)

xgb.model <- xgboost(param = list(objective = "reg:linear"), 
                     data = xgb.train.data,
                     nrounds = 1500,
                     early_stopping_rounds = 100,
                     metrics='rmse')

#### Xgb importance
col_names <- attr(xgb.train.data, ".Dimnames")[[2]]
imp <- xgb.importance(col_names, xgb.model)
xgb.plot.importance(imp)

#### THE XGBoost Explainer

explainer <- buildExplainer(xgb.model, xgb.train.data, type="regression", n_first_tree = xgb.model$best_ntreelimit - 1)

pred.breakdown <- explainPredictions(xgb.model, explainer, xgb.test.data)

i <- 3
test[i,]

showWaterfall(xgb.model, explainer, xgb.test.data, data.matrix(test[, vars]) , i, type = "regression") +
  xlab("") +
  ylab("Rp Prediction Contribution") +
  theme_bw()

ggsave(file=file.path(paths$reports, "images", "rp_waterfall.png"), width = 14, height = 11, units = "cm")

####### IMPACT AGAINST VARIABLE VALUE
ggplot(test, aes(x=test$inz_stc, y=pred.breakdown$inz_stc)) +
  geom_point() +
  xlab("Steel Code") +
  ylab("Impact on log-odds") +
  theme_bw()

ggsave(file=file.path(paths$reports, "images", "rp_xgb_sc.png"), width = 14, height = 11, units = "cm")

ggplot(test, aes(x=test$cus_diam, y=pred.breakdown$cus_diam)) +
  geom_point() +
  xlab("Diameter") +
  ylab("Impact on log-odds") +
  theme_bw()

ggsave(file=file.path(paths$reports, "images", "rp_xgb_od.png"), width = 14, height = 11, units = "cm")

ggplot(test, aes(x=test$gauge, y=pred.breakdown$gauge)) +
  geom_point() +
  xlab("Gauge") +
  ylab("Impact on log-odds") +
  theme_bw()

ggsave(file=file.path(paths$reports, "images", "rp_xgc_t.png"), width = 14, height = 11, units = "cm")

ggplot(train, aes(x=cus_diam, y=reduction)) +
geom_point()