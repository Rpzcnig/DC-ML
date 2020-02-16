#模型建立。
library(tidyverse)
library(xgboost)

df2 <- df1
df2 <- select(df2, -c('highestEdu', 'bankCard', 'id'))
# rF_data <- cbind(df2[1:nrow(tt), ], tt[, 'target'])
# randomForest_model <- randomForest(target~ ., 
#                                    data = rF_data, 
#                                    importance = T,
#                                    ntree = 100)
 


#通过IV值和相关性进行特征选择
df2_IV <- IV(cbind(df2[1:nrow(tt), ], tt[, 'target']))
data.frame(name = names(df2), value = df2_IV) %>% arrange(value)
# name        value
# 1             dist_encode 0.0000000000
# 2           certId_encode 0.0000000000
# 3   certValidBegin_encode 0.0000000000
# 4    certValidStop_encode 0.0000000000
# 5                  gender 0.0003992621
# 6                     age 0.0084777475
# 7           X5yearBadloan 0.0148397038
# 8                     x_0 0.0148826695
df2 <- select(df2, -c('gender'))

#建立Xgboost模型

#准备数据
set.seed(1254)
model_train = df2[1:nrow(tt), ]
model_test = df2[(nrow(tt)+ 1): nrow(df2), ]  #测试数据
ind = sample(nrow(tt), size = 0.8*nrow(tt))
dtrain = cbind(tt[ind, 'target'], model_train[ind, ])
dtest = cbind(tt[-ind, 'target'], model_train[-ind, ])
dtrain = xgb.DMatrix(data = data.matrix(dtrain[, -1]),
                     label = dtrain$target)
dtest = xgb.DMatrix(data = data.matrix(dtest[, -1]),
                    label = dtest$target)
watchlist = list(train = dtrain,
                 test = dtest)  #xgb模型输入数据

d_test = xgb.DMatrix(data = data.matrix(model_test))


#开始使用grid search调参
eta = c(0.01, 0.05, 0.09)
nrounds = 100
max_depth = seq(4, 8, 2)
min_child_weight = seq(20, 35, 5)
gamma = seq(1.6, 3.2, 0.4)
subsample = c(0.5, 0.9, 0.2)
colsample_bytree = c(0.6, 0.9, 0.1)
#expand.grid实现穷举
hype_grid = expand.grid(
  eta = eta, #0.05
  nrounds =  nrounds,
  max_depth = max_depth,
  gamma = gamma, #2.2
  min_child_weight = seq(20, 35, 5),
  subsample = subsample,
  colsample_bytree = colsample_bytree
)
xgb_model_grid <- function(eta = eta, 
                           nrounds = nrounds,
                           max_depth = max_depth,
                           gamma = gamma,
                           min_child_weight = min_child_weight,
                           subsample = subsample,
                           colsample_bytree = colsample_bytree
                          ) {
  set.seed(1239)
  xgb_mod <- xgb.train(
                       data = dtrain,
                       #1
                       eta = eta,
                       nrounds = 100,
                       #2
                       max_depth = max_depth,
                       gamma = gamma,
                       min_child_weight = min_child_weight,
                       #3
                       subsample = subsample,
                       colsample_bytree = colsample_bytree,
                       #4
                       max_delta_step = 5,
                       lamda = 5,
                       #其他
                       objective = 'binary:logistic', #hinge,直接输出0/1
                       eval_metric = 'auc',
                       scale_pos_weight = 4,
                       early_stopping_rounds = 100,
                       seed = 138,
                       nflod = 5,
                       watchlist = watchlist,
                       nthread = 4
  )
  
  #参照参数，调整。
  # bind_cols(
  #   xgb_mod$evaluation_log %>% 
  #     dplyr::slice(c(xgb_mod$best_iteration)),
  #   xgb_mod$params
  # )
  
  bind_cols(
    best_score = xgb_mod$best_score,
    niter = xgb_mod$niter,
    besst_iteration = xgb_mod$best_iteration
  )
}

set.seed(1239)
find_auc <- hype_grid %>% 
  mutate(
    mod = pmap(
    list(
      eta = hype_grid$eta,
      nrounds = hype_grid$nrounds,
      max_depth = hype_grid$max_depth,
      gamma = hype_grid$gamma,
      min_child_weight = hype_grid$min_child_weight,
      subsample = hype_grid$subsample,
      colsample_bytree = hype_grid$colsample_bytree
        ),
    xgb_model_grid
              )
    ) 
top_auc <- find_auc %>% 
  unnest(mod) %>% 
  top_n(100, wt = best_score)

#特征重要性topN
weight <- sum(tt$target == 0)/sum(tt$target == 1)  #0.007316701
xgb <- xgb.train(
  data = dtrain,
  eta = 0.05,
  nrounds = 1000,
  max_depth = 5,
  gamma = 2.2,
  subsample = 0.7,
  colsample_bytree = 0.8,
  min_child_weight = 25, #32
  max_delta_step = 6.3,
  lamda = 4,
  objective = 'binary:logistic', #hinge,直接输出0/1
  eval_metric = 'auc',
  scale_pos_weight = 4.6,
  early_stopping_rounds = 300,
  seed = 1238,
  nflod = 5,
  watchlist = watchlist,
  nthread = 4
)
importance_matrix = xgb.importance(xgb, feature_names = xgb$feature_names)
xgb.plot.importance(importance_matrix)
df2 <- df2[, names(df2) %in% importance_matrix[[1]]]

#分层抽样

model_train = df2[1:nrow(tt), ]
model_test = df2[(nrow(tt)+ 1): nrow(df2), ]  #测试数据 

set.seed(7860)
ind1_train <- sample(which(tt$target == 1), size = 0.8*sum(tt$target == 1))
ind1_test <- which(tt[-ind1_train, target == 1])
ind0_train <- sample(which(tt$target == 0), size = 0.8*sum(tt$target == 0))
ind0_test <- which(tt[-ind0_train, target == 0])
ind_train <- c(ind1_train, ind0_train) %>% sample()  #重新再随机选一编。
ind_test <- c(ind1_test, ind0_test) %>% sample()


dtrain = cbind(tt[ind_train, 'target'], model_train[ind_train, ])
dtest = cbind(tt[ind_test, 'target'], model_train[ind_test, ])
dtrain = xgb.DMatrix(data = data.matrix(dtrain[, -1]),
                     label = dtrain$target)
dtest = xgb.DMatrix(data = data.matrix(dtest[, -1]),
                    label = dtest$target)
watchlist = list(train = dtrain,
                 test = dtest)  #xgb模型输入数据
d_test = xgb.DMatrix(data = data.matrix(model_test))

weight <- sum(tt$target == 0)/sum(tt$target == 1)  #136.6736
xgb_cv <- xgb.cv(  #84%
  data = dtrain,
  eta = 0.01,
  nrounds = 2300,
  max_depth = 8,
  gamma = 0.7,
  subsample = 0.85,
  colsample_bytree = 0.95,
  nfold = 5,
  max_delta_step = 4,
  min_child_weight = 4,
  objective = 'binary:logistic', #hinge,直接输出0/1
  eval_metric = 'auc',
  scale_pos_weight = 1.4,
  early_stopping_rounds = 100,
  seed = 1234,
  watchlist = watchlist,
  nthread = 4,
  prediction = T
)


xgb <- xgb.train(
  data = dtrain,
  eta = 0.01,
  nrounds = 550,
  max_depth = 8,
  gamma = 0.7,
  subsample = 0.8,
  colsample_bytree = 0.95,
  min_child_weight = 4,
  max_delta_step = 4,
  #lambda = weight,
  objective = 'binary:logistic', #hinge,直接输出0/1
  eval_metric = 'auc',
  scale_pos_weight = 1.4,
  early_stopping_rounds = 1000,
  seed = 1235,
  watchlist = watchlist,
  nthread = 4
)

result <- predict(xgb, d_test) %>% data.frame(id = test$id, 
                                    target = .)
write.csv(result, 'res1.csv', row.names = F)

xgb.importance(model = xgb, feature_names = xgb$feature_names)





#0212
model_train = df2[1:nrow(tt), ]
model_test = df2[(nrow(tt)+ 1): nrow(df2), ]  #测试数据

set.seed(02123)
ind = sample(nrow(tt), size = 0.8*nrow(tt))
dtrain = cbind(tt[ind, 'target'], model_train[ind, ])
dtest = cbind(tt[-ind, 'target'], model_train[-ind, ])
dtrain = xgb.DMatrix(data = data.matrix(dtrain[, -1]),
                     label = dtrain$target)
dtest = xgb.DMatrix(data = data.matrix(dtest[, -1]),
                    label = dtest$target)
watchlist = list(train = dtrain,
                 test = dtest)  #xgb模型输入数据
d_test = xgb.DMatrix(data = data.matrix(model_test))
#weight <- sum(tt$target == 0)/sum(tt$target == 1)  #0.007316701

xgb <- xgb.train(
  data = dtrain,
  eta = 0.1, #0.1
  nrounds = 600, 
  max_depth = 4, #4
  gamma = 3, #3
  scale_pos_weight = 4.3, #4.6 4.3
  subsample = 0.85, #0.85
  colsample_bytree = 0.8, #0.8
  min_child_weight = 20, #20
  #max_delta_step = 9, #6.3
  objective = 'binary:logistic', #hinge,直接输出0/1
  eval_metric = 'auc',
  early_stopping_rounds = 100,
  seed = 1238,
  watchlist = watchlist,
  nthread = 4
)

