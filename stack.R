#使用logistic进行xgb-catboost-lgb两层stacking融合
set.seed(0212)
stack_ind <- cut(1:nrow(tt), breaks = 5, labels = F) %>% 
  sample(., nrow(tt))
#catboost10折交叉验证
catpre <- numeric(nrow(tt))
cattestpre <- NULL

for(i in 1:5) {
  
  dtrain2.0 <- catboost.load_pool(data = model_train[stack_ind != i, ],
                                  label = target$target[stack_ind != i])
  dtest2.0 <- catboost.load_pool(data = model_train[stack_ind == i, ],
                                 label = target$target[stack_ind == i])
  fit_params <- list(learning_rate = 0.1,
                     iterations = 500,
                     loss_function = 'Logloss', #CrossEntropy
                     custom_loss = c('AUC'),
                     l2_leaf_reg = 30, #17
                     use_best_model = T,
                     #border_count = 32,
                     depth = 5,
                     class_weights = c(1, 20), #c(1, 20)
                     rsm = 0.85, #列抽样比例
                     random_seed = 1518,
                     od_type = 'Iter',  #检测过拟合类型
                     od_wait = 30,
                     #train_dir =  '/train_dir', #储存trian信息
                     logging_level = 'Info')
  cat <- catboost.train(dtrain2.0, dtest2.0, fit_params)
  catpre[stack_ind == i] <- catboost.predict(cat, 
                             catboost.load_pool(model_train[stack_ind == i,]),
                             prediction_type = 'Probability')
  cattestpre <- catboost.predict(cat, 
                                 catboost.load_pool(model_test),
                                 prediction_type = 'Probability')  %>%
    cbind(cattestpre, .)
}
cattestpre <- apply(cattestpre, 1, mean) %>% 
  data.frame()
catpre <- catpre %>% data.frame(cat_train = .)
names(cattestpre) <- c('cat_test')

#xgb5折交叉。
xgb_pre <- numeric(nrow(tt))
xgbtestpre <- NULL
d_test = xgb.DMatrix(data = data.matrix(model_test)) #测试集数据，
for (i in 1:5) {
  dtrain_xgb <- xgb.DMatrix(data = data.matrix(model_train[stack_ind != i, ]),
                        label = target$target[stack_ind != i])
  dtest_xgb <- xgb.DMatrix(data = data.matrix(model_train[stack_ind == i, ]),
                       label = target$target[stack_ind == i])
  watchlist <- list(train = dtrain_xgb, 
                    test = dtest_xgb)
  xgb <- xgb.train(
    data = dtrain_xgb,
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
  xgbval <- xgb.DMatrix(data = data.matrix(model_train[stack_ind == i, ]))
  xgb_pre[stack_ind == i] <- predict(xgb, xgbval) 
  xgbtestpre <- predict(xgb, d_test) %>%
    cbind(xgbtestpre, .)
}
##得到xgb的测试集和第二层的输入特征sgb_pre
xgbtestpre <- apply(xgbtestpre, 1, mean) %>% 
  data.frame()
xgb_pre <- xgb_pre %>% data.frame(xgb_train = .)
names(xgbtestpre) <- c('xgb_test')


#lgb
lgb_pre <- read_csv('C:/Users/nigpz/Desktop/r/dcdata/train_pre1.csv')
lgbtestpre <- read_csv('C:/Users/nigpz/Desktop/r/dcdata/dtest_pre1.csv', 
                       col_names = F)
names(lgb_pre) <- c('lgb_train')
names(lgbtestpre) <- c('lgb_test')


#logistic stacking.
stack_train <- cbind(target = tt$target, catpre, xgb_pre, lgb_pre) %>% 
  data.frame()
stack_test <- cbind(cattestpre, xgbtestpre, lgbtestpre) %>% data.frame  
logit <- glm(target~., family = binomial(link = 'logit'), data = stack_train)
re <- predict.glm(logit, type = 'response')
roc1 <- roc(tt$target, re)
plot(roc1,print.auc=T, auc.polygon=T, auc.polygon.col="skyblue")
names(stack_test) <- names(stack_train[,2:4])
res1 <- predict(logit, type = 'response', newdata = stack_test)  %>% 
  data.frame(id = test$id, target = .)
write.csv(res1, 'res1.csv', row.names = F)
