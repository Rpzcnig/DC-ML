# -*- coding: utf-8 -*-
"""
Created on Tue Feb 11 09:59:56 2020

@author: nigpz
"""
import lightgbm as lgb
import numpy as np
import pandas as pd
from sklearn.model_selection import KFold
import random
from sklearn.model_selection import GridSearchCV

#导入数据，此数据已经在R中分析、清洗、特征过了的。
model_train = pd.read_csv('model_train.csv')
model_test = pd.read_csv('model_test.csv')
target = pd.read_csv('train_target.csv')
model_train.shape
model_test.shape
model_train.head()
model_train.info()

#数据准备
random.seed(123)
ind = random.sample(range(1,model_train.shape[0] + 1), 
                    k = round(model_train.shape[0]*0.8))
train = model_train.loc[ind, ]
y_train = target.loc[ind, ].target
test = model_train.loc[list(set(range(1,model_train.shape[0] + 1)).difference(ind)), ]
y_test = target.loc[list(set(range(1,model_train.shape[0] + 1)).difference(ind)), ].target
lgb_train = lgb.Dataset(data = train, 
                        label = y_train )
                        #categorical_feature = range(1, model_train.shape[0]+1))
lgb_test = lgb.Dataset(data = test, 
                       label = y_test)
                       #categorical_feature = range(1, model_train.shape[0]+1),

param = {
        'boosting_type' : 'gbdt',
        'objective' : 'binary',
        'metric' : 'auc',
        'learning_rate' : 0.01, #0.01
        'num_iterations' : 1000,
        'max_depth' : 5, #5
        'min_data_in_leaf': 20,
        'feature_fraction' : 0.95, #0.95
        'scale_pos_weight' : 11, #11
        'bagging_fraction' : 0.85, #0.85
        'bagging_freq': 4, #4
        'seed' : 123,
        'nthreds' : 4
        } #0.7265
lgb_model = lgb.train(
        param,
        lgb_train,
        valid_sets = [lgb_train, lgb_test],
        early_stopping_rounds = 200,
        )

#A榜0.75455 
#10折交叉验证，（stacking lgb数据）
train_pre = np.zeros(len(model_train))
dtest_pre = np.zeros(len(model_test))
folds = KFold(n_splits = 10, shuffle = True, random_state = 2020)
for fold, (train_idx, test_idx) in enumerate(folds.split(model_train, target.target)):
    print("fold n°{}".format(fold+1))
    trn_lgb = lgb.Dataset(data = model_train.loc[train_idx], label = target.target[train_idx])
    tet_lgb = lgb.Dataset(data = model_train.loc[test_idx], label = target.target[test_idx])
    lgb_cv_model = lgb.train(
        param,
        trn_lgb,
        valid_sets = [trn_lgb, tet_lgb],
        early_stopping_rounds = 200
        )
    train_pre[test_idx] = lgb_cv_model.predict(model_train.loc[test_idx], 
             num_iteration = lgb_cv_model.best_iteration)
    dtest_pre += lgb_cv_model.predict(data = model_test, 
             num_iteration = lgb_cv_model.best_iteration) / folds.n_splits
                         
pd.DataFrame(train_pre).to_csv('train_pre.csv', index = False)
pd.DataFrame(dtest_pre).to_csv('dtest_pre.csv', index = False, header = False)



#第二次，使用较多特征
df = pd.read_csv('df3.csv')
model_train2 = df.loc[0:(len(target)-1),]
model_test2 = df.loc[len(target): len(df), ]

random.seed(1234)
ind2 = random.sample(range(1,model_train2.shape[0] + 1), 
                    k = round(model_train2.shape[0]*0.8))

train2 = model_train2.loc[ind2, ]
y_train2 = target.loc[ind2, ].target
test2 = model_train2.loc[list(set(range(1,model_train2.shape[0] + 1)).difference(ind2)), ]
y_test2 = target.loc[list(set(range(1,model_train2.shape[0] + 1)).difference(ind2)), ].target
lgb_train2 = lgb.Dataset(data = train2, 
                        label = y_train2)
lgb_test2 = lgb.Dataset(data = test2, 
                       label = y_test2)
param = {
        'boosting_type' : 'gbdt',
        'objective' : 'binary',
        'metric' : 'auc',
        'learning_rate' : 0.1, #0.01
        'num_iterations' : 1000,
        'max_depth' : 5, #5
        'min_data_in_leaf': 30,
        'feature_fraction' : 0.95, #0.95
        'scale_pos_weight' : 135, #11
        'bagging_fraction' : 0.85, #0.85
        'bagging_freq': 2, #4
        'max_bin' : 200,
        'lambda_l2' : 5,
        'seed' : 123,
        'nthreds' : 4
        } #0.7265
lgb_model2 = lgb.train(
        param,
        lgb_train2,
        valid_sets = [lgb_train2, lgb_test2],
        early_stopping_rounds = 200,
        )
#效果不明显


#超参数网格搜索。
param_grid = {
        'learning_rate' : np.linspace(0.01, 2, 5),
        'max_depth' : range(4,8), #4
        'scale_pos_weight' : range(5, 40, 5), #10
        'feature_fraction' : np.linspace(0.85, 0.98, 5), #0.85
        'min_data_in_leaf' : range(20, 40, 5) #35
        }
lgb_grid = lgb.LGBMClassifier()
grid = GridSearchCV(lgb_grid,
                    param_grid,
                    cv = 3,
                    scoring = 'roc_auc',
                    n_jobs = 4
                    )
grid.fit(model_train, target.target)
best_estimator = grid.best_estimator_
print(best_params)
'''
{'feature_fraction': 0.85,
 'learning_rate': 0.01,
 'max_depth': 4,
 'min_data_in_leaf': 35,
 'scale_pos_weight': 10}
'''

from sklearn.externals import joblib
joblib.dump(grid, 'grid_fit')
fit=joblib.load('grid_fit')


#调试bagging_fraction/freq，
param = {
        'boosting_type' : 'gbdt',
        'objective' : 'binary',
        'metric' : 'auc',
        'learning_rate' : 0.01, #0.01
        'num_iterations' : 1000,
        'max_depth' : 4, #5, 4
        'min_data_in_leaf': 35,
        'feature_fraction' : 0.72, #0.95, 0.85, 0.8
        'scale_pos_weight' : 10, #11 10
        'bagging_fraction' : 0.8, #0.85, 0.8
        'bagging_freq': 4, #4
        'seed' : 123,
        'nthreds' : 4
        } #0.7265(123), 0.7286 0.7295(12345) 0.7354
lgb_model = lgb.train(
        param,
        lgb_train,
        valid_sets = [lgb_train, lgb_test],
        early_stopping_rounds = 200,
        )

train_pre = np.zeros(len(model_train))
dtest_pre = np.zeros(len(model_test))
folds = KFold(n_splits = 10, shuffle = True, random_state = 2020)
for fold, (train_idx, test_idx) in enumerate(folds.split(model_train, target.target)):
    print("fold n°{}".format(fold + 1))
    trn_lgb = lgb.Dataset(data = model_train.loc[train_idx], label = target.target[train_idx])
    tet_lgb = lgb.Dataset(data = model_train.loc[test_idx], label = target.target[test_idx])
    lgb_cv_model = lgb.train(
        param,
        trn_lgb,
        valid_sets = [trn_lgb, tet_lgb],
        early_stopping_rounds = 200
        )
    train_pre[test_idx] = lgb_cv_model.predict(model_train.loc[test_idx], 
             num_iteration = lgb_cv_model.best_iteration)
    dtest_pre += lgb_cv_model.predict(data = model_test, 
             num_iteration = lgb_cv_model.best_iteration) / folds.n_splits
                         
pd.DataFrame(train_pre).to_csv('train_pre1.csv', index = False)
pd.DataFrame(dtest_pre).to_csv('dtest_pre1.csv', index = False, header = False)


'''
param = {'boosting_type': 'gbdt',
         'num_leaves': 20,
         'min_data_in_leaf': 20, 
         'objective':'regression',
         'max_depth':6,
         'learning_rate': 0.01,
         "min_child_samples": 30,
         
         "feature_fraction": 0.8,
         "bagging_freq": 1,
         "bagging_fraction": 0.8 ,
         "bagging_seed": 11,
         "metric": 'mse',
         "lambda_l1": 0.1,
         "verbosity": -1}
folds = KFold(n_splits=5, shuffle=True, random_state=2018)
oof_lgb = np.zeros(len(X_train_))
predictions_lgb = np.zeros(len(X_test_))

for fold_, (trn_idx, val_idx) in enumerate(folds.split(X_train, y_train)):
    print("fold n°{}".format(fold_+1))
   # print(trn_idx)
   # print(".............x_train.........")
   # print(X_train[trn_idx])
  #  print(".............y_train.........")
  #  print(y_train[trn_idx])
    trn_data = lgb.Dataset(X_train[trn_idx], y_train[trn_idx])
    
    val_data = lgb.Dataset(X_train[val_idx], y_train[val_idx])

    num_round = 10000
    clf = lgb.train(param, trn_data, num_round, valid_sets = [trn_data, val_data], verbose_eval=200, early_stopping_rounds = 100)
    oof_lgb[val_idx] = clf.predict(X_train[val_idx], num_iteration=clf.best_iteration)
    
    predictions_lgb += clf.predict(X_test, num_iteration=clf.best_iteration) / folds.n_splits

print("CV score: {:<8.8f}".format(mean_squared_error(oof_lgb, y_train_)))
'''