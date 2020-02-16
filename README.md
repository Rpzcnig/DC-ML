# DC-ML
# DC-ML
## 1、简介
该项目是pkbigdata平台上的一个比赛。
目的是用机器学习去解决类别不平衡问题和矩阵稀疏性问题，预测用户是否违约。
A榜最终成绩0.76155
## 2、说明
plot.r 文件里，主要是数据可视化及EDA
process.r 进行EDA，包括，缺失值处理，异常值检验，数据转换（卡方分箱、label encode等等）
feature.r 特征工程处理，
          1.特征衍生: 业务逻辑、数学公式、特征交叉
          2.特征选择：IV值，xgb important topN
datamining.r 数据探索
其他：模型的建立，catboot、lgb、catboost
stack.r 对三个建立的模型使用logistic进行两层stack融合。
result文件夹此次项目运行环境，里面包含各种结果、数据和完整代码。
