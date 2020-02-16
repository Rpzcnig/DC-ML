#install.packages('discretization')
library(discretization)
#install.packages('parallel')
library(parallel)



#特征工程
df1 <- df

#特征衍生
#lmt，可能会造成信息泄露。因为涉及到了目标值
mean(tt[target == 0, lmt]) #6.021054 共三类。 可能泄露信息了。注意这点
mean(tt[target == 1, lmt]) #4.175942，生成新特征，小于4.17为一类，大于6.02为一类。

df1 <- mutate(df1, lmt_level = case_when(
  df1$lmt <= 6.021 ~ 0,
  df1$lmt <= 4.176 ~ 1,
  TRUE ~ 2
))

#df1$lmt <- log(df1$lmt)  #对金额用log做正态变换，减小极值，噪声等带来的误差

#根据身份证有效时间，增加一列，时间长度
df1 <- mutate(df1, certValidtime = certValidStop - certValidBegin)

#提取certId前两位数字，只有31个，分析可能是31个省份。
df1 <- mutate(df1, 
             province = str_sub(df1$certId, start = 1, end = 2) %>% 
               as.character())

str_sub(df$certId, start = 1, end = 2) %>% unique()
str_sub(df$dist, start = 1, end = 2) %>% unique()

#由上一步，进一步分析，卡号与地区的前两位都可能是地区。所以猜想地区的省份与卡号上
#的省份不同与客户想隐瞒信息或幸福程度有关，试想卡号上的省份肯定是你出生，但地区
#不同，意味着你可能迁户，或其他。这就很大程度的增加违约的风险。

df1 <- mutate(df1, diff = if_else(str_sub(df$certId, start = 1, end = 2)== 
                                    str_sub(df$dist, start = 1, end = 2), 
                                  0, 1)) #省份相同为1，不同为0

#对身份证进行label encoding
Encode <- function(data, a = 1) {
  #data最后一列包含目标变量，且只有两列数据，第一列需要编码的变量，第二列为target
  

  p <- sum(data[, 2] == 1)/sum(data[, 2] == 0)  #IR,也是后验概率系数。
  len <- nrow(data) 
  encode <- data.frame()
  encode[1, 1] <- p
  
  #根据catBoost中的编码公式进行计算。
  for(i in seq(2, len)) {
    same_num <- sum(data[1:(i-1), 1] == data[i, 1])
    same_value <- sum(data[which(data[1:(i-1), 1] == data[i, 1]), 2])
    encode[i, 1] <- (same_value + p)/(same_num + 1)
  }
  return(encode)
}

Encode(cbind(data.frame(tt$certId), data.frame(tt$target)))


#lmt/basicLevel 构建平均信用等级授信金额特征
df1 <- mutate(df1, avg_bas_lmt = lmt/basicLevel)

chiM(as.matrix(tt$lmt, tt$target), alpha = 0.05)


# cir <- function(x) {return(x^2)}
# num <- c(1:3)
# #system.time({
# Cores_num <- detectCores() #检查当前可用核数
# c1 <- makeCluster(Cores_num)  #初始化四核心集群
# result <- parLapply(c1,  Encode(data.frame(tt$certId[1:10000]),
#                                 target = data.frame(tt$target[1:10000]), p = p)) 
#                     )
# result <- do.call(c, result) %>% data.frame(certId_code = .)
# stopCluster(c1)
# 
# #})


# 
# system.time({cir(num)})
#   
# t1 <- proc.time()
# cir(1:10000)
# t2 <- proc.time()


#对多个需要编码的变量用多线程进行处理。
parallel_encode <- function(encode_data, cores_num) {
  #encode_data包括需要重编码的数据，以及最后一列为目标变量，先将数据转化成多个组，
  #每个组第一列为编码变量，第二列为目标变量。最后通过parallel包进行多核运算。
  #cores_num为当前能运行的最大线程数，（本机虚拟核心数）
  library(tidyverse)
  library(parallel)
  
  encode_data_v <- list()  #准备用作parllel
  num <- ncol(encode_data) - 1
  
  for(i in seq(1, num)) {
    #切分数据，将数据融合成多线程输入list数据类型。
    encode_data_v[[i]] <- cbind(encode_data[, i], encode_data[, num + 1])
  } 
  
  c1 <- makeCluster(cores_num) #初始化四核心集群
  result <- parLapply(c1, encode_data_v, Encode) %>% 
    do.call('c', .) %>% data.frame() 
  names(result) <- names(encode_data[, 1:num])
  stopCluster(c1)  #关闭集群
  return(result)
}

#调用函数
encode_data <- select(tt, 
                      c('certId', 'dist', 'certValidBegin', 
                        'certValidStop', 'target')) #需要重编码的变量特征。
cores_num <- detectCores() # 4
system.time(
  encoding_data <- parallel_encode(data.frame(encode_data), cores_num)
) #计算，并检测运行时间

names(encoding_data) <- c('new_certId', 'new_dist', 'new_certValidBegin', 
                          'new_certValidStop')

encoding_temp_data <- cbind(tt, encoding_data)

avg_encode <- function(dt) {
  avg_v <- dt %>% group_by(a = dt[, 1]) %>% 
    summarise(avg = mean(dt[, 2]))
  return(avg_v)
}

df1 <- left_join(df1, 
          avg_encode(dt = cbind(tt[,'dist'],  ##注tt这里是data.table类型，
                                encoding_data['new_dist']
                                ) %>% 
                       data.frame() 
                     ) %>% rename(a= a, dist_encode = avg),
          by = c('dist' = 'a')
)
          
df1 <- left_join(df1, 
                 avg_encode(dt = cbind(tt[,'certId'],  ##注tt这里是data.table类型，
                                       encoding_data['new_certId']
                 ) %>% 
                   data.frame()
                 )%>% rename(a= a, certId_encode = avg),
                 by = c('certId' = 'a')
)

df1 <- left_join(df1, 
                 avg_encode(dt = cbind(tt[,'certValidBegin'],  ##注tt这里是data.table类型，
                                       encoding_data['new_certValidBegin']
                 ) %>% 
                   data.frame()
                 ) %>% rename(a= a, certValidBegin_encode = avg),
                 by = c('certValidBegin' = 'a')
)

df1 <- left_join(df1, 
                 avg_encode(dt = cbind(tt[,'certValidStop'],  ##注tt这里是data.table类型，
                                       encoding_data['new_certValidStop']
                 ) %>% 
                   data.frame()
                 ) %>% rename(a= a, certValidStop_encode = avg),
                 by = c('certValidStop' = 'a')
)
sapply(df1[, c(72:76)], function(x){sum(is.na(x))}) ##缺失值情况。

df1$dist_encode[which(is.na(df1$dist_encode))] <- mean(df1$dist_encode, na.rm = T)
df1$certId_encode[which(is.na(df1$certId_encode))] <- mean(df1$certId_encode,                                                            na.rm = T)
df1$certValidBegin_encode[which(is.na(df1$certValidBegin_encode))] <- 
  mean(df1$certValidBegin_encode, na.rm = T)
df1$certValidStop_encode[which(is.na(df1$certValidStop_encode))] <- 
  mean(df1$certValidStop_encode, na.rm = T)

#卡方分箱（最大箱数）

#先将数据排序，按等频分组（每组频数130000/50= 2600）。
#data %>% group_by(lmt) %>% summarise(num = n()) %>% arrange(-num)
cutpoint <- tt[, 'lmt'] %>% 
  arrange(lmt) %>% 
  mutate(rank = seq(1, nrow(tt))) %>%
  .[.$rank %% 2600 == 0, ]#划分出51个组，生成切分点（lmt）

#编码序号,离散数据（合并箱子）
lisan_data <- tt[, 'lmt'] %>% 
  arrange(lmt) %>% 
  mutate(c_k = rep(1:51, each = 2600, len = nrow(tt)),
         target = tt$target) %>% select(-lmt) 

chiSq_max_number <- function(lisan_data, max_number = 5) { 
  #lisan_data:第一列为特征，第二列为目标变量。
  #使用最大箱分箱法。合并具有最小卡方值的相邻区间，一直到满足最大箱为止，
  #最后看看单调性。及每个区间的坏客户占比，均衡
  
  cutcode_num <- length(unique(lisan_data[, 1])) #目前箱数
  num <- cutcode_num 
  
  #下面的循环，对每个编码都是，从1- 的模式循环。
  while(num > max_number) {
    cutcode_num <- num
    chisq_value <- numeric(num - 1)  #为相邻区间合并的卡方值赋值。
    for(i in 1:(cutcode_num - 1)) {
      temp_data <- lisan_data[which(lisan_data[, 1] == i | lisan_data[, 1] == i+1),
                              ] %>% table()
      chisq_value[i] <- chiSq(temp_data) #计算卡方值
    }
    rk <- which.min(chisq_value)
    lisan_data[which(lisan_data[, 1] == rk + 1), 1] <- rk
    lisan_data[, 1] <- as.numeric(as.factor(lisan_data[, 1])) 
    num <- length(unique(lisan_data[, 1]))
  }
  return(lisan_data)
}

chi_lisan_data <- chiSq_max_number(lisan_data, max_number = 3)
v <- cbind(tt[, 'lmt'] %>% arrange(lmt), chi_lisan_data)
ggplot(v, aes(c_k)) + geom_bar()  #判断单调性

unique(v[, -3]) %>% group_by(c_k) %>% summarise(intval = max(lmt))
#查看编码对应的区间，
# # A tibble: 3 x 2
# c_k intval
# <dbl>  <dbl>
#   1     1   3.03
# 2     2   8.93
# 3     3  99.6 

df1 <- df1 %>% mutate(c_k = case_when(
  df1$lmt <= 3.03 ~ 1,
  df1$lmt <= 8.93 ~ 2,
  T ~ 3
)) #%>% data.frame(t = .) %>% ggplot(data = ., aes(t)) + geom_bar()


#特征衍生之特征交叉
 # 将信息增益/IV值较大的几个特征进行特征交叉组合，

IV <- function(data) {
  # data为data.frame,且其中的特征为离散类型，最后一列为target
  # y为响应变量，也就是好坏客户,data.frame,且名字为target
  # 0表示好客户，也就是y_no, 1表示坏客户用y_yes

  y_yes <- NULL
  y_no <- NULL
  n <- length(data)
  IV <- numeric(n-1)
  y_group <- data %>% group_by(target) %>% summarise(n = n()) # 2*2
  n_s <- y_group$n[1] #好客户总数
  y_s <- y_group$n[2] #坏客户总数
  
  for(i in 1: (n - 1)) {
    iv <- 0
    feature <- unique(data[[i]])
    for(num in feature) {
      y_no <- length(which(data[[i]] == num & data$target == 0))/n_s
      y_yes <- length(which(data[[i]] == num & data$target == 1))/y_s
      iv <- (y_yes - y_no)*(log(y_yes/y_no)) + iv
    }
    IV[i] <- iv
  }
  return(IV)
}

ivdata <- cbind(df1[1:nrow(tt),], tt[, 'target'])
IV_v <- IV(cbind(df1[1:nrow(tt), !names(df1) %in% c('id', 'certId', 'dist', 'certValidBegin',
                                    'certValidStop', 'bankCard', 'residentAddr',
                                    'highestEdu', 'linikRela')], tt[, 'target'])) 
IV_v %>% data.frame(name = names(df1[, !names(df1) %in% c('id', 'certId', 'dist', 'certValidBegin',
                                                                      'certValidStop', 'bankCard', 'residentAddr',
                                                                      'highestEdu', 'linikRela')]), value = .) %>% arrange(-value)
# 19                  x_45 0.1682374636
# 20        unpayOtherLoan 0.1682374636
# 21           loanProduct 0.1675960004
# 22                  x_46 0.1424358393
# 23                   c_k 0.1291173693
# 24             lmt_level 0.1043472701
# 25                  x_20 0.1004794616

###用信息增益计算
calcshannonEnt <- function(data) {            
  labelcounts <- NULL
  
  ##求各类别的个数
  for (i in 1:nrow(unique(data))) {   
    lab <- unique(data)[[1]][i]
    labelcounts[as.character(lab)] <- length(which(data$target == lab))
  }
  
  ###求信息熵
  num <- sum(labelcounts)    
  shannonEnt <- 0
  for (i in seq_along(labelcounts)) {
    shannonEnt <- -(labelcounts[[i]]/num)*log2(labelcounts[[i]]/num) + shannonEnt
  }
  
  return(shannonEnt)
}

Ent <- calcshannonEnt(tt[, 'target']) #总的信息熵。

## 计算所有特征的信息增益
gain <- function(data) {
  n <- length(data)  #特征维数，包括了目标变量
  m <- nrow(data)
  all_shannonEnt <- NULL
  
  #对需要计算信息增益的特征变量进行遍历计算
  for(i in 1:(n-1)) {
    shannonEnt <- 0
    
    #对每个特征变量里的属性进行遍历求信息熵
    for(feature in unique(data[[i]])){
      calcshannonEnt_data <- data[which(data[i] == feature), n]
      pro <- length(calcshannonEnt_data)/m
      shannonEnt <- pro*calcshannonEnt(data.frame(target = calcshannonEnt_data)) + 
        shannonEnt #注意将这里的信息熵数据矩阵名字为target,因为函数里直接引用了它
    }
    all_shannonEnt[i] <- shannonEnt
  }
  return(all_shannonEnt)
}

top_col <- gain(cbind(df1[1:nrow(tt),], tt[, 'target']) %>% data.frame())

(Ent-top_col)*100/Ent   #各特征的信息增益百分比
# [1] 20.810428545  1.438521375  0.003359763  0.069842361 15.756231311  0.195448030
# [7]  0.604009818  7.690047573  0.169072443  0.155624591  0.173010581  0.155708261
# [13]  0.157890517  0.983215788  0.155624591  0.192176463  0.178185875  0.252601563
# [19]  0.197267108  0.257735998  0.181630338  0.177772687  0.156387829  0.579234525
# [25]  0.211903484  0.161748100  0.155751795  0.279895676  0.162367739  0.198218717
# [31]  0.155674928  1.560671013  1.198428181  0.262065526  0.301659596  0.244403582
# [37]  0.506025766  0.311990438  0.205286556  0.232798812  0.167718887  0.177357210
# [43]  0.155714392  0.301714857  0.320055076  0.195036492  0.161613380  0.205523973
# [49]  0.158078571  0.360721280  0.546468560  0.188087740  0.209724044  0.473105713
# [55]  0.231609634  0.184566413  0.337981773  0.252485518 23.618012805 29.636749050
# [61]  0.358085208 25.938327730  0.179177898  0.477433754  0.125540589  0.301714857
# [67]  0.198218717  1.560671013  0.279895676  0.155118673  0.298079316  0.700079128
# [73]  0.837208163 29.636749050  0.348870919 13.024415009  1.094327039  0.000000000
# [79]  0.000000000  0.000000000  0.000000000

data.frame(name = names(df1), value = top_col) %>% arrange(value)

#增加交叉特征
cross_data <- select(df1, 
                     c('x_45', 'unpayOtherLoan', 'loanProduct', 
                       'x_46', 'c_k', 'lmt_level', 'x_20'))
c_d_len <- length(cross_data)
c_d_v <- list() #用作存储多线程输入数据。
k <- 1
for(i in 1: (c_d_len - 1)) {
  for(j in ((i+1):c_d_len)){
    c_d_v[[k]] <- cbind(cross_data[i], cross_data[j])
    k <- k + 1
  }
}

CrossToNumeric <- function(x) {
  #将两个特征进行交叉转换。
  library(tidyverse)
  return(c_f <- paste0(x[[1]], x[[2]]) %>% factor() %>% as.numeric())
} 

Cores_num <- detectCores()
c1 <- makeCluster(Cores_num)
cross_feature <- parLapply(c1, c_d_v, CrossToNumeric) %>% 
  do.call('data.frame', .)
stopCluster(c1)

names(cross_feature) <- letters[1: length(cross_feature)]
df1 <- cbind(df1, cross_feature) %>% data.frame()

df1 <- cbind(df1[, !names(df1) %in% c('certId', 'dist', 'certValidBegin',
                                     'certValidStop', 'residentAddr',
                                     'linikRela', 'edu', 'certValidtime')], sapply(df1[, names(df1) %in% c('certId', 'dist', 'certValidBegin',
                                   'certValidStop', 'residentAddr',
                                   'linikRela', 'edu', 'certvalidtime')], function(x){factor(x) %>% as.numeric()}) %>% data.frame())

