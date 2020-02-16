## 数据预处理
describe(data)
df <- data


#age
unique(df$age) #年龄在19~54之间，并且有一个异常值。4个117
ggplot(df, aes(rep(1, nrow(df)), age)) + geom_boxplot() + theme_bw()
df$age[which(df$age > 54)]
df[df$age == 117]
  #24426 32102 38265 152628
tt$target[tt$id == 152628] #发现他们都是好客户。故直接用均值补。
df$age[which(df$age == 117)] <- ceiling(mean(df$age[which(df$age <= 54)]))

#dist 又发现异常值，根据编码形式，一般都是6位数，而有7个7位数(110000)的,
#采用众数进行替换
df$dist[which(df$dist > 999999)]
df[which(df$dist > 999999)]
  #56559 83127 87518 93402 94369 109261 123095
tt$target[tt$id == 83127]

df %>% group_by(dist) %>% summarise(mode = n()) %>% arrange(-mode)
df$dist[which(df$dist == 1100000)] <- 640300

#edu
tt$edu[which(tt$target == 1)] ##发现坏用户edu总体为0，2个30，1个数70，没有-999
df$highestEdu[which(df$edu == -999)]
df$edu[which(df$highestEdu == 99)]
  #使用众数0替换,特征变换时，可以尝试进行分桶
df$edu[which(df$edu == -999)] <- 0

#lmt,并无异常值，区间0~100
ggplot(df, aes(rep(1, nrow(df)), lmt)) + geom_boxplot() + theme_bw()
tt$target[which(tt$lmt ==  0.167)]

#basicLevel,考虑到-999的坏客户率太高，达0.01769，故将其作为新的特征取值，用0代替
tt[which(tt$target ==  1),] %>% group_by(basicLevel) %>% summarise(n())
df$basicLevel <- as.numeric(as.factor(df$basicLevel))

#certValidBegin， 有异常值（600多点）,并且发现证件号起始日期相同，结束日期只能为
  #两个中的一个。所以这种情况可能是，对于不同省份的证件有效期时长可能不同
  #故使用贝叶斯进行替换异常值。
df$id[which(df$certValidStop > 9999999999)]
tt$target[which(df$certValidStop > 9999999999)] #坏客户率占比0.012
df$certValidBegin[which(df$certValidStop < 999999999)]
unique(df$certValidStop[which(df$certValidBegin == 3719347200)])
 #3766003200 3669926400 3719347200 3761683200


#此值为31，也就是31个省份，故前两个是省份的代码
length(unique(str_sub(df$dist, 1, 2)))
bdf <- select(df, c('certValidBegin', 'certValidStop'))
bdf$dist <- str_sub(df$dist, 1, 2) %>% data.frame()
unique(bdf$certValidStop[which(bdf$certValidBegin == 3748636800 &
                                 bdf$dist == '55')])
## 根据地区及起始的时间，填补certValidStop
newcer <- NULL
stop <- function(x, y) {
  for (i in 1:nrow(x)) {
    all_stop <- unique(y$certValidStop[which(y$certValidBegin == x$certValidBegin[i])])
    if(length(all_stop) == 0) {
      newcer[i] <- x$certValidBegin[i]
    }
    else{
      newcer[i] <- sample(rep(all_stop, 2), 1)
    }
  }
  return(newcer)
}
newcer <- stop(bdf[which(bdf$certValidStop == 256000000000), ],
  bdf[-which(bdf$certValidStop == 256000000000), ])
bdf[which(bdf$certValidStop == 256000000000), ] <- newcer
df$certValidStop <- bdf$certValidStop
###---------------------------------------------------------------------------

# library(e1071)
# bdf <- select(df, c('certValidBegin', 'certValidStop'))
# bdf$dist <- str_sub(df$dist, 1, 2) %>% data.frame()
# bdf$certValidBegin <- as.character(as.factor(bdf$certValidBegin))
# bys <- naiveBayes(certValidStop ~ ., 
#                   bdf[-which(bdf$certValidStop == 256000000000), ])
# predict(bys, bdf[which(bdf$certValidStop == 256000000000), -'certValidStop'])
###--------------------------------------------------------------------------

###bankcarad,缺失值和异常值,通过分析，-999,跟na值的坏客户率最大，所以这里不进行填补
###而是将这两类作为新的两个类别。将-999换成666666667，na换成666666666
df %>% group_by(bankCard) %>% summarise(n = n()) %>% arrange(-n)
tt$bankCard[which(tt$target == 1)] %>% data.frame(a = .) %>% 
  group_by(a) %>% summarise(n = n()) %>% arrange(-n)

df$bankCard <- ifelse(is.na(df$bankCard), 666666666, df$bankCard)
df$bankCard <- if_else(df$bankCard == -999, 666666667, df$bankCard)

#residentAddr,缺失严重,达0.4389，只能将其当做独立的一份。
df$residentAddr[which(df$residentAddr == -999)]
df$residentAddr <- ifelse(df$residentAddr == -999, 1111111, df$residentAddr)

#highestEdu 缺失值达82.3，考虑删除
tt$target[which(tt$x_32 == 1)]

## 以下是对未做说明的数据进行处理

# 对于观测值缺失80%以上的，由于观察到其，可能有较大的意义，所以不选择删除，而
# 作为新的一个属性，可以新增加一列：1表示大量缺失，0表示不缺失
#考虑到x_i是严重的稀疏，先对固定的-999，进行增加一列。然后在对应做一些填补
#但注意到x_0,就只有两个值{-999,0}，故其实并不用进行添加，直接用x_0表示即可
#df <- df %>% mutate(addna = ifelse(x_0 == -999, 1, 0))
describe(df[names(df) %like% c('x')])

#发现x_0,1,2 都是一样的故，可以删除2个，保留x_0, 对数据再次观察，并用相关系数验证
# 存在一些变量取值是一样的，考虑删除，因为相关性达100%,相同的有29个

sameCol <- function(data, fcol) {
  ##data是数据框形式的数据，
  ##fcol为需要为其找出相同的列
  
  n <- length(data)
  sCol <- NULL
  for(i in 1:n) {
    if(identical(data[[paste0('x_', i)]], fcol)) {
      sCol <- cbind(sCol, paste0('x_', i))
    }
  }
  return(sCol)
} 
sCol <- sameCol(df[names(df) %like% c('x_')], df[['x_0']]) %>% data.frame() %>%
  gather(key = 'name', value = 'col')
df <- df %>% select(-c(sCol$col))
df$x_0 <- ifelse(df$x_0 == -999, 1, 0) ##用1来代替-999，

df <- select(df, -c('x_70'))  #x_70 与x_0是相对的，cor = -1

#计算IV值，再根据值判读是够将-999替换为众数

IV <- function(data, y) {
  # data为data.frame,且其中的特征为离散类型，
  # y为响应变量，也就是好坏客户,data.frame,且名字为target 
  # 0表示好客户，也就是y_no, 1表示坏客户用y_yes
  
  y_yes <- NULL
  y_no <- NULL
  iv <- 0; IV <- NULL
  n <- length(data)
  y_group <- y %>% data.frame() %>% group_by(target) %>% summarise(n = n()) # 2*2
  n_s <- y_group$n[1] #好客户总数
  y_s <- y_group$n[2] #坏客户总数
  for(i in 1:n) {
    feature <- unique(data[[i]])
    for(num in feature) {
      y_no <- length(which(data[i] == num & data$target == 0))/n_s
      y_yes <- length(which(data[i] == num & data$target == 1))/y_s
      iv <- (y_yes - y_no)*(log(y_yes/y_no)) + iv
    }
    IV[i] <- iv 
  }
  return(IV)
}

ivdata <- cbind(df[1:nrow(tt), names(df) %like% c('x_')], tt[, 'target'])
IV(ivdata, ivdata['target'])


# y_group <- tt['target'] %>% data.frame() %>% group_by(target) %>% summarise(n = n()) 
# y_group
# length(which(ivdata[[1]] == 0 & ivdata$target == 1))/131070


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

calcshannonEnt(ivdata['target'])
  
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
gain(ivdata)

# [1] 0.06194194 0.06195268 0.06195133 0.06143922 0.06195273 0.06193005 0.06193873
# [8] 0.06189256 0.06192689 0.06188937 0.06193660 0.06193899 0.06195226 0.06168988
# [15] 0.06191781 0.06194893 0.06195265 0.06195273 0.06187562 0.06194855 0.06192630
# [22] 0.06195270 0.06108091 0.06130568 0.06188669 0.06186212 0.06189765 0.06173531
# [29] 0.06185571 0.06192192 0.06190485 0.06194523 0.06193925 0.06195268 0.06186208
# [36] 0.06185070 0.06192828 0.06194902 0.06192177 0.06195121 0.06182547 0.06171022
# [43] 0.06193259 0.06191916 0.06175574 0.06190558 0.06193477 0.06183958 0.06189263
# [50] 0.06195273


##用众数替补缺失值

#求一个向量的众数,mode
mode <- function(vector) {
  max <- 0
  for(lab in unique(vector)) {
    len <- length(which(vector == lab))
    if(len > max) {
      max <- len
      lable <- lab
    }
  }
  return(lable)
}

# vector <- c(1,1,1,11,22,1,1)
#得出众数
name <- names(ivdata[,!names(ivdata) %in% c('target', 'x_0')]) %>% paste0('ivdata$',.)
mode_all <- NULL
for(col in name){
  mode_all <- rbind(data.frame(colName = col, 
                               mode = col %>% parse(text = .) %>% 
                                 eval() %>% mode()), 
                    mode_all)
}


# > mode_all
# colName mode
# 1   ivdata$x_0    0
# 2  ivdata$x_76    0
# 3  ivdata$x_75    0
# 4  ivdata$x_74    1
# 5  ivdata$x_73    0
# 6  ivdata$x_72    0
# 7  ivdata$x_71    1
# 8  ivdata$x_69    0
# 9  ivdata$x_68    0
# 10 ivdata$x_67    1
# 11 ivdata$x_66    0
# 12 ivdata$x_65    0
# 13 ivdata$x_64    0
# 14 ivdata$x_63    0
# 15 ivdata$x_62    1
# 16 ivdata$x_61    1
# 17 ivdata$x_56    0
# 18 ivdata$x_55    0
# 19 ivdata$x_54    0
# 20 ivdata$x_53    0
# 21 ivdata$x_52    1
# 22 ivdata$x_51    1
# 23 ivdata$x_50    0
# 24 ivdata$x_49    0
# 25 ivdata$x_48    0
# 26 ivdata$x_47    0
# 27 ivdata$x_46    0
# 28 ivdata$x_45    1
# 29 ivdata$x_44    0
# 30 ivdata$x_43    0
# 31 ivdata$x_42    0
# 32 ivdata$x_41    0
# 33 ivdata$x_40    1
# 34 ivdata$x_39    0
# 35 ivdata$x_35    0
# 36 ivdata$x_34    4
# 37 ivdata$x_33    3
# 38 ivdata$x_32    0
# 39 ivdata$x_31    0
# 40 ivdata$x_30    0
# 41 ivdata$x_29    0
# 42 ivdata$x_28    0
# 43 ivdata$x_27    0
# 44 ivdata$x_26    0
# 45 ivdata$x_25    0
# 46 ivdata$x_22    1
# 47 ivdata$x_20    1
# 48 ivdata$x_16    1
# 49 ivdata$x_14    0
# 50 ivdata$x_12    0

ivdata <- test %>% select(names(ivdata[, !names(ivdata) %in% c('target')])) %>% 
  rbind(ivdata[, !names(ivdata) %in% c('target')], .)
ivdata$x_0 <- ifelse(ivdata$x_0 == -999, 1, 0)

ivdata$x_12[which(ivdata$x_12 == -999)] <- 0
ivdata$x_14[which(ivdata$x_14 == -999)] <- 0
ivdata$x_16[which(ivdata$x_16 == -999)] <- 1
ivdata$x_20[which(ivdata$x_20 == -999)] <- 1
ivdata$x_22[which(ivdata$x_22 == -999)] <- 1
ivdata$x_25[which(ivdata$x_25 == -999)] <- 0
ivdata$x_26[which(ivdata$x_26 == -999)] <- 0
ivdata$x_27[which(ivdata$x_27 == -999)] <- 0
ivdata$x_28[which(ivdata$x_28 == -999)] <- 0
ivdata$x_29[which(ivdata$x_29 == -999)] <- 0
ivdata$x_30[which(ivdata$x_30 == -999)] <- 0
ivdata$x_31[which(ivdata$x_31 == -999)] <- 0
ivdata$x_32[which(ivdata$x_32 == -999)] <- 0
ivdata$x_33[which(ivdata$x_33 == -999)] <- 3
ivdata$x_34[which(ivdata$x_34 == -999)] <- 4
ivdata$x_35[which(ivdata$x_35 == -999)] <- 0
ivdata$x_39[which(ivdata$x_39 == -999)] <- 0
ivdata$x_40[which(ivdata$x_40 == -999)] <- 1
ivdata$x_41[which(ivdata$x_41 == -999)] <- 0
ivdata$x_42[which(ivdata$x_42 == -999)] <- 0
ivdata$x_43[which(ivdata$x_43 == -999)] <- 0
ivdata$x_44[which(ivdata$x_44 == -999)] <- 0
ivdata$x_45[which(ivdata$x_45 == -999)] <- 1
ivdata$x_46[which(ivdata$x_46 == -999)] <- 0
ivdata$x_47[which(ivdata$x_47 == -999)] <- 0
ivdata$x_48[which(ivdata$x_48 == -999)] <- 0
ivdata$x_49[which(ivdata$x_49 == -999)] <- 0
ivdata$x_50[which(ivdata$x_50 == -999)] <- 0
ivdata$x_51[which(ivdata$x_51 == -999)] <- 1
ivdata$x_52[which(ivdata$x_52 == -999)] <- 1
ivdata$x_53[which(ivdata$x_53 == -999)] <- 0
ivdata$x_54[which(ivdata$x_54 == -999)] <- 0
ivdata$x_55[which(ivdata$x_55 == -999)] <- 0
ivdata$x_56[which(ivdata$x_56 == -999)] <- 0
ivdata$x_61[which(ivdata$x_61 == -999)] <- 1
ivdata$x_62[which(ivdata$x_62 == -999)] <- 1
ivdata$x_63[which(ivdata$x_63 == -999)] <- 0
ivdata$x_64[which(ivdata$x_64 == -999)] <- 0
ivdata$x_65[which(ivdata$x_65 == -999)] <- 0
ivdata$x_66[which(ivdata$x_66 == -999)] <- 0
ivdata$x_67[which(ivdata$x_67 == -999)] <- 1
ivdata$x_68[which(ivdata$x_68 == -999)] <- 0
ivdata$x_69[which(ivdata$x_69 == -999)] <- 0
ivdata$x_71[which(ivdata$x_71 == -999)] <- 1
ivdata$x_72[which(ivdata$x_72 == -999)] <- 0
ivdata$x_73[which(ivdata$x_73 == -999)] <- 0
ivdata$x_74[which(ivdata$x_74 == -999)] <- 1
ivdata$x_75[which(ivdata$x_75 == -999)] <- 0
ivdata$x_76[which(ivdata$x_76 == -999)] <- 0


df0 <- df[!names(df) %like% c('x_')] %>% 
  cbind(., ivdata)

das









library(tidyverse)
df3 <- cbind(df1[, !names(df1) %in% c('certId', 'dist', 'certValidBegin',
                                      'certValidStop', 'residentAddr',
                                      'linikRela', 'edu', 'certValidtime')], 
             sapply(
               df1[, names(df1) %in% c('certId', 'dist', 'certValidBegin',
                                       'certValidStop', 'residentAddr','linikRela',
                                       'edu', 'certvalidtime')], 
               function(x){factor(x) %>% as.numeric()}
             ) %>% 
               data.frame())
df3 <- select(df3, -c('highestEdu', 'bankCard', 'id', 'gender'))
df3 <- select(df3, lmt, everything())
write.csv(df3, 'df3.csv', row.names = F)
# df3[, 2:length(cat_df)] <- sapply(cat_df[, 2:length(cat_df)], 
#                                      function(x){factor(x, levels = sort(unique(x)))}) %>% 
#   data.frame





# n <- length(ivdata)  #特征维数，包括了目标变量
# m <- nrow(ivdata)
# all_shannonEnt <- NULL
# 
# #对需要计算信息增益的特征变量进行遍历计算
# for(i in 1:50) {
#   shannonEnt <- 0
#   
#   #对每个特征变量里的属性进行遍历求信息熵
#   for(feature in unique(ivdata[[i]])){
#     calcshannonEnt_data <- ivdata[which(ivdata[i] == feature), n]
#     pro <- length(calcshannonEnt_data)/m
#     shannonEnt <- pro*calcshannonEnt(data.frame(target = calcshannonEnt_data)) + 
#       shannonEnt  #注意将这里的信息熵数据矩阵名字为target,因为函数里直接引用了它
#   }
#   all_shannonEnt[i] <- shannonEnt
# 
# }
# print(all_shannonEnt)


