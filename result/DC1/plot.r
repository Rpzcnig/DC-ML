library(tidyverse)
library(data.table)
library(ggpubr)
library(Hmisc)

##准备数据
train <- fread('train.csv') 
test <- fread('test.csv') 
target <- fread('train_target.csv') 
tt <- cbind(target = target$target, train)
sum(tt$target == 0)/sum(tt$target == 1)

data <- rbind(train, test)

describe(data)

#查看训练集和测试集数据缺失分布情况。从图中观察缺失分布一致。
queshi <- function(df){
  return(value <- apply(df, 1, function(x) {sum(x == -999, na.rm = T)}))
}
plot(1:nrow(test), sort(queshi(test)), type = 'p')
plot(1:nrow(train), sort(queshi(train)), type = 'p')


##可视化数据，进行EDA
## IR = 137.6736
#根据特征变量，分别从用户信息，借贷信息，用户征信信息三个维度进行探索
#1.1 用户基本属性信息

#certId,有相同的，考虑后面做分箱处理
length(unique(data$certId))

#性别
p1 <- ggplot(test, aes(gender)) + geom_bar() + theme_bw()
p2 <- ggplot(train, aes(gender)) + 
  geom_bar() + theme_bw()
ggarrange(p1, p2)
ggplot(tt, aes(gender, fill = as.character(target))) +
  geom_bar(position = 'dodge')

#age, 后面也考虑对年龄进行分箱处理,注意构建后的值要分布一致
ggplot(data, aes(age)) + geom_histogram() 
p1 <- ggplot(test, aes(age)) + 
  geom_histogram() + theme_bw()+ labs(title = 'test')
p2 <- ggplot(train, aes(age)) + 
  geom_histogram() + theme_bw()+ labs(title = 'train')
ggarrange(p1, p2) ##有异常值

#dist地区
length(unique(data$dist))

#edu, highestedu有缺失值,可以尝试将一些少量人数的学历合并防止过拟合
length(unique(data$edu))
p1 <- ggplot(test[which(test$highestEdu >= 0), ], aes(highestEdu)) + geom_bar() +
  theme_bw() + labs(title = 'test')
p2 <- ggplot(train[which(train$highestEdu >= 0), ], aes(highestEdu)) +
  geom_bar() + theme_bw() + labs(title = 'train')
ggarrange(p1, p2)

p1 <- ggplot(test[which(test$edu >= 0), ], aes(edu)) + geom_bar() +
  theme_bw() + labs(title = 'test')
p2 <- ggplot(train[which(train$edu >= 0), ], aes(edu)) +
  geom_bar() + theme_bw() + labs(title = 'train')
ggarrange(p1, p2)

#job,工作16可能就代表不会逾期还款，意味着test中有较多的少类。
length(unique(data$job))
p1 <- ggplot(test, aes(job)) + geom_bar() +
  theme_bw() + labs(title = 'test')
p2 <- ggplot(train, aes(job)) +
  geom_bar() + theme_bw() + labs(title = 'train')
ggarrange(p1, p2)

#ethnic民族
p1 <- ggplot(test, aes(ethnic)) + geom_bar() +
  theme_bw() + labs(title = 'test')
p2 <- ggplot(train, aes(ethnic)) +
  geom_bar() + theme_bw() + labs(title = 'train')
ggarrange(p1, p2)

#certValidBegin,certValidStop, 有异常值
#注意到证件失效时间有些大致总体是相同的，可以尝试将那些target为0的数据单独看看
data$certValidBegin <- as.numeric(data$certValidBegin)
tt$certValidBegin <- as.numeric(tt$certValidBegin)
tt$certValidBegin[which(tt$target == 1)]

tt[, time := tt$certValidStop - tt$certValidBegin]
ggplot(tt, aes(certValidBegin)) + geom_histogram(binwidth = 1000)

## 从下面的式子可以看出，失效的人的证件有效时间都小于中位数，有重要意义
tt$time[which(tt$target == 1)] > median(tt$time) 
# ggplot(data, aes(certValidStop)) + geom_density()
# data$certValidStop[1]
# data %>% summarise_all(is.integer) %>% gather(key = "name", value = "value") %>%
#   filter(value == FALSE)


## 2.借贷相关信息

#产品类型中训练集与测试集里的分布不一致，再观察是不是大多数逾期的人都是产品1
#进一步分析，由于训练集中的数据存在严重不平衡的情况，所以test中大多是逾期的，
#这个可能是一个重要的变量，或者是毫无关系。再最后的模型中可以尝试去除。
p1 <- ggplot(test, aes(loanProduct)) + 
  geom_bar() + theme_bw() + labs(title = 'test')
p2 <- ggplot(train, aes(loanProduct)) + 
  geom_bar() + theme_bw() + labs(title ='train')
ggarrange(p1, p2)

tt$loanProduct[which(tt$target == 1)] %>% data.table() %>% 
  setnames(c('.'), c('new')) %>%
group_by(new) %>% group_size()

# lmt预授信金额,好像有存在异常值
min(unique(train$lmt))
p1 <- ggplot(test, aes(lmt)) + 
  geom_density() + theme_bw() + labs(title = 'test')
p2 <- ggplot(train, aes(lmt)) + 
  geom_density() + theme_bw() + labs(title ='train')
ggarrange(p1, p2)
max(tt$lmt[which(tt$target == 1)])

#basicLevel, train中有缺失值，并且有注意到test中没有第5类,train中唯一的5级是好用户
p1 <- ggplot(test, aes(basicLevel)) + 
  geom_bar() + theme_bw() + labs(title = 'test')
p2 <- ggplot(train[which(train$basicLevel>0),], aes(basicLevel)) + 
  geom_bar() + theme_bw() + labs(title ='train')
ggarrange(p1,p2)
unique(test$basicLevel)
tt$target[which(tt$basicLevel == 5)]
tt$basicLevel[which(tt$target == 1)] %>% data.table() %>% 
  setnames(c('.'), c('new')) %>% group_by(new) %>% group_size()

#bankcard,存在12.95%(20152)的缺失值+45747 的-999。约为35.9%,可以观察是否某些
#卡号的逾期率较高
tt$bankCard[which(tt$target == 1)]
length(which(data$bankCard == -999))
unique(tt$bankCard)

#residentAddr 居住地
p1 <- ggplot(test, aes(residentAddr)) + 
  geom_bar() + theme_bw() + labs(title = 'test')
p2 <- ggplot(train, aes(residentAddr)) + 
  geom_bar() + theme_bw() + labs(title ='train')
ggarrange(p1,p2)
















##2020/01/05  EDA
length(train[which(test$basicLevel == -999),]) #2261/132029=0.01712503 缺失率
p1 <- ggplot(tt[which(tt$basicLevel >= 0), ], aes(basicLevel, fill = as.character(target))) + 
  geom_bar(position = 'dodge') +
  theme_bw()
p1

#查看basicLevel违约客户占比，发现第四类占比最高，-999第二。是其他的2-3倍。
#先用众数填补，观察效果，不行再尝试其他填补
len <- NULL  #计算每个类中，违约客户的占比
for(i in seq(1,5)){
  m_len <- length(which(tt$basicLevel == i))
  n_len <- length(which(tt$basicLevel == i & tt$target == 1))
  len[i] <- paste0(n_len*100/m_len, '%')
}
length(which(tt$basicLevel == -999 & tt$target == 1))/length(which(tt$basicLevel == -999))

#lmt,很明显越高，越不可能违约。反之，违约客户大多预授信用金额较低
#可以增加一类特征，与信用金额平均相关等级。
ggplot(data, aes(lmt)) + geom_density() + theme_bw()
tt[which(tt$lmt > 50), target]
mean(tt[target == 0, lmt]) #6.021054
mean(tt[target == 1, lmt]) #4.175942

#job,c(10, 11, 12, 13)这几个可以合并在一起。
len <- NULL
for(i in seq(1,16)){
  m_len <- length(which(tt$job == i))
  n_len <- length(which(tt$job == i & tt$target == 1))
  len[i] <- paste0(n_len*100/m_len, '%')
}

#申请周期
p1 <- ggplot(tt[target == 0,], aes(weekday)) + geom_density(fill = 'red')
p2 <- ggplot(tt[target == 1,], aes(weekday)) + geom_density(fill = 'red')
ggarrange(p1, p2)

#linkRela
p1 <- ggplot(test, aes(linkRela)) + geom_bar() + theme_bw()
p2 <- ggplot(train, aes(linkRela)) + geom_bar() + theme_bw()
ggarrange(p1, p2)
length(which(tt$linkRela == -999 & tt$target == 1))/length(which(tt$linkRela == -999))

#可以看出大多数违约客户大约在下午3点左右。可以尝试把放款日期与时间特征组合。
#进行更细致的分区，再合并某些组合。
p1 <- ggplot(tt[target == 0,], aes(setupHour)) + geom_density(fill = 'red')
p2 <- ggplot(tt[target == 1,], aes(setupHour)) + geom_density(fill = 'red')
ggarrange(p1, p2)

#highestEdu
p1 <- ggplot(tt[tt$highestEdu >= 0, ], aes(highestEdu, fill = as.character(target))) + 
  geom_bar(position = 'dodge') + theme_bw() 

ggplot(tt[tt$edu >= 0, ], aes(edu, fill = as.character(target))) + 
  geom_bar(position = 'dodge') + theme_bw() 
length(which(tt$edu >= 60 & tt$target == 1))/length(which(tt$edu >= 60))

#certId前2为31个，可能为省份。dist,11为异常值，应该正常的应该是31个省份才对。
#直接用它的certId进行填补。
length(unique(str_sub(data$certId, start = 1, end = 2)))







