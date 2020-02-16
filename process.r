library(corrplot)
## 数据预处理

describe(data)
df <- data %>% data.frame()  #准备数据



#age，将年龄进行区间合并，合并成9个区间。
#[18,22),[22,26)...[46,50),[50,54]并标号1-9
unique(df$age) #年龄在19~54之间，并且有一个异常值。4个117,将117归到最后一个区间
df$age <- case_when(
  df$age < 22 ~ 1,
  df$age < 26 ~ 2,
  df$age < 30 ~ 3,
  df$age < 34 ~ 4,
  df$age < 38 ~ 5, 
  df$age < 42 ~ 6,
  df$age < 46 ~ 7,
  df$age < 50 ~ 8,
  TRUE ~ 9
)

#dist 发现异常值，根据编码形式，一般都是6位数，而有7个7位数(1100000)的,
#因为查看这些值对应的目标变量都是0，且发现有些样例certId与dist一样，
#故猜测可能有关系。直接用对应的certId值替换
df$dist[which(df$dist > 999999)]
df[which(df$dist > 999999),] #56559 83127 87518 93402 94369 109261 123095
#tt$target[tt$id == 93402]
df$dist <- if_else(df$dist == 1100000, 
                   df$certId, 
                   df$dist)

#edu
tt$edu[which(tt$target == 1)] ##发现坏用户edu总体为0，2个30，1个数70，没有-999
sum(df$edu == -999) #31个缺失值，直接用众数0替换
df$edu[which(df$edu == -999)] <- 0

#lmt
p1 <- ggplot(tt[target == 0,], aes(lmt)) + 
  geom_density(fill = 'red') + 
  geom_vline(xintercept = mean(tt[target == 0, lmt]), show.legend = '均值') +
  theme_bw()   #加均值的垂直线，更直观的看出违约客户的授信金额均值更小。
p2 <- ggplot(tt[target == 1,], aes(lmt)) + 
  geom_density(fill = 'red') + 
  geom_vline(xintercept = mean(tt[target == 1, lmt]), show.legend = '均值') +
  theme_bw()
ggarrange(p1, p2)

#basicLever
sum(df$basicLevel == -999)  #有2261个缺失值。第一次尝试，就用众数填补
zhongshu <- df["basicLevel"] %>% group_by(basicLevel) %>% 
  summarise(num = n()) %>% arrange(-num)  #求众数,1
df$basicLevel[which(df$basicLevel == -999)] <- zhongshu[[1]][1]


#certValidStop中有异常值。
temp <- (df$certValidStop - df$certValidBegin) %>% data.frame(x= .) %>% 
  cbind(df[, c('certValidStop', 'certValidBegin', 'certId')])
id_avg <- temp[which(temp$x < 1000000000), ] %>% 
  group_by(certId) %>% summarise(avg = mean(x))
temp <- left_join(temp, id_avg, by = 'certId')
df$certValidStop <- if_else(temp$x < 1000000000, 
                            temp$certValidStop, 
                            as.numeric(temp$certValidBegin + temp$avg))


# temp[which(temp$x < 1000000000 & temp$x>99999999),] %>% unique
# (df[which(df$certValidStop < 1000000000),]$certValidStop-df[which(df$certValidStop < 1000000000),]$certValidBegin) %>% unique()

#考虑到，银行卡号是贷款后才有的，且从编码了。没有什么信息可提取，故去除。

sapply(df[(ncol(df)-11): ncol(df)], function(x){sum(x == -999)})
# residentAddr       highestEdu         linkRela        setupHour          weekday 
# 59300           128001            29857                0                0 
# ncloseCreditCard    unpayIndvLoan   unpayOtherLoan  unpayNormalLoan     5yearBadloan 
# 388              388              388              388                0 
# isNew         province 
# 0                0 

#对于居住地缺失率达38.11%，故直接使用它的dist进行填补
df$residentAddr <- if_else(df$residentAddr == -999, df$dist, df$residentAddr)

#去除highestEdu

#linkRela用众数进行填补
df %>% group_by(linkRela) %>% summarise(z = n()) %>% arrange(-z)  #0
df$linkRela <- ifelse(df$linkRela == -999, 0, df$linkRela) %>% 
  as.numeric()


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
  
  sCol <- NULL
  colname <- names(data)
  for(name in colname) {
    if(identical(data[[name]], fcol)) {
      sCol <- cbind(sCol, name)
    }
  }
  return(sCol)
} 
sCol <- rbind(sameCol(df[names(df) %like% c('x_')], df[['x_0']]) %>% data.frame() %>%
  gather(key = 'sname', value = 'col'),  #和x_0一样的，还有x_22一样
  sameCol(df[names(df) %like% c('x_')], df[['x_22']]) %>% data.frame() %>%
    gather(key = 'sname', value = 'col'))
  

df <- df %>% select(-c(sCol$col))

cordf <- cor(df[,names(df) %like% c("x_")]) %>% data.frame()
write.csv(cordf, 'cordf.txt')

#发现有20多个与x_0相同，故删除。并且分析这些缺失值的特点，都是固定的某些样例缺失，
#分析其原因，可能是因为这些数据是有填表得来的，而388个人没填的话自然不会有数据，
#故是非随机缺失，不做处理。直接使用-999。

df[names(df) %like% sCol$col]
#df <- select(df, -one_of(sCol$col))






