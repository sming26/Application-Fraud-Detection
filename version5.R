setwd('/Users/riyueyoutu/Desktop/USC/Courses/DSO562/project2/')
library(dplyr)
library(lubridate)
library(data.table)
library(stringr)
library(ggplot2)
library(h2o)

# setwd("C:/Users/peipe/Desktop/562/Project2")
app = read.csv('applications100k.csv')
# adding leading zeros in ssn,zip, homephone fields.

app$ssn = str_pad(as.character(app$ssn),width = 9, pad = "0") # Adding leading zeros using str_pad
app$zip5 = str_pad(as.character(app$zip5),width = 5, pad = "0") # Adding leading zeros using str_pad
app$homephone = str_pad(app$homephone,width = 10, pad = "0") # Adding leading zeros using str_pad

app$firstname = as.character(app$firstname)
app$lastname = as.character(app$lastname)

## concatenate name&dob, address&zip5
app$name = paste(app$firstname,app$lastname,sep=' ')
app$identifier = paste(app$name,app$dob,sep=' ')
app$address = paste(app$address,app$zip5,sep=' ')
app$date = ymd(as.character(app$date))
app$dob = ymd(as.character(app$dob))
# backup the data so far
# app1 = app
app = as.data.table(app)

## create the daily count data frames for each variable
daily_count = app[,.(countDaily=.N),by=date]

daily_name = app[,.(daily_name=.N),by=.(date,name)]
daily_dob = app[,.(daily_dob=.N),by=.(date,dob)]
daily_id = app[,.(daily_id=.N),by=.(date,identifier)]
daily_ssn = app[,.(daily_ssn=.N),by=.(date,ssn)]
daily_addr = app[,.(daily_addr=.N),by=.(date,address)]
daily_phone = app[,.(daily_phone=.N),by=.(date,homephone)]

## add the cumulative counts for past time period
daily_count$countCum = cumsum(daily_count$countDaily)

daily_name=daily_name[,.(date,daily_name,cum_name=cumsum(daily_name)),by=name]
daily_dob=daily_dob[,.(date,daily_dob,cum_dob=cumsum(daily_dob)),by=dob]
daily_id=daily_id[,.(date,daily_id,cum_id=cumsum(daily_id)),by=identifier]
daily_ssn=daily_ssn[,.(date,daily_ssn,cum_ssn=cumsum(daily_ssn)),by=ssn]
daily_addr=daily_addr[,.(date,daily_addr,cum_addr=cumsum(daily_addr)),by=address]
daily_phone=daily_phone[,.(date,daily_phone,cum_phone=cumsum(daily_phone)),by=homephone]

#### add the cumulative counts for time windows
## function to generate time window table
lagcum = function(dt,n){
  dt1 = dt
  dt[,join_date:=(date-n)]
  dt1$join_date = dt1$date
  key1 = colnames(dt)[1]
  key2 = colnames(dt)[5]
  setkeyv(dt,c(key1,key2))
  setkeyv(dt1,c(key1,key2))
  dt2 = dt1[dt,roll=T,rollends=c(T,T)]
  return (dt2)
}

## function to change column names
cname = function(name) c('date', paste0(name,'Daily'), paste0(name,'Cum'))

## add 7-day time window cumulative counts
count = daily_count[,countLag7:=shift(countCum,n=7,fill=0,type='lag')]
count$countLag7Cum = ifelse(count$countLag7==0,count$countCum,count$countCum-count$countLag7)
count = count[,c(1,2,3,5)]

name = lagcum(daily_name,7)
name$nameLag7Cum = ifelse(name$date>name$join_date,name$i.cum_name,name$i.cum_name-name$cum_name)
name = name[,c(1,6,7,8,9)]
colnames(name)[2:4]=cname('name')

dob = lagcum(daily_dob,7)
dob$dobLag7Cum = ifelse(dob$date>dob$join_date,dob$i.cum_dob,dob$i.cum_dob-dob$cum_dob)
dob = dob[,c(1,6,7,8,9)]
colnames(dob)[2:4]=cname('dob')

id = lagcum(daily_id,7)
id$idLag7Cum = ifelse(id$date>id$join_date,id$i.cum_id,id$i.cum_id-id$cum_id)
id = id[,c(1,6,7,8,9)]
colnames(id)[2:4]=cname('id')

ssn = lagcum(daily_ssn,7)
ssn$ssnLag7Cum = ifelse(ssn$date>ssn$join_date,ssn$i.cum_ssn,ssn$i.cum_ssn-ssn$cum_ssn)
ssn = ssn[,c(1,6,7,8,9)]
colnames(ssn)[2:4]=cname('ssn')

addr = lagcum(daily_addr,7)
addr$addrLag7Cum = ifelse(addr$date>addr$join_date,addr$i.cum_addr,addr$i.cum_addr-addr$cum_addr)
addr = addr[,c(1,6,7,8,9)]
colnames(addr)[2:4]=cname('addr')

phone = lagcum(daily_phone,7)
phone$phoneLag7Cum = ifelse(phone$date>phone$join_date,phone$i.cum_phone,phone$i.cum_phone-phone$cum_phone)
phone = phone[,c(1,6,7,8,9)]
colnames(phone)[2:4]=cname('phone')

## add 3-day time window cumulative counts
count[,countLag3:=shift(countCum,n=3,fill=0,type='lag')]
count$countLag3Cum = ifelse(count$countLag3==0,count$countCum,count$countCum-count$countLag3)
count = count[,c(1,2,3,4,6)]

name2 = lagcum(daily_name,3)
name$nameLag3Cum = ifelse(name2$date>name2$join_date,name2$i.cum_name,name2$i.cum_name-name2$cum_name)

dob2 = lagcum(daily_dob,3)
dob$dobLag3Cum = ifelse(dob2$date>dob2$join_date,dob2$i.cum_dob,dob2$i.cum_dob-dob2$cum_dob)

id2 = lagcum(daily_id,3)
id$idLag3Cum = ifelse(id2$date>id2$join_date,id2$i.cum_id,id2$i.cum_id-id2$cum_id)

ssn2 = lagcum(daily_ssn,3)
ssn$ssnLag3Cum = ifelse(ssn2$date>ssn2$join_date,ssn2$i.cum_ssn,ssn2$i.cum_ssn-ssn2$cum_ssn)

addr2 = lagcum(daily_addr,3)
addr$addrLag3Cum = ifelse(addr2$date>addr2$join_date,addr2$i.cum_addr,addr2$i.cum_addr-addr2$cum_addr)

phone2 = lagcum(daily_phone,3)
phone$phoneLag3Cum = ifelse(phone2$date>phone2$join_date,phone2$i.cum_phone,phone2$i.cum_phone-phone2$cum_phone)

#### add the days since last time seeing the value
name = name[,last_name:=shift(date,fill=999,1),by=name]
name$nameRecency = ifelse(name$last_name==999,999,name$date-name$last_name)
name = name[,-7]

# dob = dob[,last_dob:=shift(date,fill=0,1),by=dob]
# dob$dobRecency = ifelse(dob$last_dob==0,999,dob$date-dob$last_dob)
# dob = dob[,-7]
# replace the frivolous value for dob 19070626
# for recency, because the first recency is assumed 999, now replace frivolous values with mean value
dob_mean = colMeans(dob[dob!='1907-06-26',3:6])
dob[,3:6] = lapply(dob[,3:6],as.numeric)
dob = dob[dob=='1907-06-26',`:=`(dobDaily=dob_mean[1],dobCum=dob_mean[2],dobLag7Cum=dob_mean[3],dobLag3Cum=dob_mean[4])]
# dob_mean_recency = colMeans(dob[(dob!='1907-06-26' & dobRecency!=999),7])
# dob = dob[dob=='1907-06-26' | dobRecency==999, dobRecency:=dob_mean_recency]

id = id[,last_id:=shift(date,fill=999,1),by=identifier]
id$idRecency = ifelse(id$last_id==999,999,id$date-id$last_id)
id = id[,-7]

ssn = ssn[,last_ssn:=shift(date,fill=999,1),by=ssn]
ssn$ssnRecency = ifelse(ssn$last_ssn==999,999,ssn$date-ssn$last_ssn)
ssn = ssn[,-7]
# replace the frivolous value for ssn 737610282
ssn_mean = colMeans(ssn[ssn!='737610282',3:6])
ssn[,3:7] = lapply(ssn[,3:7],as.numeric)
ssn = ssn[ssn=='737610282',`:=`(ssnDaily=ssn_mean[1],ssnCum=ssn_mean[2],ssnLag7Cum=ssn_mean[3],ssnLag3Cum=ssn_mean[4])]
ssn_mean_recency = colMeans(ssn[(ssn!='737610282'),7])
ssn = ssn[ssn=='737610282', ssnRecency:=ssn_mean_recency]

addr = addr[,last_addr:=shift(date,fill=999,1),by=address]
addr$addrRecency = ifelse(addr$last_addr==999,999,addr$date-addr$last_addr)
addr = addr[,-7]
# replace the frivolous value for address '2602 AJTJ AVE 68138'
addr_mean = colMeans(addr[address!='2602 AJTJ AVE 68138',3:6])
addr[,3:7] = lapply(addr[,3:7],as.numeric)
addr = addr[address=='2602 AJTJ AVE 68138',`:=`(addrDaily=addr_mean[1],addrCum=addr_mean[2],addrLag7Cum=addr_mean[3],addrLag3Cum=addr_mean[4])]
addr_mean_recency = colMeans(addr[(address!='2602 AJTJ AVE 68138'),7])
addr = addr[address=='2602 AJTJ AVE 68138', addrRecency:=addr_mean_recency]

phone_mean = colMeans(phone[homephone!='9105580920',3:6])
phone[,3:6] = lapply(phone[,3:6],as.numeric)
phone = phone[homephone=='9105580920',`:=`(phoneDaily=phone_mean[1],phoneCum=phone_mean[2],phoneLag7Cum=phone_mean[3],phoneLag3Cum=phone_mean[4])]

#### add the cumulative counts for unique values for certain groups
laguni1 = function(dt,n){
  dt1 = dt
  dt[,join_date:=(date-n)]
  dt1$join_date = dt1$date
  key1 = colnames(dt)[3]
  key2 = colnames(dt)[5]
  setkeyv(dt,c(key1,key2))
  setkeyv(dt1,c(key1,key2))
  dt2 = dt1[dt,mult='last',roll=T,rollends=c(T,T)]
  return (dt2)
}

# unique name counts for same ssn (past/7/3) and neutralize values for the frivolous ssn '737610282'
# app = as.data.table(app)
name_ssn0 = app[,.(date,name,ssn)]
name_ssn0 = name_ssn0[,uni_name_ssn:=cumsum(!duplicated(name)),by=ssn]
name_ssn = laguni1(name_ssn0,7)
name_ssn$nameLag7ssn = ifelse(name_ssn$date>name_ssn$join_date,name_ssn$i.uni_name_ssn,name_ssn$i.uni_name_ssn-name_ssn$uni_name_ssn)
name_ssn = name_ssn[,c(3,6,7,9)]
colnames(name_ssn)[2:3] = substring(colnames(name_ssn)[2:3],3)
name_ssn2 = laguni1(name_ssn0,3)
name_ssn$nameLag3ssn = ifelse(name_ssn2$date>name_ssn2$join_date,name_ssn2$i.uni_name_ssn,name_ssn2$i.uni_name_ssn-name_ssn2$uni_name_ssn)
name_ssn = unique(name_ssn)
name_ssn_mean = colMeans(name_ssn[ssn!='737610282',4:5])
name_ssn[,4:5] = lapply(name_ssn[,4:5],as.numeric)
name_ssn = name_ssn[ssn=='737610282',`:=`(nameLag7ssn=name_ssn_mean[1],nameLag3ssn=name_ssn_mean[2])]
name_ssn = name_ssn[!duplicated(name_ssn),]

# unique ssn counts for same identifier (past/7/3)
ssn_id0 = app[,.(date,ssn,identifier)]
ssn_id0 = ssn_id0[,uni_ssn_id:=cumsum(!duplicated(ssn)),by=identifier]
ssn_id = laguni1(ssn_id0,7)
ssn_id$ssnLag7id = ifelse(ssn_id$date>ssn_id$join_date,ssn_id$i.uni_ssn_id,ssn_id$i.uni_ssn_id-ssn_id$uni_ssn_id)
ssn_id = ssn_id[,c(3,6,7,9)]
colnames(ssn_id)[2:3] = substring(colnames(ssn_id)[2:3],3)
ssn_id2 = laguni1(ssn_id0,3)
ssn_id$ssnLag3id = ifelse(ssn_id2$date>ssn_id2$join_date,ssn_id2$i.uni_ssn_id,ssn_id2$i.uni_ssn_id-ssn_id2$uni_ssn_id)
ssn_id = unique(ssn_id)

# unique address counts for same identifier (past/7/3)
addr_id0 = app[,.(date,address,identifier)]
addr_id0 = addr_id0[,uni_addr_id:=cumsum(!duplicated(address)),by=identifier]
addr_id = laguni1(addr_id0,7)
addr_id$addrLag7id = ifelse(addr_id$date>addr_id$join_date,addr_id$i.uni_addr_id,addr_id$i.uni_addr_id-addr_id$uni_addr_id)
addr_id = addr_id[,c(3,6,7,9)]
colnames(addr_id)[2:3] = substring(colnames(addr_id)[2:3],3)
addr_id2 = laguni1(addr_id0,3)
addr_id$addrLag3id = ifelse(addr_id2$date>addr_id2$join_date,addr_id2$i.uni_addr_id,addr_id2$i.uni_addr_id-addr_id2$uni_addr_id)
addr_id = unique(addr_id)

# unique phone counts for same identifier (past/7/3)
phone_id0 = app[,.(date,homephone,identifier)]
phone_id0 = phone_id0[,uni_phone_id:=cumsum(!duplicated(homephone)),by=identifier]
phone_id = laguni1(phone_id0,7)
phone_id$phoneLag7id = ifelse(phone_id$date>phone_id$join_date,phone_id$i.uni_phone_id,phone_id$i.uni_phone_id-phone_id$uni_phone_id)
phone_id = phone_id[,c(3,6,7,9)]
colnames(phone_id)[2:3] = substring(colnames(phone_id)[2:3],3)
phone_id2 = laguni1(phone_id0,3)
phone_id$phoneLag3id = ifelse(phone_id2$date>phone_id2$join_date,phone_id2$i.uni_phone_id,phone_id2$i.uni_phone_id-phone_id2$uni_phone_id)
phone_id = unique(phone_id)

#### join all tables generated so far to original data table
app = left_join(app,count[,-3],by=c('date'='date'))
app = left_join(app,name[,-4],by=c('date'='date','name'='name'))
app = left_join(app,dob[,-4],by=c('date'='date','dob'='dob'))
app = left_join(app,id[,-4],by=c('date'='date','identifier'='identifier'))
app = left_join(app,ssn[,-4],by=c('date'='date','ssn'='ssn'))
app = left_join(app,addr[,-4],by=c('date'='date','address'='address'))
app = left_join(app,phone[,-4],by=c('date'='date','homephone'='homephone'))
app = left_join(app,name_ssn,by=c('date'='date','ssn'='ssn','name'='name'))
app = left_join(app,ssn_id,by=c('date'='date','identifier'='identifier','ssn'='ssn'))
app = left_join(app,addr_id,by=c('date'='date','identifier'='identifier','address'='address'))
app = left_join(app,phone_id,by=c('date'='date','identifier'='identifier','homephone'='homephone'))

## change all integers to numeric
app[,12:44] = lapply(app[,12:44],as.numeric)

# colMeans(app[,12:47])
#-------------------------------------- Fraud Score Algorithm  -----------------------------------------------------------------

####z scale:excluding categorical variables
app_nmc=app[,-c(1:11)]

app_nmc_z=data.frame(scale(app_nmc))
# head(app_nmc_z)

## check the scale result whether there is NA or not after zscale
# sapply(app_nmc_z, function(x) sum(is.na(x) | x==""))

# fraud score method 1
#  heuristic algorithm

###PCA
PCA1.Z<-prcomp(app_nmc_z,center =TRUE,scale. = FALSE) 

summary(PCA1.Z)

#####Optional:CalculatingEigenvalues of PC's
# ev <- data.frame(PCA1.Z$sdev^2)
# View(ev)
####Plotting Percentage of Variance Explained by each PC
#plot(PCA1.Z, type="l")

####selecting the top 13 PCs, Variance explained by 93.5%
pca = PCA1.Z$x[,c(1:7)]

### get the top 30 pca proportaion of variance from summary
PCA_var = summary(PCA1.Z)$importance[2,1:30]
PCA_var = data.frame(cbind(seq(1,30,1),PCA_var))
colnames(PCA_var) = c("pcx","variance")

####Plotting Percentage of Variance Explained by each PC????
#plot(PCA1.Z, type="l")

# plot PCA variance chart
ggplot(PCA_var, aes(x = as.factor(pcx), y = (variance) ))+
  geom_bar(stat='identity',color='black',fill='dodgerblue3') +
  geom_line(group = 1, col = "red",size = 0.6)+
  geom_point(col = "red",size=1) +
  ylab("Variance")+
  xlab("PCA factors")+
  ggtitle("PCA variance Chart")+
  theme_classic()

#plot chart to show relation between PCA and original x
rot = PCA1.Z$rotation
pc2 = cbind(row.names(rot),rot[,2])
colnames(pc2) = c("org","pc")
pc2 = data.frame(pc2)
pc2$pc = as.numeric(as.character(pc2$pc))
# str(pc1)
pc2 %>%
  arrange(-pc) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(org,pc), y = pc))+
  geom_bar(stat = "identity",fill = "dodgerblue3" , colour = "black",size = .2,bins = 30)+
  coord_flip() +
  xlab("PC2")+
  ylab("Original variables")+
  ggtitle("PC2 vs Original Variables")+
  theme_classic()

####Heuristic Function
heur = data.frame(scale(pca))
Score.Heur = apply(heur^2,1,sum)^(1/2)
## scale the score to 0-1
Score.Heur = (Score.Heur-min(Score.Heur))/(max(Score.Heur)-min(Score.Heur))
Score.Heur = data.frame(Score.Heur)
# summary(Score.Heur)

ggplot(Score.Heur, aes(x = Score.Heur, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "white",size=.2,bins=120)+
  geom_line(stat='density',adjust=2)+
  xlab("Heuristic Fraud Score")+
  ylab("") +
  ggtitle("Heuristic Algorithm Fraud Score Histogram")+
  theme_classic()

## get the row number of top 1% heuristic fraud score
ix1 = order(Score.Heur,decreasing=T)[1:10000]


#### Autoencoder
ae_pca = h2o.init()
ae_train = as.h2o(pca,destination_frame = 'autoencoder_train')
feature_names = colnames(pca)
ae_dl = h2o.deeplearning(x=feature_names,training_frame = ae_train,
                         autoencoder=TRUE, reproducible=TRUE, seed=1234,
                         hidden=c(7),epochs=50)
ae_anon = h2o.anomaly(ae_dl, ae_train, per_feature = FALSE)
## get reconstruction error
err = as.data.frame(ae_anon)
Score.AE = err$Reconstruction.MSE^(1/6)
## scale ae fraud score to 0-1
Score.AE = (Score.AE-min(Score.AE))/(max(Score.AE)-min(Score.AE))
Score.AE = data.frame(Score.AE)
# summary(Score.AE)
ggplot(Score.AE, aes(x = Score.AE, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "white",size=.2,bins=120)+
  geom_line(stat='density',adjust=2)+
  xlab("Autoencoder Fraud Score")+
  ylab("") +
  ggtitle("Autoencoder Algorithm Fraud Score Histogram")+
  theme_classic()

## get the row number of top1% ae fraud score
ix2 = order(Score.AE,decreasing=T)[1:10000]

## calculate the overlapping rownumber of the two score, it's 52%
overlap = ifelse(ix1 %in% ix2, 1, 0)
sum(overlap)/length(overlap)

#### final score
## give 70% weight to ae score and plot the distribution
Score.Combined = 0.7*Score.AE+0.3*Score.Heur
Score.Combined = data.frame(Score.Combined)
colnames(Score.Combined)='Score.Combined'
ggplot(Score.Combined, aes(x = Score.Combined, y = ..density.. ))+ 
  geom_histogram(fill = "dodgerblue3" , colour = "white",size=.2,bins=120)+
  geom_line(stat='density',adjust=2)+
  xlab("Combined Fraud Score")+
  ylab("") +
  ggtitle("Combined Fraud Score Histogram")+
  theme_classic()

## get the top1% score and add the three scores back to original data before pca
#str(Z_ny2_smp),23
ix3 = order(Score.Combined,decreasing=T)

#### still go with records with top Autoencoder score because it's more intepratable
app_score = cbind(Score.Heur,Score.AE,Score.Combined,app[,2:9])[ix3,]
#write.csv(app_score, file = "Applications_OrderByFraudScore.csv")

