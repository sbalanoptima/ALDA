###########################################################################################################
####################################Demand prediction model################################################
###########################################################################################################

####Read in data and divide into training & test set

library(plyr)
library(dplyr)
dat=read.table("Demand.csv",sep=",",header=T)
table(dat$j)
dem1=dat[dat$j==1,]
pairs(dem1)

#### Binomial random variate for generating 75/25 Training and test data
sel.row=rbinom(10000,1,0.25)
dat2=cbind(dat,sel.row)
test.dat=dat2[dat2$sel.row==1,]
train.dat=dat2[dat2$sel.row==0,]


### Regression model for demand prediction
N = 7
regMods <- vector(mode="list", length=N)
MAD <- vector()
Rseq <- vector()
regMods[[1]]=lm(demand~temp+prec+wind,data=train.dat)
regMods[[2]]=lm(demand~temp+prec+wind+factor(j),data=train.dat)
regMods[[3]]=lm(demand~temp+prec+wind+temp:prec+temp:wind+prec:wind+temp:prec:wind,data=train.dat)
regMods[[4]]=lm(demand~temp+prec+wind+factor(j)+temp:prec+temp:wind+prec:wind+temp:prec:wind,data=train.dat)
regMods[[5]]=lm(demand~temp+prec+wind+factor(j)+temp:factor(j)+prec:factor(j)+wind:factor(j),data=train.dat)
regMods[[6]]=lm(demand~temp+prec+wind+factor(j)+temp:factor(j)+prec:factor(j)+wind:factor(j)+temp:prec+temp:wind+prec:wind+temp:prec:wind,data=train.dat)
regMods[[7]]=lm(demand~temp+prec+wind+factor(j)+temp:prec+temp:wind+prec:wind+temp:prec:wind+temp:factor(j)+prec:factor(j)+wind:factor(j)+temp:prec:factor(j)+temp:wind:factor(j)+prec:wind:factor(j)+temp:prec:wind:factor(j),data=train.dat)

###Use regression model to predict on test data
for(i in 1:N)
{
pred.test=predict(regMods[[i]],newdata = test.dat)
all.dat=cbind(test.dat,pred.test)
error=all.dat$demand-all.dat$pred.test
MAD[i]= mean(abs(error))
hist(error)
SSE=sum(error^2)
TSS=sum((test.dat$demand-mean(test.dat$demand))^2)
Rseq[i]=1-SSE/TSS
}
results <- data.frame(MAD,Rseq)
results

for (i in 1:7)
  {
summary(regMods[[i]])
  }

#####Now do this demand prediction on test cases 100 data...
dat=read.table("test_dat2.csv",sep=",",header=T)
demand.test2=matrix(nrow=nrow(dat),ncol=10)
for (i in 1:10)
{dat$j=i
demand.test2[,i]=predict(reg.1,newdata=dat)
}
colnames(demand.test2)=c("1","2","3","4","5","6","7","8","9","10")
test2=cbind(dat[-5],demand.test2)
write.table(test2,"dem_100_2.csv",sep=",",col.names=T,row.names=F)

#########################################################################################################
##################################Travel times prediction model##########################################
#########################################################################################################

###### Input the travel time data set########

travel=read.table('Travel.csv',sep=',',header=T)
len.dat=read.table('Length.csv',sep=',',header=T)
all.dat=merge(travel,len.dat,by=c('i','j'),type='left',match='all')

################Dividing training and test data##########################################################

sel.row=rbinom(nrow(all.dat),1,0.25)
all.dat2=cbind(all.dat,sel.row)
test.dat=all.dat2[all.dat2$sel.row==1,]
train.dat=all.dat2[all.dat2$sel.row==0,]
temp=table(train.dat$i,train.dat$j)
index.matrix=matrix(nrow=31,ncol=2)
k=1
for (i in 1:nrow(temp))
{for (j in 1:ncol(temp))
{if (temp[i,j]>0) {
  index.matrix[k,1]=as.numeric(rownames(temp)[i])
  index.matrix[k,2]=as.numeric(colnames(temp)[j])
  k=k+1}}}

coef.lmdat=matrix(nrow=31,ncol=4)
R2.matrix=vector(length=31)


####Fit model for each scenario on train then evaluate on test
for(k in 1:31)
{
  dat.temp=train.dat[train.dat$i==index.matrix[k,1] & train.dat$j==index.matrix[k,2],]
  nlreg1=lm(duration~temp+prec+wind+I(as.numeric(temp)^2)+temp:wind+prec:wind+I(as.numeric(temp)^2):wind,data=dat.temp)
  nlreg2=lm(duration~temp+prec+wind+I(as.numeric(temp)^2)+temp:wind+prec:wind+temp:prec+I(as.numeric(temp)^2):wind+I(as.numeric(temp)^2):prec+I(as.numeric(temp)^3),data=dat.temp)
  nlreg3=lm(duration~temp+prec+wind+I(as.numeric(temp)^2)+I(as.numeric(temp)^3),data=dat.temp)
  nlreg4=lm(duration~temp+prec+wind+temp:prec+temp:wind+wind:prec+temp:prec:wind,data=dat.temp)
  nlreg5=lm(duration~temp+prec+wind+temp:prec+temp:wind+wind:prec+temp:prec:wind+I(as.numeric(temp)^2)+I(as.numeric(wind)^2)+I(as.numeric(prec)^2),data=dat.temp)
  nlreg6=lm(duration~temp+prec+wind+temp:prec+temp:wind+wind:prec+temp:prec:wind+I(as.numeric(temp)^2)+I(as.numeric(wind)^2)+I(as.numeric(prec)^2)+I(as.numeric(temp)^2):wind+I(as.numeric(temp)^2):prec,data=dat.temp)
  nlreg7=lm(duration~temp+prec+wind,data=dat.temp)
  
  dat.temp2=test.dat[test.dat$i==index.matrix[k,1] & test.dat$j==index.matrix[k,2],]
  pred.temp=predict(nlreg7,newdata=dat.temp2)
  error=dat.temp2$duration-pred.temp
  MAD_lp1= mean(abs(error))
  hist(error)
  SSE=sum((dat.temp2$duration-pred.temp)^2)
  TSS=sum((dat.temp2$duration-mean(dat.temp2$duration))^2)
  R2=1-(SSE/TSS)
  coef.lmdat[k,]=coef(nlreg7)
  R2.matrix[k]=R2
}
MAD_lp1
###### Print R2 values for 31 combinations of cities
for (k in 1:31)
{
  aaaa=R2.matrix[k]
  print(aaaa)
}

####Now to save the models...
lm.model=vector("list",31)
for(k in 1:31)
{
  dat.temp=train.dat[train.dat$i==index.matrix[k,1] & train.dat$j==index.matrix[k,2],]
  #nlreg2=lm(duration~temp+prec+wind+I(as.numeric(temp)^2)+temp:wind+prec:wind+temp:prec+I(as.numeric(temp)^2):wind+I(as.numeric(temp)^2):prec+I(as.numeric(temp)^3),data=dat.temp)
  nlreg3=lm(duration~temp+prec+wind+I(as.numeric(temp)^2)+I(as.numeric(temp)^3),data=dat.temp)
  #nlreg6=lm(duration~temp+prec+wind+temp:prec+temp:wind+wind:prec+temp:prec:wind+I(as.numeric(temp)^2)+I(as.numeric(wind)^2)+I(as.numeric(prec)^2)+I(as.numeric(temp)^2):wind+I(as.numeric(temp)^2):prec,data=dat.temp)
  lm.model[[k]]=nlreg3
}
lm.model




####Now on the test data for 50 test cases
library(prodlim)
dat=read.table("test_dat.csv",sep=",",header=T)
test.dat1=matrix(rep(dat[1,],31),nrow=31,byrow=T)
colnames(test.dat1)=colnames(dat)
test.dat1=cbind(test.dat1,index.matrix)
colnames(test.dat1)[5:6]=c("i","j")
pred.test=vector(length=31)

for (k in 1:31)
{
  pred.test[k]=predict(lm.model[[k]],newdata=test.dat1[k,])
  }
output.test=cbind(index.matrix,pred.test)
colnames(output.test)=c("i","j","pred")

##### For all the test values for 50 test cases###############
output.matrix=matrix(nrow=31,ncol=nrow(dat))

for (kk in 1:nrow(dat))
{test.dat1=matrix(rep(dat[kk,],31),nrow=31,byrow=T)
colnames(test.dat1)=colnames(dat)
test.dat1=cbind(test.dat1,index.matrix)
colnames(test.dat1)[5:6]=c("i","j")
pred.test=vector(length=31)

for (k in 1:31)
{pred.test[k]=predict(lm.model[[k]],newdata=test.dat1[k,])}
output.matrix[,kk]=pred.test
}
output.test=cbind(index.matrix,output.matrix)
write.table(output.test,"travel_50.csv",sep=",",row.names=T,col.names=T)


##### For all the test values for 100 test cases###############

dat=read.table("test_dat2.csv",sep=",",header=T)
output.matrix=matrix(nrow=31,ncol=nrow(dat))
for (kk in 1:nrow(dat))
{test.dat1=matrix(rep(dat[kk,],31),nrow=31,byrow=T)
colnames(test.dat1)=colnames(dat)
test.dat1=cbind(test.dat1,index.matrix)
colnames(test.dat1)[5:6]=c("i","j")
pred.test=vector(length=31)
for (k in 1:31)
{pred.test[k]=predict(lm.model[[k]],newdata=test.dat1[k,])}
output.matrix[,kk]=pred.test
}
output.test=cbind(index.matrix,output.matrix)
write.table(output.test,"travel_100.csv",sep=",",row.names=T,col.names=T)

