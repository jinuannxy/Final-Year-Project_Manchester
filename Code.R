#######################some tools########################
par(mfrow=c(1,1))
par(mfrow=c(2,2))
par(mfrow=c(3,1))
par(mfrow=c(1,2))
par(mfrow=c(3,2))

#######################some tools########################








############### data upload###################################
DSfull<-read.table(file = "used_cars.txt",header = TRUE)
#remove(DS0)
DS.5<-DSfull[,-c(7:12)]
View(DS.5)
X1<-as.matrix(DS.5[c(2)])
X2<-as.matrix(DS.5[c(3)])
X3<-as.matrix(DS.5[c(4)])
X4<-as.matrix(DS.5[c(5)])
X5<-as.matrix(DS.5[c(6)])
Y<-as.matrix(DS.5[c(1)])
str(DS.5)
DS.5.m<-as.matrix(DS.5)
########## data upload##############






##########    EDA   ###########
summary(Y)
summary(X2)
summary(X1)
summary(X3)
density(X4,X4[names=1])
DS.5[which(DS.5[,4]%in%c(1))]
Da1<-DS.5[DS.5$extras1==1,]
summary(Da1$price)
Da0<-DS.5[DS.5$extras1==0,]
summary(Da0$price)
Db1<-DS.5[DS.5$extras2==1,]
summary(Db1$price)
Db0<-DS.5[DS.5$extras2==0,]
summary(Db0$price)
plot(DS.5$price~DS.5$age,xlab="Ages in Months",ylab = "Price",identify(c(2.5,2.0,6.5,6.6))





####################fitting model#################
head(DS.5)
lm1<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5)
lm1
plot(lm1)
abline(h=0,lty=2)
abline(a=0,b=1,lty=2)
summary(lm1)
####################fitting model#################

###########    plots of y to each X    ###########

##  For all  ##
library(car)
scatterplotMatrix(~price+age+kilometer+TIA,data=DS.5,
                  spread=FALSE,smoother=loessLine,smoother.args=list(lty=2),
                  main="Scatter Plot under Package Car",
                  )


plot(X1,Y,xlab = "Ages of the car in months",ylab = "Price in 1000 Euro",
     main = "Price vs Age")
abline(lm(Y~X1),col="red")
age.lo<-loess(DS.5$price~DS.5$age,data = DS.5)
newage<-seq(min(X1),max(X1),len=172)
smoothage=predict(age.lo)
plot(X1,Y)
lines(X1[order(X1)],smoothage[order(X1)],col="blue",lwd=1)


loessage<-loess(DS.5$price~DS.5$age,span = 0.8)
smoothedage80<-predict(loessage)
lines(smoothedage80,x=DS.5$age,col="blue")

?scatterplotMatrix




plot(X2,Y,xlab = "Kilometers Reading in 1000km",ylab = "Price in 1000 Euro",
     main = "Price vs Kilometers")
abline(lm(Y~X2),col="red")
kilo.lo<-loess(DS.5$price~DS.5$kilometer,data = DS.5,degree=1,span = 1/3)
newkilo<-seq(min(X2),max(X2),len=172)
smoothkilo=predict(kilo.lo,newdata = data.frame(X2=newkilo))
plot(X2,Y,color="gray",pch=16)
lines(newkilo,smoothkilo,col="blue",lwd=1)




plot(X3,Y,xlab = "Number of Months until the next Apponitment with the TIA",ylab = "Price in 1000 Euro",
     main = "Price vs TIA")
abline(lm(Y~X3),col="red")
TIA.lo<-loess(DS.5$price~DS.5$TIA,data = DS.5,degree=1,span = 0.8)
newTIA<-seq(min(X3),max(X3),len=172)
smoothTIA=predict(TIA.lo,newdata = data.frame(X3=newTIA))
lines(newTIA,smoothTIA,col="blue",lwd=1)


boxplot(DS.5$price~DS.5$TIA,col=(c("orange","gray")),
        main="TIA VS Price", xlab="TIA", ylab="Price in 1000 Euro")
boxplot(DS.5$price~DS.5$extras1,col=(c("gold","darkgreen")),
        main="ABS brake VS Price", xlab="ABS brake", ylab="Price in 1000 Euro",names=c("No","Yes"))
boxplot(DS.5$price~DS.5$extras2,col=(c("lightpink","lightblue")),
        main="Sunroof VS Price", xlab="Sunroof", ylab="Price in 1000 Euro",names=c("No","Yes"))

pairs(DS.5[1:4],label=c("Price","Age","Kilometers","TIA"))



###########    plots of y to each X    ###########





###############Loess regression############################
plot(lm1$fitted.values,lm1$residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",ylab = "Residuals")

newfv<-seq(min(lm1$fitted.values),max(lm1$fitted.values),len=172)
smoothresi=predict(resi.lo,newdata = data.frame(newfv))
lines(newfv,smoothresi,col="blue",lwd=1)
abline(h=0,lty=2)

loessrefv<-loess(lm1$residuals~lm1$fitted.values,span = 0.8)
smoothed80<-predict(loessrefv)
lines(smoothed80,x=lm1$fitted.values,col="blue",lty=2)

sse=0
calcSSE <- function(x){
  loessMod <- try(loess(lm1$residuals ~ lm1$fitted.values, span=x), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}
optim(par=c(0.5), calcSSE, method="SANN")

###############Loess regression############################






################################WRONG code#####################
lm2<-lm(formula=DS.5$price~DS.5$age+DS.5$kilometer+DS.5$TIA+DS.5$extras1,data = DS.5)
summary(lm2)
plot(lm2)
lm2.stdres = rstandard(lm2)

lm3<-lm(formula=DS.5$price~DS.5$age+DS.5$kilometer+DS.5$extras1,data = DS.5)
summary(lm3)
plot(lm3)
lm3.stdres = rstandard(lm3)

lm4<-lm(formula=DS.5$price~DS.5$age+DS.5$kilometer,data = DS.5)
summary(lm4)
plot(lm4)
lm4.stdres = rstandard(lm4)
################################WRONG code#####################







########boxplot##########################################################
boxplot(DS.5$price~DS.5$extras1,col=(c("gold","darkgreen")),
        main="ABS brake VS Price", xlab="ABS brake", ylab="Price",names=c("No","Yes"))
boxplot(DS.5$price~DS.5$extras2,col=(c("lightpink","lightblue")),
        main="Sunroof VS Price", xlab="Sunroof", ylab="Price",names=c("No","Yes"))
boxplot(DS.5$price~DS.5$age,col=(c("red","lightgreen")),
        main="Age VS Price", xlab="Age", ylab="Price")
boxplot(DS.5$price~DS.5$kilometer,col=(c("yellow","purple")),
        main="Kilometers VS Price", xlab="Kilometers", ylab="Price")
boxplot(DS.5$price~DS.5$TIA,col=(c("orange","gray")),
        main="TIA VS Price", xlab="TIA", ylab="Price")

boxplot(lm1.stdres~DS.5$price,col=(c("red","blue")),
        main="StdRes VS Price", xlab="Price", ylab="Standardized Residuals")
########boxplot###################################



#######################standardized residuals#####################
lm1.stdres = rstandard(lm1)
plot(DS.5$price~DS.5$age+DS.5$kilometer+DS.5$TIA+DS.5$extras1+DS.5$extras2,lm1.stdres)
plot(lm1.stdres,ylab = "standardzed residuals")
abline(h=2,col="red")
abline(h=-2,col="red")
plot(lm1$fitted.values,lm1.stdres,
     xlab = "Fitted Values",ylab = "Standardized Residuals",
     main = "StdRes vs Fitted Values")
abline(h=0,lty=2)
loess.stdres<-loess(lm1.stdres~lm1$fitted.values)
smooth.stdres<-predict(loess.stdres)
lines(lm1$fitted.values[order(lm1$fitted.values)],smooth.stdres[order(lm1$fitted.values)],col="blue",lty=1)
#######################standardized residuals########




################################K-S test#######################
ks.test(lm1.stdres,rnorm(172))
################################K-S test#######################


qqnorm(lm1.stdres, 
              ylab="Standardized Residuals", 
              xlab="Normal Scores", 
             main="Normal Q-Q") 
 qqline(lm1.stdres)


sqDS.5<-(DS.5)*t(DS.5)
 remove(lm2)

 
 
 
 
 ################standardized residuals####################
e.m<-as.matrix(lm1$residuals)
t(e.m)%*%e.m
 

##############studentized residuals#########################
plot(lm1$fitted.values,lm1.stdres,
       xlab = "Fitted Values",
       ylab = "Studentized Residuals",
       main = "5-Variables")
abline(0,0)

plot(lm2$fitted.values,lm2.stdres,
       xlab = "Fitted Values",
       ylab = "Studentized Residuals",
       main = "4-Variables")
abline(0,0)

plot(lm3$fitted.values,lm3.stdres,
       +      xlab = "Fitted Values",
       +      ylab = "Studentized Residuals",
       +      main = "3-Variables")
abline(0,0)

plot(lm4$fitted.values,lm4.stdres,
       +      xlab = "Fitted Values",
       +      ylab = "Studentized Residuals",
       +      main = "2-Variables")
> 
  > abline(0,0)


################### Outlier Test ###############
outlierTest(lm1)
leveragePlots(lm1)
resid.raw<-resid(lm1)
resid.std<-rstandard(lm1)
c(Raw=var(resid.raw),Standardized=var(resid.std))
hat<-hatvalues(lm1)

mean(hat)
sort(hat,decreasing = TRUE)[1:4]
plot(hat,type = "h",las=1,ylab = "Leverages Hat Values")
abline(h=2*mean(hat),col="red")
identify(hat)
scatter.smooth( rstandard(lm1) ~ fitted(lm1),
                xlab="Fitted values", ylab="Standardized residuals", las=1)



########## studentized ####################
rstudent(lm1)[3]
setseed(914)
for (i in 1:172){
  x[i]=rstudent(lm1)[i]
  print(x)
}
par(mai=c(2,0.5,0.5,0.5))
plot(stu,main = "Studentized Residuals")
legend(x=xinch(1.5),y=-yinch(2),
       col = c("Red","blue"),
       legend = c("Critical Values=0.99","Critical Values=0.95")
       ,xpd = T,lty = 1)
plot(rt(172,df=165),type = "l")
qt(1,165)
abline(h=qt(0.99,165),col="red")
abline(h=-qt(0.99,165),col="red")
abline(h=qt(0.95,165),col="blue")
abline(h=-qt(0.95,165),col="blue")
abs(x)
sort(stu,decreasing = T)[1:2]
sort(stu,decreasing = F)[1:2]
stu<-rstudent(lm1)
qt(0.99,165)

############ cook's distance  ########
lm1.cd<-cooks.distance(lm1)
plot(lm1.cd,type = "h",las=1,ylab = "Cook's distance")
abline(h=4/(172-5-1),col="red")
sort(lm1.cd,decreasing = T,)[1:3]

library(car)
dfbetasPlots(lm1,intercept = T)
plot(lm1.dfb[,1],xlab = "index",ylab = "Intercept",type = "h")
abline(h=2/sqrt(172),col="red")
abline(h=-2/sqrt(172),col="red")
abline(h=0)
plot(lm1.dfb[,2],xlab = "index",ylab = "Age",type = "h")
abline(h=2/sqrt(172),col="red")
abline(h=-2/sqrt(172),col="red")
abline(h=0)
plot(lm1.dfb[,3],xlab = "index",ylab = "Kilometer",type = "h")
abline(h=2/sqrt(172),col="red")
abline(h=-2/sqrt(172),col="red")
abline(h=0)
plot(lm1.dfb[,4],xlab = "index",ylab = "TIA",type = "h")
abline(h=2/sqrt(172),col="red")
abline(h=-2/sqrt(172),col="red")
abline(h=0)
plot(lm1.dfb[,5],xlab = "index",ylab = "ABS or not",type = "h")
abline(h=2/sqrt(172),col="red")
abline(h=-2/sqrt(172),col="red")
abline(h=0)
plot(lm1.dfb[,6],xlab = "index",ylab = "Sunroof or not",type = "h")
abline(h=2/sqrt(172),col="red")
abline(h=-2/sqrt(172),col="red")
abline(h=0)
?dfbetaPlots
lm1.dff<-dffits(lm1)
plot(lm1.dff,ylab = "DFFITs panel",type="h")
identify(lm1.dff)
dfbetaPlots(lm1, intercept=T, layout=c(3,2),  
            main="DFBETAs panel", labels=rownames(dfbeta), 
            id.method=list("x"),  
            id.n=if(id.method[1]=="identify") Inf else 0, id.cex=1,
            id.col=palette()[1], id.location="lr", col=palette()[1])
?id.method
abline(h=2*sqrt(5/172),col="red")
abline(h=-2*sqrt(5/172),col="red")
abline(h=0)

lm1.plot<-plot(lm1)
identify(hatvalues(lm1),lm1.stdres)

lm1.h<-hatvalues(lm1,type="matrix")
rank(lm1.h)

lm1.beta<-dfbetaPlots(lm1,intercept = T,)
?dfbetaPlots
dfbetaPlots(lm1, terms= ~ ., intercept=T, layout=NULL,  
            main="DFBETAS",  labels=rownames(dfbeta), 
            id.method=sqrt(5/172),  
            id.n=if(id.method[1]=="identify") Inf else 0, id.cex=1,
            id.col=palette()[1], id.location="lr", col=palette()[1], grid=TRUE)
lm1.dfb<-dfbeta(lm1)



#################  refit model  #########################
lm.1<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-1,])
summary(lm.1)
plot(lm.1)
lm.1.stdres<-rstandard(lm.1)
plot(lm.1.stdres,type = "h")
plot(cooks.distance(lm.1))

lm.3<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-3,])
summary(lm.3)
plot(lm.3)
lm.3.stdres<-rstandard(lm.3)
plot(lm.3.stdres,type = "h")
plot(cooks.distance(lm.3))

lm.18<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-18,])
summary(lm.18)
plot(lm.18)
lm.18.stdres<-rstandard(lm.18)
plot(lm.18.stdres,type = "h")
plot(cooks.distance(lm.18))

lm.25<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-25,])
summary(lm.25)
plot(lm.25)
lm.25.stdres<-rstandard(lm.25)
plot(lm.25.stdres)
plot(cooks.distance(lm.25))


lm.26<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-26,])
summary(lm.26)
plot(lm.26)
lm.26.stdres<-rstandard(lm.26)
plot(lm.26.stdres)
plot(cooks.distance(lm.26))

lm.25.35.60<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-c(25,35,60),])
summary(lm.25.35.60)
plot(lm.25)
lm.25.stdres<-rstandard(lm.25)
plot(lm.25.stdres)
plot(cooks.distance(lm.25))

lm.28<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-28,])
summary(lm.28)
plot(lm.28)
lm.28.stdres<-rstandard(lm.28)
plot(lm.28.stdres)
plot(cooks.distance(lm.28))

lm.35<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-35,])
summary(lm.35)
plot(lm.35)
lm.35.stdres<-rstandard(lm.35)
plot(lm.35.stdres)
plot(cooks.distance(lm.35))

lm.43<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-43,])
summary(lm.43)
plot(lm.43)
lm.43.stdres<-rstandard(lm.43)
plot(lm.43.stdres)
plot(cooks.distance(lm.43))

lm.53<-lm(formula=price~age+kilometer+TIA+extras1+extras2,data = DS.5[-53,])
summary(lm.53)
plot(lm.53)
lm.53.stdres<-rstandard(lm.53)
plot(lm.53.stdres)
plot(cooks.distance(lm.53))

re.lm<-lm(formula=price~age+kilometer+TIA+extras1+extras2,
          data = DS.5[-c(25,35,60),])
summary(re.lm)
plot(re.lm)
re.lm.stdres<-rstandard(re.lm)
plot(re.lm.stdres)
plot(cooks.distance(re.lm))
re.lm.in<-influence.measures(re.lm)             
which(apply(re.lm.in$is.inf,1,any))



######### variable detection #############
set.seed(9)
train.index<-sample(x=1:nrow(DS.5),
                    size = ceiling(0.8*nrow(DS.5)))
train.set<-DS.5[train.index,]
test.set<-DS.5[-c(train.index),]
null<-lm(price~1,data = train.set)
null<-lm(price~1,data = DS.5)
full<-lm(price~.,data = train.set)
full<-lm(price~.,data = DS.5)
forward.lm=step(null,
                scope = list(lower=null,upper=full),
                direction = "forward")
null.172<-lm(price~1,data=DS.5)

best_fit<-step(null,scope = list(lower=null,upper=full),
                   scale=0,direction="both")

backward.lm=step(full,
                scope = list(upper=full),
                direction = "backward")

summary(forward.lm)
summary(backward.lm)

install.packages("leaps")
library(leaps)
regfit.full<-regsubsets(price~.,data=DS.5)
summary(regfit.full)

regfit.full<-regsubsets(price~.,data=DS.5)
regfit.full.sum<-summary(regfit.full)
regfit.full.sum
full.sum<-data.frame(regfit.full.sum$outmat,BIC=regfit.full.sum$bic)
full.sum

regfit.fwd<-regsubsets(price~.,data=DS.5,method ="forward")
regfit.fwd.sum<-summary(regfit.fwd)
regfit.fwd.sum
fwd.sum<-data.frame(regfit.fwd.sum$outmat,BIC=regfit.fwd.sum$bic)
fwd.sum

regfit.bwd<-regsubsets(price~.,data=DS.5,method = "backward")
regfit.bwd.sum<-summary(regfit.bwd)
regfit.bwd.sum
bwd.sum<-data.frame(regfit.bwd.sum$outmat,BIC=regfit.bwd.sum$bic)
bwd.sum

regfit.fwd<-regsubsets(price~.,data=DS.5,method ="forward")
regfit.fwd.sum<-summary(regfit.fwd)
fwd.sum<-data.frame(regfit.fwd.sum$outmat,Adjusted.R.square=regfit.fwd.sum$adjr2)
fwd.sum


summary(lm(price~age+kilometer+extras1,data=DS.5))


?rep()

############## cross-valid stepwise by regsubsets()
k=10
set.seed(914)
 
folds<-sample(1:k,nrow(DS.5),replace=T)

cv_errors = matrix(NA,k,5,
                   dimnames = list(NULL,paste(1:5)))

predict.regsubsets<-function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
  

for(j in 1:k){
  regfit_bwd=regsubsets(price~.,data = DS.5[folds!=j,],
                        method = "backward",)
  for(i in 1:5){
    pred=predict(regfit_bwd,DS.5[folds==j,],id=i)
    cv_errors[j,i]=mean((DS.5$price[folds==j]-pred)^2)
  }
}

mean_cv_errors=apply(cv_errors,2,mean)

min=which.min(mean_cv_errors)

plot(mean_cv_errors,type = "b")
points(min,mean_cv_errors[min][1],col="red",cex=2,pch=20)

reg_best_bwd=regsubsets(price)



######## Ridge Regression
X<-t()








