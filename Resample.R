
#Age VS Length
DATA=read.csv("C:/Matias/MACN/Data.csv")  #bring in data
#check data
plot(DATA$Age,DATA$DW)

#estimate parameters
vbTypical <- DW~Linf*(1-exp(-K*(Age-to)))   #growth function
svTypical.VB = c(Linf=176,K=0.08,to=-1.8)   #initial parameter values
  
fit.vonb <- nls(vbTypical,data=DATA,start=svTypical.VB)
Coef.fit=coef(fit.vonb)
Coefs.fit.and.SE=summary(fit.vonb)$parameters[,1:2]
Model.AIC=AIC(fit.vonb)  #extract model AIC if comparing to other models (e.g. Gompertz)

#Predict model
NEW.data=data.frame(Age=1:25)    #new data, in this case a vector of ages
Preds=predict(fit.vonb,newdata=NEW.data)
plot(DATA$Age,DATA$DW,ylab="Disc width",xlab="Age")
lines(NEW.data$Age,Preds,col=2)


#Maturity VS Length
Dat=read.csv("C:/Matias/MACN/Maturity_whisk_Simpfendorferetal1998.csv")

mod <- nls(Prop.Mat~1/(1+exp(-log(19)*(FL-p50)/(p95-p50))), start=c(p50=115, p95=120), data=Dat)

fit=summary(mod)$parameters[,1:2]

fn.logis=function(dat,p50,p95) 1/(1+exp(-log(19)*(dat-p50)/(p95-p50)))

plot(Dat$FL,Dat$Prop.Mat,pch=19)
SEQ=80:140
lines(SEQ,fn.logis(SEQ,fit[1,1],fit[2,1]),col=2) 
