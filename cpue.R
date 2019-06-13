setwd('C:\\Matias\\MACN\\CPUE')
library(lubridate)
library(cede)

#-----DATA SECTION-----
dat=read.csv('Datos_8_barcos.csv',stringsAsFactors=F)


#----PARAMETER SECTION----



#----PROCEDURE SECTION----


#1. Data manipulation 
dat$Hs..Agua_drop=substr(dat$Hs..Agua,3,3)
dat=subset(dat,!Hs..Agua_drop=="?")  #remove "?"
dat$Hs..Agua=as.numeric(with(dat,ifelse(Hs..Agua=='sd',NA,Hs..Agua)))
dat$N..lances=as.numeric(with(dat,ifelse(N..lances=='sd',NA,N..lances)))
dat$Lances_dia=24*dat$N..lances/dat$Hs..Agua
dat=subset(dat,Lances_dia<12)
dat$Estado.del=factor(dat$Estado.del)

Species=c('Angel','Gallo','Gatuzo','Raya',"Tiburon")
nSp=length(Species)
Indx=match(Species,colnames(dat))

for(i in 1:nSp)
{
  if(Species[i]=='Angel')
  {
    dat[,Indx[i]]=as.numeric(ifelse(dat[,Indx[i]]%in%c('sp','sd'),NA,ifelse(dat[,Indx[i]]=='',0,dat[,Indx[i]])))
  }else
  {
    dat[,Indx[i]]=as.numeric(ifelse(dat[,Indx[i]]%in%c('sp','sd'),NA,ifelse(dat[,Indx[i]]=='',0,dat[,Indx[i]])))
    dat[,Indx[i]]=ifelse(is.na(dat[,Indx[i]]),0,dat[,Indx[i]])
  }
}

dat$Esfuerzo=dat$N..lances  #define effort variable


#definir factores
dat$Barco=factor(dat$Barco)
dat$Fecha=as.Date(dat$Fecha,format="%d/%m/%Y")
dat$Mes=factor(month(dat$Fecha),levels=1:12)
dat$Year=factor(year(dat$Fecha))

#explore variable
check.vars=c('Esfuerzo','Gatuzo')
fun.explr=function(VAR)
{
  dd=dat[,match(c(VAR,'Year','Mes'),names(dat))]
  id=match(VAR,names(dd))
  plot(dd$Year,dd[,id],main=VAR,col="pink")
  mtext("Year",1,2,cex=1.5)
  plot(dd$Mes,dd[,id],main=VAR)
  mtext("Mes",1,2,cex=1.5)
  
}
for(i in 1:length(check.vars))
{
  tiff(file=paste("Explore_",check.vars[i],".tiff",sep=''),width = 2400, height = 2400,units = "px", res = 300, compression = "lzw")
  par(mfcol=c(2,1),mai=c(.2,.2,.1,.1),oma=c(2,2,.1,.1))
  fun.explr(VAR=check.vars[i])
  dev.off()
}



#calcular cpue de cada especie
cpue.lab=paste("cpue_",Species,sep="")
dummy=as.data.frame(matrix(,nrow=nrow(dat),ncol=length(cpue.lab)))
names(dummy)=cpue.lab
for(i in 1:nSp) dummy[,i]=dat[,Indx[i]]/dat$Esfuerzo
dat=cbind(dat,dummy)


#Exploratory analyses

#captura por anio y mes
test=round(tapsum(dat,"Angel","Year","Mes"),2) 


#Estandarizacion
Species.cpue=paste("cpue_",Species,sep="")
Indx.cpue=match(Species.cpue,colnames(dat))
Factores=c('Year','Mes','Barco')
stand.vars=c(Factores,Species.cpue)
model.species=vector('list',length=nSp)
names(model.species)=Species
YRS=unique(dat$Year)
fn.stand=function(SP)
{
  Terms=stand.vars[-match(SP,stand.vars)]
  Formula=as.formula(paste(SP, "~",paste(Terms,collapse='+'),sep=''))
  modelo=glm(Formula,data=dat,family = gaussian)
  
  dummy=dat[1:length(YRS),match(stand.vars,names(dat))]
  dummy$Year=YRS
  chnge.fact=Factores[which(!Factores=='Year')]
  for(m in 1:length(chnge.fact))
  {
    Tab=sort(table(dat[,match(chnge.fact[m],names(dat))]))
    Tab=names(Tab[length(Tab)])
    dummy[,match(chnge.fact[m],names(dummy))]=factor(Tab,levels=levels(dat[,match(chnge.fact[m],names(dat))]))
  }
  
  chnge.cov=Species.cpue[which(!Species.cpue== SP)]
  for(m in 1:length(chnge.cov))
  {
    dummy[,match(chnge.cov[m],names(dummy))]=mean(dat[,match(chnge.cov[m],names(dat))],na.rm=T)
  }
  
  preds=predict(modelo,newdata=dummy,'response',se.fit=T)
  
  return(list(modelo=modelo,species=SP,dummy=dummy,pred.mean=preds$fit,pred.SE=preds$se.fit))
}

for(i in 1:nSp)
{
  model.species[[i]]=fn.stand(SP=Species.cpue[i])
}


plot(as.numeric(YRS),model.species[[1]]$pred.mean,ylim=c(20,50))
segments(as.numeric(YRS),model.species[[1]]$pred.mean,
         as.numeric(YRS),model.species[[1]]$pred.mean+1.96*model.species[[1]]$pred.SE)
segments(as.numeric(YRS),model.species[[1]]$pred.mean,
         as.numeric(YRS),model.species[[1]]$pred.mean-1.96*model.species[[1]]$pred.SE)



#----REPORT SECTION----


