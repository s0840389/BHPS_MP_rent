rm(list=ls())

require(data.table)
require(sandwich)
require(lmtest)
require(ivreg)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(reshape2)
require(dplyr)
require(stringr)
require(readxl)

setwd('~/OneDrive/Documents/research/BHPS/MP_rents_hp')

# data

  load('input/dt_clean.rda')

  dtagg=data.table(read_excel('input/svar_data_v2.xlsx',sheet='monthly',range='A2:AT386'))
  
    # create lags
  dtagg$year=year(dtagg$date)
  dtagg$month=month(dtagg$date)
  
    Y=dtagg[,.(year,month,`1 year rate`,`Exchange Rate`,`Corporate Spread`,FTSE,GDP,FSScm2,`CPI core`,svarshock)]
    
    colnames(Y)=c('year','month','r1yr','FX','corp_spread','FTSE','GDP','FSScm2_SR','CPIc','svarshock')
  
    Y$dr=(Y$r1yr-lag(Y$r1yr))/100
    Y$svarshock=Y$svarshock/100
    Y$FSScm2_SR=Y$FSScm2_SR/100
    
    Y$qtr=ceiling(Y$month/3)
    
    Yq=Y[,.(dr=sum(dr),FSScm2_SR=sum(FSScm2_SR),
            svarshock=sum(svarshock),
            GDP=mean(GDP),
            FTSE=mean(FTSE),
          CPIc=mean(CPIc),
          corp_spread=mean(corp_spread)
          ),by=.(year,qtr)]
    
    for(x in colnames(Yq)[3:ncol(Yq)]){
      
      for(l in c(1:4)){
        Yq=Yq[,paste(x,'_L',l,sep=''):=lag(Yq[[x]],l)]
      }
    }    
    
    Yq$dgdp=(lead(Yq$GDP,4)-Yq$GDP)/100
    
    macrovars=c('dr','FTSE','GDP','CPIc','corp_spread')
    macrocontrols=paste(rep(macrovars,4),'_L',kronecker(c(1:4),rep(1,length(macrovars))),sep='')
    
    
  # merge on macro data  
    
    mininc=72*52 # based on real unemployment benefit 
    
    dtm=merge(dt,Yq,by.x=c('year','qtr'),by.y=c('year','qtr'),all.x=TRUE)
    
    dtm=dtm[hoh==1&ytot>mininc &age_dv>=20&age_dv<=80&year>=1996&year<=2019&renter_so==0]
    #dtm=dtm[ytot>mininc &age_dv>=20&age_dv<=80&year>=1996&year<=2019]
    
    # additional cleaning steps
    
    dtm=dtm[dint_1 %in% c(10,11,12,13,14)&
              !(year %in% c(2007,2008,2009))
            &is.na(owner)==FALSE
            &is.na(owner_H1)==FALSE
            &is.na(owner_H2)==FALSE]


    # categorical tenure
    dtm$tenure='Owner'
    dtm$tenure=ifelse(dtm$renter_pr==1,'PrRenter',dtm$tenure)
    dtm$tenure=ifelse(dtm$renter_so==1,'SoRenter',dtm$tenure)
    
    # age cat
    dtm$agecat=cut(dtm$age_dv,c(19,30,40,50,65,100),labels=c('20-30','30-40','40-50','50-65','65+'))
    
    
    
      wgtrebal<-function(dt){ # rebalance to above to give each year same weight 
        
        dtw=dt[,.(wyear=sum(indin_lw,na.rm=TRUE),
                  wyearx=sum(indin_xw,na.rm=TRUE)),by=.(year)]
        
        dtw$wyear=1000/dtw$wyear
        dtw$wyearx=1000/dtw$wyearx
        
          
        dt=merge(dt,dtw,by=c('year'),all.x=TRUE)
        
        dt$indin_lwnorm=dt$indin_lw*dt$wyear
        dt$indin_xwnorm=dt$indin_xw*dt$wyearx
        
        minxw=quantile(dt$indin_xwnorm[dt$indin_xwnorm>0],0.0,na.rm=TRUE)
        minlw=quantile(dt$indin_lwnorm[dt$indin_lwnorm>0],0.0,na.rm=TRUE)
        
        dt$indin_xwnorm[dt$indin_xwnorm<minxw]=minxw
        dt$indin_lwnorm[dt$indin_lwnorm<minlw]=minlw
        
      return(dt)        
      }
      
      dtm=wgtrebal(dtm)
    
      dtm$wgt_sel=dtm$indin_lwnorm
  
## aggregate checks
      
    # macro controls
    Xagg=as.matrix(Yq[,macrocontrols,with=FALSE])
    Xm=as.matrix(dtm[,macrocontrols,with=FALSE])
    
    # macro specification
    
    magg6=ivreg(data=Yq,lead(GDP,6)~dr+Xagg,~FSScm2_SR+Xagg)
    V.magg6=vcovHAC(magg6)
    magg.coef6=coeftest(magg6,V.magg6)
    
  # initial check on correlation with gdp
    
    m.dgdp=summary(lm(data=dtm,dytot_H1~dgdp,weights = indin_lwnorm))

## micro regressions    
    
   # 1 year to owner transition
    
    m.h1.own=ivreg(data=dtm,owner_H1~dr*factor(owner)+Xm,~FSScm2_SR*factor(owner)+Xm,weights=indin_lwnorm)
    V.h1.own=vcovCL(m.h1.own,~year)
    coef.h1.own=coeftest(m.h1.own,V.h1.own)
    
    # 2 year to-owner transition
    
    m.h2.own=ivreg(data=dtm,owner_H2~dr*factor(owner)+Xm,~FSScm2_SR*factor(owner)+Xm,weights=indin_lwnorm)
    V.h2.own=vcovCL(m.h2.own,~wave)
    coef.h2.own=coeftest(m.h2.own,V.h2.own)
    
    
    # 1 year to-landlord transition
    
    m.h1.ll=ivreg(data=dtm,landlordF_H1~dr*factor(landlordF)+Xm,~FSScm2_SR*factor(owner)+Xm,weights=indin_lwnorm)
    V.h1.ll=vcovCL(m.h1.ll,~wave)
    coef.h1.ll=coeftest(m.h1.ll,V.h1.ll)
    
    # 2 year to-landlord transition
    
    m.h2.ll=ivreg(data=dtm,landlordF_H2~dr*factor(landlordF)+Xm,~FSScm2_SR*factor(owner)+Xm,weights=indin_lwnorm)
    V.h2.ll=vcovCL(m.h2.ll,~wave)
    coef.h2.ll=coeftest(m.h2.ll,V.h2.ll)  
    
    # 1 year hhsize 
    
    dtm$MultiAdult=1*(dtm$nadults>2)
    dtm$MultiAdult_H1=1*(dtm$nadults_H1>2)
    dtm$MultiAdult_H2=1*(dtm$nadults_H2>2)
    
    m.h1.size=ivreg(data=dtm,MultiAdult_H1~dr + MultiAdult+Xm,~FSScm2_SR*+MultiAdult+Xm,weights=indin_lwnorm)
    V.h1.size=vcovCL(m.h1.size,~wave)
    coef.h1.size=coeftest(m.h1.size,V.h1.size)  
    
    # 2 year size 
    m.h2.size=ivreg(data=dtm,MultiAdult_H2~dr +MultiAdult+Xm,~FSScm2_SR +MultiAdult+Xm,weights=indin_lwnorm)
    V.h2.size=vcovCL(m.h2.size,~wave)
    coef.h2.size=coeftest(m.h2.size,V.h2.size)  
    
    # 1 year size age
    m.h1.sizeage=ivreg(data=dtm,MultiAdult_H1~dr*agecat +MultiAdult+Xm,~FSScm2_SR*agecat +MultiAdult+Xm,weights=indin_lwnorm)
    V.h1.sizeage=vcovCL(m.h1.sizeage,~wave)
    coef.h1.sizeage=coeftest(m.h1.sizeage,V.h1.sizeage)  
    
    # 2 year size age
    m.h2.sizeage=ivreg(data=dtm,MultiAdult_H2~dr*agecat +MultiAdult+Xm,~FSScm2_SR*agecat +MultiAdult+Xm,weights=indin_lwnorm)
    V.h2.sizeage=vcovCL(m.h2.sizeage,~wave)
    coef.h2.sizeage=coeftest(m.h2.sizeage,V.h2.sizeage)  
    
    # 1 year transition by age
    
    m.h1.age=ivreg(data=dtm,owner_H1~dr*factor(agecat)+Xm,~FSScm2_SR*factor(agecat)+Xm,weights=indin_lwnorm)
    V.h1.age=vcovCL(m.h1.age,~wave)
    coef.h1.age=coeftest(m.h1.age,V.h1.age)
        
    # 2 year transition by age
    
    m.h2.age=ivreg(data=dtm,owner_H2~dr*factor(agecat)+Xm,~FSScm2_SR*factor(agecat)+Xm,weights=indin_lwnorm)
    V.h2.age=vcovCL(m.h2.age,~wave)
    coef.h2.age=coeftest(m.h2.age,V.h2.age)
    
    # 1 year transition by income
    
    m.h1.inc=ivreg(data=dtm,owner_H1~dr*factor(g2_indbar3)+Xm,~FSScm2_SR*factor(g2_indbar3)+Xm,weights=indin_lwnorm)
    V.h1.inc=vcovCL(m.h1.inc,~wave)
    coef.h1.inc=coeftest(m.h1.inc,V.h1.inc)
    
    # 2 year transition by income
    
    m.h2.inc=ivreg(data=dtm,owner_H2~dr*factor(g2_indbar3)+Xm,~FSScm2_SR*factor(g2_indbar3)+Xm,weights=indin_lwnorm)
    V.h2.inc=vcovCL(m.h2.inc,~wave)
    coef.h2.inc=coeftest(m.h2.inc,V.h2.inc)
    
## Figures    
    
    fg.hor<-function(coef,V,x,q,tit='',xlab='',ylab=''){
      
      xloc=c(grep(x,row.names(coef))[1],grep(paste(x,':',sep=''),row.names(coef)))
      
      pe=coef[xloc,1]+coef[xloc[1],1]
      pe[1]=coef[xloc[1],1]
      
      varx=diag(V)[xloc] + V[xloc[1],xloc[1]] + 2*V[xloc[1],xloc]
      varx[1]=V[xloc[1],xloc[1]] 
      
      l68=pe-sqrt(varx)*qnorm(0.84)
      l90=pe-sqrt(varx)*qnorm(0.95)
      u68=pe+sqrt(varx)*qnorm(0.84)
      u90=pe+sqrt(varx)*qnorm(0.95)
      
      dtfg=data.frame(q,pe,l68,l90,u68,u90)  
      
      fg=ggplot(data=dtfg)+geom_line(aes(x=q,y=pe),color='darkblue',linewidth=1.2)+
        geom_ribbon(aes(x=q,ymin=l68,ymax=u68),fill='blue',alpha=0.2)+
        #geom_ribbon(aes(x=q,ymin=l90,ymax=u90),fill='blue',alpha=0.1)+
        theme_minimal(base_size=12)+labs(x=xlab,y='',title=tit)
      
      return(fg) 
    }
    
    fg.age=fg.hor(coef.h1.age,V.h1.age,'dr',c(25,35,45,57.5,75),'Panel B: Prob of owning home','age','Percentage points')
    
    fg.age=fg.age+theme_bw()+theme(text=element_text(size=10))
    
    ggsave('own_age.pdf',fg.age,units = 'cm',width=15,height=12)
    
    fg.sizeageh1=fg.hor(coef.h1.sizeage,V.h1.sizeage,'dr',c(25,35,45,57.5,75),'Prob of living in multi-adult house (12 months)','age')
    ggsave('size_age_h1.pdf',fg.sizeageh1,units = 'cm',width=15,height=12)
    
    fg.sizeageh2=fg.hor(coef.h2.sizeage,V.h2.sizeage,'dr',c(25,35,45,57.5,75),'Prob of living in multi-adult house (24 months)','age')
    ggsave('size_age_h2.pdf',fg.sizeageh2,units = 'cm',width=15,height=12)
    
    fg.inc=fg.hor(coef.h1.inc,V.h1.inc,'dr',c(10,30,50,70,85,95),'Prob of owning home (12 months, pp)','Income percentile')
    ggsave('own_inc.pdf',fg.inc,units = 'cm',width=15,height=12)
    
    
    ## robustness
    
    # 1 year to owner transition [svarshock]
    m.h1.ownR1=lm(data=dtm,owner_H1~svarshock*factor(owner)+Xm,weights=indin_lwnorm)
    V.h1.ownR1=vcovCL(m.h1.ownR1,~year)
    coef.h1.ownR1=coeftest(m.h1.ownR1,V.h1.ownR1)
    
    # 1 year to owner transition [same sample period]
    sel=dtm$year>=2004
    m.h1.ownR2=ivreg(data=dtm[sel],owner_H1~dr*factor(owner)+Xm[sel,],~FSScm2_SR*factor(owner)+Xm[sel,],weights=indin_lwnorm)
    V.h1.ownR2=vcovCL(m.h1.ownR2,~year)
    coef.h1.ownR2=coeftest(m.h1.ownR2,V.h1.ownR2)
    
    # 1 year to owner transition [svarshock]
    m.h1.ownR3=lm(data=dtm[sel],owner_H1~svarshock*factor(owner)+Xm[sel,],weights=indin_lwnorm)
    V.h1.ownR3=vcovCL(m.h1.ownR3,~year)
    coef.h1.ownR3=coeftest(m.h1.ownR3,V.h1.ownR3)
    
    
    # print regression results
    sink('reg_output.txt')
    print('correlation with gdp')
    print(m.dgdp)
    print('prob ownership 12 months')
    print(coef.h1.own)
    print('prob ownership 24 months')
    print(coef.h2.own)
    print('Landlord prob: 1) 12 months 2) 24 months')
    print(coef.h1.ll)
    print(coef.h2.ll)
    print('Multi adult prob: 1) 12 months 2) 24 months')
    print(coef.h1.size)
    print(coef.h2.size)
    print('Robstness: 1) svarhsock 2) sample period 3) svarshock sample period')
    print(coef.h1.ownR1)
    print(coef.h1.ownR2)
    print(coef.h1.ownR3)
    sink()
    
