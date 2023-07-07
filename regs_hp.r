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

  dtagg=data.table(read_excel('input/svar_data.xlsx',sheet='monthly',range='A2:AT386'))
  
    # create lags
  dtagg$year=year(dtagg$date)
  dtagg$month=month(dtagg$date)
  
    Y=dtagg[,.(year,month,`1 year rate`,`Exchange Rate`,`Corporate Spread`,FTSE,GDP,FSScm2,`CPI core`,opec,oil,kanzig,svarshock)]
    
    colnames(Y)=c('year','month','r1yr','FX','corp_spread','FTSE','GDP','FSScm2','CPIc','opec','oil','kanzig','svarshock')
  
    Y$dr=(Y$r1yr-lag(Y$r1yr))/100
    Y$svarshock=Y$svarshock/100
    Y$FSScm2=Y$FSScm2/100
    
    Y$qtr=ceiling(Y$month/3)
    
    Yq=Y[,.(dr=sum(dr),FSScm2=sum(FSScm2),
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
    
    dtm=dtm[ytot>mininc &age_dv>=20&age_dv<=80&year>=1996&year<=2019]
    
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
    
    magg6=ivreg(data=Yq,lead(GDP,6)~dr+Xagg,~FSScm2+Xagg)
    V.magg6=vcovHAC(magg24)
    magg.coef6=coeftest(magg24,V.magg24)
    
  # initial check on correlation with gdp
    
    m.dgdp=summary(lm(data=dtm,dytothh_H1~dgdp,weights = indin_lw))

## micro regressions    
    
   # 1 year to owner transition
    
    m.h1.own=ivreg(data=dtm,owner_H1~dr*factor(owner),~FSScm2*factor(owner),weights=indin_lw)
    V.h1.own=vcovCL(m.h1.own,~year)
    coef.h1.own=coeftest(m.h1.own,V.h1.own)

    # 2 year to-owner transition
    
    m.h2.own=ivreg(data=dtm,owner_H2~dr*factor(owner),~FSScm2*factor(owner),weights=indin_lw)
    V.h2.own=vcovCL(m.h2.own,~wave)
    coef.h2.own=coeftest(m.h2.own,V.h2.own)
    
    # 1 year transition by age
    
    m.h1.age=ivreg(data=dtm,owner_H1~dr*factor(agecat),~FSScm2*factor(agecat),weights=indin_lw)
    V.h1.age=vcovCL(m.h1.age,~wave)
    coef.h1.age=coeftest(m.h1.age,V.h1.age)
        
    # 2 year transition by age
    
    m.h2.age=ivreg(data=dtm,owner_H2~dr*factor(agecat),~FSScm2*factor(agecat),weights=indin_lw)
    V.h2.age=vcovCL(m.h2.age,~wave)
    coef.h2.age=coeftest(m.h2.age,V.h2.age)
    
    # 1 year transition by income
    
    m.h1.inc=ivreg(data=dtm,owner_H1~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights=indin_lw)
    V.h1.inc=vcovCL(m.h1.inc,~wave)
    coef.h1.inc=coeftest(m.h1.inc,V.h1.inc)
    
    # 2 year transition by income
    
    m.h2.inc=ivreg(data=dtm,owner_H2~dr*factor(g2_indbar3),~FSScm2*factor(g2_indbar3),weights=indin_lw)
    V.h2.inc=vcovCL(m.h2.inc,~wave)
    coef.h2.inc=coeftest(m.h2.inc,V.h2.inc)
    
## Figures    
    
    fg.hor<-function(coef,V,x,q,tit='',xlab=''){
      
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
        geom_ribbon(aes(x=q,ymin=l90,ymax=u90),fill='blue',alpha=0.1)+
        theme_minimal(base_size=12)+labs(x=xlab,y='',title=tit)
      
      return(fg) 
    }
    
    fg.age=fg.hor(coef.h2.age,V.h2.age,'dr',c(25,35,45,57.5,75),'Prob of owning home (24 months, pp)','age')
    ggsave('own_age.pdf',fg.age,units = 'cm',width=15,height=12)
    
    fg.inc=fg.hor(coef.h2.inc,V.h2.inc,'dr',c(10,30,50,70,85,95),'Prob of owning home (24 months, pp)','Income percentile')
    ggsave('own_inc.pdf',fg.inc,units = 'cm',width=15,height=12)
    
    