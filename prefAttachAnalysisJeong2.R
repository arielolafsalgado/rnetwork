require(ggplot2)
load('fullNet2.RData')
auxEnv = new.env()
load('macroPlots/macroPlots.RData',envir = auxEnv)
bigDates = as.list(auxEnv)$bigDates
N = as.list(auxEnv)$N
N = N[,1]
rm(auxEnv)
source('networkFunctions.R')
load('prefAttachAnalysisJeongv2.RData')
load('prefAttachAnalysisJeongSv2.RData')
save.image('paperPlots/prefAttachAnalysisJeong2.RData')
spTs = range(as.Date(unlist(str_split(V(g)$lifespan,' > '))),na.rm = TRUE)
T0s = bigDates
T1s = T0s + 60*6
dTs = ifelse(as.numeric(T0s-min(T0s))>500,30,10)
reduction = which(T0s> spTs[1] & T1s + dTs<= spTs[2])
T0s = T0s[reduction]
T1s = T1s[reduction]
dTs = dTs[reduction]
jeongFit = list()
for(i in 1:length(T0s)){ 
  #lapply(1:length(T0s),function(i){
  print(i)
  jeongFit[i] = list(prefAttachDistJeong(g,T0s[i],T1s[i],dTs[i]))
}#)
save.image('prefAttachAnalysisJeongv2.RData')
T0s = T0s[1:length(jeongFit)]
T1s = T1s[1:length(jeongFit)]
dTs = dTs[1:length(jeongFit)]
xlim = c(1,1e3)
ylim= c(1e-5,1e0)
colors = rainbow(length(jeongFit))
plot(1,1,col='white',log='xy',xlim=xlim,ylim=ylim)
i = 30
for(i in 1:length(jeongFit)){
  x = jeongFit[[i]]$Degrees
  h = hist(x,breaks=c(0,2**(0:15)),plot=F)
  # points(h$mids,cumsum(h$density),col=colors[i])
  # lines(h$mids,cumsum(h$density),col=colors[i])
  lines(h$mids,h$density,col=colors[i],pch=18)
}
plotDir = 'macroPlots/'
plotName = 'jeong.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Degree')+ylab('Probability of choose')+ 
    scale_x_log10() + scale_y_log10() +
    ggtitle('Jeong PA')
  u = NULL
  dateu = NULL
  uig = NULL
  for(i in seq(5,length(jeongFit),4)){
    x = jeongFit[[i]]$Degrees
    if(length(x)>0){
      h = hist(x,breaks=c(0,2**(0:15)),plot=F)
      df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=as.Date(T1s[i]))
      gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
      x = log(df$mids)
      y = log(df$density)
      mod = lm(y~x)
      u = c(u,-mod$coefficients[2])
      dateu = c(dateu,as.Date(T1s[i]))
      df$mod = exp(predict(mod,newdata = data.frame('x'=log(df$mids))))
      gg = gg + geom_line(data=df,mapping=aes(x=mids,y=mod,color=date))
      print(mod$coefficients)
    }
  }
  print(gg)
  dev.off()
}


plotDir = 'paperPlots'
plotName = 'jeongCumulated_Dep.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Degree')+ylab('Cumulated probability of choose')+ 
    scale_x_log10() + scale_y_log10() +
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle('Jeong PA Dependency')
  v = NULL
  datev = NULL
  sv = NULL
  X = NULL
  for(i in seq(1,length(jeongFit),by=4)){
    x = jeongFit[[i]]$Degrees
    if(length(x)>0){
      X_ = mean(x)
      #h = hist(x,breaks=0:(max(x)+1)-.5,plot=F)
      h = hist(x,breaks=c(0,2**(0:(log(max(x,1),base=2)+1))),plot=F)
      df = data.frame('mids'=h$mids,'density'=cumsum(h$density*diff(h$breaks)),'date'=as.Date(T1s[i]))
      df = df[df$density>0,]
      df = df[df$mids>1,]
      if(nrow(df)>1){
        gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
        gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
        #if(bigDates[i]>as.Date('2013-01-01')){
        if(i==length(jeongFit)){
          x = log(df$mids)
          y = log(df$density)
          y = y[is.finite(x)]
          x = x[is.finite(x)]
          x = x[is.finite(y)]
          y = y[is.finite(y)]
          #x = x[y<log(.8)]
          #y = y[y<log(.8)]
          mod = lm(y~x)
          v = c(v,-mod$coefficients[2]+1)
          datev = c(datev,as.Date(T1s[i]))
          X = c(X,X_)
          sv = c(sv,summary(mod)$coefficients[2,2])
          df$mod = exp(predict(mod,newdata = data.frame('x'=log(df$mids))))
          gg = gg + geom_line(data=df,mapping=aes(x=mids,y=mod),color='black')
        }
        if(i==length(jeongFit)){
          gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density),color='black')
        }
      }
    }
  }
  print(gg)
  dev.off()
}

plotName = 'jeongDep_cum_normalized.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Normalized degree')+ylab('Cumulated probability of choose')+ 
    scale_x_log10() + scale_y_log10() +
    ggtitle('Jeong PA')
  v = NULL
  datev = NULL
  sv = NULL
  X = NULL
  for(i in seq(1,length(jeongFit),by=5)){
    x = jeongFit[[i]]$Degrees
    if(length(x)>0){
      X_ = mean(x)
      #g_ = getNetworkSnapshot(g,as.Date(T1s[i]))
      #X__ = mean(degree(g_,mode='out'))
      #x = x/X__
      #h = hist(x,breaks=0:(max(x)+1)-.5,plot=F)
      h = hist(x,breaks=c(0,1.2**(-15:(log(max(x,1),base=1.2)+1))),plot=F)
      df = data.frame('mids'=h$mids,'density'=cumsum(h$density*diff(h$breaks)),'date'=as.Date(T1s[i]))
      df = df[df$density>0,]
      df = df[df$mids>1,]
      if(nrow(df)>1){
        gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
        gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
        x = log(df$mids)
        y = log(df$density)
        y = y[is.finite(x)]
        x = x[is.finite(x)]
        x = x[is.finite(y)]
        y = y[is.finite(y)]
        #x = x[y<log(.8)]
        #y = y[y<log(.8)]
        mod = lm(y~x)
        v = c(v,-mod$coefficients[2]+1)
        datev = c(datev,as.Date(T1s[i]))
        X = c(X,X_)
        sv = c(sv,summary(mod)$coefficients[2,2])
        df$mod = exp(predict(mod,newdata = data.frame('x'=log(df$mids))))
        gg = gg + geom_line(data=df,mapping=aes(x=mids,y=mod,color=date))
      }
    }
  }
  print(gg)
  dev.off()
}


plotName = 'jeongDep_cumcoef.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Exponent')+ 
    #scale_x_log10() + scale_y_log10() +
    ggtitle('Jeong PA')
  gg = gg + geom_point(data=data.frame(date=as.Date(datev,origin=as.Date('1970-01-01')),expo = v),mapping=aes(x=date,y=expo))
  print(gg)
  dev.off()
}

plotName = 'jeongDep_mdeg.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  alfas = seq(0.5,1.5,.5)
  kmeanlist = lapply(alfas,function(alfa){
    k1alfa = sapply(DDO[['dependency']],function(x) mean(x**(1+alfa)))
    kalfa = sapply(DDO[['dependency']],function(x) mean(x**alfa))  
    return(k1alfa/kalfa)
  })
  k1v = sapply(DDO[['dependency']],function(x) mean(x**(1+median(v))))
  kv = sapply(DDO[['dependency']],function(x) mean(x**median(v)))  
  kmean_fit = k1v/kv    
  k1u = sapply(DDO[['dependency']],function(x) mean(x**(1+median(u,na.rm=T))))
  ku = sapply(DDO[['dependency']],function(x) mean(x**median(u,na.rm=T)))  
  kmean_fit2 = k1u/ku    
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Mean selected degree')+ 
    #scale_x_log10() + 
    scale_y_log10() +
    ggtitle('Jeong PA')
  gg = gg + geom_point(data=data.frame(date=as.Date(datev,origin=as.Date('1970-01-01')),expo = X),mapping=aes(x=date,y=expo))
  for(i in 1:length(kmeanlist)){
    gg = gg + geom_line(data=data.frame(date=as.Date(bigDates,origin=as.Date('1970-01-01')),expo = kmeanlist[[i]],alfa=alfas[i]),mapping=aes(x=date,y=expo,color=alfa))
  }
  gg = gg + geom_point(data=data.frame(date=as.Date(bigDates,origin=as.Date('1970-01-01')),expo = kmean_fit),mapping=aes(x=date,y=expo),color='red')
  gg = gg + geom_point(data=data.frame(date=as.Date(bigDates,origin=as.Date('1970-01-01')),expo = kmean_fit2),mapping=aes(x=date,y=expo),color='green')
  print(gg)
  dev.off()
}

jeongFitS = list()
for(i in 1:length(T0s)){ 
  #lapply(1:length(T0s),function(i){
  print(i)
  jeongFitS[i] = list(prefAttachDistJeong(g,T0s[i],T1s[i],dTs[i],relation = 'suggest'))
}#)
save.image('prefAttachAnalysisJeongSv2.RData')


plotName = 'jeongCumulated_Sug.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Degree')+ylab('Cumulated probability of choose')+ 
    scale_x_log10() + scale_y_log10() +
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle('Jeong PA Suggest')
  v = NULL
  datev = NULL
  sv = NULL
  X = NULL
  Xm = NULL
  for(i in seq(1,length(jeongFitS),by=4)){
    x = jeongFitS[[i]]$Degrees
    if(length(x)>0){
      X_ = mean(x)
      Xm_ = median(x)
      if(length(x)>0){
        #h = hist(x,breaks=0:(max(x)+1)-.5,plot=F)
        h = hist(x,breaks=c(0,2**(0:(log(max(x,1),base=2)+1))),plot=F)
        df = data.frame('mids'=h$mids,'density'=cumsum(h$density*diff(h$breaks)),'date'=as.Date(T1s[i]))
        df = df[df$density>0,]
        df = df[df$mids>1,]
        if(nrow(df)>1){
          gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
          gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
          if(i==length(jeongFitS)){
            gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density),color='black')
          }
          # x = log(df$mids[df$mids<=1e2])
          # y = log(df$density[df$mids<=1e2])
          # y = y[is.finite(x)]
          # x = x[is.finite(x)]
          # x = x[is.finite(y)]
          # y = y[is.finite(y)]
          # x = x[y<log(.8)]
          # y = y[y<log(.8)]
          # if(length(x)>1){
          #   mod = lm(y~x)
          #   v = c(v,-mod$coefficients[2]+1)
          #   datev = c(datev,as.Date(T1s[i]))
          #   sv = c(sv,summary(mod)$coefficients[2,2])
          #   X = c(X,X_)
          #   Xm = c(Xm,Xm_)
          #   df$mod = exp(predict(mod,newdata = data.frame('x'=log(df$mids))))
          #   gg = gg + geom_line(data=df,mapping=aes(x=mids,y=mod,color=date))
          # }
        }
      }
    }
  }
  print(gg)
  dev.off()
}

plotName = 'jeongDep_cumcoefS.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Exponent')+ 
    #scale_x_log10() + scale_y_log10() +
    ggtitle('Jeong PA')
  gg = gg + geom_point(data=data.frame(date=as.Date(datev,origin=as.Date('1970-01-01')),expo = v),mapping=aes(x=date,y=expo))
  print(gg)
  dev.off()
}



plotName = 'jeongDep_mdeg.png'
png(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  alfas = seq(0.5,1.5,.5)
  kmeanlist = lapply(alfas,function(alfa){
    k1alfa = sapply(DDA[['suggest']],function(x) mean(x**(1+alfa)))
    kalfa = sapply(DDA[['suggest']],function(x) mean(x**alfa))  
    return(k1alfa/kalfa)
  })
  k1v = sapply(DDA[['suggest']],function(x) mean(x**(1+median(v))))
  kv = sapply(DDA[['suggest']],function(x) mean(x**median(v)))  
  kmean_fit = k1v/kv    
  k1u = sapply(DDA[['suggest']],function(x) mean(x**(1+median(u,na.rm=T))))
  ku = sapply(DDA[['suggest']],function(x) mean(x**median(u,na.rm=T)))  
  kmean_fit2 = k1u/ku    
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Mean selected degree')+ 
    #scale_x_log10() + 
    scale_y_log10() +
    ggtitle('Jeong PA')
  gg = gg + geom_point(data=data.frame(date=as.Date(datev,origin=as.Date('1970-01-01')),expo = X,type='Mean'),mapping=aes(x=date,y=expo,pch=type))
  gg = gg + geom_point(data=data.frame(date=as.Date(datev,origin=as.Date('1970-01-01')),expo = Xm,type='Median'),mapping=aes(x=date,y=expo,pch=type))
  for(i in 1:length(kmeanlist)){
    gg = gg + geom_line(data=data.frame(date=as.Date(bigDates,origin=as.Date('1970-01-01')),expo = kmeanlist[[i]],alfa=alfas[i],type='Estimated'),mapping=aes(x=date,y=expo,color=alfa))
  }
  gg = gg + geom_point(data=data.frame(date=as.Date(bigDates,origin=as.Date('1970-01-01')),expo = kmean_fit,type='Fit in density'),mapping=aes(x=date,y=expo,pch=type),color='red')
  gg = gg + geom_point(data=data.frame(date=as.Date(bigDates,origin=as.Date('1970-01-01')),expo = kmean_fit2,type='Fit in cum. density'),mapping=aes(x=date,y=expo,pch=type),color='green')
  print(gg)
  dev.off()
}
