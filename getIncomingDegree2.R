require(patchwork)
require(ggplot2)
require(Matrix)
require(igraph)
require(parallel)
load('fullNet2.RData')
source('networkFunctions.R')
auxEnv = new.env()
load('macroPlots/macroPlots.RData',envir = auxEnv)
bigDates = as.list(auxEnv)$bigDates
N = as.list(auxEnv)$N
N = N[,1]
IF = auxEnv$I/auxEnv$N
BCCF = auxEnv$CC/auxEnv$N
rm(auxEnv)
incommers = lapply(bigDates,function(date0){
  dates = as.Date(date0) + -30:30
  arrivingID = lapply(dates,function(date){
    arriving_in_degrees(g,date)
  })
  do.call(c,arrivingID)
})

incommersSug = lapply(bigDates,function(date0){
  dates = as.Date(date0) + -30:30
  arrivingID = lapply(dates,function(date){
    arriving_in_degrees(g,date,relation = 'suggest')
  })
  do.call(c,arrivingID)
})

plotDir = 'paperPlots'
plotName = 'inDegDistAtArrivalDep.png'
png(paste(plotDir,plotName,sep='/'))
{
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
    theme_bw()+ 
    ggtitle('Normalized In degree at arrival (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Normalized in degree at arrival')+ylab('Density')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  breaks = c(0,1.2**(0:20))
  X = NULL
  xAjuste = NULL
  yAjuste = NULL
  for(q in c(seq(length(bigDates),1,-1),length(bigDates))){
    date = bigDates[q]
    degs = incommers[[q]]
    degs = degs[degs>0]
    degs = degs/mean(degs)
    if(N[q]>5e3){
      X = c(X,degs)
      h = hist(degs,breaks=breaks,plot=FALSE)
      xAjuste = c(xAjuste,h$mids)
      yAjuste = c(yAjuste,h$density)
    }
    df = data.frame(mids=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=date)
    gg = gg + geom_point(data = df,mapping = aes(x=mids,y=density,color=date))
    gg = gg + geom_line(data = df,mapping = aes(x=mids,y=density,color=date))
    if(q==length(bigDates))   gg = gg + geom_point(data = df,mapping = aes(x=mids,y=density),color='black')
  }
  # Ajuste de grado 2
  xA = log(xAjuste)
  yA = log(yAjuste)
  xA = xA[yAjuste>0]
  yA = yA[yAjuste>0]
  xA2 = xA**2
  mod = lm(yA~xA+xA2)
  xa = sort(unique(xA))
  df = data.frame('x'=exp(xa),'y'=exp(predict(mod,newdata = data.frame('xA'=xa,'xA2'=xa**2))))
  gg = gg + geom_line(data = df,mapping = aes(x=x,y=y),color='black',lwd=1.2)
  summary(mod)
  print(gg)
  dev.off()
}

plotDir = 'paperPlots'
plotName = 'meandegandp0_Dep.png'
png(paste(plotDir,plotName,sep='/'))
{
  
  gg1 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
    theme_bw()+ 
    #ggtitle('In degree at arrival (mean[>0]) (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Mean degree at arrival (non-independent)')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  
  mdeg = sapply(incommers,function(x) mean(x[x>0],na.rm=T))
  df = data.frame(date=bigDates,mean=mdeg)
  gg1 = gg1 + geom_point(data = df,mapping = aes(x=date,y=mean,color=date))
  Y = log(df$mean[bigDates>as.Date('2013-01-01')])
  X = df$date[bigDates>as.Date('2013-01-01')]
  mod = lm(Y~X)
  summary(mod)
  df$predict = exp(predict(mod,newdata = data.frame('X'=df$date)))
  gg1 = gg1 + geom_line(data = df[bigDates>as.Date('2013-01-01'),],mapping = aes(x=date,y=predict,color=date))
  #print(gg1)
  
  
  
  gg2 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
    theme_bw()+ 
    #ggtitle('Probability of new independent package (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Prob. of new indep. package (dependency)')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  
  p0 = sapply(incommers,function(x) mean(x==0,na.rm=T))
  df = data.frame(date=bigDates,mean=p0)
  gg2 = gg2 + geom_point(data = df,mapping = aes(x=date,y=mean,color=date))
  print(gg1 / gg2)
  dev.off()
}


plotDir = 'paperPlots'
plotName = 'inDegDistAtArrivalSug.png'
png(paste(plotDir,plotName,sep='/'))
{
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
    theme_bw()+ 
    ggtitle('Normalized In degree at arrival (suggests)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Normalized in degree at arrival')+ylab('Density')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  breaks = c(0,1.2**(0:20))
  X = NULL
  xAjuste = NULL
  yAjuste = NULL
  for(q in c(seq(length(bigDates),1,-1),length(bigDates))){
    date = bigDates[q]
    degs = incommersSug[[q]]
    degs = degs[degs>0]
    degs = degs/mean(degs)
    if(N[q]>5e3){
      X = c(X,degs)
      h = hist(degs,breaks=breaks,plot=FALSE)
      xAjuste = c(xAjuste,h$mids)
      yAjuste = c(yAjuste,h$density)
    }
    df = data.frame(mids=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=date)
    gg = gg + geom_point(data = df,mapping = aes(x=mids,y=density,color=date))
    gg = gg + geom_line(data = df,mapping = aes(x=mids,y=density,color=date))
    if(q==length(bigDates))   gg = gg + geom_point(data = df,mapping = aes(x=mids,y=density),color='black')
  }
  # Ajuste de grado 2
  xA = log(xAjuste)
  yA = log(yAjuste)
  xA = xA[yAjuste>0]
  yA = yA[yAjuste>0]
  xA2 = xA**2
  mod = lm(yA~xA+xA2)
  xa = sort(unique(xA))
  df = data.frame('x'=exp(xa),'y'=exp(predict(mod,newdata = data.frame('xA'=xa,'xA2'=xa**2))))
  gg = gg + geom_line(data = df,mapping = aes(x=x,y=y),color='black',lwd=1.2)
  summary(mod)
  print(gg)
  dev.off()
}


plotDir = 'paperPlots'
plotName = 'meandegandp0_Sug.png'
png(paste(plotDir,plotName,sep='/'))
{
  
  gg1 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
    theme_bw()+ 
    #ggtitle('In degree at arrival (mean[>0]) (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Mean degree at arrival (non-independent)')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  
  mdeg = sapply(incommersSug,function(x) mean(x[x>0],na.rm=T))
  df = data.frame(date=bigDates,mean=mdeg)
  gg1 = gg1 + geom_point(data = df,mapping = aes(x=date,y=mean,color=date))
  Y = log(df$mean[bigDates>as.Date('2013-01-01')])
  X = df$date[bigDates>as.Date('2013-01-01')]
  mod = lm(Y~X)
  summary(mod)
  df$predict = exp(predict(mod,newdata = data.frame('X'=df$date)))
  gg1 = gg1 + geom_line(data = df[bigDates>as.Date('2013-01-01'),],mapping = aes(x=date,y=predict,color=date))
  #print(gg1)
  
  
  
  gg2 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
    theme_bw()+ 
    #ggtitle('Probability of new independent package (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Prob. of new indep. package (suggest)')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  
  p0 = sapply(incommersSug,function(x) mean(x==0,na.rm=T))
  df = data.frame(date=bigDates,mean=p0)
  gg2 = gg2 + geom_point(data = df,mapping = aes(x=date,y=mean,color=date))
  print(gg1 / gg2)
  dev.off()
}

p0D = sapply(incommers,function(x) mean(x==0,na.rm=T))
p0S = sapply(incommersSug,function(x) mean(x==0,na.rm=T))
mdegD = sapply(incommers,function(x) mean(x[x>0],na.rm=T))
mdegS = sapply(incommersSug,function(x) mean(x[x>0],na.rm=T))


plotDir = 'paperPlots'
plotName = 'meandegandp0_vsBCC.png'
png(paste(plotDir,plotName,sep='/'))
{
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
  #scale_x_log10()+
  #  scale_y_log10()+
  theme_bw()+ 
  #ggtitle('In degree at arrival (mean[>0]) (dependency)')+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab('')+
  xlab('Proportion of packages in BCC')+ 
  #annotation_logticks(sides = 'l')+ 
  scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
colnames(IF) = paste('I',c('D','S','B'),sep='')
colnames(BCCF) = paste('BCC',c('D','S','B'),sep='')
df = as.data.frame(IF)
df = cbind(df,BCCF)
df$p0D = p0D
df$p0S = p0S
df$mdegD = mdegD
df$mdegS = mdegS
df$date = bigDates
dCut = as.Date('2015-01-01')
gg = gg + geom_point(data = df[bigDates>dCut,],mapping = aes(x=BCCD,y=p0D,color=date,pch='dependency|NIP'))
gg = gg + geom_point(data = df[bigDates>dCut,],mapping = aes(x=BCCD,y=p0D,color=date,pch='dependency|NIP'))
gg = gg + geom_point(data = df[bigDates>dCut,],mapping = aes(x=BCCS,y=p0S,color=date,pch='suggest|NIP'))
gg = gg + geom_point(data = df[bigDates>dCut,],mapping = aes(x=BCCD,y=mdegD,color=date,pch='dependency|MD'))
gg = gg + geom_point(data = df[bigDates>dCut,],mapping = aes(x=BCCS,y=mdegS,color=date,pch='suggest|MD'))
mod = lm(p0D~BCCD,data=df[bigDates>dCut,])
df$p0D_ = predict(mod,newdata = df)
mod = lm(mdegD~BCCD,data=df[bigDates>dCut,])
df$mdegD_ = predict(mod,newdata = df)
mod = lm(p0S~BCCS,data=df[bigDates>dCut,])
df$p0S_ = predict(mod,newdata = df)
mod = lm(mdegS~BCCS,data=df[bigDates>dCut,])
df$mdegS_ = predict(mod,newdata = df)
gg = gg + geom_line(data = df[bigDates>dCut,],mapping = aes(x=BCCD,y=p0D_,color=date,pch='dependency|NIP'))
gg = gg + geom_line(data = df[bigDates>dCut,],mapping = aes(x=BCCS,y=p0S_,color=date,pch='suggest|NIP'))
gg = gg + geom_line(data = df[bigDates>dCut,],mapping = aes(x=BCCD,y=mdegD_,color=date,pch='dependency|MD'))
gg = gg + geom_line(data = df[bigDates>dCut,],mapping = aes(x=BCCS,y=mdegS_,color=date,pch='suggest|MD'))
print(gg)
dev.off()}



plotDir = 'paperPlots'
plotName = 'inDegDistAtArrivalDepAndSug.png'
png(paste(plotDir,plotName,sep='/'))
{
  gg1 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
    theme_bw()+ 
    #ggtitle('Normalized In degree at arrival (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Normalized dependency in degree at arrival')+
    ylab('Density')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  breaks = c(0,1.2**(0:20))
  X = NULL
  xAjuste = NULL
  yAjuste = NULL
  for(q in c(seq(length(bigDates),1,-1),length(bigDates))){
    date = bigDates[q]
    degs = incommers[[q]]
    degs = degs[degs>0]
    degs = degs/mean(degs)
    if(N[q]>5e3){
      X = c(X,degs)
      h = hist(degs,breaks=breaks,plot=FALSE)
      xAjuste = c(xAjuste,h$mids)
      yAjuste = c(yAjuste,h$density)
    }
    df = data.frame(mids=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=date)
    gg1 = gg1 + geom_point(data = df,mapping = aes(x=mids,y=density,color=date))
    gg1 = gg1 + geom_line(data = df,mapping = aes(x=mids,y=density,color=date))
    if(q==length(bigDates))   gg = gg + geom_point(data = df,mapping = aes(x=mids,y=density),color='black')
  }
  # Ajuste de grado 2
  xA = log(xAjuste)
  yA = log(yAjuste)
  xA = xA[yAjuste>0]
  yA = yA[yAjuste>0]
  xA2 = xA**2
  mod = lm(yA~xA+xA2)
  xa = sort(unique(xA))
  df = data.frame('x'=exp(xa),'y'=exp(predict(mod,newdata = data.frame('xA'=xa,'xA2'=xa**2))))
  gg1 = gg1 + geom_line(data = df,mapping = aes(x=x,y=y),color='black',lwd=1.2)

  
  
  gg2 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
    theme_bw()+ 
    #ggtitle('Normalized suggest degree at arrival')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Normalized suggest degree at arrival')+
    ylab('Density')+ 
    annotation_logticks(sides = 'l')+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  breaks = c(0,1.2**(0:20))
  X = NULL
  xAjuste = NULL
  yAjuste = NULL
  for(q in c(seq(length(bigDates),1,-1),length(bigDates))){
    date = bigDates[q]
    degs = incommersSug[[q]]
    degs = degs[degs>0]
    degs = degs/mean(degs)
    if(N[q]>5e3){
      X = c(X,degs)
      h = hist(degs,breaks=breaks,plot=FALSE)
      xAjuste = c(xAjuste,h$mids)
      yAjuste = c(yAjuste,h$density)
    }
    df = data.frame(mids=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=date)
    gg2 = gg2 + geom_point(data = df,mapping = aes(x=mids,y=density,color=date))
    gg2 = gg2 + geom_line(data = df,mapping = aes(x=mids,y=density,color=date))
    if(q==length(bigDates))   gg = gg + geom_point(data = df,mapping = aes(x=mids,y=density),color='black')
  }
  # Ajuste de grado 2
  xA = log(xAjuste)
  yA = log(yAjuste)
  xA = xA[yAjuste>0]
  yA = yA[yAjuste>0]
  xA2 = xA**2
  mod = lm(yA~xA+xA2)
  xa = sort(unique(xA))
  df = data.frame('x'=exp(xa),'y'=exp(predict(mod,newdata = data.frame('xA'=xa,'xA2'=xa**2))))
  gg2 = gg2 + geom_line(data = df,mapping = aes(x=x,y=y),color='black',lwd=1.2)
  summary(mod)
  print(gg1 / gg2)
  dev.off()
}

