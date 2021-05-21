require(patchwork)
require(ggplot2)
require(Matrix)
require(igraph)
require(parallel)
require(vistime)

load('fullNet2.RData')
source('networkFunctions.R')
relations=c('dependency','suggest','both')

pchRel = 16:18
colorRel = c('blue','red','violet')
names(pchRel) = relations
names(colorRel) = relations
par.list=list(
  'pch'=18,
  'cex'=1.3
)
plotDir = 'paperPlots'
dir.create(plotDir)

plotType = 'pdf'
if(plotType=='png'){
  pFun = function(name,width=480*2,height=480*2){
    name = paste(name,'.png',sep='')
    png(name,width,height)
  } 
}else if(plotType=='eps'){
  pFun = function(name,width=480*2,height=480*2){
    name = paste(name,'.eps',sep='')
    setEPS()
    postscript(name)
  }
}else if(plotType=='pdf'){
  pFun = function(name,width=480*2,height=480*2){
    name = paste(name,'.pdf',sep='')
    pdf(name)
  }
}

theme_set(theme_bw()+
            theme(axis.title.y = element_text(size = rel(1.7)),
                  axis.title.x = element_text(size = rel(1.7)),
                  axis.text.x = element_text(size=rel(2)),
                  axis.text.y = element_text(size=rel(2)),
                  legend.text = element_text(size=rel(1.3)),
                  legend.title = element_text(size=rel(1.3)))
)
update_geom_defaults("point", list(size = 5))



##### MP
MP =new.env()
load('paperPlots/macroPlots.RData',envir = MP)
MP = as.list(MP)

with(MP,{
plotName = 'f1A_NandM_vs_date'
#plotName = 'f1CSS2021'
  
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  custom_breaks = seq(as.Date('1999-01-01'),as.Date('2020-01-01'),'1 y')
  labels = gsub('-01-01','',as.character(custom_breaks))
  labels[-seq(2,length(custom_breaks),5)] = ""
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(1,0.5)-.2,
          axis.text.x = element_text(size=rel(1.8)),
          axis.text.y = element_text(size=rel(1.8))) +
    xlab('Date')+ylab('')+ 
    ggtitle('Number of connections and nodes') + 
    scale_x_continuous(breaks=custom_breaks,labels=labels)
  for(relation in c('dependency','suggest')){
    df = data.frame(date=bigDates,x=M[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  gg = gg + geom_point(data=data.frame(date=bigDates,x=N[,1],relation='packages'),aes(x=date,y=x,color=relation,pch=relation))
  #dateR = as.Date(c('2004-10-04','2012-10-01','2015-06-01'))
  #gg = gg + geom_vline(xintercept = dateR,lwd=1,lty='dotted')
  
  print(gg)
  dev.off()
}


plotName = 'f1B_M_vs_N'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  #xlim = c(0,3e3)
  #ylim = c(0,3e4)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
      
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(1,0.5)-.2) +
    xlab('N')+ylab('')+ 
    scale_x_log10()+
    scale_y_log10()+
    ggtitle('Number of connections and nodes')
  for(relation in c('dependency','suggest')){
    df = data.frame(date=N[,1],x=M[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
    x = df$date
    y = df$x
    y = y[x>1e3]
    x = x[x>1e3]
    x = log(x)
    y = log(y)
    mod = lm(y~x)
    df$predicted = exp(predict(mod,newdata = data.frame('x'=log(df$date))))
    gg = gg + geom_line(data=df,aes(x=date,y=predicted,color=relation))
    print(relation)
    print(summary(mod))
    # De acá concluyo que M ~ N^a, con a~1,4
  }
  print(gg)
  dev.off()
}

plotName = 'f2A_mdeg_vs_date'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
      
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(.5,1)-.2) +
    xlab('Date')+ylab('Mean number of relations')+ 
    ggtitle('Mean degree')
  z = 2*M/N
  for(relation in c('dependency','suggest')){
    df = data.frame(date=bigDates,x=z[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
    x = df$date[27:56]
    y = df$x[27:56]
    mod = lm(y~x)
    df1 = df[27:56,]
    print(relation)
    print(summary(mod))
    df1$mod = predict(mod)
    gg = gg + geom_line(data=df1,aes(x=date,y=mod,color=relation))
    x = df$date[57:nrow(df)]
    y = df$x[57:nrow(df)]
    mod = lm(y~x)
    df2 = df[57:nrow(df),]
    print(summary(mod))
    df2$mod = predict(mod)
    gg = gg + geom_line(data=df2,aes(x=date,y=mod,color=relation))
  }
  print(gg)
  dev.off()
}


plotName = 'f2B_BCCandIsolvs_date'
#plotName = 'f2CSS2021'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  custom_breaks = seq(as.Date('1999-01-01'),as.Date('2020-01-01'),'1 y')
  labels = gsub('-01-01','',as.character(custom_breaks))
  labels[-seq(2,length(custom_breaks),5)] = ""
  
  BCCP = CC/N
  isolP = I/N
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
      
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(0.6,0.11),
          legend.text = element_text(size=rel(0.7)),
          legend.title = element_text(size=rel(0.7)),
          legend.direction = 'horizontal',
          legend.spacing.y = unit(0,units = 'cm'),
          legend.background = element_rect(fill = '#FFFFFF00'),
          axis.text.x = element_text(size=rel(1.8)),
          axis.text.y = element_text(size=rel(1.8))) +
    xlab('Date')+ylab('Fraction of nodes')+ 
    ggtitle('Nodes in components') + 
    scale_x_continuous(breaks=custom_breaks,labels=labels)
  
  for(relation in relations[1:2]){
    df = data.frame(date=bigDates,x=BCCP[,relation],relation=relation,set = 'BCC')
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=set))    
    df = data.frame(date=bigDates,x=isolP[,relation],relation=relation,set = 'Independent')
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=set))    
    df = data.frame(date=bigDates,x=(BCCP+isolP)[,relation],relation=relation,set = 'Sum')
    gg = gg + geom_line(data=df,aes(x=date,y=x,color=relation))    
  }

  # dateR = as.Date(c('2004-10-04','2012-10-01','2015-06-01'))
  # gg = gg + geom_vline(xintercept = dateR,lwd=1,lty='dotted')

  print(gg)
  dev.off()
}

plotName = 'f3B_dependency_ddistOut'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'dependency'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
      
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.7,.8)) +
    xlab('Out degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle(relation)
  for(q in 1:length(bigDates)){
    dd = DDO[[relation]][[q]]
    h = hist(dd,breaks=breaks,plot=F)
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density),color='black',lwd=1)
  print(gg)
  dev.off()
}

plotName = 'f3A_dependency_ddistIn'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'dependency'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    
    ggtitle(relation)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.7,0.8)) +
    xlab('In degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  for(q in 1:length(bigDates)){
    dd = DDI[[relation]][[q]]
    h = hist(dd,breaks=breaks,plot=F)
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density),color='black',lwd=1)
  print(gg)
  dev.off()
}

plotName = 'f3C_suggest_ddistAll'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'suggest'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
     
    ggtitle(relation)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.7,0.7)) +
    xlab('Total degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  for(q in 1:length(bigDates)){
    dd = DDA[[relation]][[q]]
    h = hist(dd,breaks=breaks,plot=F)
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density),color='black')
  print(gg)
  dev.off()
}


plotName = 'f7A_transferDep2Sug'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
      
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title=element_blank(),
          legend.position = c(.5,.15)) +
    xlab('Date')+ylab('Fraction of relations')+ 
    ggtitle('Relations between networks')
  df = as.data.frame(relsTransfer)
  df$date=bigDates[2:length(bigDates)]
  df$tFD2S = df$dep2sug/df$newSug
  df$tFS2D = df$sug2dep/df$newDep
  df$jaccard = df$depYsug/df$depOsug
  gg = gg + geom_point(data=df,aes(x=date,y=tFD2S,color='D->S'))    
  gg = gg + geom_point(data=df,aes(x=date,y=tFS2D,color='S->D'))    
  gg = gg + geom_point(data=df,aes(x=date,y=jaccard,color='J(D,S)'))    
  print(gg)
  dev.off()
}

plotName = 'f7C_lostNandM_frac'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
      
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.5,.15)) +
    xlab('Date')+ylab('Removed/New')+ 
    ggtitle('Removed elements')
  df = data.frame('DepE'=relsNewAndLost$dependency[,2]/relsNewAndLost$dependency[,1])
  df$SugE =relsNewAndLost$suggest[,2]/relsNewAndLost$suggest[,1]
  df$PackE =packsNewAndLost[,2]/packsNewAndLost[,1]
  df$date=bigDates[2:length(bigDates)]
  gg = gg + geom_point(data=df,aes(x=date,y=DepE,color='dependency'))    
  gg = gg + geom_point(data=df,aes(x=date,y=SugE,color='suggests'))    
  gg = gg + geom_point(data=df,aes(x=date,y=PackE,color='packages'))    
  print(gg)
  dev.off()
}


plotName = 'f7B_newVsPrevFrac'
pFun(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
      
    #scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.75,.95)) +
    xlab('Date')+ylab('Links between previous / New links')+ 
    ggtitle('Links between previous elements')
  df = data.frame('oldD'=relsNewAndLost$dependency[,3]/relsNewAndLost$dependency[,1])
  df$oldS =relsNewAndLost$suggest[,3]/relsNewAndLost$suggest[,1]
  df$date=bigDates[2:length(bigDates)]
  gg = gg + geom_point(data=df,aes(x=date,y=oldD,color='dependency'))    
  gg = gg + geom_point(data=df,aes(x=date,y=oldS,color='suggests'))    
  print(gg)
  dev.off()
}
})

##### GID
GID =new.env()
load('paperPlots/getIncomingDegree2.RData',envir = GID)
GID = as.list(GID)

with(GID,{

plotName = 'inDegDistAtArrivalDep'
pFun(paste(plotDir,plotName,sep='/'))
{
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
     
    #ggtitle('Normalized DID at arrival')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(.3,.3)) +
    xlab('Normalized DID at arrival')+ylab('Density')+ 
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
plotName = 'meandegandp0_Dep'
pFun(paste(plotDir,plotName,sep='/'))
{
  
  gg1 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
     
    #ggtitle('In degree at arrival (mean[>0]) (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(.7,.3)) +
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
  print(gg1)
  
  
  
  gg2 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
     
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
plotName = 'f5A_inDegDistAtArrivalSug'
pFun(paste(plotDir,plotName,sep='/'))
{
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
     
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
plotName = 'f5B_meandegandp0_Sug'
pFun(paste(plotDir,plotName,sep='/'))
{
  
  gg1 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    scale_y_log10()+
     
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


plotDir = 'paperPlots'
plotName = 'f4A_meandegandp0_vsBCC'
pFun(paste(plotDir,plotName,sep='/'))
{
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    #scale_x_log10()+
    #  scale_y_log10()+
     
    #ggtitle('In degree at arrival (mean[>0]) (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(size=rel(.8)),
          legend.title = element_text(size=rel(.8)),
          legend.position=c(.5,.35),
          legend.box = 'horizontal',
          legend.background = element_rect(color='#FFFFFF00')) +
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
plotName = 'f4B_inDegDistAtArrivalDepAndSug'
pFun(paste(plotDir,plotName,sep='/'))
{
  gg1 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
     
    #ggtitle('Normalized In degree at arrival (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = 'none',
          axis.title.x = element_text(size=rel(1)),
          axis.title.y = element_text(size=rel(1))) +
    xlab('Normalized DID at arrival')+
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
  #print(gg1)
  
  
  gg2 = ggplot() +# expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+
    scale_y_log10()+
    #xlim(0,5)+
     
    #ggtitle('Normalized suggest degree at arrival')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = c(.2,.4),
          legend.background = element_rect(fill='#FFFFFF00'),
          legend.text = element_text(size=rel(.6)),
          legend.title = element_text(size=rel(.6)),
          axis.title.x = element_text(size=rel(1)),
          axis.title.y = element_text(size=rel(1))) +
    xlab('Normalized sug. degree at arrival')+
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
  #print(gg2)
  print(gg1 / gg2)
  dev.off()
}
})

### JPA
JPA =new.env()
load('paperPlots/prefAttachAnalysisJeong2.RData',envir = JPA)
JPA = as.list(JPA)

with(JPA,{
plotName = 'f6A_jeongCumulated_Dep'
pFun(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
      
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(.7,.3)) +
    xlab('Dependency out degree')+ylab('Cumulated probability of choose')+ 
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

plotName = 'f6B_jeongCumulated_Sug'
pFun(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
      
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(.7,.3)) +
    xlab('Suggest degree')+ylab('Cumulated probability of choose')+ 
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

if(F){plotName = 'PA_forCSS2021'
pFun(paste(plotDir,plotName,sep='/'))
{
  #par(par.list)
  ies = seq(1,length(jeongFit),by=8)
  relT = 1.7
  gg1 = ggplot() + #expand_limits(x=xlim,y=ylim)+
    
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position='none',
          axis.text.x = element_text(size=rel(relT))) +
    xlab(element_blank())+ylab(element_blank())+ 
    scale_x_log10() + scale_y_log10() +
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle('')
  v = NULL
  datev = NULL
  sv = NULL
  X = NULL
  for(i in ies){
    x = jeongFit[[i]]$Degrees
    if(length(x)>0){
      X_ = mean(x)
      #h = hist(x,breaks=0:(max(x)+1)-.5,plot=F)
      h = hist(x,breaks=c(0,2**(0:(log(max(x,1),base=2)+1))),plot=F)
      df = data.frame('mids'=h$mids,'density'=cumsum(h$density*diff(h$breaks)),'date'=as.Date(T1s[i]))
      df = df[df$density>0,]
      df = df[df$mids>1,]
      if(nrow(df)>1){
        gg1 = gg1 + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
        gg1 = gg1 + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
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
          #gg1 = gg1 + geom_line(data=df,mapping=aes(x=mids,y=mod),color='black')
        }
        if(i==max(ies)){
          gg1 = gg1 + geom_point(data=df,mapping=aes(x=mids,y=density),color='black')
        }
      }
    }
  }
  #print(gg1)
  
  gg2 = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position=c(.7,.3),
          axis.text.x = element_text(size=rel(relT))) +
    xlab('Degree of chosen package')+ylab('Cumulated probability of choose')+ 
    scale_x_log10() + scale_y_log10() +
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle('Jeong PA Suggest')
  v = NULL
  datev = NULL
  sv = NULL
  X = NULL
  Xm = NULL
  for(i in ies){
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
          gg2 = gg2 + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
          gg2 = gg2 + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
          if(i==max(ies)){
            gg2 = gg2 + geom_point(data=df,mapping=aes(x=mids,y=density),color='black')
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
  #print(gg2)
  print(gg1/gg2)
  dev.off()
}
}

})

EI =new.env()
load('paperPlots/externalData.RData',envir = EI)
EI = as.list(EI)
### EI
with(EI,{
plotName = 'f8_externalEvolution'
pFun(paste(plotDir,plotName,sep='/'))
{
  q = dfSO$counts[11]/dfSO$countsN[11]
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
     
    #ggtitle('Normalized In degree at arrival (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.y.right = element_text(size=13),
          legend.position = c(.3,.7),
          legend.background = element_rect(fill='#FFFFFF'),
          legend.text = element_text(size=rel(0.9)),
          legend.title  = element_blank()) +
    xlab('Date')+
    ylab('Counts') +
    scale_y_continuous(name = "Books",sec.axis = sec_axis(~.*q, name="StackOverflowViews")) 
  
  dfBook = data.frame('date'=as.Date(as.character(h$mids),'%Y'),'counts'=h$counts)  
  dfSO = data.frame('date'=binsMed,'counts'=vC,'countsN'=vC*max(dfBook$counts)/max(vC))  
  gg = gg + geom_line(data=dfBook,aes(x=date,y=counts,color='Books'))
  gg = gg + geom_line(data=dfSO,aes(x=date,y=countsN,color='StackOverflowViews'))
  for(k in 1:4){
    dfR = Rver[grep(paste('R-',k,sep=''),Rver$Name),]
    gg = gg + geom_vline(xintercept = min(dfR$Date),lwd=0.2)
  }
  print(gg)
  dev.off()}
  

})

plotName = 'f9_timeline'
pFun(paste(plotDir,plotName,sep='/'))
{
  eventsNet = data.frame('start'=c('2004-01-01',
                                   '2000-10-27',
                                   '2007-05-24',
                                   '2007-05-24',
                                   '2017-07-10',
                                   '2015-03-06',
                                   '2011-06-01',
                                   '2005-01-01'
  ),
  'end'=c('2006-01-01',
          '2000-10-27',
          '2007-05-24',
          '2015-01-22',
          '2017-07-10',
          '2015-03-06',
          '2011-06-01',
          '2015-01-01'
  ),
  'event'=c('Sug. First Rise',
            'First Slope Change dep. MD',
            'BCC and Indep crossover (dep.)',
            'Stable BCC (sug.)',
            'BCC and Indep crossover (sug.)',
            'Stabilization of DID and SD arrival',
            'Emergence of superlinear SD attachment',
            'Non-null Jaccard Coef.'
  )
  )
  eventsHist = data.frame('start'=c('2000-01-01',
                                    '2004-01-01',
                                    '2013-01-01',
                                    '2005-05-03',
                                    '2011-10-01',
                                    '2015-01-01',
                                    '2014-01-01',
                                    '2011-02-01',
                                    '2004-01-01',
                                    '2009-06-01',
                                    '2017-01-01'
  ),
  'end'=c('2004-01-01',
          '2013-01-01',
          '2020-01-01',
          '2010-05-03',
          '2014-10-01',
          '2015-01-01',
          '2014-01-01',
          '2011-02-01',
          '2004-01-01',
          '2009-06-01',
          '2017-01-01'
  ),
  'event'=c('R 1.0',
            'R 2.0',
            'R 3.0',
            'Most books published',
            'Most post in StackOverflow',
            'R packages',
            'Advanced R',
            'RStudio',
            'useR!',
            'R journal',
            'R for Data Science'
  )
  )
  eventsHist$group='R-History'
  eventsNet$group='CNA'
  events = rbind(eventsHist,eventsNet)
  
  gg = gg_vistime(data=events)
  gg = gg + theme(axis.text.x = element_text(size=rel(2)),
                  axis.text.y = element_text(size=rel(2),angle=90))
  print(gg)
  dev.off()
}


rm(list=ls())
gc()

plot(bigDates,M[,2],type='l',log='y')
bigDates[M[,2]>1 & M[,2]<200]

