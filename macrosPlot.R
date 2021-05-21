require(ggplot2)
require(Matrix)
require(igraph)
load('fullNet2.RData')
source('networkFunctions.R')
if(file.exists('macroPlots/macroPlots.RData')){
  load('macroPlots/macroPlots.RData')
  relations=c('dependency','suggest','both')
}else{
  bigDates = as.character(seq(as.Date('2000-01-01'),as.Date('2020-01-01'),100))
  gSeq =  lapply(relations,function(relation) lapply(bigDates,function(date) getNetworkSnapshot(g,date,relation)))
  names(gSeq) = relations
  bigDates = as.Date(bigDates)
  N = sapply(gSeq,function(gSd) sapply(gSd,vcount))
  M = sapply(gSeq,function(gSd) sapply(gSd, function(gS) ecount(simplify(as.undirected(gS)))))
  I = sapply(gSeq,function(gSd) sapply(gSd,function(gS) sum(degree(gS)==0)))
  CC = sapply(gSeq,function(gSd) sapply(gSd, function(gS) max(clusters(gS)$csize)))
  transi = sapply(gSeq,function(gSd) sapply(gSd,function(gS) transitivity(gS,'global')))
  triang = sapply(gSeq,function(gSd) sapply(gSd,function(gS) sum(count_triangles(gS))))
  triangDDS = sapply(1:length(bigDates),function(q){
    gS = gSeq$suggest[[q]]
    gD = gSeq$dependency[[q]]
    XS =  as_adjacency_matrix(simplify(as.undirected(gS)))
    XD =  as_adjacency_matrix(gD)
    result = sum(diag(XS%*%XD%*%XD))
    return(result)
  })
  triangDSD = sapply(1:length(bigDates),function(q){
    gS = gSeq$suggest[[q]]
    gD = gSeq$dependency[[q]]
    XS =  as_adjacency_matrix(simplify(as.undirected(gS)))
    XD =  as_adjacency_matrix(gD)
    result = sum(diag(t(XD)%*%XS%*%XD))/2
    return(result)
  })
  DDO = lapply(gSeq,function(gSd) sapply(gSd,function(gS) degree(gS,mode='out')))
  DDI = lapply(gSeq,function(gSd) sapply(gSd,function(gS) degree(gS,mode='in')))
  DDA = lapply(gSeq,function(gSd) sapply(gSd,function(gS) degree(simplify(as.undirected(gS)),mode='all')))
  DDO.PLF = lapply(DDO,function(DDs) lapply(DDs, function(dd) fit_power_law(dd)))
  DDO.PLF.alpha = lapply(DDO.PLF,function(X) sapply(X, function(x) x$alpha))
  DDO.PLF.xmin = lapply(DDO.PLF,function(X) sapply(X, function(x) x$xmin))
  DDA.PLF = lapply(DDA,function(DDs) lapply(DDs, function(dd) fit_power_law(dd)))
  DDA.PLF.alpha = lapply(DDA.PLF,function(X) sapply(X, function(x) x$alpha))
  DDA.PLF.xmin = lapply(DDA.PLF,function(X) sapply(X, function(x) x$xmin))
  save(bigDates,gSeq,N,M,I,CC,transi,triang,DDO,DDI,DDA,DDO.PLF,
       DDO.PLF.alpha,DDO.PLF.xmin,
       DDA.PLF,DDA.PLF.alpha,DDA.PLF.xmin,file='macroPlots/macroPlots.RData')
}

packsNewAndLost = do.call(rbind,lapply(1:length(bigDates),function(i) {
    if(i==1){
      r = NULL
    }else{
      gi = gSeq[[1]][[i]]
      gi_ = gSeq[[1]][[i-1]]
      packsNew= length(setdiff(V(gi)$name,V(gi_)$name))
      packsLost= length(setdiff(V(gi_)$name,V(gi)$name))
      r = c(packsNew,packsLost)   
    }
    return(r)
    }))

relsNewAndLost = lapply(gSeq,function(gSd){
  do.call(rbind,lapply(1:length(bigDates),function(i) {
    if(i==1){
      r = NULL
    }else{
      gi = gSd[[i]]
      gi_ = gSd[[i-1]]
      e = ends(gi,es = E(gi),names = TRUE)
      e = paste(e[,1],e[,2])
      e_ = ends(gi_,es = E(gi_),names = TRUE)
      e_ = paste(e_[,1],e_[,2])
      eNew = setdiff(e,e_)
      if(length(eNew)>0){
        eNew = str_split(eNew,' ',simplify=TRUE)
        eOldE = sum(is.element(eNew[,1],V(gi_)$name) & is.element(eNew[,2],V(gi_)$name))
      }else{
        eOldE = 0
      }
      relsNew= length(setdiff(e,e_))
      relsLost= length(setdiff(e_,e))
      r = c(relsNew,relsLost,eOldE)   
    }
    return(r)
  }))
})

relsTransfer = do.call(rbind,lapply(1:length(bigDates),function(i) {
  if(i==1){
    r = NULL
  }else{
    gDi = gSeq[[1]][[i]]
    gSi = gSeq[[2]][[i]]
    gDi_ = gSeq[[1]][[i-1]]
    gSi_ = gSeq[[2]][[i-1]]
    eD = ends(gDi,es = E(gDi),names = TRUE)
    eS = ends(gSi,es = E(gSi),names = TRUE)
    eD_ = ends(gDi_,es = E(gDi_),names = TRUE)
    eS_ = ends(gSi_,es = E(gSi_),names = TRUE)
    eD = paste(eD[,1],eD[,2])
    eS = c(paste(eS[,1],eS[,2]),paste(eS[,2],eS[,1]))
    eD_ = paste(eD_[,1],eD_[,2])
    eS_ = c(paste(eS_[,1],eS_[,2]),paste(eS_[,2],eS_[,1]))
    
    dep_2sug = length(intersect(eD_,eS))
    sug_2dep = length(intersect(eD,eS_))
    depYsug = length(intersect(eD,eS))
    depOsug = length(union(eD,eS))
    newDep = length(setdiff(eD,eD_))
    newSug = length(setdiff(eS,eS_))
    r = c(dep_2sug,sug_2dep,depYsug,depOsug,newDep,newSug)   
  }
  return(r)
}))
colnames(relsTransfer) = c('dep2sug','sug2dep','depYsug','depOsug','newDep','newSug')

reverseRel = sapply(gSeq,function(gSd) sapply(gSd,function(gS){
  X = as_adjacency_matrix(gS)
  #q = rowSums(t(X)*X)/rowSums(X)
  #mean(q[is.finite(q)],na.rm=T)
  sum(t(X)*X)
})) 

corDIDO = sapply(gSeq,function(gSd) sapply(gSd, function(gS){
  cor(degree(gS,mode='in'),degree(gS,mode='out'))
}))
save.image('paperPlots/macroPlots.RData')

assortaDODO = sapply(gSeq,function(gSd) sapply(gSd, function(gS){
  
}))
interDepSug = sapply(1:length(bigDates),function(q){
  gS = gSeq$suggest[[q]]
  gD = gSeq$dependency[[q]]
  ecount(intersection(gS,gD))
})

meanDist = sapply(gSeq,function(gSd) sapply(gSd, function(gS){
  average.path.length(gS,directed=F)
}))
par.list=list(
  'pch'=18,
  'cex'=1.3
)
pchRel = 16:18
colorRel = c('blue','red','violet')
names(pchRel) = relations
names(colorRel) = relations

plotDir = paste('macroPlots',sep='/')
dir.create(plotDir)
plotDir = 'paperPlots'

plotName = 'NandM_vs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('')+ 
    ggtitle('Number of connections and nodes')
  for(relation in c('dependency','suggest')){
    df = data.frame(date=bigDates,x=M[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  gg = gg + geom_point(data=data.frame(date=bigDates,x=N[,1],relation='packages'),aes(x=date,y=x,color=relation,pch=relation))
  print(gg)
  dev.off()
}

plotName = 'M_vs_N.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  #xlim = c(0,3e3)
  #ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
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

plotName = 'dN_vs_N.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(0,3e-1)
  ylim = c(0,3e-1)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('N')+ylab('dN/dT')+ 
    ggtitle('Evolution of number of packages')
  df = data.frame(date=N[2:nrow(N),1],x=diff(N[,1])/as.numeric(diff(bigDates)))
  gg = gg + geom_point(data=df,aes(x=date,y=x))    
  x = df$date
  y = df$x
  y1 = y[x<1e4]
  x1 = x[x<1e4]
  y2 = y[x>=1e4]
  x2 = x[x>=1e4]
  mod1 = lm(y1~x1) 
  mod2 = lm(y2~1) 
  print('Mod1')
  print(summary(mod1))
  print('Mod2')
  print(summary(mod2))
  df$predicted1 =  predict(mod1,newdata = data.frame('x1'=df$date))
  df$predicted2 =  predict(mod2,newdata = data.frame('x2'=df$date))
  gg = gg + geom_line(data=df,aes(x=date,y=predicted1))
  gg = gg + geom_line(data=df,aes(x=date,y=predicted2))
  # De acá concluyo que hay una primer etapa con un crecimiento exponencial
  # Donde N ~ exp(a*t), con t medido en días y a = (6.32 \pm 0.09)e-4
  # El segundo tramo es consistente con ser constante, con media u = 5 \pm 2
  print(gg)
  dev.off()
}

plotName = 'dM_vs_M.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(0,3e-1)
  ylim = c(0,3e-1)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('M')+ylab('dM/dT')+ 
    ggtitle('Evolution of number of relations')
  for(relation in relations[1:2]){
    df = data.frame(date=M[2:nrow(M),relation],x=diff(M[,relation])/as.numeric(diff(bigDates)),relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation))    
    gg = gg + geom_line(data=df,aes(x=date,y=x,color=relation))    
    x = df$date
    y = df$x
    y1 = y[x<1e4]
    x1 = x[x<1e4]
    y2 = y[x>=1e4]
    x2 = x[x>=1e4]
    mod1 = lm(y1~x1) 
    mod2 = lm(y2~1) 
    print('Mod1')
    print(summary(mod1))
    print('Mod2')
    print(summary(mod2))
    df$predicted1 =  predict(mod1,newdata = data.frame('x1'=df$date))
    df$predicted2 =  predict(mod2,newdata = data.frame('x2'=df$date))
    gg = gg + geom_line(data=df,aes(x=date,y=predicted1,col=relation))
    gg = gg + geom_line(data=df,aes(x=date,y=predicted2,col=relation))
    # De acá concluyo que hay una primer etapa con un crecimiento exponencial
    # Donde N ~ exp(a*t), con t medido en días y a = (6.32 \pm 0.09)e-4
    # El segundo tramo es consistente con ser constante, con media u = 5 \pm 2
  }
  print(gg)
  dev.off()
}

plotName = 'dM_vs_N.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(0,3e-1)
  ylim = c(0,3e-1)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('N')+ylab('dM/dN')+ 
    ggtitle('Evolution of number of relations')
  for(relation in relations[1:2]){
    df = data.frame(date=N[2:nrow(N),1],x=diff(M[,relation])/as.numeric(diff(N[,1])),relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation))    
    gg = gg + geom_line(data=df,aes(x=date,y=x,color=relation))    
    x = df$date
    y = df$x
    y1 = y[x>1e3]
    x1 = x[x>1e3]
    mod1 = lm(y1~x1) 
    print('Mod1')
    print(summary(mod1))
    df$predicted1 =  predict(mod1,newdata = data.frame('x1'=df$date))
    gg = gg + geom_line(data=df,aes(x=date,y=predicted1,col=relation))
    # De acá concluyo que hay una primer etapa con un crecimiento exponencial
    # Donde N ~ exp(a*t), con t medido en días y a = (6.32 \pm 0.09)e-4
    # El segundo tramo es consistente con ser constante, con media u = 5 \pm 2
  }
  print(gg)
  dev.off()
}


plotName = 'rho_vs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,4e-3)
  rho = M/(N*(N-1)/2)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Fraction of total possible connections')+ 
    ggtitle('Connection density')
  for(relation in c('dependency','suggest','both')){
    df = data.frame(date=bigDates,x=rho[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  print(gg)
  dev.off()
}




plotName = 'mdeg_vs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
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

plotName = 'isolvs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  isolP = I/N
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Fraction of packages')+ 
    ggtitle('Independent packages')
  for(relation in relations[1:2]){
    df = data.frame(date=bigDates,x=isolP[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  print(gg)
  dev.off()
}





plotName = 'isolvs_z.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  z = M/N
  xlim = c(min(z),max(z))
  ylim = c(0,1)
  isolP = I/N
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    #scale_y_log10() + 
    scale_x_log10() + 
    xlab('Mean degree')+ylab('Fraction of packages')+ 
    ggtitle('Independent packages')
  for(relation in relations){
    df = data.frame(date=z[,relation],x=isolP[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  print(gg)
  dev.off()
}


plotName = 'isolvs_BCC.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(0,1)
  ylim = c(0,1)
  isolP = I/N
  BCCP = CC/N
  z = M/N
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    #scale_y_log10() + 
    #scale_x_log10() + 
    xlab('Fraction in BCC')+ylab('Fraction of independent packages')+ 
    ggtitle('Independent packages')
  for(relation in relations[1:2]){
    df = data.frame(date=BCCP[,relation],x=isolP[,relation],relation=relation,z=z[,relation])
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=z,pch=relation))    
  }
  print(gg)
  dev.off()
}


plotName = 'BCCvs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  BCCP = CC/N
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Fraction of nodes')+ 
    ggtitle('Nodes in BCC')
  for(relation in relations){
    df = data.frame(date=bigDates,x=BCCP[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  print(gg)
  dev.off()
}


plotName = 'BCCvs_z.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  z = M/N
  BCCP = CC/N
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Mean degree')+ylab('Fraction of nodes')+ 
    scale_x_log10() +
    #scale_y_log10() + 
    ggtitle('Nodes in BCC')
  for(relation in relations[1:2]){
    df = data.frame(date=z[,relation],x=BCCP[,relation],relation=relation)
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=relation))    
  }
  print(gg)
  dev.off()
}




plotName = 'nonBCCorISOLvs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,.3)

  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Fraction of nodes')+ 
    ggtitle('Non BCC or independent packages')
  x = 1-(CC+I)/N
  for(relation in relations){
    df = data.frame(date=bigDates,x=x[,relation],relation=relation)
    gg = gg + geom_point(data=df,mapping=aes(x=date,y=x,color=relation,pch=relation))    
  }
  print(gg)
  dev.off()
}


plotName = 'BCCandIsolvs_date.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,1)
  BCCP = CC/N
  isolP = I/N
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Fraction of nodes')+ 
    ggtitle('Nodes in components')
  for(relation in relations[1:2]){
    df = data.frame(date=bigDates,x=BCCP[,relation],relation=relation,set = 'BCC')
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=set))    
    df = data.frame(date=bigDates,x=isolP[,relation],relation=relation,set = 'Indep.')
    gg = gg + geom_point(data=df,aes(x=date,y=x,color=relation,pch=set))    
    df = data.frame(date=bigDates,x=(BCCP+isolP)[,relation],relation=relation,set = 'Sum')
    gg = gg + geom_line(data=df,aes(x=date,y=x,color=relation))    
  }
  print(gg)
  dev.off()
}


plotName = 'dependency_ddistOut.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'dependency'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank(),
     panel.background = element_blank()) +
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

plotName = 'dependency_ddistOut_normalized.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'dependency'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Out degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle(relation)
  X = NULL
  Y = NULL
  Z = NULL
  for(q in 1:length(bigDates)){
    dd = DDO[[relation]][[q]]
    Z = c(Z,mean(dd))
    dd = dd/mean(dd)
    h = hist(dd,breaks=breaks,plot=F)
    X = c(X,h$mids[h$density>0])
    Y = c(Y,h$density[h$density>0])
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  Y = Y[X>=1]
  X = X[X>=1]
  Y = log(Y)
  X = log(X)
  mod = lm(Y~X)
  df = data.frame('x' = 1:500,'y'=exp(predict(mod,newdata=data.frame('X'=log(1:500)))))
  gg = gg + geom_line(data = df,mapping=aes(x=x,y=y))
  print(gg)
  summary(mod)
  dev.off()
}
plot(X,Y)
lines(log(1:500),predict(mod,newdata = data.frame('X'=log(1:500))))


plotName = 'dependency_ddistIn.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'dependency'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+
    ggtitle(relation)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
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

plotName = 'suggest_ddistOut.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'suggest'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+ 
    ggtitle(relation)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Out degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  for(q in 1:length(bigDates)){
    dd = DDO[[relation]][[q]]
    h = hist(dd,breaks=breaks,plot=F)
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  print(gg)
  dev.off()
}

plotName = 'suggest_ddistIn.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'suggest'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+ 
    ggtitle(relation)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('In degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))
  for(q in 1:length(bigDates)){
    dd = DDI[[relation]][[q]]
    h = hist(dd,breaks=breaks,plot=F)
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_line(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  print(gg)
  dev.off()
}

plotName = 'suggest_ddistAll.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'suggest'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+ 
    ggtitle(relation)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
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

plotName = 'suggest_ddistAll_normalized.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(1,5e3)
  ylim = c(1e-7,1e1)
  relation = 'suggest'
  breaks = c(0,2**(0:20))
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    scale_x_log10()+scale_y_log10()+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Degree')+ylab('Density')+ 
    annotation_logticks()+ 
    scale_color_gradient(low="blue", high="red",labels=function(q) as.Date(q,origin = '1970-01-01'))+
    ggtitle(relation)
  X = NULL
  Y = NULL
  Z = NULL
  for(q in 1:length(bigDates)){
    dd = DDA[[relation]][[q]]
    Z = c(Z,mean(dd))
    dd = dd/mean(dd)
    h = hist(dd,breaks=breaks,plot=F)
    X = c(X,h$mids[h$density>0])
    Y = c(Y,h$density[h$density>0])
    df = data.frame('mids'=h$mids[h$density>0],'density'=h$density[h$density>0],'date'=bigDates[q])
    gg = gg + geom_point(data=df,mapping=aes(x=mids,y=density,color=date))
  }
  Y = Y[X>=1]
  X = X[X>=1]
  Y = log(Y)
  X = log(X)
  mod = lm(Y~X)
  df = data.frame('x' = 1:500,'y'=exp(predict(mod,newdata=data.frame('X'=log(1:500)))))
  gg = gg + geom_line(data = df,mapping=aes(x=x,y=y))
  print(gg)
  summary(mod)
  dev.off()
}



plotName = 'dependency_DDOalphas.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,5)
  relation = 'dependency'

  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Alphas')+ 
    ggtitle(relation)
  dd = DDO.PLF.alpha[[relation]]
  df = data.frame('date'=bigDates,'alpha'=dd)
  gg = gg + geom_point(data=df,mapping=aes(x=date,y=alpha))
  print(gg)
  dev.off()
}

plotName = 'suggest_DDAalphas.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(1e-7,1e1)
  relation = 'suggest'
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Alphas')+ 
    ggtitle(relation)
  dd = DDA.PLF.alpha[[relation]]
  df = data.frame('date'=bigDates,'alpha'=dd)
  gg = gg + geom_point(data=df,mapping=aes(x=date,y=alpha))
  print(gg)
  dev.off()
}

plotName = 'trianglesPerPackage.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(1e-7,5e0)
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Triangles per package')+ 
    ggtitle('Triangles')
  for(relation in relations){
    x = triang[,relation]
    n = N[,relation]
    df = data.frame('date'=bigDates,'triangles'=x/n,'relation'=relation)
    gg = gg + geom_point(data=df,mapping=aes(x=date,y=triangles,pch=relation,color=relation))
  }
  print(gg)
  dev.off()
}


plotName = 'trianglesPerPackage_perType.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(1e-7,1e0)
  
  gg = ggplot() + expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Triangles per package')+ 
    ggtitle('Triangles')
  x = triang[,'dependency']
  n = N[,'dependency']
  df = data.frame('date'=bigDates,'triangles'=x/n,'type'='DDD')
  gg = gg + geom_point(data=df,mapping=aes(x=date,y=triangles,pch=type,color=type))
  x = triang[,'suggest']
  df = data.frame('date'=bigDates,'triangles'=x/n,'type'='SSS')
  gg = gg + geom_point(data=df,mapping=aes(x=date,y=triangles,pch=type,color=type))
  x = triangDDS
  df = data.frame('date'=bigDates,'triangles'=x/n,'type'='DDS')
  gg = gg + geom_point(data=df,mapping=aes(x=date,y=triangles,pch=type,color=type))
  x = triangDSD
  df = data.frame('date'=bigDates,'triangles'=x/n,'type'='DSD')
  gg = gg + geom_point(data=df,mapping=aes(x=date,y=triangles,pch=type,color=type))
  print(gg)
  dev.off()
}




plotName = 'transitivity.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(1e-7,5e0)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Transitivity')+ 
    ggtitle('Transitivity')
  for(relation in relations){
    x = transi[,relation]
    df = data.frame('date'=bigDates,'x'=x,'relation'=relation)
    gg = gg + geom_point(data=df,mapping=aes(x=date,y=x,pch=relation,color=relation))
  }
  print(gg)
  dev.off()
}

plotName = 'interDepSug.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(1e-7,5e0)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Transitivity')+ 
    ggtitle('Transitivity')
  for(relation in relations){
    x = transi[,relation]
    df = data.frame('date'=bigDates,'x'=x,'relation'=relation)
    gg = gg + geom_point(data=df,mapping=aes(x=date,y=x,pch=relation,color=relation))
  }
  print(gg)
  dev.off()
}



plotName = 'mean_dist.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(1e-7,5e0)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Mean undirected distance')+ 
    ggtitle('Mean distance')
  for(relation in relations){
    x = meanDist[,relation]
    df = data.frame('date'=bigDates,'x'=x,'relation'=relation)
    gg = gg + geom_point(data=df,mapping=aes(x=date,y=x,pch=relation,color=relation))
  }
  print(gg)
  dev.off()
}


plotName = 'ddout_vs_ddin.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    scale_x_log10()+
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Out degree')+ylab('In degree')+ 
    ggtitle('Out-in degree')
  
  for(i in 1:length(DDO[['dependency']])){
    x = DDO[['dependency']][[i]]
    y = DDI[['dependency']][[i]]
    rangeX = range(x)
    breaksX = c(-Inf,0:as.integer(log(rangeX[2],base=2)+1))
    iBreaksX = findInterval(x,2**breaksX)
    breaksY = sapply(1:length(breaksX),function(q) mean(y[iBreaksX==q]))
    breaksX = breaksX[!is.na(breaksY)]
    breaksY = breaksY[!is.na(breaksY)]
    df = data.frame('x'=2**breaksX,'y'=breaksY,'date'=as.Date(bigDates[i]))
    gg = gg + geom_line(data=df,mapping=aes(x=x,y=y,color=date))
  }
  print(gg)
  dev.off()
}

# Interrelations

plotName = 'transferDep2Sug.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    #scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    xlab('Date')+ylab('Transferred relations')+ 
    ggtitle('Transferred relations between networks')
  df = as.data.frame(relsTransfer)
  df$date=bigDates[2:length(bigDates)]
  gg = gg + geom_point(data=df,aes(x=date,y=dep2sug,color='dep2sug'))    
  gg = gg + geom_point(data=df,aes(x=date,y=sug2dep,color='sug2dep'))    
  print(gg)
  dev.off()
}
plotName = 'transferDep2Sug.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.text =element_text(size=10)) +
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

plotName = 'lostNandM.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.text =element_text(size=10)) +
    xlab('Date')+ylab('Removed packages and relations')+ 
    ggtitle('Removed elements')
  df = data.frame('DepE'=relsNewAndLost$dependency[,2])
  df$SugE =relsNewAndLost$suggest[,2]
  df$PackE =packsNewAndLost[,2]
  df$date=bigDates[2:length(bigDates)]
  gg = gg + geom_point(data=df,aes(x=date,y=DepE,color='dependency'))    
  gg = gg + geom_point(data=df,aes(x=date,y=SugE,color='suggests'))    
  gg = gg + geom_point(data=df,aes(x=date,y=PackE,color='packages'))    
  print(gg)
  dev.off()
}

plotName = 'lostNandM_frac.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.text =element_text(size=10)) +
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


plotName = 'newVsPrevFrac.png'
png(paste(plotDir,plotName,sep='/'))
{
  par(par.list)
  xlim = c(min(bigDates),max(bigDates))
  ylim = c(0,3e4)
  gg = ggplot() + #expand_limits(x=xlim,y=ylim)+
    theme_bw()+  
    #scale_y_log10()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.text =element_text(size=10)) +
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
