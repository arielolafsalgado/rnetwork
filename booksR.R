require(xml2)
require(rvest)
url = 'https://www.r-project.org/doc/bib/R-jabref.html'
tmp = tempfile()
download.file(url,tmp)
text = readLines(tmp)
webpage = read_html(url)
tab = html_table(webpage)
x = as.numeric(tab[[1]]$Author)
for(cn in colnames(tab[[1]])){
  y = as.numeric(tab[[1]][,cn])
  x[is.na(x)] = y[is.na(x)]
}
require(stringr)
spt = unlist(str_split(text,'<td>|<\td>'))
spt = as.numeric(spt)
table(spt)
years = text[grep('year =',text)]
years = as.numeric(str_trim(gsub('year = |,|\\{|\\}','',years)))



plotDir = 'paperPlots'
plotName = 'booksperyear.png'

png(paste(plotDir,plotName,sep='/'))
{
  
h = hist(years,breaks=seq(1988,2019,1)-.5,plot=F)
plot(h$mids,h$counts,type='b',xlab='Year',ylab='Number of books',main='')
axis(side=1,at=h$mids,label=NA)

dev.off()}


df = read.csv('QuestionsOnStackOverflow2.csv')
df = df[grep('<r>',df$Tags),]
nrow(df)
plot(as.Date(df$CreationDate),df$ViewCount)
df$Date = as.Date(df$CreationDate)
dates = sort(unique(df$Date))
bins = seq(as.Date('2008-01-01'),as.Date('2020-01-01'),'3 m')
vCCum = sapply(bins,function(bin) sum(df$viewCount[df$Date<=bin]))
plot(bins,vCCum,type='l')
binsMed = bins[2:length(bins)]-diff(bins)/2
vC = diff(vCCum)
class(h)
h2 = h
plotName = 'viewsperyear.png'

png(paste(plotDir,plotName,sep='/'))
{
  plot(binsMed,vC,type='b',xlab='Date',ylab='Total questions view count')
  axis(side=1,at=seq(as.Date('2008-01-01'),as.Date('2020-01-01'),'1 y'),label=NA)
dev.off()}

df = read.csv('QuestionsOnStackOverflow3.csv')
plot(as.Date(df$CreationDate),df$ViewCount)
df$Date = as.Date(df$CreationDate)
dates = sort(unique(df$Date))
bins = seq(as.Date('2008-01-01'),as.Date('2020-01-01'),'3 m')
vCCum = sapply(bins,function(bin) sum(df$ViewCount[df$Date<=bin]))
plot(bins,vCCum,type='l')
binsMed = bins[2:length(bins)]-diff(bins)/2
vC = diff(vCCum)
class(h)
h2 = h
plotName = 'viewsperyearSOD3.png'

png(paste(plotDir,plotName,sep='/'))
{
  plot(binsMed,vC,type='b',xlab='Date',ylab='Total questions view count')
  axis(side=1,at=seq(as.Date('2008-01-01'),as.Date('2020-01-01'),'1 y'),label=NA)
  dev.off()}



###### R versions
require(stringr)
require(XML)
Rver = NULL
for(k in 0:4){
  url = sub('X',k,'https://cran.r-project.org/src/base/R-X/')
  tmp = tempfile()
  download.file(url,tmp)
  # Capturo los nombres de los paquetes
  
  tablas = readHTMLTable(tmp)
  tablas = tablas$`NULL`
  tablas = tablas[2:(nrow(tablas)-1),]
  tablas$Name = gsub('.tgz','',tablas$Name)
  Rver = rbind(Rver,tablas)
}
Rver$Date = as.Date(Rver$`Last modified`,format = '%Y-%m-%d')
View(Rver)
Rver = Rver[grep('R-',Rver$Name),]


h = hist(years,breaks=seq(1988,2019,1)-.5,plot=F)
df = read.csv('QuestionsOnStackOverflow2.csv')
df = df[grep('<r>',df$Tags),]
df$Date = as.Date(df$CreationDate)
dates = sort(unique(df$Date))
bins = seq(as.Date('2008-01-01'),as.Date('2020-01-01'),'6 m')
vCCum = sapply(bins,function(bin) sum(df$viewCount[df$Date<=bin]))
binsMed = bins[2:length(bins)]-diff(bins)/2
vC = diff(vCCum)

require(ggplot2)
plotName = 'externalEvolution.png'
png(paste(plotDir,plotName,sep='/'))
{
  q = dfSO$counts[11]/dfSO$countsN[11]
  gg = ggplot() +# expand_limits(x=xlim,y=ylim)+
    theme_bw()+ 
    #ggtitle('Normalized In degree at arrival (dependency)')+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.y.right = element_text(size=13)) +
    xlab('Date')+
    ylab('Counts') +
    scale_y_continuous(name = "Books",sec.axis = sec_axis(~.*q, name="StackOverflowViews"))
  
  dfBook = data.frame('date'=as.Date(as.character(h$mids),'%Y'),'counts'=h$counts)  
  dfSO = data.frame('date'=binsMed,'counts'=vC,'countsN'=vC*max(dfBook$counts)/max(vC))  
  gg = gg + geom_line(data=dfBook,aes(x=date,y=counts,color='Books'))
  gg = gg + geom_line(data=dfSO,aes(x=date,y=countsN,color='StackOverflow'))
  for(k in 1:4){
    dfR = Rver[grep(paste('R-',k,sep=''),Rver$Name),]
    gg = gg + geom_vline(xintercept = min(dfR$Date),lwd=0.2)
  }
  print(gg)
  dev.off()}
