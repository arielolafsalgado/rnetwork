require(rvest)
require(XML)
require(stringr)
source('helpersDataGet.R')
fileName = 'archivePage.html'
archivePage = readHTMLTable(fileName,stringsAsFactors=F)[[1]]
archivePage[,'Name'] = sub('\\/','',as.character(archivePage[,'Name']))
whenArchived = read.csv('whenArchived.csv',stringsAsFactors=F)
whenArchived$While = archivePage[,'Last modified'][match(whenArchived$Package,archivePage$Name)]
whenArchived$While[!whenArchived$Removed] = NA 
write.csv(whenArchived,'whenArchived.csv',row.names=F)