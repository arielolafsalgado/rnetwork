require(rvest)
require(XML)
require(stringr)
source('helpersDataGet.R')
fileName = 'archivePage.html'
url <- 'https://cran.r-project.org/src/contrib/Archive'
download_html(url,fileName)
archivePage = readHTMLTable(fileName)[[1]]
archivedPackages = sub('\\/','',as.character(archivePage[,'Name']))

alreadySeenPackages = read.csv('../../../ListaEventos Nuevo/dataBases/creationDate.csv',stringsAsFactors=F)
mDate = max(as.Date(alreadySeenPackages$CreationDate),na.rm=TRUE)
archivedPackages = str_trim(archivedPackages[as.Date(as.character(archivePage[,3]),format='%Y-%m-%d')<=mDate])
notSeenPackages = setdiff(archivedPackages,alreadySeenPackages$Package)
notSeenPackages = notSeenPackages[!is.na(notSeenPackages)]
folderName = 'archive-pages'
if(!dir.exists(folderName)) dir.create(folderName)
k = 0
for(pack in notSeenPackages){
    print(pack)
    if(!is.element(pack,dir(folderName))){
        url = paste('https://cran.r-project.org/src/contrib/Archive/',pack,'/',sep='')
        download_html(url,paste(folderName,pack,sep='/'))
    }
    k = k+1
    print(k/length(notSeenPackages))
}

file.remove(paste(folderName,'README',sep='/'))
# Ahora descargo los tar.gz para cada uno

descFolder = 'archived-packages-descriptions'
generalURL = 'https://cran.r-project.org/src/contrib/Archive'
dir.create(descFolder)
for(pack in dir(folderName)){
    fileContent = readLines(paste(folderName,pack,sep='/'))
    dir.create(paste(descFolder,pack,sep='/'))
    searchPattern = paste('>',pack,'_',sep='')
    thisLines = grep(searchPattern,fileContent,value=T)
    for(line in thisLines){
        splittedLine = str_split(line,'>|<')[[1]]
        thisString = grep(sub('>','^',searchPattern),splittedLine,value=T)
        outPath = paste(descFolder,pack,thisString,sep='/')
        downloadURL = paste(generalURL,pack,thisString,sep='/')
        insistent.download(downloadURL,outPath)
    }
}
