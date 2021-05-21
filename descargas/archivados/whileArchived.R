require(rvest)
require(XML)
require(stringr)

packages = dir('archive-pages')
folder = 'archive-webpages'
dir.create(folder)
baseURL = 'https://cran.r-project.org/web/packages/DUMMYTEXT/index.html'
for(pack in packages){
    url = sub('DUMMYTEXT',pack,baseURL)
    fileName = paste(folder,pack,sep='/')
    if(!file.exists(fileName)) download_html(url,fileName)
}
generalPattern = 'Package &lsquo;DUMMYTEXT&rsquo; was removed'
archivedPattern = 'Archived on '
output = data.frame('Package'=packages,'Removed'=NA,'While'=NA,stringsAsFactors=F)
for(pack in packages){
    filePath = paste(folder,pack,sep='/')
    fileText = readLines(filePath)
    searchPattern = sub('DUMMYTEXT',pack,generalPattern)
    removedText = grep(searchPattern,fileText,value=T)
    if(length(removedText)>0){
        output[output$Package==pack,'Removed'] = TRUE
        whileText = grep(archivedPattern,fileText,value=T)
        if(length(whileText)>0){
            whileText = str_split(whileText,archivedPattern)[[1]]
            whileText = whileText[length(whileText)]
            whileText = str_split(gsub(',',' ',whileText),' ')[[1]]
            whileText = as.Date(whileText,format='%Y-%m-%d')
            output[output$Package==pack,'While'] = as.character(whileText[!is.na(whileText)][1])
        }
    }else{
        output[output$Package==pack,'Removed'] = FALSE
    }
}

write.csv(output,'whenArchived.csv',row.names=F)