insistent.download<- function(url,outpath){
    tryCatch(download.file(url,outpath),error=function(e) insistent.download(url,outpath))
}
