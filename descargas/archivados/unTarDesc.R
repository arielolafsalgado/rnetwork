inFolderPath = 'archived-packages-tar'
outFolderPath = 'archived-packages-description'
dir.create(outFolderPath)

for(folder in dir(inFolderPath)){
    oldFolder = paste(inFolderPath,folder,sep='/')
    newFolder = paste(outFolderPath,folder,sep='/')
    dir.create(newFolder)
    tarFiles = dir(oldFolder)
    for(tF in tarFiles){
        print(tF)
        tFPath = paste(oldFolder,tF,sep='/')
        inDescPath = paste(folder,'DESCRIPTION',sep='/')
        untar(tarfile=tFPath,files=inDescPath,exdir=newFolder)
        fromFilePath = paste(newFolder,inDescPath,sep='/')
        if(!file.exists(fromFilePath)){
            inDescPath = paste('.',inDescPath,sep='/')
            untar(tarfile=tFPath,files=inDescPath,exdir=newFolder)
            fromFilePath = paste(newFolder,inDescPath,sep='/')
        }
        toFilePath = paste(newFolder,sub('.tar.gz','',tF),sep='/')
        file.copy(from=fromFilePath,to=toFilePath)
        unlink(sub('/DESCRIPTION','',fromFilePath),recursive=T)
    }
}
