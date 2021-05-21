require(stringr)
require(XML)
if(TRUE){ # sólo correr la primera vez
	if(!file.exists('listado')){
		download.file('https://cran.r-project.org/web/packages/available_packages_by_name.html','listado')
	}
	pagina = file.path("listado")
	doc<-htmlTreeParse(file.path("listado"))
	root <- xmlRoot(doc,skip=FALSE)
	# Capturo los nombres de los paquetes

	tablas = readHTMLTable(pagina)
	paquetes = tablas[[1]]['V1']
	paquetes = paquetes[,1]
	paq_desc = tablas[[1]]['V2']
	paquetes = paquetes[paquetes!='']
	# Ahora capturo los links
	html = paste(readLines(pagina), collapse="\n") 
	links = str_match_all(html, "<a href=\"(.*?)\"")[[1]][,1]
	links = links[27:length(links)]
	links = sub('<a href=\"../..','https://cran.r-project.org',links)
	links = sub('\"','',links)
	# Para capturar el link de un paquete hago
	# grep(paquete,links)
}


if(TRUE){ #Sólo correr para descargar los paquetes
	if(!dir.exists('paquetes')){
		dir.create('paquetes')
	}
  toDownloadPacks = paste('paquetes/',paquetes,sep='')
  toDownloadPacks = setdiff(toDownloadPacks,dir('paquetes',full.names = TRUE))
  if(length(toDownloadPacks)>0){
    N = length(toDownloadPacks)
    k = 1
    K = min(20,N)
    onlyOne =  K==1
    while(k<K | onlyOne){
      ps = toDownloadPacks[k:K]
      lk = sapply(ps,function(pack) grep(paste("/",sub('paquetes/','',pack),"/",sep=''),links,value=TRUE))
			download.file(lk,ps)
			gc()
			k = K
			K = min(K+20,N)
			onlyOne = FALSE
		}
	}
}



if(TRUE){ #Para conseguir los old sources, las fechas de publicación de un paquete
	if(!dir.exists('paq-archiv')){
		dir.create('paq-archiv')
	}
	lks = sapply(1:length(paquetes),function(i){
		pack = paquetes[i]
		if(i%%1e3==0) print(i/length(paquetes)*100)
		if(!file.exists(paste('paq-archiv/',pack,sep=''))){
			pag_pack = file.path(paste('paquetes/',pack,sep=''))
			tab_pack = readHTMLTable(pag_pack)
			if(length(tab_pack)==0){
			  lk = paste('https://cran.r-project.org/src/contrib/Archive/',pack,sep='')		  
			  whto =  paste('paq-archiv/',pack,sep='')
			  result = c(whto,lk)
			}else{
  			tp = tab_pack[[2]][[1]] #Acá están metidas las categorías (depends, suggests)
  			wh = grep('Old',tp)
  			if(length(wh)>0){
  				lk = paste('https://cran.r-project.org/src/contrib/Archive/',pack,sep='')
  				whto =  paste('paq-archiv/',pack,sep='')
  				result = c(whto,lk)
  			}else{
  			  result = NULL
  			}
			}
		}else{
		  result = NULL
		}
		return(result)
	})
	lks = do.call(rbind,lks)
	N = nrow(lks)
	k = 1
	K = min(20,N)
	while(k<K){
	  lk = lks[k:K,2]
	  ps = lks[k:K,1]
	  download.file(lk,ps)
	  k = K
	  K = min(K+20,N)
	}
}

if(TRUE){
	if(!dir.exists('archivo')){
		dir.create('archivo')
	}
	for(i in 1:length(paquetes)){
		pack = paquetes[i]
		if(i%%1e3==0) print(i/length(paquetes)*100)
		if(file.exists(paste('paq-archiv/',pack,sep=''))){
			if(!dir.exists(paste('archivo/',pack,sep=''))){
				dir.create(paste('archivo/',pack,sep=''))
			}
			pag_pack = file.path(paste('paq-archiv/',pack,sep=''))
			html_pack = paste(readLines(pag_pack), collapse="\n") 
			links_pack = str_match_all(html_pack, "<a href=\"(.*?)\"")[[1]][,1]
			links = sub('<a href=\"',paste('https://cran.r-project.org/src/contrib/Archive/',pack,'/',sep=''),links_pack)
			links = links[grep('tar.gz',links)]
			links = sub('\"','',links)
			whto = paste('archivo/',pack,'/',sep='')
			filename = sub(paste('https://cran.r-project.org/src/contrib/Archive/',pack,'/',sep=''),'',links)
			filename = sub('\"','',filename)
			already.done = dir(paste('archivo/',pack,sep=''))
			tarfile = paste(whto,filename,sep='')
			tarfile.done = which(is.element(sub(paste('archivo/',pack,'/',sep=''),'',sub('.tar.gz','',tarfile)),already.done))
			newfilename = sub('.tar.gz','',filename)
			newfilename = paste('archivo/',pack,'/',newfilename,sep='')
			oldfilename = paste('archivo/',pack,'/',pack,'/DESCRIPTION',sep='')
			if(length(tarfile.done)>0){
  			tarfile = tarfile[-tarfile.done]
  			newfilename = newfilename[-tarfile.done]
  			links = links[-tarfile.done]
			}
			if(length(tarfile)>0){
  			download.file(links,tarfile)
  			for(i in 1:length(tarfile)){
  			  tf = tarfile[i]
  			  nfn = newfilename[i]
  				if(!file.exists(nfn)){
  					untar(tf,paste(pack,'/DESCRIPTION',sep=''),exdir=paste('archivo/',pack,sep=''))
  					file.rename(oldfilename,nfn)
  					file.remove(tf)
  				}
  			}
  			file.remove(paste('archivo/',pack,'/',pack,sep=''))
			}
		}	
	}
}


if(TRUE){ #Junta los paquetes asociados a cada paquete
	source('../../funciones.R')
	Depends = rep('n',length(paquetes))
	Suggests = rep('n',length(paquetes))
	Published.last = rep('n',length(paquetes))
	Published.first = rep('n',length(paquetes))
	In.views = rep('n',length(paquetes))
	for(i in 1:length(paquetes)){
		pack = paquetes[i]
		pag_pack = file.path(paste('paquetes/',pack,sep=''))
		tab_pack = readHTMLTable(pag_pack)
		tp1 = tab_pack[[1]][[1]] #Acá están metidas las categorías (depends, suggests)
		tp2 = tab_pack[[1]][[2]] #Acá están los valores de las categorías
		tp3 = tab_pack[[2]][[1]] #Acá están los valores de las categorías
		i1 = grep('Depends',tp1)
		i2 = grep('Suggests',tp1)
		i3 = grep('Published',tp1)
		i4 = grep('views',tp1)
		i5 = grep('Old',tp3)
		if(length(i1)==1){ Depends[i] = as.character(tp2[i1]) }
		if(length(i2)==1){ Suggests[i] = as.character(tp2[i2]) }	
		if(length(i3)==1){ Published.last[i] = as.character(tp2[i3]) }
		if(length(i4)==1){ In.views[i] = as.character(tp2[i4]) }
		if(length(i5)==1){ 
			pag_pack2 = file.path(paste('paq-archiv/',pack,sep=''))
			tp4 = readHTMLTable(pag_pack2)[[1]]
			i6 = grep('Parent',tp4[,2])
			l6 = length(tp4[,3])
			aux6 = min(as.Date(as.character(tp4[,3][i6+1:l6])),na.rm=TRUE)
			Published.first[i] = as.character(aux6)
		}
		else{
			Published.first[i] = Published.last[i]
		}
	}
	ip = data.frame('nombre'=paquetes,'links'=links,'depends'=Depends,'suggests'=Suggests,'published.first'=Published.first, 'published.last'=Published.last, 'in.views'=In.views)
	ip$depends = string.less.pattern(strings = ip$depends,pattern = ' \\(|\\)')
	ip$suggests = string.less.pattern(strings = ip$suggests,pattern = ' \\(|\\)')
	
}


