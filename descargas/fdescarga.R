f.descarga <- function(flag1,flag2,flag3,flag4,flag5){
	require(stringr)
	require(XML)
	if(flag1){ # sólo correr la primera vez
		if(!file.exists('pagina')){
			download.file('https://cran.r-project.org/web/packages/available_packages_by_name.html','pagina')
		}
		pagina = file.path("pagina")
		doc<-htmlTreeParse(file.path("pagina"))
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


	if(flag2){ #Sólo correr para descargar los paquetes
		for(pack in paquetes){
			if(!file.exists(paste("paquetes/",pack,sep=''))){
				lk = grep(paste("/",pack,"/",sep=''),links,value=TRUE)
				download.file(lk,paste("paquetes/",pack,sep=''))
			}
		}
	}



	if(flag3){ #Para conseguir los old sources, las fechas de publicación de un paquete
		for(i in 1:length(paquetes)){
			pack = paquetes[i]
			if(!file.exists(paste('paq-archiv/',pack,sep=''))){
				pag_pack = file.path(paste('paquetes/',pack,sep=''))
				tab_pack = readHTMLTable(pag_pack)
				tp = tab_pack[[2]][[1]] #Acá están metidas las categorías (depends, suggests)
				wh = grep('Old',tp)
				if(length(wh)>0){
					lk = paste('https://cran.r-project.org/src/contrib/Archive/',pack,sep='')
					whto =  paste('paq-archiv/',pack,sep='')
					download.file(lk,whto)
				}
			}
		}
	}

	if(flag4){
		if(!dir.exists('archivo')){
			dir.create('archivo')
		}
		for(i in 1:length(paquetes)){
			pack = paquetes[i]
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
				for(link in links){
					filename = sub(paste('https://cran.r-project.org/src/contrib/Archive/',pack,'/',sep=''),'',link)
					filename = sub('\"','',filename)
					tarfile = paste(whto,filename,sep='')
					newfilename = sub('.tar.gz','',filename)
					newfilename = paste('archivo/',pack,'/',newfilename,sep='')
					oldfilename = paste('archivo/',pack,'/',pack,'/DESCRIPTION',sep='')
					if(!file.exists(newfilename)){
						if(!file.exists(paste(whto,filename,sep=''))){
							download.file(link,tarfile)
						}
						untar(tarfile,paste(pack,'/DESCRIPTION',sep=''),exdir=paste('archivo/',pack,sep=''))
						file.rename(oldfilename,newfilename)
						file.remove(tarfile)
					}
				}
				file.remove(paste('archivo/',pack,'/',pack,sep=''))
			}	
		}
	}


	if(flag5){ #Junta los paquetes asociados a cada paquete
		source('funciones.R')
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

}
