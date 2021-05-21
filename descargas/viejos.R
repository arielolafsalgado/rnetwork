# Este script es para descargar la información de las dependencias de paquetes viejas, contenida en RedR/archivo ( OCT-2017)
# La idea es que debería tomar la información de 3 fuentes:
# La información de la versión más actual del paquete -> en la carpeta RedR/paquetes
# La información de las fechas de cada paquete -> en la carpeta RedR/paq-archiv
# La información de las dependencias viejas. -> en la carpeta RedR/archivo

# Comienzo capturando la información de los nombres de todos los paquetes.
library(stringr)
library(XML)
if(TRUE){ # sólo correr la primera vez
	if(!file.exists('pagina')){
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

if(TRUE){
	source('../../funciones.R')
	df = data.frame('paquete'=paquetes) #este es el data.frame en el que voy a guardar toda la data
	df$depends.new = NA
	df$published.new = NA
	df$in.views.new = NA
	df$mantainer.new = NA
	df$suggests.new = NA
	df$imports.new = NA
	df$enhances.new = NA

	df$depends.old = NA
	df$published.old = NA
	df$mantainer.old = NA
	df$suggests.old = NA
	df$imports.old = NA
	df$enhances.old = NA
	for(pack in paquetes){
		print(pack)
		this = df$paquete == pack
		## Información más nueva 
		if(!file.exists(paste('paquetes/',pack,sep=''))){
			print('ERROR: El paquete no está en /paquetes')
		}
		else{
			pag.new = file.path(paste('paquetes/',pack,sep=''))			
			tab.new = readHTMLTable(pag.new)
			if(length(tab.new)){
  			tp1 = tab.new[[1]][[1]] #Acá están metidas las categorías (depends, suggests)
  			tp2 = tab.new[[1]][[2]] #Acá están los valores de las categorías
  			tp3 = tab.new[[2]][[1]] #De acá veo si hay data más vieja!file.exists(paste('paq-arch
  			i1 = grep('Depends',tp1)
  			i2 = grep('Published',tp1)
  			i3 = grep('views',tp1)
  			i4 = grep('Maintainer',tp1)
  			i5 = grep('Suggests',tp1)
  			i6 = grep('Imports',tp1)
  			i7 = grep('Enhances',tp1)
  			iOld = grep('Old',tp3)
  			if(length(i1)==1){ df$depends.new[this] = as.character(tp2[i1]) }
  			if(length(i2)==1){ df$published.new[this] = as.character(tp2[i2]) }
  			if(length(i3)==1){ df$in.views.new[this] = as.character(tp2[i3]) }
  			if(length(i4)==1){ df$mantainer.new[this] = as.character(tp2[i4]) }
  			if(length(i5)==1){ df$suggests.new[this] = as.character(tp2[i5]) }
  			if(length(i6)==1){ df$imports.new[this] = as.character(tp2[i6]) }
  			if(length(i7)==1){ df$enhances.new[this] = as.character(tp2[i7]) }
			}else{
			  iOld=1
			}
		}
		
		## Información de lista de fechas
		if(length(iOld)==1){
			## Información de lista de fechas
			if(!file.exists(paste('paq-archiv/',pack,sep=''))){
				print('ERROR: El paquete no está en /paq-archiv')
			}else{
				pag.of = file.path(paste('paq-archiv/',pack,sep=''))
				tab.of = readHTMLTable(pag.of)[[1]]
				iofa = min(grep(pack,tab.of[,2]))
				lofb = max(grep(pack,tab.of[,2]))
				lof = length(tab.of[,3])
				aux.of = as.Date(as.character(tab.of[,3][iofa:lofb]))
				aux.of = aux.of[!is.na(aux.of)]
				published.of = paste(aux.of,collapse='-//-')
				df$published.old[this] = published.of
			}
			if(!dir.exists(paste('archivo/',pack,sep=''))){
				print('ERROR: El paquete no tiene carpeta dentro de /archivo')
			}else{
				files = list.files(paste('archivo/',pack,sep=''))
				aux.do = rep(NA,length(files))
				aux.so = rep(NA,length(files))
				aux.io = rep(NA,length(files))
				aux.eo = rep(NA,length(files))
				aux.mo = rep(NA,length(files))
				aux.vo = rep(NA,length(files))
				for(i in 1:length(files)){
					ifile = files[i]
					txt.f = file.path(paste('archivo/',pack,'/',ifile,sep=''))
					aux.f = readLines(txt.f)
					line.df = grep('Depends',aux.f,value=TRUE)
					line.df = sub('Depends: |Depends:\t|Depends :|Depends:','',line.df)
					line.sf = grep('Suggests',aux.f,value=TRUE)
					line.sf = sub('Suggests: |Suggests:\t|Suggests :|Suggests:','',line.sf)
					line.If = grep('Imports',aux.f,value=TRUE)
					line.If = sub('Imports: |Imports:\t|Imports :|Imports:','',line.If)
					line.ef = grep('Enhances',aux.f,value=TRUE)
					line.ef = sub('Enhances: |Enhances:\t|Enhances :|Enhances:','',line.ef)
					line.mf = grep('Mantainer',aux.f,value=TRUE)
					line.mf = sub('Mantainer: ','',line.mf)
					line.vf = grep('In views',aux.f,value=TRUE)
					line.vf = sub('In views:','',line.vf)
					if(length(line.df)>0){
						aux.do[i] = line.df
					}
					if(length(line.sf)>0){
						aux.so[i] = line.sf
					}
					if(length(line.If)>0){
						aux.io[i] = line.If
					}
					if(length(line.ef)>0){
						aux.eo[i] = line.ef
					}
					if(length(line.mf)>0){
						aux.mo[i] = line.mf
					}
					if(length(line.vf)>0){
						aux.vo[i] = line.vf
					}
				}
				df$depends.old[this] = paste(aux.do,collapse='-//-')
				df$suggests.old[this] = paste(aux.so,collapse='-//-')
				df$imports.old[this] = paste(aux.io,collapse='-//-')
				df$enhances.old[this] = paste(aux.eo,collapse='-//-')
				df$mantainer.old[this] = paste(aux.mo,collapse='-//-')
				df$in.views.old[this] = paste(aux.vo,collapse='-//-')
			}
	
		}
	}	
	#df$depends.new = string.less.pattern(strings = df$depends.new,pattern = '\\(|\\)')
	#df$depends.old = string.less.pattern(strings = df$depends.old,pattern = '\\(|\\)')
	#df$depends.new = sub(x = df$depends.new,pattern = ' ,',replacement=',')
	#df$depends.old = sub(x = df$depends.old,pattern = ' ,',replacement=',')

}
write.csv(df,'info_evolution_2020-10-29.csv')
