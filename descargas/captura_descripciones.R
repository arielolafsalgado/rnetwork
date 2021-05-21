library(stringr)
library(XML)
source('funciones.R')
if(!file.exists('pagina')){
	download.file('https://cran.r-project.org/web/packages/available_packages_by_name.html','pagina')
}
pagina = file.path("pagina")
tablas = readHTMLTable(pagina)
paquetes = tablas[[1]]['V1']
paquetes = paquetes[,1]
paq_desc = tablas[[1]]['V2']
paq_desc = paq_desc[paquetes!='',1]
paquetes = paquetes[paquetes!='']

df = data.frame('paquete'=paquetes) #este es el data.frame en el que voy a guardar toda la data
df$descripcion = NA
for(pack in paquetes){
	print(pack)
	this = df$paquete == pack
	## Información más nueva 
	if(!file.exists(paste('paquetes/',pack,sep=''))){
		print(paste('ERROR: El paquete',pack,'no está en /paquetes'))
		df$descripcion[this] = NA
	}
	else{
		pag.new = file.path(paste('paquetes/',pack,sep=''))			
		text.new = readLines(pag.new)
		first.id = grep(pattern='<body',x=text.new)+2
		second.id = first.id-1 + min(grep(pattern='</p>',x=text.new[first.id:length(text.new)]))
		texto = paste(str_trim(text.new[first.id:second.id]),collapse='<p></p>')
		df$descripcion[this] = texto
	}
}
write.csv(df,'descargas/descripciones.csv')
