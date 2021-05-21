path = '~/Dropbox/DOCTORADO/Doctorado/RedR/descargas/descarga.R'
#source('~/Dropbox/DOCTORADO/Doctorado/RedR/descargas/descarga_robusta.R')
descarga.robusta <- function(path,k=0,k_max=100){
	if(k<=k_max){
		tryCatch(
			source(path),
			error = descarga.robusta(path,k=k+1)
		)
	}
}

