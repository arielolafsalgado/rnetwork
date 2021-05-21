

################################# para mi compu
library(stringr)
library(XML)
Sys.setenv(http_proxy='http://proxy.uba.ar:8080')
setwd('/home/olaf/Dropbox/DOCTORADO/Doctorado')
# bajo la pagina
download.file('http://cran.r-project.org/web/packages/arules/index.html','pagina')
# leo la pagina
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root <- xmlRoot(doc,skip=FALSE)
# saco los datos utiles
namesug<-xmlValue(root[[2]][[3]][[3]])
namedesc<-xmlValue(root[[2]][[1]])
depend<-xmlValue(root[[2]][[3]][[2]][[2]])
#autores<-xmlValue(root[[2]][[3]][[4]])
fecha<-xmlValue(root[[2]][[3]][[4]])
autor<-xmlValue(root[[2]][[3]][[5]])
mantainer<-xmlValue(root[[2]][[3]][[6]])


# lo hacemos para un set de paquetes a ver si baja bien la informacion

download.file('http://cran.r-project.org/web/packages/bindata/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_bindata <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/biomod2/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_biomod2 <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/chron/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_chron <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/GEVcdn/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_GEVcdn <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/ipred/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_ipred <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/JOP/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_JOP <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/linLIR/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_linLIR <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/lsr/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_lsr <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/lsr/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_lsr <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/mada/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_mada <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/Rcpp/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_Rcpp <- xmlRoot(doc,skip=FALSE)

download.file('http://cran.r-project.org/web/packages/SemiParBIVProbit/index.html','pagina')
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root_SemiParBIVProbit <- xmlRoot(doc,skip=FALSE)

write.table(root_bindata,"bindata")

# probar con 
lista<- xmlToList(root)

# algunas cosas para chequear:
# parece ser el titulo del paquete
lista$body$h2
# parece ser una descripcion del paquete
lista$body$p
# parece ser nombre del paquete : titulo del paquete
lista$body$h2


if(FALSE){
# miramos si la structura de las paginas es parecida:
# algunos paquetes:
#2012-11-14 	bindata 	Generation of Artificial Binary Data
#2012-11-14 	biomod2 	Ensemble platform for species distribution modeling
#2012-11-14 	chron 	Chronological objects which can handle dates and times
#2012-11-14 	GEVcdn 	GEV conditional density estimation network
#2012-11-14 	ipred 	Improved Predictors
#2012-11-14 	JOP 	Joint Optimization Plot
#2012-11-14 	linLIR 	linear Likelihood-based Imprecise Regression
#2012-11-14 	lsr 	Companion package to "Learning Statistics with R"
#2012-11-14 	mada 	Meta-Analysis of Diagnostic Accuracy (mada)
#2012-11-14 	Rcpp 	Seamless R and C++ Integration
#2012-11-14 	SemiParBIVProbit 	Semiparametric Bivariate Probit Modelling
#2012-11-14 	sensitivity 	Sensitivity Analysis
#2012-11-14 	sExtinct 	Calculates the historic date of extinction given a series of sighting events
#2012-11-14 	soilwater 	soilwatercurve: Implements paramatric formulas for soil water retention or conductivity curve
#2012-11-14 	tmle 	Targeted Maximum Likelihood Estimation
#2012-11-14 	tseries 	Time series analysis and computational finance
#2012-11-14 	visualFields


}

# leemos info de un set de paquetes:
totp<-10


paquetes<-c("A3","abc","abcdeFBA","ABCExtremes","ABCoptim","ABCp2","abctools","abd","abf2","abind")
namesug<-vector(length=totp,mode="numeric")
namedesc<-vector(length=totp,mode="numeric")
depend<-vector(length=totp,mode="numeric")
fecha<-vector(length=totp,mode="numeric")
autor<-vector(length=totp,mode="numeric")
mantainer<-vector(length=totp,mode="numeric")
det_des<-vector(length=totp,mode="numeric")
prueba<-vector(length=totp,mode="numeric")


for(i in 1:10){
library(XML)
Sys.setenv(http_proxy='http://proxy.uba.ar:8080')
setwd('/home/olaf/Dropbox/DOCTORADO/Doctorado')
# bajamos la pagina dire
dire<-str_c("http://cran.r-project.org/web/packages/",paquetes[i],"/index.html",sep="")
download.file(dire,'pagina')
# leemos la pagina
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root <- xmlRoot(doc,skip=FALSE)
# sacamos los datos utiles
namesug[i]<-xmlValue(root[[2]][[3]][[3]])
namedesc[i]<-xmlValue(root[[2]][[1]])
det_des[i]<-xmlValue(root[[2]][[2]])
depend[i]<-xmlValue(root[[2]][[3]][[2]][[2]])
fecha[i]<-xmlValue(root[[2]][[3]][[4]])
autor[i]<-xmlValue(root[[2]][[3]][[5]])
mantainer[i]<-xmlValue(root[[2]][[3]][[6]])
prueba[i]<-xmlValue(root[[2]][[3]][[9]])
}



# leemos el listado de paquetes de la página del R:
totp<-6109
paquetes<-vector(length=totp,mode="numeric")
paquetes_c<-vector(length=totp,mode="numeric")
download.file('http://cran.r-project.org/web/packages/available_packages_by_name.html','pagina')
doc<-htmlTreeParse(file.path('/home/olaf/Dropbox/DOCTORADO/Doctorado/Ine/proyectos/r_se_mira','pagina'))
root_p <- xmlRoot(doc,skip=FALSE)
for(i in 1:totp){
if(length(xmlValue(root_p[[2]][[3]][[i]][[1]]))==1){
paquetes[(i-1)]<-xmlValue(root_p[[2]][[3]][[i]][[1]])}}

lviews<-vector(length=33,mode="numeric")
download.file('http://cran.r-project.org/web/views/','pagina')
doc<-htmlTreeParse(file.path('/home/olaf/Dropbox/DOCTORADO/Doctorado/Ine/proyectos/r_se_mira','pagina'))
root_p <- xmlRoot(doc,skip=FALSE)
nviews<-xmlSize(root_p[[2]][[2]])
for(i in 1:nviews){
lviews[i]<-xmlValue(root_p[[2]][[2]][[i]][[1]])}

# probamos leer info de los paquetes y luego identificar los campos que nos interesan:
info_paq<-matrix(ncol=20,nrow=totp)
dep_detalle<-matrix(ncol=40,nrow=totp)
suggest<-vector(length=totp,mode="numeric")
namedesc<-vector(length=totp,mode="numeric")
depend<-vector(length=totp,mode="numeric")
fecha<-vector(length=totp,mode="numeric")
autor<-vector(length=totp,mode="numeric")
mantainer<-vector(length=totp,mode="numeric")
det_des<-vector(length=totp,mode="numeric")
prueba<-vector(length=totp,mode="numeric")
ndep<-vector(length=totp,mode="numeric")
views<-vector(length=totp,mode="numeric")

library(XML)
for(i in 6014:totp){
if(paquetes[i]!="0"){
Sys.setenv(http_proxy='http://proxy.uba.ar:8080')
setwd('/home/olaf/Dropbox/DOCTORADO/Doctorado')
# bajamos la pagina dire
dire<-str_c("http://cran.r-project.org/web/packages/",paquetes[i],"/index.html",sep="")
download.file(dire,'pagina')
# leemos la pagina
doc<-htmlTreeParse(file.path("/home/olaf/Dropbox/DOCTORADO/Doctorado","pagina"))
root <- xmlRoot(doc,skip=FALSE)
# sacamos los datos utiles
namedesc[i]<-xmlValue(root[[2]][[1]])
det_des[i]<-xmlValue(root[[2]][[2]])
nj<-xmlSize(root[[2]][[3]])
for(j in 1:nj){info_paq[i,j]<-xmlValue(root[[2]][[3]][[j]])}
}}


# fallo el paquete 6013! 

# extraemos info de los campos que nos interesan:
for(i in 1:totp){
for(j in 1:20){
views[i]<-ifelse(grepl("views:",info_paq[i,j])==T,strsplit(info_paq[i,j], ":")[[1]][[2]]
,views[i])
autor[i]<-ifelse(grepl("Author:",info_paq[i,j])==T,strsplit(info_paq[i,j], ":")[[1]][[2]]
,autor[i])
mantainer[i]<-ifelse(grepl("Maintainer:",info_paq[i,j])==T,strsplit(info_paq[i,j], ":")[[1]][[2]],mantainer[i])
}}

#write.table(paquetes,"listado_paquetes.csv")
paquetes<-read.table("listado_paquetes.csv")

paquetes_c<-read.table("listado_paquetes.csv")

#write.table(lviews,"listado_views.csv")

info_paq_prueba<-read.table("info_paquetes.csv",sep=";;")
#write.csv(info_paq,"info_paquetes.csv",sep=";;")

# nota: guarde el data frame con sep=;;



# armamos la red de paquetes sugeridos:
library(igraph)

totp<-6109
g<-graph.empty(n=totp, directed=TRUE)

V(g)$name<-paquetes[1:totp]
# agregamos otros atributos a los nodos de la red:
V(g)$views<-views[1:totp]


for(i in 1:totp){
for(j in 1:20){
if(grepl("Suggests:",info_paq[i,j])==T){
sug_paq<-grep("Suggests:",info_paq[i,j],value=T)
sugt<-unlist(strsplit(sug_paq, "Suggests:", fixed = TRUE))
sugt<-unlist(strsplit(sugt, ",", fixed = TRUE))

ns<-length(sugt)
for(is in 1:ns){
nodo<-sub(" ","",sugt[[is]])
if(length(which(paquetes==nodo))==1){
g<-g+edge(paquetes[i],nodo)}
}
}}}



# leemos la info de los paquetes antes de armar las redes:
info_paquetes_prueba<-read.csv("info_paquetes.csv",sep=";")


totp<-6109
g_prueba<-graph.empty(n=totp, directed=TRUE)


V(g_prueba)$name<-paquetes[1:totp]
# agregamos otros atributos a los nodos de la red:
V(g_prueba)$views<-views[1:totp]

for(i in 1:totp){
for(j in 1:20){
if(grepl("Suggests:",info_paquetes_prueba[i,j])==T){
sug_paq<-grep("Suggests:",info_paquetes_prueba[i,j],value=T)
sugt<-unlist(strsplit(sug_paq, "Suggests:", fixed = TRUE))
sugt<-unlist(strsplit(sugt, ",", fixed = TRUE))

ns<-length(sugt)
for(is in 1:ns){
nodo<-sub(" ","",sugt[[is]])
if(length(which(paquetes==nodo))==1){
g_prueba<-g_prueba+edge(paquetes[i],nodo)}
}
}}}





clus<-clusters(g)
table(clus$csize)

max(degree(g))



paq<-data.frame(paquetes[order(degree(g),decreasing=T)],degree(g)[order(degree(g),decreasing=T)],clus$membership[order(degree(g),decreasing=T)],views[order(degree(g),decreasing=T)])
names(paq)<-c("npaq","grado","cluster","views")
options(width=200)
paq[1:100,1:4]






if(grepl(x=depend[i],pattern="Depends:")==TRUE){
ndep[i]<-length(strsplit(strsplit(depend[i], "Depends:")[[1]][[2]],",")[[1]])
if(ndep[i]>=1){
for(j in 1:ndep[i]){dep_detalle[i,j]<-strsplit(strsplit(depend[i], "Depends:")[[1]][[2]],",")[[1]][[j]]}}}
}


# separamos los strings para obtener el vector de paquetes que son dependencias y sugeridos por cada uno de los paquetes:
for(i in 1:totp){
if(grepl(x=depend[i],pattern="Depends:")==TRUE){
ndep[i]<-length(strsplit(strsplit(depend[i], "Depends:")[[1]][[2]],",")[[1]])
if(ndep[i]>=1){
for(j in 1:ndep[i]){dep_detalle[i,j]<-strsplit(strsplit(depend[i], "Depends:")[[1]][[2]],",")[[1]][[j]]}}}
}




r <- xmlRoot(doc)
xmlName(r)
xmlSize(r)
r[[1]]
sapply(xmlChildren(r[[1]]), xmlName)

xmlSApply(r[[1]], xmlName)


#########################################################
# 23/12/2014 
#########################################################
library(igraph)
library(stringr)

totp<-6109

paquetes_c<-read.table("listado_paquetes.csv")
paquetes<-vector(length=totp,mode="numeric")
for(i in 1:totp){paquetes[i]<-as.character(paquetes_c[i,])}

# esto aun no lo probe:
lviews_c<-read.table("listado_views.csv")
paquetes<-vector(length=33,mode="numeric")
for(i in 1:33){lviews[i]<-as.character(lviews_c[i,])}


# leemos la info de los paquetes antes de armar las redes:
info_paq<-read.csv("info_paquetes.csv",sep=";")

dep_detalle<-matrix(ncol=40,nrow=totp)
suggest<-vector(length=totp,mode="numeric")
namedesc<-vector(length=totp,mode="numeric")
depend<-vector(length=totp,mode="numeric")
fecha<-vector(length=totp,mode="numeric")
autor<-vector(length=totp,mode="numeric")
mantainer<-vector(length=totp,mode="numeric")
det_des<-vector(length=totp,mode="numeric")
prueba<-vector(length=totp,mode="numeric")
ndep<-vector(length=totp,mode="numeric")
views<-vector(length=totp,mode="numeric")

library(stringr)

# extraemos info de los campos que nos interesan:
for(i in 1:totp){
for(j in 1:20){
views[i]<-ifelse(grepl("views:",info_paq[i,j])==T,strsplit(as.character(info_paq[i,j]), ":")[[1]][[2]]
,views[i])
autor[i]<-ifelse(grepl("Author:",info_paq[i,j])==T,strsplit(as.character(info_paq[i,j]), ":")[[1]][[2]]
,autor[i])
mantainer[i]<-ifelse(grepl("Maintainer:",info_paq[i,j])==T,strsplit(as.character(info_paq[i,j]), ":")[[1]][[2]],mantainer[i])
}}


# armamos la red de paquetes sugeridos:
g<-graph.empty(n=totp, directed=TRUE)
V(g)$name<-paquetes[1:totp]
# agregamos otros atributos a los nodos de la red:
V(g)$views<-views[1:totp]




for(i in 1:totp){
for(j in 1:20){
if(grepl("Suggests:",info_paq[i,j])==T){
sug_paq<-grep("Suggests:",info_paq[i,j],value=T)
sugt<-unlist(strsplit(sug_paq, "Suggests:", fixed = TRUE))
sugt<-unlist(strsplit(sugt, ",", fixed = TRUE))

ns<-length(sugt)
for(is in 1:ns){
nodo<-sub(" ","",sugt[[is]])
if(length(which(paquetes==nodo))==1){
g<-g+edge(paquetes[i],nodo)}
}
}}}

# generamos la red de los depends:
gd<-graph.empty(n=totp, directed=TRUE)
V(gd)$name<-paquetes[1:totp]
# agregamos otros atributos a los nodos de la red:
V(gd)$views<-views[1:totp]

for(i in 1:totp){
for(j in 1:20){
if(grepl("Depends:",info_paq[i,j])==T){
sug_paq<-grep("Depends:",info_paq[i,j],value=T)
sugt<-unlist(strsplit(sug_paq, "Depends:", fixed = TRUE))
sugt<-unlist(strsplit(sugt, ",", fixed = TRUE))

ns<-length(sugt)
for(is in 1:ns){
nodo<-sub(" ","",sugt[[is]])
if(length(which(paquetes==nodo))==1){
gd<-gd+edge(paquetes[i],nodo)}
}
}}}


# Observamos algunas magnitudes:
postscript("red_depends_dg.eps")
plot(seq(0,length(degree.distribution(gd))-1),degree.distribution(gd),xlim=c(0,31),xlab="grado",ylab="probabilidad",main="Red de Depends: distribución de grados")
dev.off()

# observamos la estructura de clusters:
table(clusters(gd)$csize)

# nos quedamos con el cluster mas grande:

clus_gd<-clusters(gd)
gd_1<-induced.subgraph(gd,clus_gd$csize[clus_gd$membership]==2894)
postscript("red_depends_dg_1.eps")
plot(seq(0,length(degree.distribution(gd))-1),degree.distribution(gd),xlim=c(0,31),xlab="grado",ylab="probabilidad",main="Red de Depends: distribución de grados (rojo= cluster mayor)")
points(seq(0,length(degree.distribution(gd_1))-1),degree.distribution(gd_1),xlim=c(0,31),col="red")
dev.off()

# calculamos propiedades de la red:
# transitividad:
postscript("red_depends_c.eps")
hist(transitivity(gd_1,type="local"),xlab="transitividad",ylab="probabilidad",main="Cluster  mas grande de Red de Depends")
dev.off()

gd_1u<-as.undirected(gd_1)
postscript("red_depends.eps")
plot(gd_1u,vertex.size=0,vertex.label=NA,vertex.color="black", main="Cluster  mas grande de Red de Depends")
dev.off()

# camino minimo promedio:
average.path.length(gd_1)
paq_view<-matrix(ncol=33,nrow=totp,data=0)



# usamos los views:
# definimos una matriz con la info de los views asociados a cada paquete:

for(i in 1:totp){
for(j in 1:33){
if(grepl(lviews[j],views[i])){
paq_view[i,j]<-1}
}}

# veamos la red de un view en particular:
# el primer view:
j<-1
gd_v1<-induced.subgraph(gd,paq_view[,j]==1)
gd_v1u<-as.undirected(gd_v1)

postscript("red_view_bayesian.eps",fonts=c("serif", "Palatino"))
plot(gd_v1u,vertex.size=0,vertex.label=V(gd_v1)$name,vertex.label.cex=0.6,vertex.color="black", main="Red de Depends de Paquetes con View 'Bayesian'")
dev.off()

j<-4
gd_v<-induced.subgraph(gd,paq_view[,j]==1)
gd_v<-as.undirected(gd_v4)

postscript("red_view_cluster.eps",fonts=c("serif", "Palatino"))
plot(gd_v,vertex.size=0,vertex.label=V(gd_v)$name,vertex.label.cex=0.6,vertex.color="black", main="Red de Depends de Paquetes con View 'Cluster'")
dev.off()


j<-27
gd_v<-induced.subgraph(gd,paq_view[,j]==1)
gd_v<-as.undirected(gd_v)

postscript("red_view_socialsciences.eps",fonts=c("serif", "Palatino"))
plot(gd_v,vertex.size=2,vertex.label=V(gd_v)$name,vertex.label.cex=0.8,vertex.color="black", main="Red de Depends de Paquetes con View 'Social Sciences'")
dev.off()



j<-31
gd_v<-induced.subgraph(gd,paq_view[,j]==1)
gd_v<-as.undirected(gd_v)

postscript("red_view_socialsciences.eps",fonts=c("serif", "Palatino"))
plot(gd_v,vertex.size=2,vertex.label=V(gd_v)$name,vertex.label.cex=0.8,vertex.color="black", main="Red de Depends de Paquetes con View 'Social Sciences'")
dev.off()

# Tratemos de ver la red de Views. 
# nodos: views
# el tamaño de un nodo tendria que ser la cantidad de paquetes asociados a ese view.
# links: cada vez que hay un paquete que se asocia a dos views a la vez, definimos un link entre esos dos views.

mviews<-matrix(ncol=33,nrow=33,data=0)

for(i in 1:totp){
for(j in 1:32){
for(jj in (j+1):33){
if(paq_view[i,j]==1 && paq_view[i,jj]==1){
mviews[j,jj]<-mviews[j,jj]+1}
}}}

# registramos la cantidad de paquetes que tiene asociado cada view:
sviews<-vector(length=33,mode="numeric")
for(i in 1:totp){
for(j in 1:33){
if(paq_view[i,j]==1){
sviews[j]<-sviews[j]+1}
}}

# armamos la red de views:
gviews<-graph.adjacency(mviews,weight=TRUE)
gviews<-as.undirected(gviews)
V(gviews)$name<-lviews
V(gviews)$size<-sviews

postscript("red_views.eps",fonts=c("serif", "Palatino"))
pdf("red_views.pdf",fonts=c("serif", "Palatino"))
plot(gviews,vertex.size=V(gviews)$size/10,vertex.label=V(gviews)$name,vertex.label.cex=0.8,vertex.color="grey",vertex.label.color="black", main="Red de Views",edge.width=E(gviews)$weight/5)
dev.off()

