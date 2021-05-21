info = read.csv('info_paquetes.csv')
require(igraph)
require(stringr)
g = make_empty_graph(directed=TRUE)

g = g + vertices(name = 'R', fecha = as.character('1993-08-15'),in.views='n')
g = g + vertices(name = as.character(info$nombre), fecha = as.character(info$published.first),in.views=as.character(info$in.views))
no.depends = NULL
no.entraron = NULL
for(i in 1:nrow(info)){
	pack1 = as.character(info$nombre[i])
	packD2 = info$depends[i]
	if(packD2 != 'n'){
		packD2 = str_trim(str_split(string=packD2,pattern=', ')[[1]])
		for(p in packD2){
			if(is.element(p,c('base','stats','utils','compiler','methods','parallel','graphics','grDevices','grid','splines','limma','graph','RBGL','qvalue','tools','Rgraphviz','snpStats','datasets','tcltk','stats4'))){ 
				p = 'R'
			}
			if(is.element(p,V(g)$name)){				
				g = g + edge(p,pack1) ### Acá tengo un problema para discutir con Inés: hay paquetes que figura con un nombre
			}	      ## como "abc" en las dependencias, pero en la lista de paquetes están como "abc1,abc2"
			else{
				no.entraron = c(no.entraron,p)
				if(sum(is.element(packD2,V(g)$name))==0){
					no.depends = c(no.depends,pack1)
				}
			}	
		}
	}
	else{
		g = g + edge('R',pack1) ### Acá tengo un problema para discutir con Inés: hay paquetes que figura con un nombre
		#no.depends = c(no.depends,pack1)
	}		
}
g = simplify(g)
no.entraron = sort(unique(no.entraron))
V(g)$cluster = clusters(g)$membership
sg = induced_subgraph(g,vids = V(g)$cluster==1)
# los que tienen grado más alto:
#degree(sg)[bigges]
#       R     MASS  ggplot2   Matrix survival  mvtnorm  lattice     Rcpp 
#    9462      426      260      248      231      190      184      152 
#      sp   igraph      ape 
#     129      106       83 
lsg = layout_as_tree(sg,root='R',circular=TRUE)

