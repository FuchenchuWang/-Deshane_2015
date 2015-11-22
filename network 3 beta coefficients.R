require(igraph)
require(gdata)

setwd("C:/Users/Delia Wang/Desktop/R/work/network")
data=read.table("beta_coefficients.txt",sep="\t",header=T,row.names =1)
data=as.matrix(data)
nodegroup=list()
nodegroup$cell=colnames(data)[1:9]
nodegroup$phylum=colnames(data)[10:25]
name=data[1:25,1:25]
obvious =name
obvious[ lower.tri(obvious, diag=TRUE) ]<- 0
cutoff = 0.3;
obvious[ abs(obvious) < cutoff]<- 0
network=graph.adjacency(abs(obvious)>cutoff, weighted=TRUE, mode="upper")
E(network)$weight=obvious[abs(obvious)>cutoff]
E(network)$color[ E(network)$weight <0 ] = "blue"
E(network)$color[ E(network)$weight >= 0 ] = "red"
V(network)$label<- colnames(name)
V(network)[10:25]$color <- "lightblue"
V(network)[1:9]$color <- "pink"
layout1 <- layout.fruchterman.reingold(network)
network$layout <- layout.fruchterman.reingold
edge=abs(E(network)$weight)
plot(decompose.graph(network)[[which.max(sapply(decompose.graph(network), vcount))]],edge.width =edge/150,frame=T) 

