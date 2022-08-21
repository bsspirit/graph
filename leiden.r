##########################
# leiden
# 详细文章介绍
# http://blog.fens.me/r-graph-leidan/
# @author: DanZhang
##################

library(leidenAlg)
library(igraph)
library(RColorBrewer)

setwd("C:/work/R/graph")

e<-exampleGraph
V(e)$label<-""
V(e)$label.cex<-0.5
plot(e)

lc<-leiden.community(e)
node.cols <- brewer.pal(max(c(3, lc$membership)),"Pastel1")[lc$membership]
plot(e, vertex.color = node.cols, vertex.label="")


# 递归 leiden 社区 使用 leiden.community 构建一个 n 步递归聚类
# 返回树状图的虚假社区对象
r2 = rleiden.community(e, n.cores=1,max.depth=2)
rs2<-as.dendrogram.fakeCommunities(r2)
plot(rs2)

r3 = rleiden.community(e, n.cores=1,max.depth=3)
rs3<-as.dendrogram.fakeCommunities(r3)
plot(rs3)


g <- make_star(10) 
g<-add_edges(g,c(2,3,3,4, 4,5,2,4,2,5))
g<-add_edges(g,c(6,7,6,8,6,9))
E(g)$arrow.size<-0.2
E(g)$arrow.width<-0.2
plot(g)
g

E(g)$width <- sample(1:3,ecount(g),replace = TRUE)
E(g)$color<-E(g)$width
E(g)$width 
plot(g)


V(g)$color<-find_partition(g, E(g)$width)
plot(g )
