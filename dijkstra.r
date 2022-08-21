##########################
# dijkstra
# 详细文章介绍
# http://blog.fens.me/r-graph-shortest-path-dijkstra/
# @author: DanZhang
##################

library(igraph)

df<-data.frame(
  from=c("A","A","A","B","D","B","C","D","C","F","E","F"),
  to=c("B","D","C","C","C","E","E","F","F","E","G","G"),
  weight=c(4,6,6,1,2,7,6,5,4,1,6,8)
)

a<-graph_from_data_frame(df, directed = TRUE, vertices = NULL)
E(a)$arrow.size<-1
E(a)$arrow.width<-0.2
E(a)$label<-df$weight
plot(a)

shortest.paths(a,"A", algorithm = "dijkstra")

shortest.paths(a, algorithm = "dijkstra")

s_node<-all_shortest_paths(a,"A","G")$res[[1]]$name;snode
cols<-data.frame(label=V(a)$name,color=0)
cols$color[which(cols$label %in% s_node)]<-1
V(a)$color<-cols$color
plot(a)

s_vert<-data.frame(from=s_node[-length(s_node)],to=s_node[-1])
a_vert<-as_data_frame(a,what="edges")
idx<-which(paste0(a_vert$from,"-",a_vert$to) %in% paste0(s_vert$from,"-",s_vert$to))
idx

a_vert$color<-"gray"
a_vert$color[idx]<-"red"
E(a)$color<-a_vert$color
plot(a)

get.shortest.paths(a, "A")
