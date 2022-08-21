##########################
# Bellman-Ford
# 详细文章介绍
# http://blog.fens.me/r-graph-shortest-path-bellman-ford/
# @author: DanZhang
##################

library(igraph)

df<-data.frame(
  from=c("A","A","B","B","B","D","D","E"),
  to=c("B","C","C","D","E","B","C","D"),
  weight=c(-1,4,3,2,2,1,5,-3)
)


a<-graph_from_data_frame(df, directed = TRUE)
E(a)$arrow.size<-1
E(a)$arrow.width<-0.2
E(a)$label<-df$weight
plot(a,layout=layout.gem)

shortest.paths(a,"A", algorithm = "bellman-ford")


# bellman-ford 最短距离算法
bellman_ford<-function(df,start="A"){
  
  # 初始化结果数据集
  rs<-data.frame(from=start,to=start,weight=0)
  
  # 初始化结束节点
  finishnode<-c()
  
  # 初始化开始节点
  nodes<-rs
  
  # 当开始节点数量<结束节点数量时，继续循环计算
  while(length(finishnode)<length(rs$to)){
    
    # 过滤已计算过的结束节点
    if(length(finishnode)>0){
      nodes<-rs[-which(rs$to %in% finishnode),]  
    }
    
    # 对原始数据进行循环，遍历每一行
    for(i in 1:nrow(df)){
      row<-df[i,];row
      #print(row)
      
      # 对结束节点进行循环，遍历每一个结束节点
      for(j in nodes$to){
        if(row$from==j){
          # 把之前结束节点的权重叠加本次计算中
          row$weight<-row$weight+min(rs$weight[which(rs$to==j)])
          #print(row$weight)
          # 把结束节点加入到结果数据集
          rs<-rbind(rs,row)
          #print(rs)
        } 
      }
    }
    
    # 保留最短的唯一结束节点
    rs2<-rs[order(rs$weight),]
    n<-which(duplicated(rs2$to))
    if(length(n)>0){
      rs2<-rs2[-n,]
      rs<-rs2[order(as.numeric(row.names(rs2))),]
    }
    
    # 更新已经完成的结束节点
    finishnode<-c(finishnode,nodes$to)
  }
  return(rs)
}

rs<-bellman_ford(df,"A")
rs

# 计算2点间的最短路径
get_shortest_path<-function(rs,start,end){
  
  # 初始化节点
  q<-c(start)
  idx<-which(rs$from==start)
  
  # 判断有几个分支
  if(length(idx)>0){
    for(i in idx){
      # 递归计算
      q2<-get_shortest_path(rs,rs$to[i],end)
      if(tail(q2,1)==end){
        q<-c(q,q2)
      }
    }
  }
  
  # 查看队列状态
  # print(q)
  
  # 当找到结束节点，程序停止
  if(tail(q,1)==end){
    return(q)
  }
  return(q)
}


path<-get_shortest_path(rs[-1,],"A","D")
path

# 最后，我们给A到D的最短路径涂色

path2<-data.frame(from=path[-length(path)],to=path[-1])
idx<-which(paste0(df$from,"-",df$to) %in% paste0(path2$from,"-",path2$to))
idx

df$color<-"gray"
df$color[idx]<-"red"
E(a)$color<-df$color
plot(a,layout=layout.gem)


# new df

df2<-data.frame(
  from=c("A","B","C","D","C","F","B","B","F","G","H","F","E"),
  to=  c("B","C","D","C","E","E","F","G","G","H","I","I","J"),
  weight=c(5,20,10,-5,75,25,30,60,5,-50,-10,50,100)
)

a2<-graph_from_data_frame(df2, directed = TRUE)
E(a2)$arrow.size<-1
E(a2)$arrow.width<-0.2
E(a2)$label<-df2$weight
plot(a2,layout=layout.gem)

rs<-bellman_ford(df2,"A")
rs

path<-get_shortest_path(rs[-1,],"A","D")
path

path2<-data.frame(from=path[-length(path)],to=path[-1])
idx<-which(paste0(df2$from,"-",df2$to) %in% paste0(path2$from,"-",path2$to))
df2$color<-"gray"
df2$color[idx]<-"red"
E(a2)$arrow.size<-1
E(a2)$arrow.width<-0.2
E(a2)$color<-df2$color
plot(a2,layout=layout.gem)

path<-get_shortest_path(rs[-1,],"A","I")
path

path2<-data.frame(from=path[-length(path)],to=path[-1])
idx<-which(paste0(df2$from,"-",df2$to) %in% paste0(path2$from,"-",path2$to))
df2$color<-"gray"
df2$color[idx]<-"red"
E(a2)$arrow.size<-1
E(a2)$arrow.width<-0.2
E(a2)$color<-df2$color
plot(a2,layout=layout.gem)
  
