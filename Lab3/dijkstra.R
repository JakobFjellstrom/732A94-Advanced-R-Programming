dijkstra <- function(graph, init_node){
  stopifnot(is.numeric(init_node))
  stopifnot(is.data.frame(graph))  
  
  Q <- unique(graph$v1)    # Vertex set
  dist <- rep(Inf, length(Q)) # Distances
  prev <- rep(NA, length(Q))  # Previously visited
  dist[init_node] <- 0
  
  while(length(Q) > 0){
    u <- Q[which.min(dist)]
   
    ind <- which(Q == u)
    Q <- Q[-ind]
  
    neighbours <- graph$v1[which(graph$v2 == u)]
    for(neighbor in neighbours){
      len <- graph$w[which(graph$v1 == neighbor & graph$v2 == u)]
      alt <- dist[u] + len
      if(alt < dist[neighbor]){
        dist[neighbor] <- alt
        prev[neighbor] <- u
      }
    }
  }
  return(dist)
}
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

