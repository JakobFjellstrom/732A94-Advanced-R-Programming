library(testthat)

test_that("dijkstra rejects wrong inputs", {
  wiki_graph <-
    data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
               v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
               w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
  
  expect_error(dijkstra(c(1,2,3), 2))
  expect_error(dijkstra(matrix(c(1,2,3,4), nrow=2, ncol=2), 2))
  expect_error(dijkstra(wiki_graph, "three"))
})

test_that("dijkstra returns a vector", {
  wiki_graph <-
    data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
               v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
               w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
  expect_true(is.vector(dijkstra(wiki_graph, 3)))
})

test_that("dijkstra returns a distance for every node in the graph", {
  wiki_graph <-
    data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
               v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
               w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
  expect_true(length(dijkstra(wiki_graph, 3)) == length(unique(wiki_graph$v1)))
})
