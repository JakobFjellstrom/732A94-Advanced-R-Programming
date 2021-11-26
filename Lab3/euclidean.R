euclidean <- function(a,b){
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(b))
  
  if(b == 0){return(a)}
    else{
      return(euclidean(b, a %% b))
    }
}
euclidean(123612, 13892347912)
euclidean(100, 1000)
