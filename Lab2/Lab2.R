name = "Jakob Fjellström"
liuid = "jakfj222"

sheldon_game <- function(player1, player2){
  
  p1_check <- isTRUE(player1 %in% c("rock", "paper", "scissors", "lizard", "spock"))
  p2_check <- isTRUE(player2 %in% c("rock", "paper", "scissors", "lizard", "spock"))
  
  if(p1_check == FALSE | p2_check == FALSE ){
    stop("Invalid option.")
  } 
  
  options_game <- c("rock", "paper", "scissor", "lizard", "spock")
  
  if(player1 == "rock" && player2 == "paper"){
    print("Player 2 wins!")
  }else
    if(player1 == "rock" && player2 == "scissor"){
      print("Player 1 wins!")
    }else
      if(player1 == "rock" && player2 == "lizard"){
        "Player 1 wins!"
      }else
        if(player1 == "rock" && player2 == "spock"){
          print("Player 2 wins!")
        }else
          if(player1 == "paper" && player2 == "rock"){
            print("Player 1 wins!")
          }else
            if(player1 == "paper" && player2 == "scissor"){
              print("Player 2 wins!")
            }else
              if(player1 == "paper" && player2 == "lizard"){
                print("Player 2 wins!")
              }else
                if(player1 == "paper" && player2 == "spock"){
                  print("Player 1 wins!")
                }else
                  if(player1 == "scissor" && player2 == "rock"){
                    print("Player 2 wins!")
                  }else
                    if(player1 == "scissor" && player2 == "paper"){
                      print("Player 1 wins!")
                    }else
                      if(player1 == "scissor" && player2 == "lizard"){
                        print("Player 1 wins!")
                      }else
                        if(player1 == "scissor" && player2 == "spock"){
                          print("Player 2 wins!")
                        }else
                          if(player1 == "lizard" && player2 == "rock"){
                            print("Player 2 wins!")
                          }else
                            if(player1 == "lizard" && player2 == "paper"){
                              print("Player 1 wins!")
                            }else
                              if(player1 == "lizard" && player2 == "scissor"){
                                print("Player 2 wins!")
                              }else
                                if(player1 == "lizard" && player2 == "spock"){
                                  print("Player 1 wins!")
                                }else
                                  if(player1 == "spock" && player2 == "rock"){
                                    print("Player 1 wins!")
                                  }else
                                    if(player1 == "spock" && player2 == "paper"){
                                      print("Player 2 wins!")
                                    }else
                                      if(player1 == "spock" && player2 == "scissor"){
                                        print("Player 1 wins!")
                                      }else
                                        if(player1 == "spock" && player2 == "lizard"){
                                          print("Player 2 wins!")
                                        }else
                                          if(player1 == "spock" && player2 == "spock"){
                                            "DRAW!"
                                          }else
                                            if(player1 == player2){
                                              print("Draw!")
                                            } 
                                              
}


my_moving_median <- function(x,n, ...){
  
  if(is.numeric(x) == FALSE | is.numeric(n) == FALSE){stop("Must be numeric.")}
  
  md <- c()
  for(i in 1:(length(x)-n)){
      md[i] <- median(c(x[i:(i+n)]), ...)
  }
  return(md)
}


for_mult_table <- function(from, to){
  stopifnot(is.numeric(from))
  stopifnot(is.numeric(to))
  
  m <- matrix(NA, nrow=to, ncol=to)
  for(i in from:to){
    for(j in from:to){
      m[i,j] <- i*j
    }
  }
  m <- m[from:to, from:to]
  colnames(m) <- c(from:to)
  row.names(m) <- c(from:to)
  return(m)
}

cor_matrix <- function(X){
  stopifnot(is.data.frame(X))
  
  variances <- rep(0, ncol(X))
  covs <- matrix(NA, ncol=ncol(X), nrow=ncol(X))
  
  for(i in 1:ncol(X)){
    variances[i] <- sum( (X[,i] - mean(X[,i]) )^2 )/(nrow(X))
  }
  
  for(i in 1:ncol(covs)){
    for(j in 1:nrow(covs)){
      covs[i,j] <- 1/nrow(X) * sum( (X[,i] - mean(X[,i])) *  (X[,j] - mean(X[,j])))
    }
  }  
  m <- matrix(NA, ncol=ncol(X), nrow=ncol(X))
  for(i in 1:ncol(m)){
    for(j in 1:nrow(m)){
      m[i,j] <- covs[i,j]/sqrt(variances[i]*variances[j])
    }
  }
  return(m)
}

find_cumsum <- function(x, find_sum){
  stopifnot(is.numeric(x))
  stopifnot(is.vector(x))
  stopifnot(is.numeric(find_sum))
  
  csm <- x[1]
  i <- 1
  while(csm < find_sum & i <= length(x)-1){
    csm <- csm + x[i+1]
    i <- i + 1
  }
  return(csm)
}

while_mult_table <- function(from, to){
  m <- matrix(NA, ncol=to, nrow=to)
  
  i <- from
  j <- from
  while(i <= to){
    j <- from
    while(j <= to){
      m[i,j] <- i*j
      j <- j +1
    }
    i <- i + 1
  }
  m <- m[from:to, from:to]
  colnames(m) <- c(from:to)
  row.names(m) <- c(from:to)
  return(m)
}


trial_division_factorization <- function(x){
  a <- c()
  f <- 2
  i <- 1
  while(x > 1){
    if(x %% f == 0){
      a[i] <- f
      x <- x/f
      i <- i +1
    }else{
      f <- f + 1
    }
  }
  return(a)
}

repeat_find_cumsum <-function(x, find_sum){
  stopifnot(is.numeric(x))
  stopifnot(is.vector(x))
  stopifnot(is.numeric(find_sum))
  
  csm <- x[1]
  i <- 1
  
  repeat{
    csm <- csm + x[i+1]
    i <- i + 1
    if(csm > find_sum | i > (length(x)-1)){break}
  }
  return(csm)
}

repeat_my_moving_median <- function(x, n, ...){
  if(is.numeric(x) == FALSE | is.numeric(n) == FALSE){stop("Must be numeric.")}
  
  md <- c()
  i <- 1
  repeat{
    md[i] <- median(c(x[i:(i+n)]), ...)
    i <- i + 1
    if(i > (length(x) - n)){break}
  }
  return(md)
}

in_environment <- function(env){
  return(ls(env))
}

cov <- function(X){
  stopifnot(is.data.frame(X))
  
  return(t(lapply(X, function(x) sd(x)/mean(x))))
}

moment<-function(i){
  
  if(!is.numeric(i)){stop("Wrong input")}
  
  function(x){mean((x-mean(x))^i)}
  
}


mcmc_counter_factory <- function(thin, burnin){
  i <- 0
  yes <- 0
  tb <- thin + burnin
  keep <- c()
  keep[1] <- tb
  
  for(n in 2:100){
    keep[n] <- tb + thin*(n-1)
  }
  
  function(){
    i <<- i + 1
    if(i %in% keep){
      yes <<- yes + 1
      l <- list(i, TRUE, yes)
    }else{
      l <- list(i, FALSE, yes)
    }  
    return(l)
  }
}

