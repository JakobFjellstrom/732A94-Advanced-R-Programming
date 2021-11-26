name = "Jakob Fjellström"
liuid = "jakfj222"

my_num_vector <- function(){
  return(c(log10(11), cos(pi/5), exp(pi/3), (1173 %% 7)/19))
}

filter_my_vector <- function(x,leq){
  x[x>=leq] <- NA
  return(x)
}

dot_prod <- function(a,b){
  return(as.numeric(a%*%b))
}

approx_e <- function(N){
  e <- sapply(0:N, function(i) 1/factorial(i))
  return(sum(e))
}

my_magic_matrix <- function(){
  return(t(matrix(c(4,9,2, 3,5,7, 8,1,6), nrow=3, ncol=3)))
}


calculate_elements <- function(A){
  return(ncol(A)*nrow(A))
}

row_to_zero <- function(A,i){
  A[i,] <- 0
  return(A)
}

add_elements_to_matrix <- function(A,x,i,j){
  A[i,j] <- A[i,j] + x
  return(A)
}


my_magic_list <- function(){
  l <- list(info = "my own list", my_num_vector(), my_magic_matrix())
  return(l)
}

change_info <- function(x, text){
  x$info <- text
  return(x)
}

add_note <- function(x, note){
  x$note <- note
  return(x)
}

sum_numeric_parts <- function(x){
  return(sum(unlist(x)))
}


my_data.frame <- function(){
  df <- data.frame(id=1:3,name=c("John", "Lisa", "Azra"), rich=c(F,F,T))
  return(df)
}

sort_head <- function(df, var.name, n){
  df <- head(df[order(-df[[var.name]]),],n)
  return(df)
}

add_median_variable <- function(df, j){
  md <- median(df[,j])
  df$compared_to_median <- ifelse(df[,j] > md, "Greater", "Smaller")
  df$compared_to_median <- ifelse(df[,j] == md, "Median", df$compared_to_median)  
  return(df)
}

analyze_columns <- function(df,j){
  col_1 <- c(mean=mean(df[,j[1]]),median= median(df[,j[1]]),sd= sd(df[,j[1]]))
  col_2 <- c(mean=mean(df[,j[2]]),median= median(df[,j[2]]),sd= sd(df[,j[2]]))
  cor_matrix <- cor(df[,j])
  
  data_list = list(col_1, col_2, cor_matrix)
  names(data_list) = c(colnames(df[,j]), "correlation_matrix")
  return(data_list)
}






