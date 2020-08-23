load("data.r")

n <- nrow(data)

X <- as.matrix(data[,-c(1:3)])

y <- numeric(nrow(data))
y[data$word=="so"] <- 1

library(fastDummies)
Group <- dummy_cols(as.character(data$source))
conversation <- Group$.data
Group <- as.matrix(Group[,-1])

p <- length(unique(conversation))


ns <- numeric(p)
for (i in seq_len(p)) {
  ns[i] <- max(which(conversation == unique(conversation)[i]))
}
ns <- cbind(c(1,(ns[-p]+1)), ns)

permutation <- apply(ns, 1, FUN = function(x){
  x[1]:x[2]
})


permutation_mat <- function(X,permutation){
  (sapply(X = permutation, FUN = function(location){
    n <- length(location)
    mean((X[location[-c(n-1,n)]] >= X[location[-c(1,n-1)]] 
          & X[location[-c(1,n-1)]] >= X[location[-c(1,2)]]) | 
           (X[location[-c(n-1,n)]] <= X[location[-c(1,n-1)]] 
            & X[location[-c(1,n-1)]] <= X[location[-c(1,2)]]) )
  }))
}

RP <- apply(X, 2, permutation_mat, permutation)

permot <- function(X){
  apply(ns, 1, FUN = function(x){
    sample(x = x[1]:x[2], size = x[2]-x[1], replace = FALSE) 
  })}

distr <- matrix(0, nrow = nrow(RP), ncol = ncol(RP))
k <- 10^5
set.seed(10072020)
for (i in 1:k) {
  distr <- distr + (apply(X, 2, permutation_mat, permot(X)) >= RP)
  print(i/k)
}


sum(distr/k >= 0.95)/(dim(distr)[1]*dim(distr)[2])

set.seed(11072020)
norm_test <- (rnorm(n = n, mean = mean(X), sd = sd(X)))
norm_test <- data.frame(norm = norm_test, 
                        norm_0.25 = norm_test + 1:n * 0.25 * sd(norm_test), 
                        norm_0.50 = norm_test + 1:n * 0.50 * sd(norm_test), 
                        norm_0.75 = norm_test + 1:n * 0.75 * sd(norm_test), 
                        norm_1 = norm_test + 1:n * 1 * sd(norm_test))

library(reshape2)
library(dplyr)
norm_test %>%
  cbind(Index = seq_len(n)) %>%
  melt(id = "Index") %>%
  ggplot(aes(x = Index, y =(value), color = variable))+
  geom_line()

DP <- apply(norm_test, 2, permutation_mat, permutation)
distr <- matrix(0, nrow = nrow(DP), ncol = ncol(DP))
k <- 10^5

for (i in 1:k) {
  distr <- distr + (apply(norm_test, 2, permutation_mat, permot(norm_test)) <= DP)
  print(i/k)
}
colMeans(distr/k >= 0.95)
