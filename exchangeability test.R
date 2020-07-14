Exchangeability_test <- function(X, group, k = 10 ^ 5) {
  X <- as.matrix(X)
  group <- as.factor(group)
  n <- nrow(X)
  p <- length(levels(group))
  
  ns <- numeric(p)
  for (i in seq_len(p)) {
    ns[i] <- max(which(group == levels(group)[i]))
  }
  ns <- cbind(c(1, (ns[-p] + 1)), ns)
  
  permutation_mat <- function(X, permutation) {
    (sapply(
      X = permutation,
      FUN = function(location) {
        n <- length(location)
        mean((X[location[-c(n - 1, n)]] >= X[location[-c(1, n)]]
              & X[location[-c(1, n)]] >= X[location[-c(1, 2)]]) |
               (X[location[-c(n - 1, n)]] <= X[location[-c(1, n)]]
                & X[location[-c(1, n)]] <= X[location[-c(1, 2)]]))
      }
    ))
  }
  
  permutation <- apply(
    ns,
    1,
    FUN = function(x) {
      x[1]:x[2]
    }
  )
  
  RP <- apply(X, 2, permutation_mat, permutation)
  
  create_permutation <- function() {
    apply(
      ns,
      1,
      FUN = function(x) {
        sample(x = x[1]:x[2],
               size = x[2] - x[1],
               replace = FALSE)
      }
    )
  }
  
  empirical_distribution <-
    numeric(k)
  set.seed(10072020)
  for (i in 1:k) {
    empirical_distribution[i] <-
      sum(apply(X, 2, permutation_mat, permot()))
  }
  
  return(sum(empirical_distribution <= RP) / k)
  
}
