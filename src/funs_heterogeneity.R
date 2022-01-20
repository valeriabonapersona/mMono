'heterogeneity formulas'


# calculate i2
get_i2_ML <- function(dat, mod) {
  #determining how the total variance is distributed over the 3 levels of the meta-analytic model
  #adapted from Assink(2016)
  n <- length(dat$vi)
  list.inverse.variances <- 1 / (dat$vi)
  sum.inverse.variances <- sum(list.inverse.variances)
  squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
  list.inverse.variances.square <- 1 / (dat$vi ^ 2)
  sum.inverse.variances.square <- sum(list.inverse.variances.square)
  
  numerator <- (n - 1) * sum.inverse.variances
  denominator <- squared.sum.inverse.variances - sum.inverse.variances.square
  estimated.sampling.variance <- numerator / denominator
  
  I2_1 <- (estimated.sampling.variance) / (mod$sigma2[1] +
                                             mod$sigma2[2] + estimated.sampling.variance)
  I2_2 <- (mod$sigma2[1]) / (mod$sigma2[1] +
                               mod$sigma2[2] + estimated.sampling.variance)
  I2_3 <- (mod$sigma2[2]) / (mod$sigma2[1] +
                               mod$sigma2[2] + estimated.sampling.variance)
  
  modVariance_1 <- I2_1 * 100
  modVariance_2 <- I2_2 * 100
  modVariance_3 <- I2_3 * 100
  
  return(
    data.frame(
      i2_1 = round(modVariance_1, 2),
      i2_2 = round(modVariance_2, 2),
      i2_3 = round(modVariance_3, 2)
    )
  )
}


get_i2_total <- function(dat, mod) {
  # calculate I2
  W <- diag(1/dat$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  i2 <- 100 * sum(mod$sigma2) / (sum(mod$sigma2) + 
                                   (mod$k-mod$p)/sum(diag(P))) 
  return(round(i2, 2))
  
}

get_i2 <- function(dat, mod) {
  cbind(
    data.frame(total = get_i2_total(dat, mod)),
    get_i2_ML(dat, mod)
  )
}