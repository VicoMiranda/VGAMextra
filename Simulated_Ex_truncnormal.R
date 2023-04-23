# library("VGAM")
# library("truncnorm")
# set.seed(150223)

####################################################
# Symmetric truncation where mu is the midpoint
tpvec <- (1:7) / 10
nn <- 1000
mymu = 100
mysd = 15
fits_list <- list()
for (tp in 1:length(tpvec)) {
  y1 <- rtruncnorm(nn, mean = mymu, sd = mysd,
                   a = qnorm(tpvec[tp]/2, mean=mymu, sd=mysd),
                   b = qnorm(1 - tpvec[tp]/2, mean=mymu, sd=mysd))
  y1_df <- data.frame(y1 = y1)
  
  # Fit a truncated normal distribution to the simulated data
  fits_list[[tp]] <- vglm(y1 ~ 1,
                          truncnormal(min.supp = qnorm(tpvec[tp]/2,
                                                       mean=mymu, sd=mysd),
                                      max.supp = qnorm(1 - tpvec[tp]/2,
                                                       mean=mymu, sd=mysd)),
                          data = y1_df, trace = F)
  cat("Iterations for tp=", tpvec[tp], ": ", fits_list[[tp]]@iter, "\n")
}

##################################
# Asymmetric truncation example
tpvec <- (1:7) / 10
nn <- 1000
mymu = 100
mysd = 15
fit2 <- list()
for (tp in 1:length(tpvec)) {
  y1 <- rtruncnorm(nn, mean = mymu, sd = mysd,
                   a = qnorm(tpvec[tp]/2, mean=mymu, sd=mysd),
                   b = Inf)
  y1_df <- data.frame(y1 = y1)
  # Fit a truncated normal distribution to the simulated data
  fit2[[tp]] <- vglm(y1 ~ 1,
                     truncnormal(min.supp = qnorm(tpvec[tp]/2,
                                                  mean=mymu, sd=mysd),
                                 max.support = Inf),
                     data = y1_df, trace = F)
  cat("Iterations for tp=", tpvec[tp], ": ", fit2[[tp]]@iter, "\n")
}