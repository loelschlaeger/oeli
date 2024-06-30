simulate_data <- function(
    names = c("A", "B", "C"),
    n = 100,
    marginals = list(),
    correlation = diag(length(names)),
    verbose = FALSE
  ) {

  ### input checks


  ### draw data
  if (length(marginals) == 0) {
    ### no marginals specified, so draw from multivariate normal


  } else {
    ### marginals specified
  }

  ### return data

}


# library("SimMultiCorrData")
#
# # Set seed and sample size
# seed <- 11
# n <- 10000
#
# # Calculate standardized cumulants
# # Those for the normal distribution are rounded to ensure the correct values
# # are obtained.
# M1 <- round(calc_theory(Dist = "Gaussian", params = c(0, 1)), 8)
# M2 <- calc_theory(Dist = "Chisq", params = 4)
# M3 <- calc_theory(Dist = "Beta", params = c(4, 2))
# M <- cbind(M1, M2, M3)
#
# # Binary and Ordinal Distributions
# marginal <- list(c(0.3, 0.75), c(0.2, 0.5, 0.9))
# support <- list() # default support will be generated inside simulation
#
# # Poisson Distributions
# lam <- c(1, 5, 10)
#
# # Negative Binomial Distributions
# size <- c(3, 6)
# prob <- c(0.2, 0.8)
#
# ncat <- length(marginal)
# ncont <- ncol(M)
# npois <- length(lam)
# nnb <- length(size)
#
# # Create correlation matrix from a uniform distribution (0.2, 0.7)
# set.seed(seed)
# Rey <- diag(1, nrow = (ncat + ncont + npois + nnb))
# for (i in 1:nrow(Rey)) {
#   for (j in 1:ncol(Rey)) {
#     if (i > j) Rey[i, j] <- runif(1, 0.2, 0.7)
#     Rey[j, i] <- Rey[i, j]
#   }
# }
#
# # Check to see if Rey is positive-definite
# min(eigen(Rey, symmetric = TRUE)$values) < 0
#
# #
# # Lower <- list()
# #
# # # list of standardized kurtosis values to add in case only invalid power
# # #     method pdfs are produced
# # # Skurt <- list(seq(0.5, 2, 0.5), seq(0.02, 0.05, 0.01), seq(0.02, 0.05, 0.01))
# #
# # start.time <- Sys.time()
# # for (i in 1:ncol(M)) {
# #   Lower[[i]] <- calc_lower_skurt(method = "Polynomial", skews = M[3, i],
# #                                  fifths = M[5, i], sixths = M[6, i],
# #                                  Skurt = NULL, seed = 104)
# # }
# #
# #
# # stop.time <- Sys.time()
# # Time <- round(difftime(stop.time, start.time, units = "min"), 3)
# # cat("Total computation time:", Time, "minutes \n")
#
#
# # Make sure Rey is within upper and lower correlation limits
# valid <- valid_corr(k_cat = ncat, k_cont = ncont, k_pois = npois,
#                     k_nb = nnb, method = "Polynomial", means =  M[1, ],
#                     vars =  (M[2, ])^2, skews = M[3, ], skurts = M[4, ],
#                     fifths = M[5, ], sixths = M[6, ], marginal = marginal,
#                     lam = lam, size = size, prob = prob, rho = Rey,
#                     seed = seed)
#
#
# A <- rcorrvar(n = 10000, k_cont = ncont, k_cat = ncat, k_pois = npois,
#               k_nb = nnb, method = "Polynomial", means =  M[1, ],
#               vars =  (M[2, ])^2, skews = M[3, ], skurts = M[4, ],
#               fifths = M[5, ], sixths = M[6, ], marginal = marginal,
#               lam = lam, size = size, prob = prob, rho = Rey, seed = seed)
#
#
# Acorr_error = round(A$correlations - Rey, 6)
# summary(as.numeric(Acorr_error))
#
#
# data <- cbind(
#   A$ordinal_variables,
#   A$continuous_variables,
#   A$Poisson_variables,
#   A$Neg_Bin_variables
# )




