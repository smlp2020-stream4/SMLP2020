#--------------------------------------------------------------
# Generate LM / LMM data frame according to model specification
# Author: Daniel Schad 
#
# Version 0.1  (2018-09-03)
#--------------------------------------------------------------

# DESCRIPTION
# This is a collection of three pairs of functions:
# - Create dataframe
#    - genDatFact
#    - genDatFactCov
# 
# - Simulate data 
#    - simDatFact
#    - simDatFactCov
#
# - Simulate data for LMM
#    - sim_LMM_Fact
#    - sim_LMM_FactCov
#--------------------------------------------------------------

# INPUT
#
#--------------------------------------------------------------
#
# OUTPUT
#
#--------------------------------------------------------------

#------------------------
# Create dataframe
#------------------------

# factor
genDatFact <- function(nSubj, nItem, nFactLev, factorLevels) {
  ndat <- nSubj * nFactLev * nItem

  dat        <- data.frame(Subj=factor(rep(1:nSubj, each=nFactLev*nItem)))
  dat$Item   <- factor(rep(1:nItem, times=nFactLev*nSubj))
  dat$Factor <- factor(rep(rep(factorLevels, each=nItem), times=nSubj))
  return(dat)
}

#--------------------------------------------------------------

# factor and covariate
genDatFactCov <- function(nSubj, nItem, nFactLev, factorLevels, covMean, covVar) {
  ndat <- nSubj * nFactLev * nItem
  # create data frame
  dat           <- data.frame(Subj=factor(rep(1:nSubj, each=nFactLev*nItem)))
  dat$Item      <- factor(rep(1:nItem, times=nFactLev*nSubj))
  dat$Factor    <- factor(rep(rep(factorLevels, each=nItem), times=nSubj))
  dat$covariate <- rnorm(nrow(dat), covMean, covVar) + 
                   rnorm(nSubj, covMean, covVar)[match( dat$Subj, factor(1:nSubj ) )] +
                   rnorm(nItem, covMean, covVar)[match( dat$Item, factor(1:nItem) )]
  return(dat)
}

#------------------------
# Simulate data
#------------------------

# factor
simDatFact <- function(dat, mContr, beta, sig_u, sig_v, sig_e, empirical) {
  factorLevels <- rownames(mContr)
  nFactLev <- length(factorLevels)
  nSubj <- length(unique(dat$Subj))
  nItem <- length(unique(dat$Item))

  # fixed effects
  contrasts(dat$Factor) <- mContr
  fixEf     <- as.numeric( model.matrix(~ Factor, dat) %*% beta )

  # random effects: subjects
  Sigma_U   <- lme4::sdcor2cov(sig_u) #* sig_e # transform to Variance-Covariance Matrix
  u         <- MASS::mvrnorm(n=nSubj, mu=rep(0, nFactLev), 
                             Sigma=Sigma_U, empirical=empirical) 

  # ... simulate subject random effects
  ranEfSubj <- data.frame(cbind(1,mContr) %*% t(u)) # compute linear predictions
  names(ranEfSubj) <- 1:nSubj # transform in suitable format
  ranEfSubj <- ranEfSubj %>% 
    mutate(Factor=factorLevels) %>% 
    gather(1:nSubj, key="Subj", value="ranEf")

  # random effects: items
  if (is.null(sig_v)) sig_v <- diag(nFactLev)
  Sigma_V   <- lme4::sdcor2cov(sig_v) #* sig_e # transform to Variance-Covariance Matrix
  v         <- MASS::mvrnorm(n=nItem, mu=rep(0, nFactLev), 
                             Sigma=Sigma_V, empirical=empirical) 

  # ... simulate item random effects
  ranEfItem <- data.frame(  cbind(1,mContr) %*% t(v)  ) # compute linear predictions
  names(ranEfItem) <- 1:nItem # transform in suitable format
  ranEfItem <- ranEfItem %>% 
    mutate(Factor=factorLevels) %>% 
    gather(1:nItem, key="Item", value="ranEf")
  
  if (identical(sig_v, diag(nFactLev))) ranEfItem$ranEf <- 0

  # residual error
  eps  <- as.numeric( MASS::mvrnorm(n=nrow(dat), mu=0, Sigma=sig_e^2, empirical=empirical) )

  # combine for generating dependent variable
  idxSubj <- match(paste(dat$Subj, dat$Factor, sep="."),
                   paste(ranEfSubj$Subj, ranEfSubj$Factor, sep="."))
  
  idxItem <- match(paste(dat$Item, dat$Factor, sep="."),
                   paste(ranEfItem$Item, ranEfItem$Factor, sep="."))
  
  dat$DV  <- fixEf + ranEfSubj$ranEf[idxSubj] + ranEfItem$ranEf[idxItem] + eps

  contrasts(dat$Factor) <- contr.treatment(nFactLev) # set to R default contrast
  return(dat)
}

#--------------------------------------------------------------

# factor + covariate
simDatFactCov <- function(dat, mContr, beta, sig_u, sig_v, sig_e, empirical) {
  factorLevels <- rownames(mContr)
  nFactLev <- length(factorLevels)
  nSubj <- length(unique(dat$Subj))
  nItem <- length(unique(dat$Item))

  # fixed effects
  contrasts(dat$Factor) <- mContr
  dat$covariate_c <- dat$covariate - mean(dat$covariate)
  fixEf     <- as.numeric( model.matrix(~ Factor*covariate_c, dat) %*% beta )

  # random effects: subjects
  Sigma_U   <- lme4::sdcor2cov(sig_u) #* sig_e # transform to Variance-Covariance Matrix
  u         <- MASS::mvrnorm(n=nSubj, mu=rep(0, length(beta)), 
                             Sigma=Sigma_U, empirical=empirical) # simulate random effects
  ranEfSubj <- c()
  for (i in 1:nSubj) { # i <- 1
    idx <- which(dat$Subj==as.character(i))
    mmi <- model.matrix(~ Factor*covariate_c, dat[idx,])
    ranEfSubj[idx] <- mmi %*% u[i,] # compute linear predictions
  }
  
  # random effects: items
  if (is.null(sig_v)) sig_v <- diag(nFactLev*2)
  Sigma_V   <- lme4::sdcor2cov(sig_v) #* sig_e # transform to Variance-Covariance Matrix
  v         <- MASS::mvrnorm(n=nItem, mu=rep(0, length(beta)), 
                             Sigma=Sigma_V, empirical=empirical) 
  
  # simulate random effects
  ranEfItem <- c()
  for (i in 1:nItem) { # i <- 1
    idx <- which(dat$Item==as.character(i))
    mmi <- model.matrix(~ Factor*covariate_c, dat[idx,])
    ranEfItem[idx] <- mmi %*% u[i,] 
    # compute linear predictions
  }
  
  if (identical(sig_v, diag(nFactLev*2))) ranEfItem[1:length(ranEfItem)] <- 0
  # residual error
  eps  <- as.numeric( MASS::mvrnorm(n=nrow(dat), mu=0, Sigma=sig_e^2, empirical=empirical) )
  # combine for generating dependent variable
  
  dat$DV  <- fixEf + ranEfSubj + ranEfItem + eps
  # clean up data frame
  contrasts(dat$Factor) <- contr.treatment(nFactLev)
  dat <- subset(dat, select=-covariate_c)
  return(dat)
}

#------------------------------------------------------------------------
# Generate data and simulate dependent variable for a LMM
#------------------------------------------------------------------------

# factor
sim_LMM_Fact <- function(
                    nSubj = 40, 
                    nItem = 40,
                    mContr = MASS::contr.sdif(4), 
                    beta = rep(0,4), 
                    sig_u = matrix(c(20,.5,.4,.3,
                                     .5, 5,.5,.4,
                                     .4,.5, 1,.5,
                                     .3,.4,.5, 5),
                            ncol=nFactLev, nrow=nFactLev, byrow=TRUE), 
                    sig_v = matrix(c(20,.5,.4,.3,
                                     .5, 5,.5,.4,
                                     .4,.5, 1,.5,
                                     .3,.4,.5, 5),
                            ncol=nFactLev, nrow=nFactLev, byrow=TRUE), 
                    sig_e = .5, 
                    empirical = FALSE
                    ) {

  dat <- genDatFact(nSubj, nItem, dim(mContr)[1], rownames(mContr)) 
  # generate data frame

  dat <- simDatFact(dat, mContr, beta, sig_u, sig_v, sig_e, empirical)
  # simulate dependent variable

  contrasts(dat$Factor) <- mContr
  # set contrast attribute for factor
  
  dat <- as.tibble(dat)
  
  return(dat)
}

#--------------------------------------------------------------

# factor and covariate
sim_LMM_FactCov <- function(nSubj = 40, 
                            nItem = 40,
                            mContr = MASS::contr.sdif(4),
                            covMean = 200,
                            covVar = 50,
                            beta = rep(0,4),
                            sig_u = diag(4),
                            sig_v = diag(4),
                            sig_e = .5,
                            empirical = FALSE
) {

  dat <- genDatFactCov(nSubj, nItem, dim(mContr)[1], rownames(mContr), covMean, covVar) 
  # generate data frame
  
  dat <- simDatFactCov(dat, mContr, beta, sig_u, sig_v, sig_e, empirical) 
  # simulate dependent variable
  
  contrasts(dat$Factor) <- mContr
  # set contrast attribute for factor
  
  dat <- as.tibble(dat)
  
  return(dat)
}
#------------------------------------------------------------------------
#------------------------------------------------------------------------

