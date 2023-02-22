library(cvam)

fit <- cvam( ~ V1 + V2, data=crime, freq=n)
summary(fit)

# make V1="yes", V2="yes" a structural zero
mfTrue <- get.mfTrue(fit)
strZero <- ( mfTrue$V1 == "yes" ) & ( mfTrue$V2 == "yes" )

# next line would throw an error because observations are found in yes, yes
# fit <- cvam( ~ V1 + V2, data=crime, freq=n, strZero=strZero )

# get rid of bad row and fit with structural zero
crime2 <- crime[-5,]
fit <- cvam( ~ V1 + V2, data=crime2, freq=n, strZero=strZero)
cvamEstimate( ~ V1 + V2, fit)
   
# verify that you get the same fit from saturated model
fit <- cvam( ~ V1 * V2, data=crime2, freq=n, strZero=strZero, saturated=TRUE)
cvamEstimate( ~ V1 + V2, fit)

# next line would throw an error, because modelMatrix not full rank
# fit <- cvam( ~ V1 * V2, data=crime2, freq=n, strZero=strZero)

# example with stroke patients from Bishop, Fienberg and Holland (1975)
# show an equivalent of their Table 5.2-6
xtabs( N ~ Initial + Final, data=strokePatients )

# define structural zeros
mfTrue <- cvam( ~ Initial + Final, freq=N, data=strokePatients,
   method="mfTrue" ) 
strZero <-
   ( ( mfTrue$Initial == "A" ) & ( mfTrue$Final %in% c("B","C","D","E") ) ) |
   ( ( mfTrue$Initial == "B" ) & ( mfTrue$Final %in% c("C","D","E") ) ) |
   ( ( mfTrue$Initial == "C" ) & ( mfTrue$Final %in% c("D","E") ) ) |
   ( ( mfTrue$Initial == "D" ) & ( mfTrue$Final == "E" ) )

# fit model of quasi-independence; fitted values should agree with those
# in Bishop, Fienberg and Holland (1975), Table 5.2-7
fit <- cvam( ~ Initial + Final, data=strokePatients, freq=N, strZero=strZero )
get.fitted(fit, type="mean")

# compute X2, reported by Bishop, Fienberg and Holland (1975) as 8.37
observed <- get.fitted(fit, type="mean")$freq
fitted <- get.fitted(fit, type="mean")$fit
pearson <- ( observed - fitted ) / sqrt(fitted)
pearson <- pearson[!strZero]
X2 <- sum(pearson^2)
X2

# compute G2, reported by Bishop, Fienberg and Holland (1975) as 9.60
fitSat <- cvam( ~ Initial * Final, data=strokePatients, freq=N,
  strZero=strZero, saturated=TRUE )
G2 <- 2 * ( get.loglik(fitSat) - get.loglik(fit) )
G2
