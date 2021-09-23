### R code from vignette source 'FittingLogLinearModelsInCvam.Rnw'

###################################################
### code chunk number 1: "UCBA.A"
###################################################
# show the structure of the object
str(UCBAdmissions)
# display slices of the table corresponding to Dept "A" and "B"
UCBAdmissions[,,1:2]


###################################################
### code chunk number 2: "UCBA.B"
###################################################
# one-way table for Dept (dimension 3)
apply( UCBAdmissions, 3, sum )
# two-way table for Gender x Admit (dimensions 2 and 1),
# with chisquare test for independence
GenderByAdmit <- apply( UCBAdmissions, c(2,1), sum )
chisq.test( GenderByAdmit )


###################################################
### code chunk number 3: "UCBA.C"
###################################################
library(cvam)
# display the first few rows
head(microUCBAdmissions)


###################################################
### code chunk number 4: "UCBA.D"
###################################################
dF <- microUCBAdmissions  # to save typing
# this reproduces the 3-way table UCBAdmissions
result <- table( Admit = dF$Admit, 
   Gender = dF$Gender, Dept = dF$Dept )
str(result)
all.equal( result, UCBAdmissions )
# do the same thing with xtabs, which accepts formula notation
result <- xtabs( ~ Admit + Gender + Dept, data=microUCBAdmissions )


###################################################
### code chunk number 5: "UCBA.E"
###################################################
result <- as.data.frame(UCBAdmissions) 
head(result)


###################################################
### code chunk number 6: "UCBAe"
###################################################
# create a Freq variable and fill it with ones
microUCBAdmissions$Freq <- 1
# use aggregate to sum the Freq variable within categories of
# Admit, Gender, and Dept
result <- aggregate( Freq ~ Admit + Gender + Dept,
   data=microUCBAdmissions, FUN=sum )
head(result)


###################################################
### code chunk number 7: "UCBA.F"
###################################################
# from a table, specifying the row and column variables
ftable( UCBAdmissions, row.vars=c("Dept","Gender"), col.vars="Admit")
# from microdata, using a formula interface
ftable( Admit ~ Dept + Gender, data=microUCBAdmissions )
# with one row variable and two column variables 
ftable( UCBAdmissions, row.vars=c("Dept"),
   col.vars=c("Gender","Admit"))
# omitted variables are summed over
ftable( Admit ~ Gender, data=microUCBAdmissions )


###################################################
### code chunk number 8: "loglinExA"
###################################################
# display observed marginal table and odds ratio
marg <- apply( UCBAdmissions, c(2,1), sum )
marg
marg[1,1] * marg[2,2] / ( marg[2,1] * marg[1,2] )


###################################################
### code chunk number 9: "loglinExB"
###################################################
# display odds ratios for each department
UCBAdmissions[1,1,] * UCBAdmissions[2,2,] / 
   ( UCBAdmissions[1,2,] * UCBAdmissions[2,1,] )


###################################################
### code chunk number 10: "loglinExC"
###################################################
dF <- as.data.frame(UCBAdmissions)
M0 <- glm( Freq ~ Dept*Gender + Dept*Admit, family=poisson(), data=dF )
M1 <- glm( Freq ~ Dept*Gender + Dept*Admit + Gender*Admit,
   family=poisson(), data=dF )
M2 <- glm( Freq ~ Dept*Gender*Admit, family=poisson(), data=dF )


###################################################
### code chunk number 11: "loglinExD"
###################################################
dF$muHat0 <- predict(M0, type="response")
dF$muHat1 <- predict(M1, type="response")
dF$muHat2 <- predict(M2, type="response")
fit0 <- xtabs( muHat0 ~ Admit + Gender + Dept, data=dF )
fit1 <- xtabs( muHat1 ~ Admit + Gender + Dept, data=dF )
fit2 <- xtabs( muHat2 ~ Admit + Gender + Dept, data=dF )


###################################################
### code chunk number 12: "loglinExE"
###################################################
# under M0, the fitted conditional OR's should be 1.0:
fit0[1,1,] * fit0[2,2,] / ( fit0[1,2,] * fit0[2,1,] )

# under M1, the fitted conditional OR's should be equal:
fit1[1,1,] * fit1[2,2,] / ( fit1[1,2,] * fit1[2,1,] )

# under M2, the fitted conditional OR's should vary, and they
# should agree with corresponding OR's based on the observed
# frequencies, because M2 is saturated:
fit2[1,1,] * fit2[2,2,] / ( fit2[1,2,] * fit2[2,1,] )


###################################################
### code chunk number 13: "loglinExF"
###################################################
anova(M0,M1,M2)


###################################################
### code chunk number 14: "loglinExFa"
###################################################
d01 <- deviance(M0)-deviance(M1)
d12 <- deviance(M1)-deviance(M2)
d02 <- deviance(M0)-deviance(M2)


###################################################
### code chunk number 15: "loglinExGa"
###################################################
# make a list of 6 data frames, one per department
list2x2 <- as.list(1:6)
for( j in 1:6 ) list2x2[[j]] <- subset(dF, Dept==levels(dF$Dept)[j]  )
# function for computing deviance for LR test of independence
# within a department
myFunc <- function( dF ) {
   M <- glm( Freq ~ Gender + Admit, family=poisson(), data=dF )
   deviance(M)
}
# apply LR test to each department, returning a vector of deviances
dev <- sapply( list2x2, myFunc )
dev
sum(dev)


###################################################
### code chunk number 16: "loglinExH"
###################################################
# fit M0, M1, and M2 using loglin
M0 <- loglin( UCBAdmissions, margin=list( c(3,2), c(3,1) ), fit=TRUE ) 
M1 <- loglin( UCBAdmissions, margin=list( c(3,2), c(3,1), c(2,1) ), fit=TRUE ) 
M2 <- loglin( UCBAdmissions, margin=list( c(3,2,1)), fit=TRUE ) 


###################################################
### code chunk number 17: "loglinExIa"
###################################################
max( abs( fit0 - M0$fit ) )
max( abs( fit1 - M1$fit ) )
max( abs( fit2 - M2$fit ) )


###################################################
### code chunk number 18: "loglinExIb"
###################################################
M1 <- loglin( UCBAdmissions, margin=list( c(3,2), c(3,1), c(2,1) ),
   fit=TRUE, eps=1e-06 ) 
max( abs( fit1 - M1$fit ) )


###################################################
### code chunk number 19: "loglinExIc"
###################################################
M0$lrt
M1$lrt
M2$lrt


###################################################
### code chunk number 20: "loglinExJ"
###################################################
library(cvam)
dF <- as.data.frame(UCBAdmissions)
M0 <- cvam( ~ Dept*Gender + Dept*Admit, data=dF, freq=Freq )
M1 <- cvam( ~ Dept*Gender + Dept*Admit + Gender*Admit, data=dF, freq=Freq )
M2 <- cvam( ~ Dept*Gender*Admit, data=dF, freq=Freq )
anova(M0,M1,M2)


###################################################
### code chunk number 21: "loglinExK"
###################################################
get.coef(M0, withSE=TRUE)


###################################################
### code chunk number 22: "loglinExL"
###################################################
# display the fitted means for the first few cells
head( get.fitted(M0, type="mean" ) )


###################################################
### code chunk number 23: "loglinExM"
###################################################
# refit M0 with microdata to see that results are the same
M0 <- cvam( ~ Dept*Gender + Dept*Admit, data=microUCBAdmissions )
get.coef(M0, withSE=TRUE)


###################################################
### code chunk number 24: "pearsonPlotFake" (eval = FALSE)
###################################################
## fit0 <- get.fitted(M0, type="mean")
## pearson <- ( fit0$freq - fit0$fit ) / sqrt( fit0$fit )
## labs <- as.character( fit0$Dept : fit0$Gender : fit0$Admit )
## plot( 1:NROW(fit0), pearson,
##    xlab="Cell index", ylab="Pearson residual" )
## identify( 1:NROW(fit0), pearson, labels=labs )


###################################################
### code chunk number 25: FittingLogLinearModelsInCvam.Rnw:964-972
###################################################
fit0 <- get.fitted(M0, type="mean")
pearson <- ( fit0$freq - fit0$fit ) / sqrt( fit0$fit )
labs <- as.character( fit0$Dept : fit0$Gender : fit0$Admit )
plot( 1:NROW(fit0), pearson, xlab="Cell index", ylab="Pearson residual" )
w <- 7
text( w, pearson[w], labels=labs[w], pos=4 )
w <- 19
text( w, pearson[w], labels=labs[w], pos=2 )


###################################################
### code chunk number 26: "loglinExN"
###################################################
# compute the deviance for model M0
M0 <- cvam( ~ Dept*Gender + Dept*Admit, data=dF, freq=Freq )
M2 <- cvam( ~ Dept*Gender*Admit, data=dF, freq=Freq, saturated=TRUE )
dev.M0 <- -2 * ( get.loglik(M0) - get.loglik(M2) )
dev.M0


###################################################
### code chunk number 27: "loglinExO"
###################################################
# fit M1 as a conditional model
M1 <- cvam( ~ Dept*Gender + Dept*Admit + Gender*Admit | Dept + Gender,
   data=dF, freq=Freq )


###################################################
### code chunk number 28: "loglinExP"
###################################################
# show the first few fitted probabilities
head( get.fitted(M1, type="prob") )


###################################################
### code chunk number 29: "crimeA"
###################################################
data(crime)   # load the crime dataset distributed with cvam
crime
sum(crime$n)


###################################################
### code chunk number 30: "crimeB"
###################################################
# fit the model of independence
M0 <- cvam( ~ V1 + V2, freq=n, data=crime )
# fit the model of non-independence
M1 <- cvam( ~ V1 * V2, freq=n, data=crime )
# compare them
anova(M0,M1, pval=TRUE)


###################################################
### code chunk number 31: "crimeC"
###################################################
summary(M0)


###################################################
### code chunk number 32: "crimeD"
###################################################
dF <- get.fitted(M0, type="mean")
dF


###################################################
### code chunk number 33: "crimeE"
###################################################
( dF$fit[1] * dF$fit[4] ) / ( dF$fit[2] * dF$fit[3] )


###################################################
### code chunk number 34: "crimeF"
###################################################
( dF$freq[1] * dF$freq[4] ) / ( dF$freq[2] * dF$freq[3] )


###################################################
### code chunk number 35: "crimeG"
###################################################
# examine the frames from get.fitted for M0 and M1
# to make sure that they use the same cell ordering
get.fitted(M0)
get.fitted(M1)
# compute the quasi-Pearson residuals
muHat <- get.fitted(M0, type="mean")$fit
fHatSat <- get.fitted(M1, type="mean")$freq
quasiPearson <- ( fHatSat - muHat ) / sqrt( muHat )
quasiPearson


###################################################
### code chunk number 36: "raceHispA"
###################################################
data(abortion2000)
CenRace <- addNA(abortion2000$CenRace)
Hisp <- addNA(abortion2000$Hisp)
RH <- Hisp:CenRace
RH <- droplevels(RH)
levels(RH) <- list(
   nonHispWhite = "nonHisp:White",
   nonHispBlack = "nonHisp:Black",
   nonHispOther = "nonHisp:Other",
   Hisp = c("Hisp:White", "Hisp:Black", "Hisp:Hisp", "Hisp:Other", "Hisp:NA"),
   nonHispNA = "nonHisp:NA",
   NAWhite = "NA:White" )
RH  <- coarsened( RH, levelsList = list(
   nonHispNA = c("nonHispWhite", "nonHispBlack", "nonHispOther"), 
   NAWhite = c("nonHispWhite", "Hisp" ) ) )
summary(RH)


###################################################
### code chunk number 37: "raceHispB"
###################################################
baseLevels(RH)


###################################################
### code chunk number 38: "raceHispC"
###################################################
coarseLevels(RH)
mapping(RH)


###################################################
### code chunk number 39: "raceHispD"
###################################################
result <- cvam( ~ RH )
summary(result, showCoef=FALSE)
# display the fitted proportions
get.fitted(result)


###################################################
### code chunk number 40: "AbAnyA"
###################################################
# copy the four variables into a data frame
dF <- data.frame( Sex = abortion2000$Sex, RH = RH,
   PolViews = abortion2000$PolViews, AbAny = abortion2000$AbAny )
# display the first few rows
head(dF)


###################################################
### code chunk number 41: "AbAnyB"
###################################################
myFormula <- ~ Sex*RH*PolViews + AbAny*Sex + AbAny*RH + AbAny*PolViews


###################################################
### code chunk number 42: "AbAnyC"
###################################################
myMod <- cvam( myFormula, data=dF )
satMod <- cvam( ~ Sex*RH*PolViews*AbAny, data=dF, saturated=TRUE )
anova( myMod, satMod, pval=TRUE )
anova( myMod, satMod, method="AIC" )
# compute and summarize the fitted values
muHat <- get.fitted(myMod, type="mean")$fit
summary( muHat )


###################################################
### code chunk number 43: "AbAnyD"
###################################################
noSex <- cvam( ~ Sex*RH*PolViews + AbAny*RH + AbAny*PolViews, data=dF)
anova( noSex, myMod, pval=TRUE )
noRH  <- cvam( ~ Sex*RH*PolViews + AbAny*Sex + AbAny*PolViews, data=dF)
anova( noRH, myMod, pval=TRUE )
noPol <- cvam( ~ Sex*RH*PolViews + AbAny*Sex + AbAny*RH, data=dF)
anova( noPol, myMod, pval=TRUE )


###################################################
### code chunk number 44: "cvamControlA" (eval = FALSE)
###################################################
## cvamControl()


###################################################
### code chunk number 45: "cvamControlB"
###################################################
# use a boundary criterion that is less strict
satMod <- cvam( ~ Sex*RH*PolViews*AbAny, data=dF, saturated=TRUE,
   control=list(critBoundary=1e+06 ) )


###################################################
### code chunk number 46: "cvamControlC"
###################################################
round( get.fitted(satMod, type="prob")$fit, 6)


###################################################
### code chunk number 47: "priorA"
###################################################
myPrior <- cvamPrior( flatten=7.2 )


###################################################
### code chunk number 48: "priorB"
###################################################
# re-fit and compare models using the flattening constant
myMod <- cvam( myFormula, data=dF, prior=myPrior )
satMod <- cvam( ~ Sex*RH*PolViews*AbAny, data=dF,
   saturated=TRUE, prior=myPrior )


###################################################
### code chunk number 49: "priorC"
###################################################
anova( myMod, satMod, pval=TRUE, method="logP")


###################################################
### code chunk number 50: "cvamOnCvam"
###################################################
# fit the saturated model to the crime data
result <- cvam( ~ V1 * V2, data=crime, freq=n)
# run it again, starting from the previous result
result <- cvam(result)
summary(result, showCoef=FALSE)


###################################################
### code chunk number 51: "AbAnyF"
###################################################
cvamEstimate( ~ Sex + AbAny, myMod )


###################################################
### code chunk number 52: "AbAnyG"
###################################################
# estimated conditional probabilities for AbAny given Sex
cvamEstimate( ~ AbAny | Sex, myMod )


###################################################
### code chunk number 53: "AbAnyH"
###################################################
# conditional probabilities for AbAny given RH and PolViews
est <- cvamEstimate( ~ AbAny | RH + PolViews, myMod ) 

# reshape the probabilities into a three-dimensional array
xtab <- xtabs( prob ~ AbAny + RH + PolViews, data = est )

# display the array as a flat table
ftable( xtab, row.vars=c("PolViews", "RH"), col.vars="AbAny" )


###################################################
### code chunk number 54: "predictA"
###################################################
# display the crime data
crime
# fit the model of non-independence
fit <- cvam( ~ V1 * V2, data=crime, freq=n )
# display predictions for V1
cvamPredict( ~ V1, fit, data=crime )


###################################################
### code chunk number 55: "predictB"
###################################################
# display predicted frequencies for V1
cvamPredict( ~ V1, fit, data=crime, freq=n )


###################################################
### code chunk number 56: "predictC"
###################################################
# display predicted frequencies for V1 and V2
cvamPredict( ~ V1 + V2, fit, data=crime, freq=n )


###################################################
### code chunk number 57: "imputeA"
###################################################
set.seed(69852)
cvamImpute( fit, data=crime )


###################################################
### code chunk number 58: "imputeB"
###################################################
cvamImpute( fit, data=crime, freq=n )


###################################################
### code chunk number 59: "approxBayesA"
###################################################
# fit the non-independence model to the crime data
fitML <- cvam( ~ V1 * V2, data=crime, freq=n )
# display the ML estimate for beta and pi
get.coef( fitML )
get.fitted( fitML, type="prob" )$fit
# draw from the approximate posterior, display new beta and pi
set.seed(83425)
obj <- cvam(fitML, method="approxBayes")
get.coef( obj )
get.fitted( obj, type="prob" )$fit


###################################################
### code chunk number 60: "approxBayesB"
###################################################
# produce 5,000 draws of beta, saving also the resulting pi vectors
obj <- cvam(fitML, method="approxBayes",
   control=list(iterApproxBayes=5000, saveProbSeries=TRUE) )
# display the first few beta and pi vectors
head( get.coefSeries(obj) )
head( get.probSeries(obj) )


###################################################
### code chunk number 61: "approxBayesC"
###################################################
pi.series <- get.probSeries(obj)
delta <- pi.series[,3] - pi.series[,2]
summary(delta)
sum( delta > 0 )


###################################################
### code chunk number 62: "mcmcA"
###################################################
set.seed(4358)
fit <- cvam( ~ V1 * V2, data=crime, freq=n, method="MCMC")
summary(fit)


###################################################
### code chunk number 63: "mcmcB"
###################################################
betaSeries <- get.coefSeries( fit )
library(coda)
summary( betaSeries )


###################################################
### code chunk number 64: "mcmcC"
###################################################
# display trace plots and density estimates
plot( betaSeries )


###################################################
### code chunk number 65: FittingLogLinearModelsInCvam.Rnw:1896-1897
###################################################
plot( betaSeries )


###################################################
### code chunk number 66: "mcmcE"
###################################################
# display autocorrelation plots
acfplot( betaSeries )


###################################################
### code chunk number 67: FittingLogLinearModelsInCvam.Rnw:1911-1912
###################################################
acfplot( betaSeries )


###################################################
### code chunk number 68: "mcmcG"
###################################################
get.fitted(fit, type="prob")


###################################################
### code chunk number 69: "mcmcg"
###################################################
set.seed(4358)
fit <- cvam( ~ V1 * V2, data=crime, freq=n, method="MCMC",
   control=list( saveProbSeries=TRUE ) )
piSeries <- get.probSeries(fit)
delta <- piSeries[,3] - piSeries[,2]
summary(delta)
sum( delta > 0 )


###################################################
### code chunk number 70: "miA"
###################################################
impList <- as.list(1:10) # a list to store the imputed datasets
set.seed(769090)         # for reproducibility
for(m in 1:10) {
   # run MCMC under the non-independence model
   tmp <- cvam( ~ V1 * V2, data=crime, freq=n, method="MCMC")
   # impute under the simulated parameters
   impList[[m]] <- cvamImpute( tmp, crime, freq=n)
}
# display the first two imputations
impList[1:2]


###################################################
### code chunk number 71: "miB"
###################################################
# run MCMC for 5,000 iterations, saving an imputation at every 500th
result <- cvam( ~ V1 * V2, data=crime, freq=n, method="MCMC",
   control=list( iterMCMC=5000, imputeEvery=500 ) )
get.imputedFreq(result)


###################################################
### code chunk number 72: "miC"
###################################################
#  run EM, then create ten imputations with approxBayes
fitML <- cvam( ~ V1 * V2, data=crime, freq=n ) 
result <- cvam( fitML, method="approxBayes",
   control=list( iterApproxBayes=10, imputeApproxBayes=TRUE ) )
get.imputedFreq(result)


###################################################
### code chunk number 73: "miD"
###################################################
set.seed(54981)
result <- cvam( fitML, method="MCMC",
   control=list( iterMCMC=5000, imputeEvery=500 ) )
impData <- get.imputedFreq(result)[-(1:2)] # just the frequencies 
est.list <- std.err.list <- as.list(1:10)  # to hold the estimates and SEs
for( m in 1:10 ) {
   f <- impData[,m]
   est.list[[m]] <- log( (f[1] * f[4]) / (f[2] * f[3]) )
   std.err.list[[m]] <- sqrt( sum(1/f) )
}
miInference( est.list, std.err.list )


###################################################
### code chunk number 74: "micromiA"
###################################################
# put the four variables into a data frame
dF <- data.frame( Sex = abortion2000$Sex, RH = RH,
   PolViews = abortion2000$PolViews, AbAny = abortion2000$AbAny )
# fit the saturated model with EM, then do a test run of MCMC
fitEM <- cvam( ~ Sex * RH * PolViews * AbAny, data=dF )
set.seed(598902)
fitMCMC <- cvam( fitEM, method="MCMC")


###################################################
### code chunk number 75: "micromiB"
###################################################
# display fitted cell probs, rounded to five decimal places
round( get.fitted(fitEM, type="prob", mfTrue=FALSE ), 5)
# display some of the coefs and SEs
head( get.coef(fitEM, withSE=TRUE) )


###################################################
### code chunk number 76: "micromiC"
###################################################
# re-run EM with a ridge factor of 0.5
fitEM.ridge <- cvam( ~ Sex * RH * PolViews * AbAny, data=dF,
   prior=cvamPrior( ridge=.5 ) )
round( get.fitted(fitEM.ridge, type="prob", mfTrue=FALSE ), 5)
head( get.coef(fitEM.ridge, withSE=TRUE) )


###################################################
### code chunk number 77: "micromiD"
###################################################
-2 * ( get.loglik(fitEM.ridge) - get.loglik(fitEM) )
exp( get.loglik(fitEM) - get.loglik(fitEM.ridge) )


###################################################
### code chunk number 78: "micromiE"
###################################################
set.seed(87900)
fitMCMC <- cvam( fitEM.ridge, method="MCMC" )


###################################################
### code chunk number 79: "micromiF"
###################################################
set.seed(87900)
fitMCMC <- cvam( fitEM.ridge, method="MCMC",
   control=list( typeMCMC="RWM", tuneRWM=c(1000,.17) ) )


###################################################
### code chunk number 80: "micromiG"
###################################################
M <- 25
impList <- as.list(1:M)  # dummy list to hold the imputed datasets
set.seed(2343)
for( m in 1:M ) {
   # take 2,500 steps of MCMC, then impute
   fitMCMC <- cvam( fitMCMC, control=list(iterMCMC=2500) )
   impList[[m]] <- cvamImpute( fitMCMC, data=dF )
}
# display the first few rows of the original data and 
# the first imputed dataset
head( dF )
head( impList[[1]] )


###################################################
### code chunk number 81: "micromiH"
###################################################
est.list <- SE.list <- as.list(1:M)
for( m in 1:M ) {
   # extract the imputed dataset
   impData <- impList[[m]]
   # create the binary response and fit the logit model
   impData$y <- 1 * ( impData$AbAny == "Yes" )
   logitFit <- glm( y ~ Sex + RH + PolViews, data=impData,
      family=binomial() )
   # extract matrix of coefficients and SEs
   coefMat <- summary(logitFit)$coef
   est.list[[m]] <- coefMat[,1]
   SE.list[[m]]  <- coefMat[,2]
}
# combine the results with Rubin's rules
miInference( est.list, SE.list )


###################################################
### code chunk number 82: "syntheticA"
###################################################
# take 2,500 more steps of MCMC and draw a synthetic dataset
fitMCMC <- cvam( fitMCMC )
synthData <- cvamImpute( fitMCMC, data=dF)
head( synthData )


###################################################
### code chunk number 83: "hivA"
###################################################
hivtest


###################################################
### code chunk number 84: "hivB"
###################################################
hivtest$L <- latentFactor( NROW(hivtest), 2 )
hivtest


###################################################
### code chunk number 85: "hivC"
###################################################
# set the RNG seed and fit the model of local independence
set.seed(125)
fit <- cvam( ~ L*A + L*B + L*C + L*D, data=hivtest, freq=COUNT,
   control = list( startValJitter=.1 ) )


###################################################
### code chunk number 86: "hivE"
###################################################
cvamEstimate( list( ~L, ~A|L, ~B|L, ~C|L, ~D|L ), fit )


###################################################
### code chunk number 87: "hivFa"
###################################################
# perform the lack-of-fit test
fitSat <- cvam( ~ A*B*C*D, data=hivtest, freq=COUNT )
anova( fit, fitSat, pval=TRUE )


###################################################
### code chunk number 88: "hivFb"
###################################################
satFrame <- get.fitted( fitSat, type="mean" )
# this frame has 16 rows; display the first few
head(satFrame)
# get rid of the fitted values, because they are redundant
satFrame$fit <- NULL


###################################################
### code chunk number 89: "hivFbb"
###################################################
LCFrame <-  get.fitted( fit, type="mean" )
# this frame has 32 rows; display the first few
head(LCFrame)


###################################################
### code chunk number 90: "hivFc"
###################################################
muHatTable <- xtabs( fit ~ A + B + C + D, data=LCFrame )
muHatFrame <- as.data.frame( muHatTable, responseName = "muHat" )
# diplay the first few rows to make sure that the
# cell order is correct
head( muHatFrame)


###################################################
### code chunk number 91: "hivFd"
###################################################
muHat <- muHatFrame$muHat
quasiPearson <- ( satFrame$freq - muHat ) / sqrt( muHat )
satFrame$muHat <- round( muHat, 3 )
satFrame$quasiPearson <- round( quasiPearson, 2 )
satFrame


###################################################
### code chunk number 92: "hivI"
###################################################
set.seed(85657)
fitLAB <- cvam( ~ L*A + L*B + L*C + L*D + L*A*B, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fit, fitLAB, fitSat, pval=TRUE)
fitLAC <- cvam( ~ L*A + L*B + L*C + L*D + L*A*C, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fit, fitLAC, fitSat, pval=TRUE)
fitLAD <- cvam( ~ L*A + L*B + L*C + L*D + L*A*D, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fit, fitLAD, fitSat, pval=TRUE)
fitLBC <- cvam( ~ L*A + L*B + L*C + L*D + L*B*C, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fit, fitLBC, fitSat, pval=TRUE)
fitLBD <- cvam( ~ L*A + L*B + L*C + L*D + L*B*D, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fit, fitLBD, fitSat, pval=TRUE)
fitLCD <- cvam( ~ L*A + L*B + L*C + L*D + L*C*D, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fit, fitLCD, fitSat, pval=TRUE)


###################################################
### code chunk number 93: "hivJ"
###################################################
fitBoth <- cvam( ~ L*A + L*B + L*C + L*D + L*A*D + L*B*C, 
   data=hivtest, freq=COUNT,
   control = list(startValJitter=.1) )
anova(fitLAD, fitBoth)
anova(fitLBC, fitBoth)


###################################################
### code chunk number 94: hivK
###################################################
# get predicted probabilities and display them with the dataset
pred <- cvamPredict( ~L, fitLBC, data=hivtest )
cbind( hivtest, round(pred, 3) )


###################################################
### code chunk number 95: "hivk"
###################################################
predFrame <- hivtest[1:8,]
predFrame$COUNT <- NULL
predFrame[["A"]][] <- NA
predFrame[["B"]][] <- NA
predFrame[["C"]][] <- NA
predFrame[["D"]][] <- NA
predFrame[["A"]][1] <- "pos"; predFrame[["A"]][2] <- "neg"
predFrame[["B"]][3] <- "pos"; predFrame[["B"]][4] <- "neg"
predFrame[["C"]][5] <- "pos"; predFrame[["C"]][6] <- "neg"
predFrame[["D"]][7] <- "pos"; predFrame[["D"]][8] <- "neg"
predFrame[["A"]] <- coarsened( predFrame[["A"]] )
predFrame[["B"]] <- coarsened( predFrame[["B"]] )
predFrame[["C"]] <- coarsened( predFrame[["C"]] )
predFrame[["D"]] <- coarsened( predFrame[["D"]] )
pred <- cvamPredict( ~L, fitLBC, data=predFrame )
cbind( predFrame, round(pred, 3) )


###################################################
### code chunk number 96: "hivL"
###################################################
pred <- cvamPredict( ~L, fit, data=predFrame )
cbind( predFrame, round(pred, 3) )


###################################################
### code chunk number 97: "hivM"
###################################################
# re-fit the model with EM using a small ridge factor
set.seed(7666)
fitLBC <- cvam( ~ L*A + L*B + L*C + L*D + L*B*C, 
   data=hivtest, freq=COUNT, prior=cvamPrior( ridge=.1 ),
   control = list(startValJitter=.1) )


###################################################
### code chunk number 98: "hivMa"
###################################################
# do a long run of MCMC and save ten imputed datasets
fitMCMC <- cvam(fitLBC, method="MCMC",
   control=list( typeMCMC="RWM", tuneRWM=c(1000,.5),
      iterMCMC=25000, imputeEvery=2500 ) )


###################################################
### code chunk number 99: "hivN"
###################################################
# check to see if any label switching has occurred
impData <- get.imputedFreq(fitMCMC)
head(impData)


###################################################
### code chunk number 100: "hivNa"
###################################################
impData$freq <- impData[["imp.1"]] # first imputation
BCL <- xtabs( freq ~ B + C + L, data=impData )
BCL


###################################################
### code chunk number 101: "hivO"
###################################################
# use multiple imputations to examine the conditional
# BC odds ratios given L=1 and L=2
est.list <- SE.list <- as.list(1:10)
for( m in 1:10 ) {
   # get the imputed marginal table BxCxL
   impName <- paste( "imp", format(m), sep="." )
   impData$freq <- impData[[impName]]
   BCL <- xtabs( freq ~ B + C + L, data=impData )
   # add 1/2 to every cell to avoid problems
   BCL <- BCL + .5
   # get BC log-odds ratio and SE for L=1
   BCL.1 <- BCL[,,"1"]
   logOR.1 <- log( ( BCL.1[1,1] * BCL.1[2,2] ) /
      ( BCL.1[1,2] * BCL.1[2,1] ) )
   SE.1 <- sqrt( sum( 1/BCL.1 ) )
   # get BC log-odds ratio and SE for L=2
   BCL.2 <- BCL[,,"2"]
   logOR.2 <- log( ( BCL.2[1,1] * BCL.2[2,2] ) /
      ( BCL.2[1,2] * BCL.2[2,1] ) )
   SE.2 <- sqrt( sum( 1/BCL.2 ) )
   # save the estimates and SEs
   est.list[[m]] <- c( logOR.1, logOR.2 )
   SE.list[[m]] <- c( SE.1, SE.2 )
}
miInference( est.list, SE.list )


