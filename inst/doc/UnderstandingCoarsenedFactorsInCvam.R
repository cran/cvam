### R code from vignette source 'UnderstandingCoarsenedFactorsInCvam.Rnw'

###################################################
### code chunk number 1: UnderstandingCoarsenedFactorsInCvam.Rnw:142-145
###################################################
oldSweaveHooks <- options()$SweaveHooks # resets to this at end of vignette
options(SweaveHooks=list(fig=function()
par(mar=c(5.1, 4.1, 1.1, 2.1))))


###################################################
### code chunk number 2: UnderstandingCoarsenedFactorsInCvam.Rnw:149-151
###################################################
oldWidth <- options()$width  # resets to this at the end of vignette
options(width=90)


###################################################
### code chunk number 3: "startup"
###################################################
library(datasets)       # attach the library, if needed
data(ChickWeight)       # load dataset into R workspace
ChickWeight[1:3,]       # look at first three rows
str(ChickWeight$Diet)   # examine structure of the variable Diet


###################################################
### code chunk number 4: "chickTable"
###################################################
# compute mean final weight at day 21 by Diet
aggregate( weight ~ Diet, data = subset(ChickWeight, Time==21),
    FUN = mean )
# side-by-side boxplots of final weight at day 21 by Diet
plot( weight ~ Diet, data = subset(ChickWeight, Time==21) )


###################################################
### code chunk number 5: UnderstandingCoarsenedFactorsInCvam.Rnw:241-242
###################################################
plot( weight ~ Diet, data = subset(ChickWeight, Time==21) )


###################################################
### code chunk number 6: "regressFactorX"
###################################################
# regress final weight at day 21 on Diet
result <- lm( weight ~ Diet, data = subset(ChickWeight, Time==21 ) )
summary(result)$coef


###################################################
### code chunk number 7: "regressFactorY"
###################################################
library(nnet)
# regress Diet on initial weight to check for balance
resultA <- multinom(Diet ~ weight,
   data = subset(ChickWeight, Time==0 ), trace=FALSE )
# compare fit to that of a null (intercept-only) model
resultB <- multinom(Diet ~ 1,
   data = subset(ChickWeight, Time==0 ), trace=FALSE )
resultB$deviance - resultA$deviance  # df = 3


###################################################
### code chunk number 8: "lme4"
###################################################
library(lme4)
# Linear growth model with random intercepts and slopes
result <- lmer( weight ~ Time + ( Time | Chick ),
   data = ChickWeight )


###################################################
### code chunk number 9: "HairEyeColor"
###################################################
HairEyeColor


###################################################
### code chunk number 10: "HairEyeColorOrig"
###################################################
tmp <- as.data.frame( ftable( HairEyeColor, row.vars=1:3 ) )
Hair <- rep( tmp$Hair, tmp$Freq )
Eye <- rep( tmp$Eye, tmp$Freq )
Sex <- rep( tmp$Sex, tmp$Freq )
HairEyeColorOrig <- data.frame( Hair, Eye, Sex )
tmp <- table(HairEyeColorOrig$Hair,
  HairEyeColorOrig$Eye, HairEyeColorOrig$Sex )
tmp <- xtabs( ~ Hair + Eye + Sex, data=HairEyeColorOrig )


###################################################
### code chunk number 11: "CreateFactor"
###################################################
weather <- c("clear", "rain", "clear", "cloudy", "snow", "clear", "rain")
weather <- factor(weather)
table(weather)


###################################################
### code chunk number 12: "Cut"
###################################################
# generate 1,000 U(0,1) random variates, then 
# classify them as low, medium, and high
uniform <- runif(1000)
lmh <- cut( uniform, breaks=c(0, .333, .667, 1),
    labels=c("low", "medium", "high") )
table(lmh)


###################################################
### code chunk number 13: "chickwtsA"
###################################################
str(chickwts)
levels( chickwts$feed )


###################################################
### code chunk number 14: "chickwtsA1"
###################################################
storage.mode( chickwts$feed )


###################################################
### code chunk number 15: "chickwtsA2"
###################################################
chickwts$feed[1:5]   # implicitly calling print
table(chickwts$feed)
xtabs(~ feed, data=chickwts)


###################################################
### code chunk number 16: "chickwtsA3"
###################################################
sum( chickwts$feed == "meatmeal" )
chickwts$weight[ chickwts$feed == "horsebean" ]


###################################################
### code chunk number 17: "chickwtsA3"
###################################################
unclass(chickwts$feed)


###################################################
### code chunk number 18: "chickwtsB"
###################################################
chickwts$feed[1:22]


###################################################
### code chunk number 19: "chickwtsC"
###################################################
droplevels( chickwts$feed[1:22] )
chickwts$feed[1:22, drop=TRUE]   # does the same thing


###################################################
### code chunk number 20: "chickwtsD"
###################################################
chickwts$feed[2] <- "HotDogs"   # this produces a missing value
chickwts$feed[1:5]
levels(chickwts$feed) <- c( levels(chickwts$feed), "HotDogs" )
chickwts$feed[ 2 ] <- "HotDogs"   # now it works
chickwts$feed[1:5]


###################################################
### code chunk number 21: "chickwtsE"
###################################################
# For an unordered factor, default contrasts use dummy indicators
contrasts(chickwts$feed)

# For an ordered factor, the default is orthogonal polynomials;
# in the example below, they are linear and quadratic 
uniform <- runif(1000)
lmh <- cut( uniform, breaks=c(0, .333, .667, 1),
    labels=c("low", "medium", "high"), ordered=TRUE )
contrasts(lmh)


###################################################
### code chunk number 22: "partyA"
###################################################
# create a factor with a missing value
party <- factor( c("Dem", "Ind", "Rep", NA, "Rep", "Ind", "Dem") )
# Note that NA is not one of the levels
party
# The missing value appears in the integer codes
unclass(party)
# is.na returns TRUE if the value is missing, FALSE otherwise
is.na(party)


###################################################
### code chunk number 23: "partyA1"
###################################################
table(party)
table(party, exclude=NULL)


###################################################
### code chunk number 24: "partyA2"
###################################################
party <- droplevels(party, exclude=c("Ind",NA))
party


###################################################
### code chunk number 25: "NAlevelA"
###################################################
party <- factor( c("Dem", "Ind", "Rep", NA, "Rep", "Ind", "Dem"),
   exclude=NULL)
party


###################################################
### code chunk number 26: "NAlevelB"
###################################################
party <- factor( c("Dem", "Ind", "Rep", NA, "Rep", "Ind", "Dem") )
party
party <- addNA(party)
party


###################################################
### code chunk number 27: "NAlevelC"
###################################################
party <- droplevels(party, exclude=NA)
party


###################################################
### code chunk number 28: "NAlevelD"
###################################################
party <- addNA(party)
unclass(party)
is.na(party)


###################################################
### code chunk number 29: "manipLevelA"
###################################################
# draw 25 values of red, green, or blue with equal probabilities
myFac <- cut( runif(25), breaks=c(0, .333, .667, 1),
    labels=c("red", "green", "blue") )
table(myFac)
# change three colors to Three Stooges
levels(myFac) <- c("Larry", "Curly", "Moe")
table(myFac)


###################################################
### code chunk number 30: "manipLevelB"
###################################################
# replace "Larry" with "Moe", "Curly with "Larry", "Moe" with "Curly"
levels(myFac) <- c("Moe","Larry", "Curly")
table(myFac)


###################################################
### code chunk number 31: "manipLevelC"
###################################################
# add the mysterious fourth Stooge, creating an empty level
levels(myFac) <- c("Moe","Larry", "Curly", "Shemp")
table(myFac)


###################################################
### code chunk number 32: "manipLevelD"
###################################################
# This will replace every occurrence of "Curly" with "Shemp"... 
levels(myFac)[3] <- "Shemp"
# ...causing "Curly" to be dropped from the levels
table(myFac)


###################################################
### code chunk number 33: "partyA"
###################################################
party <- factor( c("Dem", "Ind", "Rep", "Dem", "Rep", "Ind", "Dem") )
table(party)

# leave "Rep" alone, but combine "Dem" and "Ind" into "notRep"
levels(party) <- list( Rep = "Rep", notRep = c("Dem", "Ind") ) 
table(party)


###################################################
### code chunk number 34: "coarsenedA"
###################################################
myFac <- factor( c("red", "green", NA, "yellow",
   "notRed", "green", "notGreen") )
table(myFac, exclude=NULL)


###################################################
### code chunk number 35: "coarsenedB"
###################################################
levels(myFac)


###################################################
### code chunk number 36: "coarsenedB"
###################################################
library(cvam)
myCoarsenedFac <- coarsened( myFac, levelsList = 
   list( notGreen = c("red", "yellow"), notRed = c("green", "yellow") ) )


###################################################
### code chunk number 37: "coarsenedC"
###################################################
is.factor(myCoarsenedFac)


###################################################
### code chunk number 38: "coarsenedD"
###################################################
storage.mode(myCoarsenedFac)
nlevels(myCoarsenedFac)
levels(myCoarsenedFac)


###################################################
### code chunk number 39: "coarsenedE"
###################################################
myCoarsenedFac


###################################################
### code chunk number 40: "coarsenedAttr"
###################################################
attributes( myCoarsenedFac )


###################################################
### code chunk number 41: "attrRetrieve"
###################################################
baseLevels( myCoarsenedFac )


###################################################
### code chunk number 42: "GSSA"
###################################################
str(abortion2000)
CenRace <- abortion2000$CenRace
Hisp <- abortion2000$Hisp
table(CenRace, Hisp, exclude=NULL)


###################################################
### code chunk number 43: "GSSB"
###################################################
RH <- Hisp:CenRace
table(RH, exclude=NULL)


###################################################
### code chunk number 44: "GSSC"
###################################################
CenRace <- addNA(CenRace)
Hisp <- addNA(Hisp)
RH <- Hisp:CenRace
table(RH)
RH <- droplevels(RH)
table(RH)


###################################################
### code chunk number 45: "GSSCa"
###################################################
RH[ RH == "NA:NA" ] <- NA
RH <- droplevels(RH)


###################################################
### code chunk number 46: "GSSD"
###################################################
levels(RH) <- list(
   nonHispWhite = "nonHisp:White",
   nonHispBlack = "nonHisp:Black",
   nonHispOther = "nonHisp:Other",
   Hisp = c("Hisp:White", "Hisp:Black", "Hisp:Hisp", "Hisp:Other", "Hisp:NA"),
   nonHispNA = "nonHisp:NA",
   NAWhite = "NA:White" )
table(RH)


###################################################
### code chunk number 47: "GSSE"
###################################################
RH  <- coarsened( RH, levelsList = list(
   nonHispNA = c("nonHispWhite", "nonHispBlack", "nonHispOther"), 
   NAWhite = c("nonHispWhite", "Hisp" ) ) )
table(RH)


###################################################
### code chunk number 48: "GSSF"
###################################################
mapping(RH)


###################################################
### code chunk number 49: "GSSHa"
###################################################
abortion2000 <- data.frame(abortion2000, RH)
abortion2000$RH <- RH    # does the same thing


###################################################
### code chunk number 50: "GSSHb"
###################################################
identical( attributes(abortion2000$RH), attributes(RH) )


###################################################
### code chunk number 51: "GSSG"
###################################################
summary(RH)    # essentially the same as table(RH)


###################################################
### code chunk number 52: "GSSH"
###################################################
#  from abortion2000, a three-level factor
PolViews <- abortion2000$PolViews
# there are some missing values
table( is.na(PolViews) )
# but the NAs don't show up in a table
table(RH, PolViews)


###################################################
### code chunk number 53: "GSSI"
###################################################
table(RH, PolViews, exclude=NULL)
table(RH, PolViews = addNA(PolViews) )
table(RH, PolViews = coarsened(PolViews) )


###################################################
### code chunk number 54: "GSSJ"
###################################################
xtabs( ~ RH + PolViews, addNA=TRUE)


###################################################
### code chunk number 55: "GSSJa"
###################################################
# display a flat version of a three-way table, with Sex:RH as
# the row and PolViews as the column, showing the NAs in PolViews
Sex <- abortion2000$Sex
ftable( addNA(PolViews) ~ Sex + RH, exclude=NULL )


###################################################
### code chunk number 56: "GSSK"
###################################################
table( RH=dropCoarseLevels(RH), PolViews)


###################################################
### code chunk number 57: "groupedDataA"
###################################################
groupedData = as.data.frame( xtabs( ~ Age + Sex + CenRace + Hisp,
   data=abortion2000, addNA=TRUE) )
dim(groupedData)
head(groupedData)
# eliminate rows with Freq == 0
groupedData <- subset( groupedData, Freq > 0 )
dim(groupedData)


###################################################
### code chunk number 58: "groupedDataB"
###################################################
CenRace <- addNA(groupedData$CenRace)
Hisp <- addNA(groupedData$Hisp)
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
# copy the coarsened factor into the grouped data frame
groupedData$RH <- RH


###################################################
### code chunk number 59: "groupedDataC"
###################################################
aggregate( Freq ~ RH, FUN=sum, data=groupedData)


###################################################
### code chunk number 60: "attributesA"
###################################################
# list the attributes of our coarsened factor RH
names( attributes( abortion2000$RH ) )
# extract females using `[` and list the attributes
femOnly <- abortion2000[ abortion2000$Sex == "Female", ]
names( attributes( femOnly$RH ) )
# do the same thing with subset
femOnly <- subset( abortion2000, Sex == "Female" )
names( attributes( femOnly$RH ) )


###################################################
### code chunk number 61: "attributesD"
###################################################
newGrouped <- as.data.frame( xtabs( ~ Age + Sex + RH, data=abortion2000,
   addNA = TRUE ) )
newGrouped <- subset( newGrouped, Freq > 0 )
names( attributes( newGrouped$RH ) )


###################################################
### code chunk number 62: "attributesE"
###################################################
attributes( newGrouped$RH ) <- attributes( abortion2000$RH )


###################################################
### code chunk number 63: "estimaterhB"
###################################################
dropRH <- dropCoarseLevels( abortion2000$RH )
round( table(dropRH) / sum( table(dropRH) ), 4 )


###################################################
### code chunk number 64: "estimaterhC"
###################################################
# quickly written function that accepts a single coarsened factor,
# optionally with frequencies, and computes ML estimates for the
# base-level probabilities. This function is used for unit testing.
quickEM <- function( obj, freq = rep(1, length(obj)),
   maxits=1000, eps=1e-06 ){
   # identify the name of the coarsened factor
   mc <- match.call()
   objName <- as.character( mc[[2]] )
   if( objName == "freq" ) stop( gettext(
      "Main argument cannot be named 'freq'"), domain = NA )
   # check args
   stopifnot( inherits(obj, "coarsened") )
   stopifnot( length(freq)==length(obj) )
   stopifnot( all( !is.na(freq) ) )
   stopifnot( all(freq>=0) )
   stopifnot( maxits > 0 )
   # aggregate the data to create model frame, then 
   # pull out the coarsened factor and frequencies
#   obj <- sticky::sticky(obj)
   mf <- aggregate( freq ~ obj, FUN=sum )
   names(mf)[ names(mf)=="obj" ] <- objName
   cFac <- mf[[objName]]
   freq <- mf[["freq"]]
   # starting value: uniform probabilities
   theta <- rep( 1 / nBaseLevels(cFac), nBaseLevels(cFac) )
   names(theta) <- baseLevels(cFac)
   # prepare for iteration
   Ex <- theta.new <- theta
   iter <- 0
   converged <- FALSE
   llvec <- numeric(maxits)
   llvec[] <- NA
   while( ( ! converged ) & ( iter < maxits ) ) {
      iter <- iter + 1
      theta <- theta.new
      Ex[] <- loglik <- 0
      # e-step
      for( i in seq_along(cFac) ){
         iC <- as.integer( cFac[i] )
         if( iC %in% attr(cFac,"baseLevelCodes") ) {
            Ex[iC] <- Ex[iC] + freq[i]
            loglik <- loglik + freq[i] * log( theta[iC] )
         } else {
            w <- match(iC, attr(cFac, "coarseLevelCodes") )
            w <- ( mapping(cFac)[w,] == 1 )
            Ex[w] <- Ex[w] + freq[i] * theta[w] / sum( theta[w] )
            loglik <- loglik + freq[i] * log( sum(theta[w]) )
         }
      }
      llvec[iter] <- loglik
      # m-step
      theta.new <- Ex / sum(Ex)
      # convergence check
      converged <- all( abs(theta.new-theta) <= eps*abs(theta) )      
   }
   list( theta=theta, iter=iter, converged=converged, 
      llvec=llvec[1:iter], loglik=loglik )
}
RH <- abortion2000$RH
result <- quickEM(RH, eps=1e-08)


###################################################
### code chunk number 65: "estimaterhD"
###################################################
round( result$theta, 4)


###################################################
### code chunk number 66: UnderstandingCoarsenedFactorsInCvam.Rnw:1353-1355
###################################################
options(SweaveHooks=oldSweaveHooks)
options(width=oldWidth)


