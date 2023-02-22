###################################################################
# functions for fitting multinomial logistic regression to data in
# "wide format"

.cvamLogitWide.fit <- function( x, y, baseline=1L, critNR=1e-08,
   iterMax=25L, critBoundary ) {
   stopifnot( NROW(x) == NROW(y) )
   stopifnot( NCOL(y) >= 2L )
   n <- NROW(x)
   p <-  NCOL(x)
   r <- NCOL(y)
   storage.mode(y) <- "double"
   storage.mode(x) <- "double"
   baseline <- as.integer(baseline)[1L]
   critNR <- as.double(critNR)[1L]
   iterMax <- as.integer(iterMax)[1L]
   critBoundary <- as.double(critBoundary)
   #--------------------------------------------
   # create a matrix for holding message codes
   msg.len.max <- 40L
   msg.codes <- matrix( 0L, msg.len.max, 17L )
   #--------------------------------------------
   tmp <- .Fortran("cvam_mlogit",
      n = n,
      p = p,
      r = r,
      x = x,
      y = y,
      baseline = baseline,
      iterMax = iterMax,
      critNR = critNR,
      critBoundary = critBoundary,
      iter = integer(1L),
      convergedInt = integer(1L),
      boundaryInt = integer(1L),
      loglik = numeric(1L),
      score = numeric( p*(r-1L) ),
      hess = matrix( numeric(1L), p*(r-1L), p*(r-1L) ),
      coefficients = matrix( numeric(1L), p, r ),
      coefVec = numeric( p*(r-1L) ),
      vhatCoef = matrix( numeric(1L), p*(r-1L), p*(r-1L) ),
      piMat = matrix( numeric(1L), n, r ),
      # messaging
      status = integer(1L),
      msg.len.max = msg.len.max,
      msg.codes = msg.codes,
      msg.len.actual = integer(1L),
      PACKAGE = "cvam" )
      #--------------------------------------------
      # display message from Fortran, if present
      msg.lines <- .msgCvam( tmp$msg.codes, tmp$msg.len.actual )
      if( is.null( msg.lines ) ){
         msg <- "OK"
      } else{
         msg <- paste0( msg.lines, collapse="\n" )
      }
      msg <- paste( msg, "\n", sep="")
      if( msg!= "OK\n" ) cat( paste("Note: ", msg, sep="") )
   #--------------------------------------------
   if( tmp$status != 0 ) stop( gettext( 
      "Procedure aborted" ), domain = NA )
   #--------------------------------------------
   rownames(tmp$coefficients) <- colnames(x)
   colnames(tmp$coefficients) <- colnames(y)
   names(tmp$coefVec) <- paste( rep( colnames(x), r-1 ),
      rep( colnames(y)[-baseline], each=p ), sep="." )
   rownames(tmp$vhatCoef) <- colnames(tmp$vhatCoef) <-
      names(tmp$coefVec)
   #--------------------------------------------
   tmp$method <- "NR"
   tmp$converged <- as.logical(tmp$convergedInt)
   tmp$boundary <- as.logical(tmp$boundaryInt)
   tmp
}

cvamLogitWide <- function( form, data, baseline=1L,
   prior = c("none", "DAP"), priorFreq = NULL,
   critNR=1e-08, iterMax=25L, critBoundary=1e-08 ) {
   #--------------------------------------------
   # create model frame, extract x and y
   mc <- match.call( expand.dots=FALSE )
   mc[[1]] <- quote(stats::model.frame)
   mc$baseline <- mc$prior <- mc$priorFreqTot <-
      mc$critNR <- mc$iterMax <- NULL
   m <- match( c("form", "data"), names(mc), nomatch=0L )
   mc <- mc[ c(1L,m) ]
   names(mc)[2] <- "formula"
   mc$na.action <- as.name("na.fail")
   mc$drop.unused.levels <- FALSE
   mF <- eval( mc, parent.frame() )
   y <- model.response(mF)
   stopifnot( NCOL(y) >= 2L )
   x <- model.matrix(form, mF)
   #--------------------------------------------
   prior <- match.arg( prior )
   if( prior == "DAP" ) {
      fitInterceptOnlyModel <- FALSE
      if( is.null(priorFreq) ) {
         priorFreqTot <- as.double( NCOL(x) * ( NCOL(y) - 1L ) )
	 fitInterceptOnlyModel <- TRUE
      } else {
         storage.mode( priorFreq ) <- "double"
         if( length(priorFreq) == 1 ) {
	    stopifnot( priorFreq >= 0 )
            priorFreqTot <- priorFreq
            fitInterceptOnlyModel <- TRUE
	 } else {
            stopifnot( NROW(priorFreq) == NROW(y) )
   	    stopifnot( NCOL(priorFreq) == NCOL(y) )
            stopifnot( all( priorFreq >=0 ) )
            priorFreqTot <- sum( priorFreq )
         }
      }
      if( fitInterceptOnlyModel ) {
         interceptOnlyFit <- .cvamLogitWide.fit( rep(1,NROW(x)),
            y, baseline, critNR, iterMax, critBoundary )
         if( ! interceptOnlyFit$converged ) stop( gettext(
            "Intercept-only model failed to converge" ), domain = NA )
         alpha <- interceptOnlyFit$piMat[1L,]
         stopifnot( is.null(mF$`_freq`) ) # to detect name conflict
         mFGrouped <- mF
         mFGrouped$`_freq` <- 1
         form2 <- form
         form2[[2L]] <- as.symbol("_freq")
         mFGrouped <- aggregate(form2, data=mFGrouped, FUN=sum )
         nCovPatt <- NROW(mFGrouped)
         priorFreqPerPatt <- priorFreqTot / nCovPatt
         stopifnot( is.null(mF$`_ROW`) ) # to detect name conflict
         mF$`_ROW` <- 1:NROW(mF)
         mF <- merge( mF, mFGrouped, all.x=TRUE, all.y=FALSE, sort=FALSE )
         mF <- mF[ order(mF$`_ROW`), ]
         freq <- mF$`_freq`
         priorFreq <- matrix( alpha, NROW(y), NCOL(y), byrow=TRUE ) *
             ( priorFreqPerPatt / freq )
      }
   } else {
      priorFreq <- matrix( 0, NROW(y), NCOL(y) )
   }
   #--------------------------------------------
   result <- .cvamLogitWide.fit( x, y + priorFreq, baseline, critNR, iterMax,
      critBoundary )
   result$call <- match.call()
   result$mF <- mF
   result$formulaStr <- deparse(form)
   result$prior <- prior
   result$priorFreqTot <- if( prior == "DAP" ) priorFreqTot else NULL
   result$priorFreq <- if( prior == "DAP" ) priorFreq else NULL
   result$fitInterceptOnlyModel <- if( prior == "DAP" )
      fitInterceptOnlyModel else FALSE
   result$alpha <- if( result$fitInterceptOnlyModel ) alpha else NULL
   result$nCovPatt <- if( result$fitInterceptOnlyModel ) nCovPatt else NULL
   result$nRowData <- NROW(mF)
   result$df.residual <- result$nRowData * ( NCOL(y) - 1L ) -
      length( result$coefVec )
   result$y <- if( prior == "DAP" ) result$y - priorFreq else result$y
   result$fitted <- result$piMat * apply(result$y, 1, sum )
   rownames(result$piMat) <- rownames(result$fitted) <- rownames(y)
   colnames(result$piMat) <- colnames(result$fitted) <- colnames(y)
   result$fittedWithPrior <- if( prior == "DAP" )
      result$piMat * apply(result$y+result$priorFreq, 1, sum ) else NULL
   result$logP <- result$loglik
   if( prior == "DAP" ) {
      tmp <- result$priorFreq * log( result$piMat )
      tmp[ result$priorFreq==0 ] <- 0
      logPrior <- sum(tmp)
      result$loglik <- result$loglik - logPrior
   }
   structure( result, class = c("cvamLogitWide", "list") )
}

summary.cvamLogitWide <- function(object, showCoef=TRUE, digits=4L, ...) {
   stopifnot( inherits(object, "cvamLogitWide") )
   if( inherits(object, "summary.cvamLogitWide") ) return(object)
   result <- list()
   #--------------------------------------------
   result$formulaStr <- object$formulaStr
   result$prior <- object$prior
   #--------------------------------------------
   result$responseLevels <- colnames( object$y )
   result$baselineLev <- colnames( object$y )[[ object$baseline ]]
   result$baselineInt <- object$baseline
   result$predNames <- colnames(object$x)
   #--------------------------------------------
   result$totalFreqSupplied <- sum( object$y )
   result$nRowData <- object$nRowData
   result$priorFreqTot <- object$priorFreqTot
   result$priorFreq <- object$priorFreq
   result$fitInterceptOnlyModel <- object$fitInterceptOnlyModel
   result$nCovPatt <- object$nCovPatt
   result$alpha <- object$alpha
   #--------------------------------------------
   result$nParamEstimated <- length( object$coefVec )
   result$df.residual <- object$df.residual
   #--------------------------------------------
   result$method <- object$method
   result$iter <- object$iter
   result$converged <- object$converged
   result$boundary <- object$boundary
   result$critNR <- object$critNR
   result$lenGrad <- sqrt(sum(result$score^2))
   #--------------------------------------------
   result$logP <- object$logP
   result$loglik <- object$loglik
   #--------------------------------------------
   result$showCoef <- as.logical(showCoef)[1L]
   result$digits <- as.integer(digits)[1L]
   #--------------------------------------------
   result$headerCoef <-
      gettext("Estimated coefficients with Hessian-based SEs" )
   coefArray <- array( numeric(1L),
      c( NCOL(object$x), 4L, NCOL(object$y) ) )
   dimnames(coefArray) <- list( colnames(object$x),
         c("coef", "SE", "zstat", "pval" ), 
         paste( "Response =", colnames( object$y ) ) )
   coefArray[,"coef",] <- object$coefficients
   coefArray[,"SE",] <- .unstackCoef( sqrt( diag(object$vhatCoef) ),
      result$predNames, result$responseLevels, result$baselineLev )
   zstat <- coefArray[,"coef",] / coefArray[,"SE",]
   pval <- 2 * pnorm( - abs(zstat) )
   coefArray[,"zstat",] <- round(zstat, 2)
   coefArray[,"pval",] <- round(pval, 4)
   result$coefficients <- coefArray
   #--------------------------------------------
   structure( result, class = "summary.cvamLogitWide" )
}

print.cvamLogitWide <- function(x, ...) {
   stopifnot( inherits(x, "cvamLogitWide") )
   print( summary(x, ...) )
   invisible()
}

print.summary.cvamLogitWide <- function(x, digits=x$digits, ...) {
   stopifnot( inherits(x, "summary.cvamLogitWide") )
   #--------------------------------------------
   cat(x$formulaStr, sep="\n")
   strA <- format( c( "Prior:" ), justify="right")
   strB <- format( c( x$prior ), justify="left")
   cat( paste(strA, strB), sep="\n" )
   cat("\n") 
   #--------------------------------------------
   strA <- format( c("Response categories:",
      "Baseline category:"),
      justify="right")
   strB <- format( c( paste(x$responseLevels, collapse=" "),
      x$baselineLev),
      justify="left")
   cat( paste(strA, strB), sep="\n" )
   cat("\n") 
   #--------------------------------------------
   cat("Sample size:", sep="\n")
   strA <- format( c( "Rows of supplied data =",
      "Total N in supplied data ="), justify="right" )
   strB <- format( c( x$nRowData, x$totalFreqSupplied),
      justify = "left" )
   cat( paste(strA, strB), sep="\n")
   cat("\n")
   #--------------------------------------------
   if( x$prior == "DAP" ) {
      cat("Data-augmentation prior (DAP):", sep="\n")
      if( x$fitInterceptOnlyModel ) {
         strA <- format( c( "Number of distinct covariate patterns =",
            "Prior effective sample size =",
            "Prior N per pattern ="), justify="right" )
         strB <-  c( format( x$nCovPatt ),
             format( c( x$priorFreqTot,
               round( x$priorFreqTot / x$nCovPatt, digits=digits) ),
               justify = "left" ) )
         cat( paste(strA, strB), sep="\n")
         cat("\n")
         cat("Estimated proportions for distributing prior counts:", sep="\n")
         strA <- format( x$responseLevels, justify="right")
         strB <- format( x$alpha, digits=digits, justify="left")
         cat( paste(strA, strB), sep="\n")
      } else {
         cat("Table of prior frequencies supplied by user", sep="\n")
         strA <- format( c( "Prior effective sample size ="),
            justify="right" )
         strB <- format( c( x$priorFreqTot ), justify="left" )
         cat( paste(strA, strB), sep="\n")
      }
      cat("\n")
   }
   #--------------------------------------------
   strA <- format( c("Number of estimated parameters =",
      "Degrees of freedom ="), justify="right" )
   strB <- format( c(x$nParamEstimated, x$df.residual), justify = "left" )
   cat( paste(strA, strB), sep="\n")
   cat("\n")
   #--------------------------------------------
   cat( "Newton-Rapshon procedure:", sep="\n" )
   if( x$converged ) {
      cat( gettextf( "Converged at iteration %i", x$iter ), sep="\n")
   } else {
      cat( gettextf( "Failed to converge by iteration %i",
         x$iter ), sep="\n")
   }
   if( !is.null(x$lenGrad) ) 
       cat( gettextf( "Gradient length = %f", x$lenGrad ), sep="\n" )
   if( x$boundary ) cat("Estimate at or near boundary", sep="\n" )
   cat("\n")
   strA <- format( c("Final logP =", "Final loglik ="), justify="right")
   strB <- format( c(x$logP, x$loglik), justify="left")
   cat( paste(strA, strB), sep="\n")
   cat("\n")
   #--------------------------------------------
   if( x$showCoef ) {
      cat( x$headerCoef, sep="\n")
      print(x$coefficients, digits=x$digits, ...)
      cat("\n")
   }
   #--------------------------------------------
   invisible()
}

fitted.cvamLogitWide <- function( object, 
   type=c("mean", "prob", "link"), ...) {
   stopifnot( inherits( object, "cvamLogitWide" ) )
   type <- match.arg(type)
   if( type == "mean" ) {
      return( object$fitted )
   } else if( type == "prob" ) {
      return( object$piMat )
   } else {
      return( object$x %*% object$coefficients )
   }
}

coef.cvamLogitWide <- function( object, covMat=FALSE, ...) {
   stopifnot( inherits( object, "cvamLogitWide" ) )
   if( covMat ) {
      return( list( coef = object$coefVec, covMat = object$vhatCoef ) )
   } else {
      return( object$coefficients )
   }
}

model.matrix.cvamLogitWide <- function(object, ...) {
   stopifnot( inherits(object, "cvamLogitWide") )
   object$x
}

model.response.cvamLogitWide <- function(object, ...) {
   stopifnot( inherits(object, "cvamLogitWide") )
   object$y
}

deviance.cvamLogitWide <- function(object, ...) {
   stopifnot( inherits(object, "cvamLogitWide") )
   observed <- object$y
   expected <- fitted(object, type="mean")
   devMat <- 2 * observed * log( observed / expected )
   devMat[ observed == 0 ] <- 0
   sum(devMat)
}

residuals.cvamLogitWide <- function(object, ...) {
   stopifnot( inherits(object, "cvamLogitWide") )
   observed <- object$y
   expected <- fitted(object, type="mean")
   ( observed - expected ) / sqrt( expected )
}

anova.cvamLogitWide <-
   function( object, ..., method=c("lrt", "logP", "AIC", "BIC"),
      pval = FALSE, pvalDigits=4L, showRank=NULL ) {
   method <- match.arg(method)
   dotargs <- list(...)
   named <- if (is.null(names(dotargs))) 
        rep_len(FALSE, length(dotargs))
   else (names(dotargs) != "")
   if (any(named)) warning(
  "the following arguments to 'anova.cvamLogitWide' are invalid and dropped: ", 
      paste(deparse(dotargs[named]), collapse = ", ") )
   dotargs <- dotargs[!named]
   modList <- c( list(object), dotargs )
   if( length(modList) < 2L ) stop( gettext(
      'Need at least two objects of class "cvamLogitWide" to compare'),
      domain = NA ) 
   is.cvamLogitWide <-
      vapply(modList, function(x) inherits(x, "cvamLogitWide"), NA)
   if( any( !is.cvamLogitWide ) ) stop( gettext(
      'Some supplied objects are not of class "cvamLogitWide"'), domain = NA ) 
   summList <- lapply( modList, summary.cvamLogitWide )
   is.NR <- unlist( lapply( summList, `[[`, "method" ) ) == "NR"
   if( any( !is.NR ) ) warning( gettext(
      'Some supplied objects do not have method "NR"'), domain = NA ) 
   responseLevels <- lapply( summList, `[[`, "responseLevels" )
   sameLevels <- 
      unlist( lapply( responseLevels, function(x,y) isTRUE(all.equal(x,y)),
      y=responseLevels[[1]] ) )
   if( ! all(sameLevels) ) stop( gettext(
      'Fitted models do not all have the same response levels'),
      domain = NA )
   priorTypes <- unlist( lapply( summList, `[[`, "prior" ) )
   if( ! all( priorTypes == priorTypes[1L] ) ) warning( gettext(
      'Fitted models do not all have the same prior distribution'),
      domain = NA )
   if( priorTypes[1L] == "DAP" ) {
      priorFreqs <- unlist( lapply( summList, `[[`, "priorFreqTot" ) )
      if( ! all( priorFreqs == priorFreqs[1L] ) ) warning( gettext(
         'Fitted models do not all have the same prior sample size'),
         domain = NA )
   }
   nTotal <- unlist( lapply( summList, `[[`, "totalFreqSupplied" ) )
   if( ! all( nTotal == nTotal[1L] ) ) warning( gettext(
      'Fitted models are based on different sample sizes'),
      domain = NA )
   #----------------------------------
   formulaStr <- unlist( lapply( summList, `[[`, "formulaStr" ) )
   formulaStr <- paste("Model ", format(1:length(summList)), ": ",
      formulaStr, sep="")
   formulaStr <- paste( formulaStr, collapse="\n" )
   nParams <- unlist( lapply( summList, `[[`, "nParamEstimated" ) )
   resid.df <- unlist( lapply( summList, `[[`, "df.residual" ) )
   if( method %in% c("lrt", "logP") ) {
      meas <- if( method == "lrt" ) 
         unlist( lapply( summList, `[[`, "loglik" ) ) else
         unlist( lapply( summList, `[[`, "logP" ) )
      meas <- -2*meas
      result <- data.frame( resid.df, meas )
      names(result)[2L] <- if( method == "lrt" ) "-2*loglik" else "-2*logP" 
      result$df <- - ( c( NA, nParams[-length(nParams)]) - nParams )
      result$change <- c( NA, meas[-length(meas)] ) - meas
      pvalDigits <- as.integer(pvalDigits)[1L]
      if( pval ) result$pval <- 
         round( 1 - pchisq( result$change, result$df ), pvalDigits )
   } else {
      meas <- -2 * unlist( lapply( summList, `[[`, "loglik" ) )
      result <- data.frame( nParams, meas )
      names(result)[2] <- "-2*loglik" 
      IC <- if( method == "AIC" ) meas + 2*nParams else
         meas + log(nTotal) * nParams
      if( method == "AIC" ) meas <- result$AIC <- IC else
         meas <- result$BIC <- IC
   }
   showRank <- if( is.null(showRank) ) method %in% c("AIC", "BIC") else
      as.logical(showRank)[1L] 
   if( showRank ) result$rank <- rank(meas)
   structure( result,
      heading = formulaStr,
      class = c("anova", "data.frame") )
}

.piMat <- function( xBeta, likMat=NULL ) {
   #--- convert xBeta to probs
   shift <- apply(xBeta, 1, max)
   piMat <- exp(xBeta - shift)
   piMat <- piMat / apply(piMat, 1, sum )
   #--- multiply by likMat and normalize
   if( !is.null(likMat) ) piMat <- piMat * likMat
   piMat <- piMat / apply(piMat, 1, sum )
   piMat
}

fitLCPrev <- function( form, data, likMat, freq,
   baseline=NULL, prior=c("none", "DAP"), priorFreq=NULL,
   criterion=1e-06, iterMaxEM=500L, iterMaxNR=25L ) {
   #-------------------------
   # create model frame, extract model.matrix and model.response
   mc <- match.call( expand.dots=FALSE )
   mc[[1]] <- quote(stats::model.frame)
   m <- match( c("form", "data", "freq"), names(mc), nomatch=0L )
   mc <- mc[ c(1L,m) ]
   names(mc)[2] <- "formula"
   mc$na.action <- as.name("na.fail")
   mc$drop.unused.levels <- FALSE
   mF <- eval( mc, parent.frame() )
   x <- model.matrix(form, mF)
   L <- model.response(mF)
   if( ! is.latentFactor(L) ) stop( gettext(
      "Response variable in formula is not a latent factor" ), domain = NA )
   if( is.null( mF$`(freq)` ) ) {
      freq <- rep(1L, NROW(mF))
      freqSupplied <- FALSE
   } else {
      freq <- mF$`(freq)`
      freqSupplied <- TRUE
   }
   storage.mode(freq) <- "double"
   stopifnot( all(freq >= 0) )
   #-------------------------
   # check likMat and baseline
   if( NROW(likMat) != NROW(x) ) stop( gettextf(
      "likMat has incorrect number of rows, should be %i", NROW(x)),
      domain = NA )
   if( NCOL(likMat) != nBaseLevels(L) ) stop( gettextf(
      "likMat has incorrect number of columns, should be %i", nBaseLevels(L)),
      domain = NA )
   if( ! setequal( colnames(likMat), baseLevels(L) ) ) stop( gettext(
      "colnames(likMat) are not the baseLevels of the latent factor" ),
       domain = NA )
   if( is.null(baseline) ) {
      baseline <- baseLevels(L)[[1L]]
   } else {
      baseline <- as.character(baseline)[[1L]]
      if( ! ( baseline %in% baseLevels(L) ) ) stop( gettext(
         "baseline is not one of the baseLevels of the latent factor" ),
         domain = NA )
   }
   m <- match( baseLevels(L), colnames(likMat) )
   likMat <- data.matrix( likMat[,m] )
   baseline <- match( baseline, baseLevels(L) )
   #-------------------------
   # handle prior
   prior <- match.arg( prior )
   if( prior == "DAP" ) {
      if( is.null(priorFreq) ) {
         priorFreqTot <- as.double( NCOL(x) * ( nBaseLevels(L) - 1L ) )
         xInt <- rep(1,NROW(x))
         betaNew <- matrix(0, 1L, nBaseLevels(L) )
         converged <- FALSE
         iter <- 0L
         while( ( ! converged ) & ( iter < iterMaxEM ) ) {
	    iter <- iter + 1L
            beta <- betaNew
            Lhat <- .piMat( xInt %*% beta, likMat ) * freq
            fit <- .cvamLogitWide.fit( xInt, Lhat, baseline, criterion,
               iterMaxNR )
            if( ! fit$converged ) stop( gettext(
               "Intercept-only model failed to converge" ), domain = NA )
            betaNew[] <- fit$coefficients
            converged <- all( abs(betaNew-beta) <= criterion )
         }
         if( ! converged ) stop( gettext(
            "Intercept-only model failed to converge" ), domain = NA )
         alpha <- fit$piMat[1L,]
         stopifnot( is.null(mF$`_freq`) )
         mFGrouped <- mF
         mFGrouped$`_freq` <- 1
         form2 <- form
         form2[[2L]] <- as.symbol("_freq")
         mFGrouped <- aggregate(form2, data=mFGrouped, FUN=sum )
         nCovPatt <- NROW(mFGrouped)
         priorFreqPerPatt <- priorFreqTot / nCovPatt
         stopifnot( is.null(mF$`_ROW`) )
         mF$`_ROW` <- 1:NROW(mF)
         mF <- merge( mF, mFGrouped, all.x=TRUE, all.y=FALSE, sort=FALSE )
         mF <- mF[ order(mF$`_ROW`), ]
         pfreq <- mF$`_freq`
         priorFreq <- matrix( alpha, NROW(x), nBaseLevels(L), byrow=TRUE ) *
             ( priorFreqPerPatt / pfreq )
      } else {
         stopifnot( NROW(priorFreq) == NROW(x) )
         stopifnot( NCOL(priorFreq) == nBaseLevels(L) )
         stopifnot( all( priorFreq >=0 ) )
      }
   } else {
      priorFreq <- matrix(0, NROW(x), nBaseLevels(L) )
   }
   #-------------------------
   betaNew <- matrix(0, NCOL(x), nBaseLevels(L) )
   rownames(betaNew) <- colnames(x)
   colnames(betaNew) <- baseLevels(L)
   converged <- FALSE
   iter <- 0L
   aborted <- FALSE
   while( ( ! converged ) & ( iter < iterMaxEM ) ) {
      iter <- iter + 1L
      beta <- betaNew
      Lhat <- .piMat( x %*% beta, likMat ) * freq
      fit <- .cvamLogitWide.fit( x, Lhat+priorFreq, baseline,
         criterion, iterMaxNR )
      if( ! fit$converged ) {
         aborted <- TRUE
         break
      }
      betaNew[] <- fit$coefficients
      converged <- all( abs(betaNew-beta) <= criterion )
   }
   #-------------------------
   beta <- betaNew
   betaVec <- fit$coefVec
   pi <- .piMat( x %*% beta )
   piStar <- .piMat( x %*% beta, likMat )
   Lhat <- piStar * freq
   #-------------------------
   loglik.derivs <- .LCPrev.loglik.derivs( x, betaVec, likMat, freq, 
       baseline)
   logP <- loglik <- loglik.derivs$loglik
   score <- loglik.derivs$score
   hess <-  loglik.derivs$hess
   if( prior == "DAP" ) {
      logprior.derivs <-
         .LogitWide.loglik.derivs( x, priorFreq, betaVec, baseline )
      logP <- logP + logprior.derivs$loglik
      score <- score + logprior.derivs$score
      hess <-  hess + logprior.derivs$hess
   }
   tmp <- try( chol(-hess), silent=TRUE )
   if( inherits(tmp, "try-error") ) {
      message( gettext(
         "logP not concave, standard errors not available" ), domain = NA )
      vhatBetaVec <- NULL
   } else vhatBetaVec <- solve(-hess)
   #-------------------------
   result <- list(
      beta = betaNew,
      pi = pi,
      piStar = piStar,
      Lhat = Lhat,
      prior = prior,
      priorFreq = if( prior == "DAP" ) priorFreq else NULL,
      alpha = if( prior == "DAP" ) alpha else NULL,
      x = x,
      freq = freq,
      betaVec = betaVec,
      baseline = baseline,
      likMat = likMat,
      aborted = aborted,
      iter = iter,
      converged = converged,
      loglik = loglik,
      logP = logP,
      score = score,
      hess = hess,
      vhatBetaVec = vhatBetaVec )
   return(result)
}

.LogitWide.loglik.derivs <- function( x, y, betaVec, baseline=1L ) {
   stopifnot( NROW(x) == NROW(y) )
   stopifnot( NCOL(y) >= 2L )
   n <- NROW(x)
   p <-  NCOL(x)
   r <- NCOL(y)
   stopifnot( length(betaVec) == p*(r-1L) )
   storage.mode(betaVec) <- storage.mode(y) <- storage.mode(x) <- "double"
   baseline <- as.integer(baseline)[1L]
   #--------------------------------------------
   # create a matrix for holding message codes
   msg.len.max <- 40L
   msg.codes <- matrix( 0L, msg.len.max, 17L )
   #--------------------------------------------
   tmp <- .Fortran("cvam_mlogit_loglik_derivs",
      n = n,
      p = p,
      r = r,
      x = x,
      y = y,
      baseline = baseline,
      betaVec <- betaVec,
      loglik = numeric(1L),
      score = numeric( p*(r-1L) ),
      hess = matrix( numeric(1L), p*(r-1L), p*(r-1L) ),
      # messaging
      status = integer(1L),
      msg.len.max = msg.len.max,
      msg.codes = msg.codes,
      msg.len.actual = integer(1L),
      PACKAGE = "cvam" )
      #--------------------------------------------
      # display message from Fortran, if present
      msg.lines <- .msgCvam( tmp$msg.codes, tmp$msg.len.actual )
      if( is.null( msg.lines ) ){
         msg <- "OK"
      } else{
         msg <- paste0( msg.lines, collapse="\n" )
      }
      msg <- paste( msg, "\n", sep="")
      if( msg!= "OK\n" ) cat( paste("Note: ", msg, sep="") )
   #--------------------------------------------
   if( tmp$status != 0 ) stop( gettext( 
      "Procedure aborted" ), domain = NA )
   #--------------------------------------------
   list(
      loglik = tmp$loglik,
      score = tmp$score,
      hess = tmp$hess )
}

.LCPrev.loglik.derivs <- function( x, betaVec, likMat, freq=NULL, 
      baseline=1L ) {
   stopifnot( NROW(x) == NROW(likMat) )
   stopifnot( NCOL(likMat) >= 2L )
   n <- NROW(x)
   p <-  NCOL(x)
   r <- NCOL(likMat)
   stopifnot( length(betaVec) == p*(r-1L) )
   if( is.null(freq) ) freq <- rep(1, n)
   stopifnot( length(freq) == n )
   storage.mode(freq) <- storage.mode(betaVec) <-
      storage.mode(likMat) <- storage.mode(x) <- "double"
   baseline <- as.integer(baseline)[1L]
   #--------------------------------------------
   # create a matrix for holding message codes
   msg.len.max <- 40L
   msg.codes <- matrix( 0L, msg.len.max, 17L )
   #--------------------------------------------
   tmp <- .Fortran("cvam_lcprev_loglik_derivs",
      n = n,
      p = p,
      r = r,
      x = x,
      likMat = likMat,
      freq = freq,
      baseline = baseline,
      betaVec <- betaVec,
      loglik = numeric(1L),
      score = numeric( p*(r-1L) ),
      hess = matrix( numeric(1L), p*(r-1L), p*(r-1L) ),
      # messaging
      status = integer(1L),
      msg.len.max = msg.len.max,
      msg.codes = msg.codes,
      msg.len.actual = integer(1L),
      PACKAGE = "cvam" )
      #--------------------------------------------
      # display message from Fortran, if present
      msg.lines <- .msgCvam( tmp$msg.codes, tmp$msg.len.actual )
      if( is.null( msg.lines ) ){
         msg <- "OK"
      } else{
         msg <- paste0( msg.lines, collapse="\n" )
      }
      msg <- paste( msg, "\n", sep="")
      if( msg!= "OK\n" ) cat( paste("Note: ", msg, sep="") )
   #--------------------------------------------
   if( tmp$status != 0 ) stop( gettext( 
      "Procedure aborted" ), domain = NA )
   #--------------------------------------------
   list(
      loglik = tmp$loglik,
      score = tmp$score,
      hess = tmp$hess )
}
