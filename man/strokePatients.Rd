\name{strokePatients}

\docType{data}

\alias{strokePatients}

\title{Stroke Patient Data}

\description{
This dataset, analyzed by Bishop and Fienberg (1969) and
Bishop, Fienberg and Holland (1975, Section 5.2), describes 121
patients treated for stroke at Massachusetts General Hospital. Each
patient's symptoms of physical disability were graded on a five-point
scale upon admission and again upon discharge. Because patients
were not released if their symptoms got worse, the second score
is always equal to or better than the first score, and cells in the
two-way table above the diagonal may be regarded as structural
zeros.
}

\usage{strokePatients}

\format{a data frame with 15 rows and 3 variables:

\describe{    

    \item{\code{Initial}}{severity of disability upon admission to the
    hospital. An ordered factor with levels 
    \code{"A"} (least severe), \code{"B"}, \code{"C"}, \code{"D"}, and
    \code{"E"} (most severe)} 

    \item{\code{Final}}{severity of disability upon discharge, with
    same levels as \code{Initial}}

    \item{\code{N}}{frequency in sample}
}
}

\source{
Bishop, Y.M.M. and Fienberg, S.E. (1969) Incomplete two-dimensional
contingency tables. \emph{Biometrics}, 25, 119-128.

Bishop, Y.M.M., Fienberg, S.E. and Holland, P.W. (1975) \emph{Discrete
Multivariate Analysis: Theory and Practice}. Cambridge, MA: MIT Press.
}

\keyword{ datasets }
