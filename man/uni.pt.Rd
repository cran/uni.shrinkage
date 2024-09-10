\name{uni.pt}
\alias{uni.pt}
\title{
Shrinkage Estimation for the Univariate Normal Mean based on a Preliminary Test Estimator
}
\description{
This function computes a preliminary test (pretest) estimate for the univariate normal mean.
This function also computes the confidence interval based on a pretest estimator.
}
\usage{
uni.pt(y,s,alpha=0.05,gamma=0.05,gamma1=NA,gamma2=NA,conf.int=TRUE)
}
\arguments{
  \item{y}{
A vector of normal distributed data}
  \item{s}{Standard deviation of \code{y}}
  \item{alpha}{Significance level for the preliminary hypothesis test. This parameter satisfies 0< \code{alpha} <1. The default is \code{alpha}=0.05.}
  \item{gamma}{A constant that 1-\code{gamma} is the confidence level. This constant satisfies 0< \code{gamma} <1. The default is \code{gamma}=0.05.}
  \item{gamma1}{A constant for the 1-\code{gamma} confidence level that satisfies \code{gamma1}+\code{gamma2}=\code{gamma}. This argument is optional.}
  \item{gamma2}{A constant for the 1-\code{gamma} confidence level that satisfies \code{gamma1}+\code{gamma2}=\code{gamma}. This argument is optional.}
  \item{conf.int}{An indicator whether confidence interval is in the output or not. The default is \code{conf.int}=TRUE}
}

\value{
\item{Sample_mean}{Sample mean of y}
\item{PT}{Pretest estimator for the normal mean based on \code{y}}
\item{Lower.pivotCI}{Lower limit of the confidence interval}
\item{Upper.pivotCI}{Upper limit of the confidence interval}
}

\references{
Taketomi N, Shih JH, Emura T.(2024-). Confidence interval for the univariate normal mean based on a pretest estimator.(under review)
}

\author{
Nanami Taketomi, Takeshi Emura
}

\examples{
mu=0
s=10
y=rnorm(20,mu,s)
uni.pt(y,s)

mu=1.5
s=10
y=rnorm(20,mu,s)
uni.pt(y,s,alpha=0.10)

}
