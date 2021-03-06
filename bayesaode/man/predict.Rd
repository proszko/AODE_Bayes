% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zum_aode.R
\name{predict}
\alias{predict}
\title{Perform classification task for validation data using AODE Bayesian algorithm}
\usage{
predict(aode, data)
}
\arguments{
\item{aode}{aode object}

\item{data}{data used for model validation}
}
\value{
classification vector
}
\description{
Perform classification task for validation data using AODE Bayesian algorithm
}
\examples{
data <- data.frame ( a1=c(1,1,2), a2=c(1,2,3), a3=c(1,1,2),c=c(0,0,1))
f <- c ~ .
x <- aode_bayes(data,f)
res <- predict(x,data)
}
