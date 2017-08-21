\name{jd_cholette_multivar}
\alias{jd_cholette_multivar}

\title{
Benchmarking, Multi-variate Cholette algorithm
}
\description{
This function benchmarks a list of timeseries against a set of temporal and/or contemporaneous constraints. }
\usage{
jd_cholette_multivar(xlist,tcvector,ccvector,rho,lambda)
}

\arguments{
  \item{xlist}{
    list of named time series
}
  \item{tcvector}{
    character vector of temporal constraints
}
  \item{ccvector}{
    character vector of contemporaneous constraints
}

  \item{rho}{
    auto-regressive parameter (def=1)
}
  \item{lambda}{
    power of the weights applied to the series.
    0 for additive, 0.5 for proportional, 1 for multiplicative (def=1)
}

}
\value{
returns a list of benchmarked timeseries consistent with a given set of constraints

}
\references{
Info on JDemetra+, usage and functions :

    \url{http://ec.europa.eu/eurostat/cros/content/download_en}

}

\examples{

mylist<-list()
for (i in 1:8) {mylist[[paste0("s",i)]]<- assign(paste0("s",i), jd_airline(60))}

temporal <-c("s8=sum(s7)")
contemporaneous <- c("s3=s1+s2","s6=s4+s5")

mylist_benchmarked<- jd_cholette_multivar(mylist,temporal,contemporaneous)

}
