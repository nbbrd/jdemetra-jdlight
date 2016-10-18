\name{jd_cholette}
\alias{jd_cholette}

\title{
Benchmarking, Cholette method
}
\description{
Balancing low frequency series to a higher frequency benchmark using the Cholette method
}
\usage{
jd_cholette(s,t,rho,lambda,conversion)
}

\arguments{
  \item{s}{
    series1
}
  \item{t}{
    series2
}
  \item{rho}{
    auto-regressive parameter (def=1)
}
  \item{lambda}{
    power of the weights applied to the series.
    0 for additive, 0.5 for proportional, 1 for multiplicative (def=1)
}

  \item{conversion}{
    "Sum", "Average", ["First","Last"] (def="Sum")
}
}
\value{
returns a benchmarked 'ts' time series object

}
\references{
Info on JDemetra+, usage and functions :

    \url{http://ec.europa.eu/eurostat/cros/content/download_en}

}

\examples{
myHighFreq<-jd_airline(120)
myHighFreq2<-jd_airline(120)
myLowFreq<-jd_aggregate(myHighFreq2,4)
myBenchmarkedChol<-jd_cholette(myHighFreq,myLowFreq,rho=1,lambda=1,conversion="Sum")
}
