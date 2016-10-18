\name{jd_denton}
\alias{jd_denton}

\title{
Benchmarking, Denton method
}
\description{
Balancing low frequency series to a higher frequency benchmark using the Denton method
}
\usage{
jd_denton(s,t,mul,modified,d,conversion)
}
\arguments{
  \item{s}{
    series1
}
  \item{t}{
    series2
}
  \item{mul}{
    multiplicative (=TRUE) or additive benchmarking
}
  \item{modified}{
    Use the modified Denton procedure, which affects the initial values. Should usually be set to TRUE
}
  \item{d}{
    Power of the differences used in the objective function
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
myBenchmarked<-jd_denton(myHighFreq,myLowFreq, mul=TRUE,modified=TRUE,d=1,conversion="Sum")

}
