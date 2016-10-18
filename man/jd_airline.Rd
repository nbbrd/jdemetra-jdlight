\name{jd_airline}
\alias{jd_airline}

\title{
Randomize a time series following an airline model
}
\description{
Returns a time series that follows an airline model with random normal innovations.
}
\usage{
jd_airline(len,freq,startdate,th,bth,fixed)
}
\arguments{
  \item{len}{
      number of observations
}
  \item{freq}{
      frequency (default=12)
}
  \item{startdate}{
      startdate= time of the first observation

                 format: c(YYYY,M)

                 default= c(2000,1)
}
  \item{th}{
      regular moving average parameter (default= -.6)
}
  \item{bth}{
      seasonal moving average parameter (default= -.8)
}
  \item{fixed}{
      (default=TRUE)
      }
      When "fixed" is set to false, the parameters of the airline model are randomly drawn around the given parameters.



}
\value{
    returns a 'ts' object
}

\examples{
mytimeseries <- jd_airline(120)
}
