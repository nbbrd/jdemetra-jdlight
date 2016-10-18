\name{jd_aggregate}
\alias{jd_aggregate}

\title{
Aggregation of time series
}
\description{
aggregates a time series to a new series with a smaller frequency
}
\usage{
jd_aggregate(s, newfreq, mode)
}

\arguments{
  \item{s}{
    source time series
}
  \item{newfreq}{
    new frequency (e.g. 1 for annual, 4 for quarterly)
    }
  \item{mode}{
    optional, default="Sum"
    }
}

\value{
    returns a 'ts' object
}
\examples{
myseries<-jd_airline(120)
myseriesAnnual<-jd_aggregate(myseries,1)
myseriesQuarterly<-jd_aggregate(myseries,4)
}
