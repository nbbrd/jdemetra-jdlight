\name{jd_seasonality}
\alias{jd_seasonality}
\title{
Seasonality Test
}
\description{
Executes a seasonality test
}
\usage{
jd_seasonality(s, test, differencing, mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
    time series to be tested
}
  \item{test}{
    specification of the seasonality test

    possible options are "QS", "FRIEDMAN", "KRUSKALWALLIS","PERIODOGRAM"
}
  \item{differencing}{
    default value is set to 1
}
  \item{mean}{
    default value= TRUE

}
}
\value{
returns the corresponding test

}


\examples{

myseries<-jd_airline(120)
TESTmyseries <- jd_seasonality(myseries,test= "FRIEDMAN"
)

}
