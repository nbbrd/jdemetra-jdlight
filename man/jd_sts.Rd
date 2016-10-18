\name{jd_sts}
\alias{jd_sts}
\title{
Seasonal adjustment, STS
}
\description{
Executes a seasonal adjustment using a structural time series (STS) model
}
\usage{
jd_sts(y, preprocessing, seasmodel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
    time series to be seasonally adjusted
}
  \item{preprocessing}{
    possible options are "Tramo", "Regarima", "None"
}
  \item{seasmodel}{
    possible values are "Trigonometric" (default), "Crude", "HarrisonStevens",
    "Dummy", "Fixed", "Unused"

}
}
\value{
returns an 'mts' multiple time series object

  y as source time series

  t as trend-cycle component

  s as seasonal component

  i as irregular component

  sa as seasonally adjusted series
}


\examples{

myseries<-jd_airline(120)
saSTSmyseries <- jd_sts(myseries,preprocessing = "Tramo", seasmodel = "Trigonometric")

}
