\name{jd_tramoseats}
\alias{jd_tramoseats}
\title{
Seasonal adjustment, TRAMO-SEATS
}
\description{
Executes a seasonal adjustment using the TRAMO-SEATS method
}
\usage{
jd_tramoseats(y,method)
}

\arguments{
  \item{y}{

     time series to be seasonally adjusted
}
  \item{method}{

      "RSA0", "RSA1","RSA2", "RSA3","RSA4", "RSA5", "RSAfull" (=default value)
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
\references{
Info on JDemetra+, usage and functions :

    \url{http://ec.europa.eu/eurostat/cros/content/download_en}
    }

\examples{

myseries <- jd_airline(144)
samyseries<- jd_tramoseats(myseries,"RSA5")

}
