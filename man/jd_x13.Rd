\name{jd_x13}
\alias{jd_x13}

\title{
Seasonal adjustment, X13
}
\description{
Executes a seasonal adjustment using the X13 method
}
\usage{
jd_x13(y, method)
}

\arguments{
  \item{y}{
time series to be seasonally adjusted
}
  \item{method}{
"X11", "RSA0",RSA1","RSA2c", "RSA3","RSA4c" (=default value), "RSA5c"
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
samyseries<- jd_x13(myseries,"RSA5c")

}
