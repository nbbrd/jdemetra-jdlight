\name{jd_checklast}
\alias{jd_checklast}

\title{
Checking of the last observations (Terror-like)
}
\description{
jd_checklast is a straightforward implementation of Terror, a popular tool for detecting anomalies in the last observation(s). This function compares the out of sample forecasts of the series with the actual figures.It returns the difference in function of the standard error of the forecast.
}
\usage{
jd_checklast(s,last, method,absoluteErrors)
}
\arguments{
  \item{s}{
      series
}
  \item{last}{
      number of observations (default=1)
}
  \item{method}{
      tramo method used to compute the forecast (default= "TRfull")
}
 \item{absoluteErrors}{
      return result as absolute error (vs relative error; default= FALSE)
}
}
\value{
    returns a numeric vector
}

\examples{
data("retail")
myseries <- retail$AutomobileDealers
checkLastObservations <-jd_checklast(myseries,4)
checkLastObservations_abs <-jd_checklast(myseries,4,absoluteErrors = TRUE)
}
