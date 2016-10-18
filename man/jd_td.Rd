\name{jd_td}
\alias{jd_td}

\title{
Temporal disaggregation
}
\description{
Temporal disaggregation of a time series.
This function is largely inspired by the function 'td' of the
package "tempdisagg'. However, it uses a completely different
implementation (based on a Kalman smoother)
}
\usage{
jd_td(formula, model = "Ar1", conversion = "Sum",
      zeroinit = FALSE, truncated.rho = 0, fixed.rho = -1, to = 4)
}
\arguments{
  \item{formula}{
  regression model (usual R-formulae)
}
  \item{model}{
  model of the residuals
}
  \item{conversion}{
  aggregation mode
}
  \item{zeroinit}{
   0-initialization of the residuals (FALSE by default)
}
  \item{truncated.rho}{
   limit for the automatic search of the auto-regressive parameter for the residuals (optional)
}
  \item{fixed.rho}{
   value of the auto-regressive parameter for the residuals (optional)
}
  \item{to}{
   final item
}
}
\value{
 returns a disaggregated time series
}

\examples{
a<-jd_airline(120)
}
