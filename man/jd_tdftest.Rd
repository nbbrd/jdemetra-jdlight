\name{jd_tdftest}
\alias{jd_tdftest}
\title{
F Test on trading days
}
\description{
Executes a F test on the coefficients of trading days
}
\usage{
jd_tdftest(s, ar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
    time series to be tested
}
  \item{ami}{
    ar. Use of s(t-1) in the regression. Otherwise, regression on the differences.

    default value=TRUE
}
}
\value{
returns the F test

}


\examples{

myseries<-jd_airline(120)
TESTmyseries <- jd_tdftest(myseries)

}
