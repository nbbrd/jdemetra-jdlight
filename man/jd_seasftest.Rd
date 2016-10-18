\name{jd_seasftest}
\alias{jd_seasftest}
\title{
F Test on seasonal dummies
}
\description{
Executes a F test on the coefficients of seasonal dummies
}
\usage{
jd_seasftest(s, ami)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{
    time series to be tested
}
  \item{ami}{
    automatic modelling or pre-defined model ((0 1 1)+mean).

    default value=FALSE
}
}
\value{
returns the F test

}


\examples{

myseries<-jd_airline(120)
TESTmyseries <- jd_seasftest(myseries)

}
