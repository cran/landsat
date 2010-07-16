\name{ddate}
\alias{ddate}
\title{
Decimal Date
}
\description{
	Convert a vector containing year, month, day or individual year, month, day arguments into a decimal date in years. 
}
\usage{
	ddate(year, month, day)
}
\arguments{
  \item{year}{
	Either a numeric year OR a vector in the form of c(year, month, day). The latter option is so that ddate() can be conveniently used with apply().
}
  \item{month}{
	If year is a single value, must contain the number of the month.
}
  \item{day}{
	If year is a single value, must contain the number of the day.
}
}
\value{
	The decimal date in years.
}
\details{
	ddate() will accept a vector with the three date components so that it can be conveniently used with apply() on a data frame containing columns for year, month and day.
}
\author{
	Sarah Goslee
}
\examples{
	ddate(2001, 5, 15)
}
\keyword{ chron }
\keyword{ utilities }
