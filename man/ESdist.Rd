\name{ESdist}
\alias{ESdist}
\title{
	Earth-Sun distance for a given date.  
}
\description{
	Calculates the estimated Earth-Sun distance in Astronomical Units (AU) for a given date.
}
\usage{
ESdist(adate)
}
\arguments{
  \item{adate}{
    date in "YYYY-MM-DD" format
}
}
\value{
	Returns estimated Earth-Sun distance in AU.
}
\author{
	Sarah Goslee
}

\examples{
	ESdist("2010-08-30")
}
\keyword{ chron }
\keyword{ utilities }
