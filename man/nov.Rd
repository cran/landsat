\name{nov}
\alias{nov}
\alias{nov1}
\alias{nov2}
\alias{nov3}
\alias{nov4}
\alias{nov5}
\alias{nov61}
\alias{nov62}
\alias{nov7}
\docType{data}
\title{
	Sample Landsat ETM+ data
}
\description{
	SpatialGridDataFrame containing a 300 x 300 pixel subset (1500 x 1500 m) of the Landsat ETM+ image for path 15, row 32, obtained on 25 November 2002. Each band, including both thermal bands, is contained in a separate file.
}
\usage{data(nov1)}
\format{
	Images are in SpatialGridDataFrame format. More information is available in the documentation for the sp package.
}
\source{
	Landsat images can be obtained from the United States Geological Survey at http://landsat.usgs.gov
}
\details{
Date: 2002-11-25 \cr
Satellite: Landsat ETM+ (7) \cr
Sun elevation: 26.2 \cr
Sun azimuth: 159.5 \cr
 \cr
 \cr
-------- \cr 

band	Grescale	Brescale \cr
1	0.77569		-6.20 \cr
2	0.79569		-6.40 \cr
3	0.61922		-5.00 \cr
4	0.63725		-5.10 \cr
5	0.12573		-1.00 \cr
7	0.04373		-0.35 \cr
}

\examples{
	data(nov3)
	image(nov3)
}
\keyword{datasets}

