\name{july}
\alias{july}
\alias{july1}
\alias{july2}
\alias{july3}
\alias{july4}
\alias{july5}
\alias{july61}
\alias{july62}
\alias{july7}
\docType{data}
\title{
	Sample Landsat ETM+ data
}
\description{
	SpatialGridDataFrame containing a 300 x 300 pixel subset (1500 x 1500 m) of the Landsat ETM+ image for path 15, row 32, obtained on 20 July 2002. Each band, including both thermal bands, is contained in a separate file.
}
\usage{data(july1)}
\format{
	Images are in SpatialGridDataFrame format. More information is available in the documentation for the sp package.
}
\source{
	Landsat images can be obtained from the United States Geological Survey at http://landsat.usgs.gov
}
\details{
Date: 2002-07-20 \cr
Satellite: Landsat ETM+ (7) \cr
Sun elevation: 61.4 \cr
Sun azimuth: 125.8 \cr
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
\references{
	The Landsat Data Users Handbook is available from the United States Geological Survey at http://landsathandbook.gsfc.nasa.gov/handbook.html
}
\examples{
	data(july3)
	image(july3)
}
\keyword{datasets}

