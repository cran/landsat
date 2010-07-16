\name{dem}
\alias{dem}
\docType{data}
\title{
	Digital Elevation Model
}
\description{
	A 30-meter resolution elevation model in SpatialGridDataFrame format that matches the Landsat images nov and july.
}
\usage{data(dem)}
\details{
	Elevations are in meters.
}
\source{
	Digital Elevation Models for the United States are available from the United States Geologic Survey, http://www.usgs.gov
}
\examples{
	data(dem)
	dem.slopeasp <- slopeasp(dem)

	par(mfrow=c(1,3))
	image(dem)
	image(dem.slopeasp$slope)
	image(dem.slopeasp$aspect)
}
\keyword{datasets}
