\name{RCS}
\alias{RCS}
\title{
	Radiometric Control Sets
}
\description{
	The Radiometric Control Sets method of relative radiometric correction for Landsat data.
}
\usage{
RCS(data.tc, level = 0.01)
}
\arguments{
  \item{data.tc}{
	The output of tasscap().
}
  \item{level}{
	Threshold level to use (0 < level < 1).	
}
}
\details{
	Radiometric Control Sets (RCSs) are areas such as artificial structures and large bodies of water that can reasonably be expected to have a constant reflectance over time, rather than varying seasonally as vegetation does. Differences in RCS reflectance between dates can be assumed to be due to varying atmospheric conditions.
	Pixels with low greenness and either high or low brightness are identified.

}
\value{
	Returns an RCS mask file in the format of the original data (vector, matrix, data frame or SpatialGridDataFrame, as preseved by tasscap()) with 1 for RCS pixels and 0 for background.
}
\references{
	Hall, F.; Strebel, D.; Nickeson, J. & Goetz, S. 1991. Radiometric rectification: toward a common radiometric response among multidate, multisensor images. Remote Sensing of Environment 35:11-27.
}
\author{
	Sarah Goslee
}

\seealso{
	\code{\link{PIF}}, \code{\link{tasscap}}
}
\examples{

	# identify radiometric control set
	data(july1)
	data(july2)
	data(july3)
	data(july4)
	data(july5)
	data(july7)
	july.tc <- tasscap("july", 7)
	july.rcs <- RCS(july.tc)

	# use RCS to relate nov to july Landsat data for band 3
	# properly, would also remove cloudy areas first
	data(nov3)
	# use major axis regression: error in both x and y
	nov.correction <- lmodel2:::lmodel2(july3@data[july.rcs@data[,1] == 1, 1] ~ nov3@data[july.rcs@data[,1] == 1, 1])$regression.results[2, 2:3]
	nov3.corrected <- nov3
	nov3.corrected@data[,1] <- nov3@data[,1] * nov.correction[2] + nov.correction[1]

}
\keyword{ spatial }
