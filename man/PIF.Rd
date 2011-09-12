\name{PIF}
\alias{PIF}
\title{
	Pseudo-Invariant Features
}
\description{
	Pseudo-invariant features identification for relative radiometric normalization.
}
\usage{
PIF(band3, band4, band7, level = 0.99)
}
\arguments{
  \item{band3}{
	Landsat band 3, as a filename to be imported, a matrix, data frame, or SpatialGridDataFrame.
}
  \item{band4}{
	Landsat band 4, as a filename to be imported, a matrix, data frame, or SpatialGridDataFrame.
}
  \item{band7}{
	Landsat band 7, as a filename to be imported, a matrix, data frame, or SpatialGridDataFrame.
}
  \item{level}{
	Threshold level for identifying PIFs. (0 < level < 1)
}
}
\details{
	Pseudo-invariant features (PIFs) are areas such as artificial structures that can reasonably be expected to have a constant reflectance over time, rather than varying seasonally as vegetation does. Differences in PIF reflectance between dates can be assumed to be due to varying atmospheric conditions.
}
\value{
	Returns a PIF mask in the same format as the input files, with 1 for pseudo-invariant features and 0 for background data.
}
\references{
	Schott, J. R.; Salvaggio, C. & Volchok, W. J. 1988. Radiometric scene normalization using pseudoinvariant features. Remote Sensing of Environment 26:1-16.
}
\author{
	Sarah Goslee
}

\seealso{
\code{\link{RCS}}
}
\examples{

	# identify pseudo-invariant feature
	data(july3)
	data(july4)
	data(july7)
	july.pif <- PIF(july3, july4, july7)

	# use PIFs to related nov to july Landsat data for band 3
	# properly, would also remove cloudy areas first
	data(nov3)
	# use major axis regression: error in both x and y
	nov.correction <- lmodel2:::lmodel2(july3@data[july.pif@data[,1] == 1, 1] ~ nov3@data[july.pif@data[,1] == 1, 1])$regression.results[2, 2:3]
	nov3.corrected <- nov3
	nov3.corrected@data[,1] <- nov3@data[,1] * nov.correction[2] + nov.correction[1]
}
\keyword{ spatial }
