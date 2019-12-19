\name{minnaert}
\alias{minnaert}
\title{
	Whole-image and pixel-based Minnaert topographic correction of remote sensing data.
}
\description{
	Adds several modified Minnaert corrections to the capabilities of topocorr().
}
\usage{
minnaert(x, slope, aspect, sunelev, sunazimuth, na.value = NA, GRASS.aspect=FALSE, 
	IL.epsilon=0.000001, slopeclass = c(1, 5, 10, 15, 20, 25, 30, 45), coverclass)
}
\arguments{
  \item{x}{
	Image to be corrected, in matrix, data frame, or SpatialGridDataFrame format.
}
  \item{slope}{
	Slope image of same size and resolution as x.
}
  \item{aspect}{
	Aspect image of same size and resolution as x.
}
  \item{sunelev}{
	Sun elevation in degrees.
}
  \item{sunazimuth}{
	Sun azimuth in degrees.
}
  \item{na.value}{
	Value to use for missing data.
}
  \item{GRASS.aspect}{
	Whether aspect is measured according to GRASS defaults (counterclockwise from east) or is measured clockwise from north. If GRASS.aspect=TRUE, aspect is converted to clockwise from north before analysis.
}
  \item{IL.epsilon}{
	If IL == 0 (Illumination), some methods will give a topographically-corrected value of Inf due to division by zero. If desired, adding a small increment to zero values eliminates this.
}
  \item{slopeclass}{
  The classes into which the slope will be divided before calculating k separately for each class.
}
  \item{coverclass}{
  If present, TRUE/FALSE vector indicating which pixels to use when calculating k. This allows k to be determined separately for different cover classes.
}
}
\details{
	Calculates the Minnaert k coefficients for the whole image and for the individual slope classes.
}
\value{
   \item{allcoef }{
	The Minnaert k for the entire image. This is the value used in topocorr() (though the latter may have been truncated).
}
   \item{classcoef }{
	A data frame containing the slope class midpoints, number of pixels per class, and k for that class (for the desired cover class, if specified).
}
   \item{xout }{
	A topographically-corrected image in the same format as x.
}
   \item{xout }{
	A topographically-corrected image in the same format as x.
}


}
\references{
	Lu, D., Ge, H., He, S., Xu, A., Zhou, G., and Du, H. 2008. Pixel-based Minnaert correction method for reducing topographic effects on a Landsat 7 ETM+ image. Photogrammetric Engineering and Remote Sensing 74:1343-1350.
}
\author{
	Sarah Goslee
}
\seealso{
	\code{\link{topocorr}}
}
\examples{
	# require slope and aspect for topographic correction
	data(dem)
	dem.slopeasp <- slopeasp(dem)

	# use cosine method of topographic correction
	data(july4)
	july4.minpix <- minnaert(july4, dem.slopeasp$slope, dem.slopeasp$aspect, 
		sunelev=61.4, sunazimuth=125.8, slopeclass=c(1, 5, 10, 15, 50))
	july4.minpix$classcoef # all coefficients
}
\keyword{ spatial }

