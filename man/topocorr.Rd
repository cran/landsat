\name{topocorr}
\alias{topocorr}
\title{
	Topographic correction of remote sensing data.
}
\description{
	Implements several different methods for topographic correction of remote sensing data.
}
\usage{
topocorr(x, slope, aspect, sunelev, sunazimuth, method = "cosine", na.value = NA, GRASS.aspect=FALSE, IL.epsilon=0.000001)
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
  \item{method}{
	Topographic correction method to be used. There are currently seven methods available: "cosine", "improvedcosine", "minnaert", "ccorrection" (first four from Riano et al. 2003), "minslope" (Minnaert with slope correction, also from Riano et al. 2003), "gamma" (from Richter et al. 2009), "SCS" (Gu and Gillespie 1998, Gao and Zhang 2009).
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
}
\details{
	Uses one of the available topographic correction methods to compensate for the effects of slope and aspect on reflectance from the land surface.
}
\value{
	Returns a topographically-corrected image in the same format as x.
}
\references{
	Gao, Y. & Zhang, W. 2009. LULC classification and topographic correction of Landsat-7 ETM+ imagery in the Yangjia River Watershed: the influence of DEM resolution. Sensors 9:1980-1995.

	Gu, D. & Gillespie, A. 1998. Topographic normalization of Landsat TM images of forest based on subpixel sun-canopy-sensor geometry. Remote Sensing of Environment 64:166-175.

	Riano, D., Chuvieco, E., Salas, J. & Aguado, I. 2003. Assessment of different topographic corrections in Landsat-TM data for mapping vegetation types. IEEE Transactions on Geoscience and Remote Sensing 41:1056-1061.

	Richter, R., Kellenberger, T. & Kaufmann, H. 2009. Comparison of topographic correction methods. Remote Sensing 1:184-196.
}
\author{
	Sarah Goslee
}
\seealso{
	\code{\link{slopeasp}}
}
\examples{
	# require slope and aspect for topographic correction
	data(dem)
	dem.slopeasp <- slopeasp(dem)

	# use cosine method of topographic correction
	data(july3)
	july3.topo <- topocorr(july3, dem.slopeasp$slope, dem.slopeasp$aspect, sunelev=61.4, sunazimuth=125.8)

}
\keyword{ spatial }

