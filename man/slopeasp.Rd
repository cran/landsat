\name{slopeasp}
\alias{slopeasp}
\title{
	Calculate slope and aspect from elevation data.
}
\description{
	Uses gridded elevation data to calculate slope and aspect, by default using a 3x3 region.
}
\usage{
slopeasp(x, EWres, NSres, EWkernel, NSkernel, smoothing=1)
}
\arguments{
  \item{x}{
	gridded elevation data, either as a SpatialGridDataFrame, dataframe, or matrix.
}
  \item{EWres}{
	East-West grid resolution. May be omitted if x is a SpatialGridDataFrame.
}
  \item{NSres}{
	North-South grid resolution. May be omitted if x is a SpatialGridDataFrame.
}
  \item{EWkernel}{
	The kernel to use when calculating the East-West component of slope. If missing, a 3x3 kernel will be used.
}
  \item{NSkernel}{
	The kernel to use when calculating the North-South component of slope. If missing, a 3x3 kernel will be used.
}
  \item{smoothing}{
	A positive integer describing the additional smoothing to be applied, if any. smoothing=1 (default) means no smoothing will be used.
}
}
\details{
	By default, a 3x3 Sobel filter is used (as is standard in many GIS packages). A larger Sobel filter or a different filter will give varying results. This filter provides the third-order finite difference weighted by reciprocal of distance method proposed by Unwin (1981).
 
}
\value{
	\item{slope }{The slope of the DEM, in degrees}
	\item{aspect }{The aspect of the DEM, beginning with north and moving clockwise, and with aspect = 0 where slope = 0.}
}
\author{
	Sarah Goslee
}
\references{
       Unwin, D. 1981. Introductory Spatial Analysis. London: Methuen.
       Clarke, K.C.and Lee, S.J. 2007. Spatial resolution and algorithm choice as modifiers of downslope flow computed from Digital Elevation Models. Cartography and Geographic Information Science 34:215-230.

}

\seealso{
	\code{\link{movingwindow}}, \code{\link{topocorr}}
}
\examples{
	data(dem)
	dem.slopeasp <- slopeasp(dem)

	par(mfrow=c(1,3))
	image(dem)
	image(dem.slopeasp$slope)
	image(dem.slopeasp$aspect)
}
\keyword{ spatial }

