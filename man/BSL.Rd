\name{BSL}
\alias{BSL}
\title{
	Bare Soil Line
}
\description{
	Finds Bare Soil Line (BSL) and maximum vegetation point.
}
\usage{
BSL(band3, band4, method = "quantile", ulimit = 0.99, llimit = 0.005)
}
\arguments{
  \item{band3}{
	File name or image file (matrix, data frame, or SpatialGridDataFrame) for Landsat band 3 DN (red).
}
  \item{band4}{
	File name or image file (matrix, data frame, or SpatialGridDataFrame) for Landsat band 4 DN (NIR).
}
  \item{method}{
	Either "quantile" or "minimum" -- describes way in which soil line is identified.
}
  \item{ulimit}{
	Upper limit for quantile of band ratios (ulimit < 1)
}
  \item{llimit}{
	Lower limit for quantile of band ratios (llimit > 0)
}
}
\details{
Finding the BSL requires identifying the lowest NIR values for each level of red. The quantile method takes the lowest set of points, those with a NIR/red ratio less than the llimit-th quantile. The minimum value method takes the lowest NIR value for each level of red. However they are found, these points with low NIR for their red values are used in a major axis regression to find the Bare Soil Line.
This function also identifies the full canopy point (maximum vegetation), by using the ulimit to identify the top points, with NIR/red ratio greater than the ulimit-th quantile, and with high NIR values. Red or NIR values of 255 (saturated sensor) are omitted when calculating the BSL.
}
\value{
	\item{BSL }{Regression coefficients for the Bare Soil Line}
	\item{top }{band 3 and band 4 values for the full canopy point}
}
\references{
Maas, S. J. & Rajan, N. 2010. Normalizing and converting image DC data using scatter plot matching. Remote Sensing 2:1644-1661.
}
\author{
	Sarah Goslee
}
\examples{
	data(nov3)
	data(nov4)
	nov.bsl <- BSL(nov3, nov4)
	plot(as.vector(as.matrix(nov3)), as.vector(as.matrix(nov4)))
	abline(nov.bsl$BSL, col="red")
	points(nov.bsl$top[1], nov.bsl$top[2], col="green", cex=2, pch=16)
}
\keyword{ spatial }

