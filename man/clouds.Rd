\name{clouds}
\alias{clouds}
\title{
	Create a cloud mask from Landsat bands 1 and 6.
}
\description{
	Uses Landsat band 1 and band 6 to identify clouds and create a cloud mask.
}
\usage{
clouds(band1, band6, level = 0.0014, buffer=5)
}
\arguments{
  \item{band1}{
	File name or image file (matrix, data frame, or SpatialGridDataFrame) for Landsat band 1.
}
  \item{band6}{
	File name or image file (matrix, data frame, or SpatialGridDataFrame) for Landsat band 6.
}
  \item{level}{
	Threshold level for cloud/noncloud decision. The default threshold is appropriate for reflectance and temperature values, and must be adjusted for use with DN.
}
  \item{buffer}{
	Pixel buffer size to expand around thresholded cloud areas.
}
}
\details{
	Clouds are reflective (high) in band 1 and cold (low) in band 6, so the ratio of the two bands is high over clouds. The ratio must be adjusted for data type, whether reflectance, radiance, or DN.
}
\value{
	Returns a cloud mask in the same format as band1. Clouds are 1; noncloud areas are NA. Cloud areas are expanded by buffer pixels to ensure that cloud edges are captured.
}
\references{
	This function is loosely based on:
	Martinuzzi, S., Gould, W.A., Ramos Gonzales, O.M. 2007. Creating Cloud-Free Landsat ETM+ Data Sets in Tropical Landscapes: Cloud and Cloud-Shadow Removal. USDA Forest Service General Technical Report IITF-GTR-32.
}
\author{
	Sarah Goslee
}
\examples{
data(july1)
data(july61)
july.cloud <- clouds(july1, july61)

par(mfrow=c(1,2))
image(july1)
image(july.cloud)

}
\keyword{ spatial }

