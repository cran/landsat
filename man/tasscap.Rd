\name{tasscap}
\alias{tasscap}
\title{
	Tasseled Cap for Landsat data
}
\description{
	Tasseled cap transformation for Landsat TM, ETM+, or OLI.
}
\usage{
tasscap(basename, sat = 7)
}
\arguments{
  \item{basename}{
	Base filename (string) to which band numbers are appended, eg "july" for files named "july1", "july2", "july3", etc. Data should be at-sensor reflectance.
}
  \item{sat}{
	Landsat satellite platform: 5 for TM; 7 for ETM+; 8 for OLI.
}
}
\value{
If the input files are matrices or data frames, returns a data frame with three columns, one for each component. If the input files are SpatialGridDataFrames, returns a list with one element for each component. In either case three components are returned: Brightness, Greenness, Wetness.
}
\details{
For Landsat TM, the coefficients are to be applied to "reflectance factors", which appear to be the DN.
For ETM+ and OLI, the coefficients are for top-of-atmosphere reflectance.
For both TM and ETM+, the bands to be provided are 1, 2, 3, 4, 5, and 7. For OLI, the bands needed are 2 through 7.
Future updates will allow use of a raster stack rather than separate objects.
}
\references{
Original papers:

	Baig, M. H. A., Zhang, L., Shuai, T. & Tong, Q. 2014. Derivation of a tasselled cap transformation based on Landsat 8 at-satellite reflectance. Remote Sensing Letters 5:423-431.

	Crist, E. P. 1985. A TM tasseled cap equivalent transformation for reflectance factor data. Remote Sensing of Environment 17:301-306.

	Crist, E. & Kauth, R. 1986. The tasseled cap de-mystified. Photogrammetric Engineering and Remote Sensing 52:81-86. 

	Huang, C., Wylie, B., Yang, L., Homer, C. & Zylstra, G. 2002. Derivation of a tasseled cap transformation based on Landsat 7 at-satellite reflectance. International Journal of Remote Sensing 23:1741-1748.
}
\author{
	Sarah Goslee
}
\examples{
data(july1)
data(july2)
data(july3)
data(july4)
data(july5)
data(july7)
july.tc <- tasscap("july", 7)
}
\keyword{ spatial }

