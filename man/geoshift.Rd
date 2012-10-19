\name{geoshift}
\alias{geoshift}
\title{
	Shift and pad an image
}
\description{
	Shifts an image vertically or horizontally and adds a padded border.
}
\usage{
geoshift(mat, padx, pady, shiftx, shifty, nodata = NA)
}
\arguments{
  \item{mat}{
	A matrix, data frame or SpatialGridDataFrame
}
  \item{padx}{
	Number of pixels to add as padding in the x direction on each side of the image (along the x-axis). Should be larger than the number of pixels to shift to avoid data loss.
}
  \item{pady}{
	Number of pixels to add as padding in the y direction on each side of the image (along the y-axis). Should be larger than the number of pixels to shift to avoid data loss.
}
  \item{shiftx}{
	Number of pixels to shift (positive or negative) in the x direction (along the x-axis).
}
  \item{shifty}{
	Number of pixels to shift (positive or negative) in the y direction (along the y-axis).

}
  \item{nodata}{
	Value to use for missing data.
}
}
\details{
	This function can be used to correct spatially-referenced images that are off by a few pixels in the x or y directions. It does not warp an image, only slide it. Adding padding to the outside edge makes it possible to match several images even if they are not stored with georeferecing information. geoshift() can be used in conjunction with georef() to automatically match up geospatial images. Note: directions are relative to the image as displayed by the image() command, and not the underlying matrix representation.
}
\value{
Returns data in the same format as the function was given: matrix, data frame, or SpatialGridDataFrame.
}
\author{
	Sarah Goslee
}
\seealso{
\code{\link{georef}}
}
\examples{

testmat <- matrix(1:9, 3, 3)
geoshift(testmat, 5, 10, 0, 0)
geoshift(testmat, 5, 10, 2, 2)

# to use for georeferencing
data(nov3)
data(july3)
july.shift <- georef(nov3, july3, maxdist=50) # match july to november
july3.corr <- geoshift(july3, padx=50, pady=50, july.shift$shiftx, july.shift$shifty)

# only need to run georef once for a particular date
# use the same correction for all bands
data(july4)
july4.corr <- geoshift(july4, padx=50, pady=50, july.shift$shiftx, july.shift$shifty)


}
\keyword{ spatial }
\keyword{ utilities }
