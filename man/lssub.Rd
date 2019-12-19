\name{lssub}
\alias{lssub}
\title{
	Subset a geotiff image.
}
\description{
	Uses GDAL tools to reproject (optional) and subset a geotiff given the center point and the desired size.
}
\usage{
lssub(filename, outname, centerx, centery, centerepsg, widthx, widthy)
}
\arguments{
  \item{filename}{
	Filename (and path) to a geotiff image.
}
  \item{outname}{
	Filename (and path) for subset image.
}
  \item{centerx}{
	x coordinate of new center point.
}
  \item{centery}{
	y coordinate of new center point.
}
  \item{centerepsg}{
	Projection of the center point coordinates as 5-digit EPSG code. If missing, assume that point and geotiff have the same projection.
}
  \item{widthx}{
	Desired width of subset image.
}
  \item{widthy}{
	Desired height of subset image.
}
}
\details{
	The new image will be a subset of size (widthx, widthy) with center point (centerx, centery), with the same pixel size. If the center point coordinates are in a different projection than the original image, they will be reprojected. 
}
\value{
	The new image is exported as a geotiff. Nothing is returned within R.
}
\author{
	Sarah Goslee
}
\note{
	Requires gdalinfo and gdaltransform to be available to the operating system. Only known to work on linux. This function was written to speed processing of multiple files for a specific project, and may be dropped in future releases of the landsat package. On my computer, lssub() is over an order of magnitude faster than reading the image into R, subsetting it, and writing out the result.
}
\examples{
	\dontrun{lssub("/data/gis/testimage.tif", "/data/gis/subimage.tif", centerx = 260485, 
	centery = 4527220, centerepsg = 26918, widthx = 50, widthy = 50)}
}
\keyword{ spatial }
\keyword{ utilities }
