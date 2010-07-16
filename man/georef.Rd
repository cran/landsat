\name{georef}
\alias{georef}
\title{
	Simple image-matching georeferencing function.
}
\description{
	Finds best fit between target image and tofix image by minimizing RMSE between the two. The tofix image is moved one pixel at a time horizontally or vertically. Simple automated georeferencing is adequate for some image-processing tasks.
}
\usage{
georef(target, tofix, maxdist = 1000, startx = 0, starty = 0)
}
\arguments{
  \item{target}{
	A georeferenced base image; can be matrix, dataframe or SpatialGridDataFrame.
}
  \item{tofix}{
	The image to be georeferenced; can be matrix, dataframe or SpatialGridDataFrame.
}
  \item{maxdist}{
	The greatest distance to move the tofix image. If this is exceeded, the function will stop. 
}
  \item{startx}{
	Shift the tofix image this many pixels in the x direction before beginning, to avoid local minimum.
}
  \item{starty}{
	Shift the tofix image this many pixels in the y direction before beginning, to avoid local minimum.
}
}
\details{
	This function offers a simplistic approach to georeferencing using an iterative algorithm that at each step moves the tofix image one pixel in the direction that produces the greatest reduction in RMSE. When RMSE no longer decreases or maxdist is reached, the algorithm stops, assuming that the tofix image now matches the reference target image. This algorithm can produce local minima. Results should always be checked visually.

	Note: this algorithm is only effective with images larger than the samples included with this package. The July and November images are already georectified, but this function will show them as needing considerable adjustment. Images of at least 1000x1000 pixels are necessary for adequate results.
}
\value{
	\item{shiftx }{The x-direction shift to get the best match (lowest RMSE).}
	\item{shifty }{The y-direction shift to get the best match (lowest RMSE).}
	\item{initrmse }{Initial RMSE between target and tofix images.}
	\item{currrmse }{Lowest RMSE, after shiftx and shifty pixel adjustments. Will be 9999 if maxdist is exceeded.}
}

\author{
	Sarah Goslee
}

\seealso{
	\code{\link{geoshift}}
}
\examples{
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
