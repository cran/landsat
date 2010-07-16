\name{radiocorr}
\alias{radiocorr}
\title{
	Radiometric correction of Landsat data
}
\description{
	Implements several different methods for absolute radiometric correction of satellite data.
}
\usage{
radiocorr(x, gain, offset, Grescale, Brescale, sunelev, satzenith = 0, edist, Esun, Lhaze, method = "apparentreflectance")
}
\arguments{
  \item{x}{
	Image to be corrected, in matrix, data frame, or SpatialGridDataFrame format.
}
  \item{gain}{
	Band-specific sensor gain. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{offset}{
	Band-specific sensor offset. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{Grescale}{
	Band-specific sensor Grescale (gain). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{Brescale}{
	Band-specific sensor Brescale (bias). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{sunelev}{
	Sun elevation in degrees
}
  \item{satzenith}{
	Satellite sensor zenith angle (0 for Landsat)
}
  \item{edist}{
	Earth-Sun distance in AU.
}
  \item{Esun}{
	Exo-atmospheric solar irradiance, as given by Chandler et al. 2009 or others.
}
  \item{Lhaze}{
	Haze value, such as SHV from DOS() function. Not needed for apparent reflectance.
}
  \item{method}{
	Radiometric correction method to be used. There are currently four methods available: "apparentreflectance", "DOS" (Chavez 1989), "COSTZ"  (Chavez 1996), "DOS4" (SWS+2001).  
}
}
\details{
Uses one of four image-based radiometric correction methods to adjust a satellite image to compensate for atmospheric conditions.	
}
\value{
	Returns a radiometrically-corrected image in the same format as x.
}
\references{
Chavez, Jr., P. S. 1989. Radiometric calibration of Landsat Thematic Mapper multispectral images. Photogrammetric Engineering and Remote Sensing 55:1285-1294.

Chavez, Jr., P. S. 1996. Image-based atmospheric corrections revisited and improved. Photogrammetric Engineering and Remote Sensing 62:1025-1036.

Song, C.; Woodcock, C. E.; Seto, K. C.; Lenney, M. P. & Macomber, S. A. 2001. Classification and change detection using Landsat TM data: when and how to correct atmospheric effects? Remote Sensing of Environment 75:230-244.
}
\author{
	Sarah Goslee
}
\seealso{
	\code{\link{DOS}}
}
\examples{

	data(july1)
	data(july3)

	# One approach to choosing a Starting Haze Value is to take the lowest DN value
	# with a frequency greater than some predetermined threshold, in this case 1000 pixels. 
	SHV <- table(july1@data[,1])
	SHV <- min(as.numeric(names(SHV)[SHV > 1000]))
	# this is used as Lhaze in the radiocorr function

	# Grescale, Brescale, sun elevation comes from metadata for the SHV band
	july.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale=0.77569, Brescale=-6.20000, sunelev=61.4, edist=ESdist("2002-07-20"))$DNfinal.mean

	# DOS() returns results for the complete set of scattering coefficients
	# need to choose the appropriate one based on general atmospheric conditions
		###	-4.0: Very Clear	SHV <= 55
		###	-2.0: Clear		SHV 56-75
		### 	-1.0: Moderate		SHV 76-95
		###	-0.7: Hazy		SHV 96-115
		###	-0.5: Very Hazy		SHV >115
	# for july, SHV == 70, so use -2.0: Clear
	july.DOS <- july.DOS[ , 2]

	# Use DOS value as Lhaze in radiocorr() for DOS correction to reflectance
	july3.DOSrefl <- radiocorr(july3, Grescale=0.77569, Brescale=-6.20000, sunelev=61.4, edist=ESdist("2002-07-20"), Esun=1533, Lhaze=july.DOS[3], method="DOS")


}
\keyword{ spatial }

