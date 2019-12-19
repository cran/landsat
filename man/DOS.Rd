\name{DOS}
\alias{DOS}
\title{
	Dark Object Subtraction
}
\description{
	Calculates calibration value for the Dark Object Subtraction (DOS) method of radiometric correction.
}
\usage{
DOS(sat = 5, scattering.coef = c(-4, -2, -1, -0.7, -0.5), SHV, SHV.band, gain,
	offset, Grescale, Brescale, sunelev, edist, Esun = c(198.3, 179.6, 153.6, 
	103.1, 22, 8.34), blackadjust = 0.01)
}
\arguments{
  \item{sat}{
	Landsat satellite platform: 5 for TM; 7 for ETM+; 8 for OLI.
}
  \item{scattering.coef}{
	Atmospheric scattering coefficient; defaults are from Chavez 1988.
}
  \item{SHV}{
	Starting Haze Value
}
  \item{SHV.band}{
	Band from which the Starting Haze Value was obtained.
}
  \item{gain}{
	Band-specific sensor gain. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{offset}{
	Band-specific sensor offset. Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{Grescale}{
	Band-specific sensor $G_{rescale}$ (gain). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{Brescale}{
	Band-specific sensor $B_{rescale}$ (bias). Require either gain and offset or Grescale and Brescale to convert DN to radiance.
}
  \item{sunelev}{
	Sun elevation in degrees
}
  \item{edist}{
	Earth-Sun distance in AU.
}
  \item{Esun}{
	Exo-atmospheric solar irradiance, as given by Chander et al. 2009 or others.
}
  \item{blackadjust}{
	By default, implements 1\% adjustment value to compensate for lack of perfectly dark pixels. 
}
}
\details{
	The Dark Object Subtraction method assumes that the darkest parts of an image (water, artificial structures) should be black if not for the effects of atmospheric scatter. Corrections to make it possible to use the black value from one band to correct the remaining bands.
}
\value{
\item{DNfinal.mean }{ The Dark Object Subtraction value for the complete set of scattering coefficients (Table X in Chavez 1989). }
\item{DNfinal.approx }{ The same table as DNfinal.mean, but using a numerical approximation across the band wavelength instead of the mean wavelength. }
}
\references{
	Chavez, Jr., P. S. 1988. An improved dark-object subtraction technique for atmospheric scattering correction of multispectral data. Remote Sensing of Environment 24:459-479.

	Chavez, Jr., P. S. 1989. Radiometric calibration of Landsat Thematic Mapper multispectral images. Photogrammetric Engineering and Remote Sensing 55: 1285-1294.
}
\author{
	Sarah Goslee
}
\seealso{
	\code{\link{radiocorr}}
}
\examples{

	data(july1)
	data(july3)

	# One approach to choosing a Starting Haze Value is to take
	# the lowest DN value with a frequency greater than some 
	# predetermined threshold, in this case 1000 pixels. 
	SHV <- table(july1@data[,1])
	SHV <- min(as.numeric(names(SHV)[SHV > 1000]))
	# this is used as Lhaze in the radiocorr function

	# G_rescale, B_rescale, sun elevation comes from metadata for the SHV band
	july.DOS <- DOS(sat=7, SHV=SHV, SHV.band=1, Grescale=0.77569, 
		Brescale=-6.20000, sunelev=61.4, 
		edist=ESdist("2002-07-20"))$DNfinal.mean

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
	july3.DOSrefl <- radiocorr(july3, Grescale=0.77569, Brescale=-6.20000, 
		sunelev=61.4, edist=ESdist("2002-07-20"), Esun=1533, 
		Lhaze=july.DOS[3], method="DOS")

}
\keyword{ spatial }
