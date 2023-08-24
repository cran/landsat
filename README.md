**landsat R package**

Processing of Landsat or other multispectral satellite imagery. Includes relative normalization, image-based radiometric correction, and topographic correction options.

This package was initially developed many years ago, before Landsat 8, and before extensive facilities were available for processing Landsat data elsewhere.

As of December 2019, I've added Landsat 8 OLI coefficients to the tasseled cap transform function _tasscap_, and to the _DOS_ function. 

Most of my remote sensing work has moved to other tools, but I do think adding raster package capability to this package might be useful. 


August 2023: Minor update to remove dependency on the retiring rgdal package.
