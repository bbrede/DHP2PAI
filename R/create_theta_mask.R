# author: Benjamin Brede
# date: 2015-10-30


create_theta_mask <- function(template_raster, optical_center, projection_function, theta_range, ...) {
  # aim: create raster (tif) file with zenith angle/theta according to camera calibration
  # template_raster [raster]
  # optical_center [numeric{2}]
  # projection_function [function: pixel -> theta (in degree)]
  # theta_range [numeric{2}] (in degree)
  
  require(raster)
  
  # warn if 'real' geo-data rasters are read (with col/rows starting not with 0)
  ext <- extent(template_raster)
  if (ext@xmin != 0 | ext@ymin != 0 | !is.na(crs(template_raster)@projargs))
    warning('The template seems to be a geo-raster. Results may be wrong!')
  
  # raster that hold values which represent distance to optical_center (distance in pixel)
  dist2centre <- distanceFromPoints(subset(template_raster, 1), matrix(optical_center, ncol = 2))
  
  # theta [degree]
  theta <- calc(dist2centre, projection_function)
  
  # set all values NA that should not be included, otherwise set them 1
  reclassify(theta, c(-Inf, min(theta_range), NA, 
                      min(theta_range), max(theta_range), 1, 
                      max(theta_range), Inf, NA))
}


