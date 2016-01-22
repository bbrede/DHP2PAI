# author: Benjamin Brede
# date: 2016-01-22


calc_Pgap <- function(image, theta_mask, classify, band=3) {
  # calc gap fraction from image with mask (contains 1 for all areas that are part of the angle under consideration)
  # image [RasterBrick] > brightness values
  # theta_mask [RasterLayer]
  # classify [function] brightness values -> # of pixels identified as vegetation
  
  if (!all((dim(theta_mask) == dim(image))[1:2]))
    stop('Template and image do not have the same resolution/dimensions!')
  
  # extract values at the desired angle with help of mask
  brightness_values <- na.omit(values(image[[band]] * theta_mask))
  
  # number of pixels within the mask
  mask_n <- sum(values(theta_mask), na.rm = TRUE)
  
  # devide by mask_n > proportion of non-vegetation pixels to all pixels within the mask
  classify(brightness_values) / mask_n
}