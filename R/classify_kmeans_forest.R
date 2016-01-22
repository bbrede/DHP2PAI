# author: Benjamin Brede
# date: 2016-01-22


classify_kmeans_forest <- function(brightness_values) {
  # classify the brightness images into vegetation (low brightness) and sky (high brightness) for forest scenes
  # brightness values [numeric] > brightness values of image band
  
  # execute kmeans with max/min brightness values as initial cluster centers
  rkmeans <- kmeans(brightness_values, range(brightness_values))
  # select the center which has higher (mean) value > center for sky pixels
  rkmeans$size[which(rkmeans$centers == max(rkmeans$centers))]
}