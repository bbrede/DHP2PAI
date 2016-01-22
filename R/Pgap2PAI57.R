# author: Benjamin Brede
# date: 2016-01-21

Pgap2PAI57 <- function(Pgap) {
  # convert Pgap to PAI at the magic angle (57 degree)
  
  -log(Pgap) / 0.93
}
