# author: Benjamin Brede
# date: 2016-01-21


source('R/create_theta_mask.R')
source('R/classify_kmeans.R')
source('R/calc_Pgap.R')
source('R/Pgap2PAI57.R')


setwd('E:/2015_Speulderbos/L1_DHP/')

library(raster)
library(snowfall)
library(lubridate)
library(tools)
library(ggplot2)

rm(list=ls())


template <- brick('DSC_template.tif')

# b <- brick('DSC_template.tif')
# idx <- sample(1:ncell(b), 1000)
# blue <- values(b[[3]])[idx]
# green <- values(b[[2]])[idx]
# red <- values(b[[1]])[idx]

# c(col, row)
optical_center <- c(2452, 1496)

proj_function <- function(r) {
  0.00000002 * r ^ 3 + -0.00003070 * r ^ 2 + 0.06741084 * r
}

mask <- create_theta_mask(template, optical_center, proj_function, c(50, 60))

# test_image <- brick('tifs/DSC_5487.tif')
# system.time(Pgap2PAI57(calc_Pgap(test_image, mask, classify_kmeans_forest)))



# possible parameters:
# - band (red, green, blue)
# - EV value (-1, -2) -> defined with files


tifs <- list.files(path = 'tifs/', pattern = 'DSC_[0-9]{4}.tif$', full.names = TRUE, recursive = TRUE)


sfInit(TRUE, 3)
# sfExport(list = c('derive_PAI', 'Pgap2PAI', 'mask'))
sfExportAll()
sfLibrary(raster)
sfLibrary(tools)


system.time(dhp_results <- do.call(rbind, sfClusterApplyLB(tifs, function(tif) {
  filename <- basename(file_path_sans_ext(tif))
  csv <- paste0('csv/', filename, '.csv')
  tryCatch({
    if (!file.exists(csv)) {
      pgap_blue <- calc_Pgap(brick(tif), mask, classify = classify_kmeans_forest, 3)
      pgap_red <- calc_Pgap(brick(tif), mask, classify = classify_kmeans_forest, 1)
      tab <- data.frame(Filename = filename, Pgap = as.numeric(c(pgap_blue, pgap_red)), Band = c('Blue', 'Red'))
      write.csv(tab, csv, row.names = FALSE)
      tab
    } else {
      read.csv(csv)
    }
  }, error = function(x) {
    data.frame(Filename = filename, Pgap = NA, Band = NA)
  })
})))

sfStop()

regexe <- function(pattern, text) regmatches(text, regexpr(pattern, text)) 


# fieldsheet compilation
fieldsheets_files <- list.files('E:/2015_Speulderbos/L0_DHP/fieldsheets/', 'csv$', full.names = TRUE, recursive = TRUE)
fieldsheets <- do.call(rbind, lapply(fieldsheets_files, function(sheet) {
  date <- strptime(regexe('2015-[0-9]{2}-[0-9]{2}', sheet), '%Y-%m-%d', 'UTC')
  plot <- regexe('[ABCDE]', basename(sheet))#head(tail(strsplit(sheet, '/')[[1]], 2), 1)
  cbind(read.csv(sheet), Date = date, Plot = plot)
}))
names(fieldsheets) <- c('Filename', 'Position', 'EV', 'Date', 'Plot')


df <- merge(fieldsheets, dhp_results)
df$PAI <- Pgap2PAI57(df$Pgap)

df$Position <- factor(df$Position, levels = sort(levels(df$Position)))
df$Plot <- factor(df$Plot, levels = sort(levels(df$Plot)))


sub <- subset(df, EV == -2 & Band == 'Blue')

ggplot(sub, aes(x = Date, y = PAI, color = Position)) +
  geom_point() + geom_line() + 
  facet_grid(Plot ~ .)
