##%######################################################%##
#                                                          #
####        Process land use data from MapBiomas        ####
#                                                          #
##%######################################################%##

# Process land use data from MapBiomas for the years 1985 and 2024,
# focusing on natural wooded vegetation, shrublands, and forest vegetation.
# The code reads in the land use data, identifies the relevant classes
# for each vegetation type, creates binary rasters indicating the presence
# of these vegetation types, and resamples them to a 1km resolution for
# further analysis.

# Require packages
{
  require(dplyr)
  require(terra)
}

lnduse <- readxl::read_excel("./Land use/landuse classes.xlsx", sheet = 1)
ar <- terra::rast("./Land use/Argentina.tif")

# Natural wooded vegetation
# 2024
lnd <- terra::rast("./Land use/argentina_coverage_2024.tif")
lnd <- lnd %>% crop(., ar)
terra::NAflag(lnd) <- 0
lnd2 <- terra::ifel(
  lnd %in% lnduse$pixel_id[lnduse$wooded_vegetation == 1],
  1,
  0
)
lnd2 <- mask(lnd2, lnd)
plot(lnd2)
terra::writeRaster(
  lnd2,
  "./Land use/woodded_vegetation_2024_bin.tif",
  overwrite = TRUE
)

# Natural shrublands vegetation
# 2024
lnd <- terra::rast("./Land use/argentina_coverage_2024.tif")
lnd <- lnd %>% crop(., ar)
terra::NAflag(lnd) <- 0
lnduse[lnduse$wooded_vegetation == 1, ]
lnd2 <- terra::ifel(lnd %in% c(66, 77), 1, 0)
lnd2 <- mask(lnd2, lnd)
lnd3 <- resample(
  lnd2,
  terra::rast("./Land use/Argentina.tif"),
  method = "average"
)
plot(lnd3)
# terra::writeRaster(lnd3, "./Land use/woodded_shrublands_2024_habitat.tif",
#   overwrite = TRUE
# )

# Natural forest vegetation
# 2024
lnd <- terra::rast("./Land use/argentina_coverage_2024.tif")
lnd <- lnd %>% crop(., ar)
terra::NAflag(lnd) <- 0
lnduse[lnduse$wooded_vegetation == 1, ]
lnd2 <- terra::ifel(lnd %in% c(3, 4, 6), 1, 0)
lnd2 <- mask(lnd2, lnd)

plot(lnd)
plot(lnd2)
# resample
lnd3 <- resample(
  lnd2,
  terra::rast("./Land use/Argentina.tif"),
  method = "average"
)
plot(lnd3)
# terra::writeRaster(lnd3, "./Land use/woodded_forest_2024_habitat.tif",
#   overwrite = TRUE
# )

# Natural wooded vegetation
# 1985
lnd <- terra::rast("./Land use/argentina_coverage_1985.tif")
lnd <- lnd %>% crop(., ar)
terra::NAflag(lnd) <- 0
lnd2 <- terra::ifel(
  lnd %in% lnduse$pixel_id[lnduse$wooded_vegetation == 1],
  1,
  0
)
lnd2 <- mask(lnd2, lnd)
plot(lnd2)
terra::writeRaster(
  lnd2,
  "./Land use/woodded_vegetation_1985_bin.tif",
  overwrite = TRUE
)

# Resample to 1km resolution
# 1km resolution
ar <- terra::rast("./Land use/Argentina.tif")

lnd <- terra::rast("./Land use/woodded_vegetation_2024_bin.tif")
lnd2 <- resample(lnd, ar, method = "average")
plot(lnd2)
terra::writeRaster(
  lnd2,
  "./Land use/woodded_vegetation_2024_habitat.tif",
  overwrite = TRUE
)

lnd <- terra::rast("./Land use/woodded_vegetation_1985_bin.tif")
lnd2 <- resample(lnd, ar, method = "average")
plot(lnd2)
terra::writeRaster(
  lnd2,
  "./Land use/woodded_vegetation_1985_habitat.tif",
  overwrite = TRUE
)
