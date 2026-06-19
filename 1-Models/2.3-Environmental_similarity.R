## %######################################################%##
#                                                          #
####  Estimate environmental distance for species with  ####
####                      <5 occurrences                ####
#                                                          #
## %######################################################%##
# devtools::install_github("sjevelazco/flexsdm")
{
  require(dplyr)
  require(terra)
  require(flexsdm)
  require(here)
  require(progress)
  require(raster)
  require(dismo)
  require(ggplot2)
}


## %######################################################%##
#                                                          #
####             Read occurrences databases             ####
#                                                          #
## %######################################################%##
occ <- qs::qread("occurrences_cleaned_final_FILTERED_menores5") %>% tibble()
occ$species %>%
  unique() %>%
  sort()

# Count number of presences
n_occ <- occ %>%
  pull(species) %>%
  table() %>%
  sort()

# List of rasters with environmental variable
env <- "./1-SDM/1_Inputs/3_Calibration_area/" %>%
  list.files(full.names = TRUE, patter = ".tif$")
names(env) <- basename(env) %>% gsub(".tif$", "", .)


# Create directory to save raster
dir.create("1-SDM/2_Outputs/1_Current/FinalModels")

## %######################################################%##
#                                                          #
####        Loop for calculating environmental         ####
####             similarity species with <5             ####
#                                                          #
## %######################################################%##
sp <- names(n_occ[n_occ > 1])

# i=145
for (i in 1:length(sp)) {
  message(paste("Estimating sp", i, sp[i]))
  pa <- occ %>% dplyr::filter(species == sp[i])

  # Extract environmental variables
  env_r <- terra::rast(env[sp[i]])
  env_names <- names(env_r)
  pa <- sdm_extract(pa, x = "x", y = "y", env_layer = env_r, filter_na = TRUE)

  #### domain - Gower distance ####
  prd <- dismo::domain(
    pa %>%
      dplyr::select({
        env_names
      }) %>%
      as.matrix()
  )

  if (nrow(pa) < 3) {
    buf <- terra::buffer(
      terra::vect(pa[c("x", "y")], geom = c("x", "y"), crs = crs(env_r)),
      width = 50000 # 50 km
    )
  } else {
    buf <-
      flexsdm::calib_area(
        pa,
        x = "x",
        y = "y",
        method = c('bmcp', width = 50000),
        crs = crs(env_r)
      ) # 50 km
  }

  env_masked <- env_r %>%
    terra::crop(., buf) %>%
    terra::mask(., buf)
  prd <- prd %>%
    raster::predict(., as.data.frame(env_masked))
  prd_2 <- env_masked[[1]]
  prd_2[!is.na(prd_2)] <- prd
  prd_2[!is.na(env_masked[[1]]) & is.na(prd_2)] <- 0
  prd_2 <- c(prd_2, prd_2)
  names(prd_2) <- c("layer", "layer")
  plot(prd_2[[1]])
  plot(buf, add = T)
  points(pa[c("x", "y")])
  sum_val <- terra::global(prd_2[[1]], sum, na.rm = TRUE)[1, 1]
  if (sum_val > 0) {
    terra::writeRaster(
      prd_2,
      here(
        "1-SDM/2_Outputs/1_Current/FinalModels",
        paste0(sp[i], ".tif")
      ),
      overwrite = TRUE
    )
  }
}
