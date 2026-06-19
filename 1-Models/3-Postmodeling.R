## %######################################################%##
#                                                          #
####               Models post-processing                ####
#                                                          #
## %######################################################%##
{
  require(terra)
  require(dplyr)
  require(flexsdm)
  require(here)
  require(ggplot2)
}

## %######################################################%##
#                                                          #
####               Correct overprediction               ####
#                                                          #
## %######################################################%##
bmcp <- function(records, x, y, buffer, cont_suit) {
  data_pl <- data.frame(records[, c(x, y)])
  data_pl <- data_pl[grDevices::chull(data_pl), ]
  data_pl <- data.frame(
    object = 1,
    part = 1,
    data_pl,
    hole = 0
  )
  data_pl <- terra::vect(as.matrix(data_pl), type = "polygons")
  terra::crs(data_pl) <- terra::crs(cont_suit)
  data_pl <- terra::buffer(data_pl, width = buffer)
  hull <- terra::rasterize(data_pl, cont_suit)
  hull[is.na(hull)] <- 0
  result <- cont_suit * hull
  return(result)
}

# Study area boundaries
# Traformar a raster
occ <- data.table::fread("species_records_final_unfiltered.gz") %>%
  as_tibble() %>%
  dplyr::select(x, y, db_id, species) # unfiltered occurrences!
names(occ)

# occ$species %>% unique %>% sort %>% grep("tif", ., value = T)
d <- "1-SDM/2_Outputs/1_Current/Algorithm/median" %>%
  list.files(pattern = ".tif", full.names = TRUE)
names(d) <- gsub(".tif$", "", basename(d))
occ <- occ %>% dplyr::filter(species %in% names(d))

sp <- names(d)

# dir.create("./1-SDM/2_Outputs/1_Current/FinalModels")

for (i in 1:length(sp)) {
  message(paste("Estimating sp", i, sp[i]))
  p <- occ %>%
    dplyr::filter(species == sp[i])
  r <- terra::rast(d[sp[i]])
  r <- bmcp(p, x = "x", y = "y", buffer = 100000, cont_suit = r)
  # r[r<0] <- 0
  # plot(r[[2]])
  # plot(r)

  sum_val <- terra::global(r[[1]], sum, na.rm = TRUE)[1, 1]
  if (sum_val > 0) {
    terra::writeRaster(
      r,
      here(
        "1-SDM/2_Outputs/3_FinalModels/1_Current",
        paste0(sp[i], ".tif")
      ),
      overwrite = TRUE
    )
  } else {
    message(paste("Estimating sp", i, sp[i]), "without suitability values")
  }
}

## %######################################################%##
#                                                          #
####     Save distribution figure for each species      ####
#                                                          #
## %######################################################%##

l <- list.files(
  "1-SDM/2_Outputs/1_Current/FinalModels",
  pattern = ".tif$",
  full.names = TRUE
)
names(l) <- basename(l) %>% gsub(".tif$", "", .)
dir.create("./1-SDM/2_Outputs/3_FinalModels/FiguresCurrent")
i <- 20
for (i in 1:length(l)) {
  r <- terra::rast(l[i])
  sp <- names(l[i])
  p <- occ %>%
    dplyr::filter(species == sp)

  png(
    file = file.path(
      "1-SDM/2_Outputs/3_FinalModels/FiguresCurrent",
      paste0(sp, ".png")
    ),
    width = 23,
    height = 20,
    units = "cm",
    res = 100
  )
  plot(r[[2]], main = sp, col = pals::viridis(20))
  points(p[, c("x", "y")], pch = 19, col = "red")
  dev.off()
}


## %######################################################%##
#                                                          #
####     Restrict future projection within current      ####
####      distribution area and within Argentina       ####
#                                                          #
## %######################################################%##
# Argentina boundaries
study_a <- vect(file.path(
  getwd() %>% dirname() %>% dirname(),
  "Spatial data/Argentina.gpkg"
))

# list of current distribution
crnt <- list.files(
  "./1-SDM/2_Outputs/3_FinalModels/1_Current/",
  pattern = ".tif$",
  full.names = TRUE
)
names(crnt) <- basename(crnt) %>% gsub(".tif$", "", .)

# list of future projections
dirs <- list.dirs("./1-SDM/2_Outputs/2_Projection") %>%
  grep("median", ., value = T) %>%
  lapply(., list.files, pattern = ".tif$", full.names = T) %>%
  unlist()
dirs <- data.frame(
  species = basename(dirs) %>% gsub(".tif$", "", .),
  dirs = dirs
)

# Create directories
dirs$dirs %>%
  dirname() %>%
  dirname() %>%
  dirname() %>%
  unique() %>%
  gsub("2_Projection", "3_FinalModels", .) %>%
  sapply(., dir.create)

sps <- names(crnt)
for (i in 1:length(sps)) {
  message("sp ", i)
  r <- terra::rast(crnt[sps[i]])
  r <- r %>%
    crop(., study_a) %>%
    mask(., study_a)
  plot(r)

  # future projection
  f <- dirs$dirs[dirs$species == sps[i]]
  for (ii in 1:length(f)) {
    fr <- terra::rast(f[ii]) %>%
      terra::crop(., r) %>%
      terra::mask(., r)

    if (global(fr[[2]], sum, na.rm = T)[1, 1] > 0) {
      terra::writeRaster(
        fr,
        f[ii] %>%
          gsub("2_Projection", "3_FinalModels", .) %>%
          gsub("Algorithm/median/", "", .),
        overwrite = TRUE
      )
    }
  }
}


## %######################################################%##
#                                                          #
####               Richness maps based on               ####
####       continuous suitability above threshold       ####
#                                                          #
## %######################################################%##
d <- list.files(
  "./1-SDM/2_Outputs/3_FinalModels/1_Current",
  pattern = ".tif$",
  full.names = TRUE
)
names(d) <- basename(d) %>% gsub(".tif$", "", .)


study_a <- vect(file.path(
  getwd() %>% dirname() %>% dirname(),
  "Spatial data/Argentina.gpkg"
))

r <- rast("./Variables/Allvariables/1981-2010/elevation.tif")
r <- r %>%
  crop(., study_a) %>%
  mask(., study_a)
r[!is.na(r)] <- 0
plot(r)

# terra::writeRaster(r, "Argentina.tif", overwrite = TRUE)

i <- 1
for (i in 1:length(d)) {
  message("sp ", i)
  r0 <- terra::rast(d[i])[[2]]
  r0 <- r0 %>%
    crop(study_a) %>%
    mask(study_a)
  if (ext(r0) != ext(r)) {
    r0 <- (resample(r0, r))
    r0[!is.na(r) & is.na(r0)] <- 0
  }
  r <- r + r0
}
plot(r)
png(
  file = file.path(
    "1-SDM/2_Outputs/3_FinalModels/FiguresCurrent/00_Richness.png"
  ),
  width = 19,
  height = 25,
  units = "cm",
  res = 200
)
plot(r, col = pals::viridis(20), main = "Species richness")
dev.off()

plot(r > 5)
plot(r > 20)
plot(r > 80)
hist(r)
# terra::writeRaster(r, "1-SDM/2_Outputs/1_Current/richnes_map.tif", overwrite = TRUE)
