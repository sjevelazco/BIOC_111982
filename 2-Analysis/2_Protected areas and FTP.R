## %######################################################%##
#                                                          #
####   Process Argentinean protected areas and FTP      ####
#                                                          #
## %######################################################%##
# require packages
{
  require(dplyr)
  require(terra)
  require(ggplot2)
  require(tidyterra)
  require(readr)
}


## %######################################################%##
#                                                          #
####         Process Argentinean protected areas         ####
#                                                          #
## %######################################################%##

pa_prov <- terra::vect("./PAs/areas_protegidas_provinciales_poligonos.shp")
pa_prov$type <- "AP_provincial"

pa_res <- terra::vect("./PAs/reservas_mab.shp")
pa_res$type <- "Reserva_bio"

pa_apn <- terra::vect("./PAs/SiFAP_APN.shp")
pa_apn$type <- "APN"

pa_ram <- terra::vect("./PAs/sitios_ramsar.shp")
pa_ram$type <- "ramsar"
pa_ram <- pa_ram["type"]


arg_pa <- rbind(pa_prov, pa_res, pa_apn, pa_ram)
plot(arg_pa)

# Calculate area
arg_pa$km2 <- terra::expanse(arg_pa, unit = "km")
hist(arg_pa$km2)

# terra::writeVector(arg_pa, "Protected areas.gpkg")

## %######################################################%##
#                                                          #
####           Update protected area network            ####
####               with Baldi et al 2025                ####
#                                                          #
## %######################################################%##
library(terra)
library(sf)

# https://siga.proyungas.org.ar/recursos/
# Baldi, G., Aguilar, A.G., Cirignoli, S., Falabella, V., González Roglich, M.,
# Gómez Vinassa, M.L., Juliá, M.S., Názaro, G., Nori, J., Pacheco, S.,
# Pérez Cubero, E., Schauman, S.A., Schneider, C., Tomba, A.N.,
# Aragón, R., 2025. La red de áreas protegidas en la Argentina: Análisis de
# extensión, sesgos espaciales y desafíos para la conservación.
# Ecol Austral 232–250. https://doi.org/10.25260/EA.25.35.2.0.2520

ar <- terra::vect("Argentina.gpkg")
plot(ar)

# Data base 1
baldi_1 <- terra::vect("PAs/Baldi et al 2025/ProteccionPublica_filtered.gpkg")
# baldi_1 <- crop(baldi_1, ar)
table(is.valid(baldi_1))
baldi_1 <- terra::makeValid(baldi_1)
table(is.valid(baldi_1))
baldi_1 <- crop(baldi_1, ar)

plot(ar)
plot(baldi_1, col = "red", add = T)

# Filter by IUCN category I-IV
table(baldi_1$IUCN)
baldi_1 <- baldi_1[baldi_1$IUCN %in% c("I", "Ia", "Ib", "II", "III", "IV")]
plot(ar)
plot(baldi_1, col = "red", add = T)

baldi_1 <- baldi_1 %>% janitor::clean_names()
# terra::writeVector(baldi_1, "Protected areas.gpkg", overwrite=TRUE)

# Rasterize protected areas
pa <- terra::vect("Protected areas.gpkg")
ar_anth <- 1 - terra::rast("./Land use/Argentina.tif")
pa_r <- terra::rasterize(pa, ar_anth, cover = TRUE)
pa_r[is.na(pa_r)] <- 0
pa_r <- terra::mask(pa_r, ar_anth)
plot(pa_r)
terra::writeRaster(pa_r, "Protected areas.tif", overwrite = TRUE)
# pa_r_old <- "Protected areas.tif" %>% terra::rast()
# plot(pa_r+pa_r_old)

## %######################################################%##
#                                                          #
####                    Process OTBN                    ####
#                                                          #
## %######################################################%##
ar <- terra::rast("./Land use/Argentina.tif")

# PAs
pa <- terra::vect("Protected areas.gpkg")
pa$type <- "PA"
pa <- pa["type"]

# OTBN I
otbn <- "D:/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/OTBN_I.gpkg" |>
  terra::vect()
otbn$type <- "OTNB"
otbn <- otbn["type"]
otbn <- rbind(otbn, pa)
otbn_I <- terra::rasterize(otbn, ar, cover = TRUE)
otbn_I[is.na(otbn_I)] <- 0
otbn_I <- terra::mask(otbn_I, ar)
plot(otbn_I)
terra::writeRaster(otbn_I, "Protected areas+OTNB I.tif", overwrite = TRUE)

# OTBN I+II
otbn <- "D:/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/OTBN_I-II.gpkg" |>
  terra::vect()
otbn$type <- "OTNB"
otbn <- otbn["type"]
otbn <- rbind(otbn, pa)
table(otbn$type)
otbn_I <- terra::rasterize(otbn, ar, cover = TRUE)
otbn_I[is.na(otbn_I)] <- 0
otbn_I <- terra::mask(otbn_I, ar)
plot(otbn_I)
terra::writeRaster(otbn_I, "Protected areas+OTNB I-II.tif", overwrite = TRUE)


# only OTBN I
ar <- terra::rast("./Land use/Argentina.tif")
otbn <- "E:/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/OTBN_I.gpkg" |>
  terra::vect()
otbn$type <- "OTNB"
otbn <- otbn["type"]
otbn_I <- terra::rasterize(otbn, ar, cover = TRUE)
otbn_I[is.na(otbn_I)] <- 0
otbn_I <- terra::mask(otbn_I, ar)
plot(otbn_I)
terra::writeRaster(otbn_I, "Only+OTNB I.tif", overwrite = TRUE)

# only OTBN I+II
otbn <- "E:/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/OTBN_I-II.gpkg" |>
  terra::vect()
otbn$type <- "OTNB"
otbn <- otbn["type"]
otbn_I <- terra::rasterize(otbn, ar, cover = TRUE)
otbn_I[is.na(otbn_I)] <- 0
otbn_I <- terra::mask(otbn_I, ar)
plot(otbn_I)
terra::writeRaster(otbn_I, "Only+OTNB I-II.tif", overwrite = TRUE)

# Calculate area
v <- "/Volumes/Expansion/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/OTBN_I.gpkg" %>%
  terra::vect()
area_I <- v %>% terra::expanse(unit = "km") %>% sum() # 111982.9
area_I = 111982.9
v <- "/Volumes/Expansion/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/OTBN_I-II.gpkg" %>%
  terra::vect()
area_II <- v %>% terra::expanse(unit = "km") %>% sum()
area_II <- area_II - area_I

v <- "/Volumes/Expansion/Projects/66-ProyectoConicet/Spatial data/otbn_nacional/otbn_nacional.shp" %>%
  terra::vect()
area_total <- v %>% terra::expanse(unit = "km") %>% sum()
area_total
area_I / area_total
area_II / area_total
(area_I + area_II) / area_total
(area_I + area_II) - area_total
# > area_total
# [1] 537466.9
# > area_I/area_total
# [1] 0.2083531
# > area_II/area_total
# [1] 0.5996539
# > (area_I + area_II)/area_total
# [1] 0.808007
# > (area_I + area_II) - area_total
# [1] -103189.9
