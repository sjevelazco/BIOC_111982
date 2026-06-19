## %######################################################%##
#                                                          #
####          Data analysis Argentinean trees           ####
#                                                          #
## %######################################################%##
# require packages
{
  require(dplyr)
  require(terra)
  require(ggplot2)
  require(tidyterra)
  require(readr)
  require(ggnewscale)
  require(ggspatial)
  require(patchwork)
  require(biscale)
  require(scales)
  require(magrittr)
  require(cowplot)
  require(patchwork)
}

fig_save <- getwd() |>
  dirname() |>
  file.path("2-Manuscript/Figures_2026")


## %######################################################%##
#                                                          #
####        Table of listed and modeled species         ####
#                                                          #
## %######################################################%##
# df <- readxl::read_excel("E:/Projects/66-ProyectoConicet/1_Arboles_Argentina/108_Models_trees_current/2-Manuscript/Figures_2026/Tables.xlsx", sheet = 1)
df <- readxl::read_excel(
  "D:/Projects/66-ProyectoConicet/1_Arboles_Argentina/108_Models_trees_current/2-Manuscript/Figures_2026/Tables.xlsx",
  sheet = 1
)
df$Species <- stringr::str_squish(df$Species)

df$Family %>%
  unique() %>%
  length()
df$Species %>%
  unique() %>%
  length()
df$Species %>%
  unique() %>%
  sort()
df$Modeled <- 0
table(df$Modeled)


"00_performance_all_models.txt" %>%
  readr::read_tsv() %>%
  dplyr::filter(model == "median")
"00_performance_all_models.txt" %>%
  readr::read_tsv() %>%
  dplyr::filter(model == "median") %>%
  dplyr::filter(round(SORENSEN_mean, 1) >= 0.7)

# Check ranges
asdf <- list.files(
  "Models/1-SDM/2_Outputs/3_FinalModels/Argentina_1km",
  full.names = TRUE
)
names(asdf) <- basename(asdf) %>% gsub(".tif$", "", .)
sort(names(asdf))
for (i in 1:length(asdf)) {
  # message(paste(i, "in",  i/length(asdf)))
  r <- terra::rast(asdf[i])[[2]] > 0
  df$Modeled[df$Species == names(asdf[i])] <- global(r, sum, na.rm = TRUE)[1, 1]
}

# Remove some species not more accepted in Flora of Argentina
df <- df[!df$Species == "Sebastiania brasiliensis", ]
df <- df[!df$Species == "Annona nutans", ]

df |>
  dplyr::filter(Modeled == 0) |>
  dplyr::arrange(Species)

df <- df %>% dplyr::arrange(Family, Species)

df <- df %>%
  mutate(Modeled2 = ifelse(Modeled > 0, "*", NA))
table(df$Modeled > 0)


families <- df %>%
  dplyr::filter(Modeled > 5) %>%
  pull(Family) %>%
  table() %>%
  sort()
sum(families == 1)

# readr::write_tsv(df %>% dplyr::select(-Modeled), file.path(fig_save, "0_modeled species.txt"), na = "")

## %######################################################%##
#                                                          #
####     Compare vegetation type and FTP I and II       ####
#                                                          #
## %######################################################%##
{
  require(dplyr)
  require(terra)
  require(ggplot2)
  require(biscale)
  require(cowplot)
  require(tidyterra)
}

# Load raster
ar <- terra::vect("Spatial data/Argentina.gpkg")
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions
pa_I <- terra::rast("Only+OTNB I.tif") |> mask(ar_for)
pa_I_II <- terra::rast("Only+OTNB I-II.tif") |> mask(ar_for)

ar_shrub <- terra::rast("./Land use/woodded_shrublands_2024_habitat.tif") |>
  mask(ar_for)
ar_forest <- terra::rast("./Land use/woodded_forest_2024_habitat.tif") |>
  mask(ar_for)

#### Comparison Shrubland ####
rr <- c(ar_shrub, pa_I, pa_I_II)
names(rr) <- c("Shrubland", "FTP-I", "FTP-I-II")
rr <- flexsdm::homogenize_na(rr)
plot(rr > 0.5)
rr <- (rr > 0.5) %>% as.numeric()
rr[[1]] <- rr[[1]] * 2
plot(rr)

rr <- c(
  sum(rr[[c("Shrubland", "FTP-I")]]),
  sum(rr[[c("Shrubland", "FTP-I-II")]])
)
rr[rr == 0] <- NA
rr <- as.factor(rr)
levels(rr[[1]]) <- data.frame(
  ID = c(1, 2, 3),
  cover = c("FTP", "Shrubland", "FTP + Shrubland")
)
levels(rr[[2]]) <- data.frame(
  ID = c(1, 2, 3),
  cover = c("FTP", "Shrubland", "FTP + Shrubland")
)
names(rr) <- c("Shrubland & FTP-I", "Shrubland & FTP-I-II")

A <- ggplot(ar) +
  geom_spatvector() +
  geom_spatraster(data = rr) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    values = c(
      "FTP" = "#376387",
      "Shrubland" = "forestgreen",
      "FTP + Shrubland" = "#f3b300"
    ),
    na.value = "transparent"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())
A


#### Comparison Forest ####
rr_f <- c(ar_forest, pa_I, pa_I_II)
names(rr_f) <- c("Forest", "FTP-I", "FTP-I-II")
rr_f <- flexsdm::homogenize_na(rr_f)
plot(rr_f > 0.5)
rr_f <- (rr_f > 0.5) %>% as.numeric()
rr_f[[1]] <- rr_f[[1]] * 2
plot(rr_f)

rr_f <- c(
  sum(rr_f[[c("Forest", "FTP-I")]]),
  sum(rr_f[[c("Forest", "FTP-I-II")]])
)
rr_f[rr_f == 0] <- NA
rr_f <- as.factor(rr_f)
levels(rr_f[[1]]) <- data.frame(
  ID = c(1, 2, 3),
  cover = c("FTP", "Forest", "FTP + Forest")
)
levels(rr_f[[2]]) <- data.frame(
  ID = c(1, 2, 3),
  cover = c("FTP", "Forest", "FTP + Forest")
)
names(rr_f) <- c("Forest & FTP-I", "Forest & FTP-I-II")

B <- ggplot(ar) +
  geom_spatvector() +
  geom_spatraster(data = rr_f) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(
    values = c(
      "FTP" = "#376387",
      "Forest" = "forestgreen",
      "FTP + Forest" = "#f3b300"
    ),
    na.value = "transparent"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

B


fig_save <- getwd() |>
  dirname() |>
  file.path("2-Manuscript/Figures_2026")

# ggsave(plot = A/B, file.path(fig_save, "Vegetation and FTP.png"),
#        units = "cm", scale = 1.5, width = 14, height = 20, dpi = 300)

## %######################################################%##
#                                                          #
####                    Richness map                    ####
#                                                          #
## %######################################################%##
ar <- terra::vect("Argentina.gpkg")
base_ <- terra::rast("Protected areas.tif")
base_[base_ >= 0] <- 0

# List species raster
l <- list.files(
  path = "Models/1-SDM/2_Outputs/3_FinalModels/Argentina_1km",
  full.names = TRUE,
  pattern = "tif$"
)
sp <- l[1] |>
  terra::rast() %>%
  .[[2]] |>
  crop(ar) |>
  mask(ar) |>
  resample(base_)
sp[is.na(sp) & !is.na(base_)] <- 0
rich <- sp
plot(rich)
for (i in i:length(l)) {
  message(i)
  sp <- l[i] |>
    terra::rast() %>%
    .[[2]] |>
    crop(base_) |>
    resample(base_) |>
    mask(base_)
  sp[is.na(sp) & !is.na(base_)] <- 0
  rich <- rich + sp
}
plot(rich)
# terra::writeRaster(rich, "species_richness.tif", overwrite=TRUE)

## %######################################################%##
#                                                          #
####                    Figures_2026                    ####
#                                                          #
## %######################################################%##
fig_save <- getwd() |>
  dirname() |>
  file.path("2-Manuscript/Figures_2026")

# Argentina polygon
ar <- "Spatial data/Argentina.gpkg" |>
  terra::vect()
# Fores ecoregions
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions


# Protectd areas
pa_r <- terra::rast("Protected areas.tif") |> mask(ar_for)
pa_I <- terra::rast("Protected areas+OTNB I.tif") |> mask(ar_for)
pa_I_II <- terra::rast("Protected areas+OTNB I-II.tif") |> mask(ar_for)
ar_anth <- terra::rast("./Land use/woodded_vegetation_2024_habitat.tif") |>
  mask(ar_for)

# Vegetation types
ar_shr <- terra::rast("./Land use/woodded_shrublands_2024_habitat.tif") |>
  mask(ar_for)
ar_forest <- terra::rast("./Land use/woodded_forest_2024_habitat.tif") |>
  mask(ar_for)
ar_anth <- terra::rast("./Land use/woodded_vegetation_2024_habitat.tif") |>
  mask(ar_for)

# Richness map
rich <- terra::rast("species_richness.tif") |> mask(ar_for)
plot(rich > 5)


#### Forest region of argentina ####
fr <- ggplot(ar) +
  geom_spatvector(fill = "transparent") +
  geom_spatvector(data = ar_for, aes(fill = region2), alpha = 0.5) +
  scale_color_brewer(palette = "Accent") +
  theme_bw() +
  annotation_scale() +
  theme(legend.title = element_blank())
fr
ggsave(
  plot = fr,
  filename = file.path(fig_save, "Forest regions - Argentina.png"),
  width = 10,
  height = 11,
  dpi = 300,
  units = "cm",
  scale = 1.3
)

#### Wooded, forest, shurblands vetetation ####
Pas <- c(ar_shr, ar_forest, ar_anth)
names(Pas) <- c("Shrubland", "Forest", "Wooded")
Pas <- mask(Pas, ar_for)
plot(Pas)


require(tidyterra)
data("grass_db")
pals_all <- unique(grass_db$pal)
pals_all |> sort()
pals::pal.bands(grass.colors(15, palette = "forest_cover"))

fr_2 <- ggplot(ar) +
  geom_spatvector() +
  geom_spatraster(data = Pas) +
  tidyterra::scale_fill_grass_c(palette = "forest_cover") +
  geom_spatvector(data = ar_for, fill = NA, col = "black") +
  facet_wrap(~lyr, ncol = 3) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())


fr / fr_2 + plot_layout(heights = c(1, 1))

# Figure or Forest, shrublands and wooded vegetation
ggsave(
  file.path(fig_save, "Forest_shrublands_wooded.png"),
  width = 20,
  height = 23,
  dpi = 300,
  units = "cm",
  scale = 1.3
)


#### Protected areas and forest territory planning ####
Pas <- c(pa_r, pa_I, pa_I_II)
names(Pas) <- c("PA", "PA + TP-I", "PA + TP-I + TP-II")
Pas <- mask(Pas, ar_for)
plot(Pas)

data("princess_db")
princess_db$pal
pals::pal.bands(princess.colors(15, palette = "america"))
pals::pal.bands(princess.colors(15, palette = "america")[-c(1:4)])

Pas2 <- Pas
Pas2[Pas2 == 0] <- NA
ggplot(ar) +
  geom_spatvector() +
  geom_spatvector(data = ar_for, fill = "#FFFAFA", col = "black") +
  geom_spatraster(data = Pas2) +
  scale_fill_continuous(
    palette = princess.colors(15, palette = "america")[-c(1:4)],
    na.value = "transparent"
  ) +
  facet_wrap(~lyr, ncol = 3) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Figure or PA and TP
ggsave(
  file.path(fig_save, "PA_TP.png"),
  width = 15,
  height = 14,
  dpi = 300,
  units = "cm",
  scale = 1.3
)


## %######################################################%##
#                                                          #
#### Figure anthropogenic land use and species richness ####
#                                                          #
## %######################################################%##
reg <- ar_for |>
  as.data.frame() %>%
  .[2] |>
  as_tibble() |>
  dplyr::select(region = region2) |>
  dplyr::mutate(total_a = NA, anthrop = NA)
for (i in 1:nrow(reg)) {
  subr <- ar_for |> tidyterra::filter(region2 == reg$region[i])
  subr <- ar_anth |>
    crop(subr) |>
    mask(subr)
  r_area <- terra::cellSize(subr, unit = "km") |> mask(subr)
  reg$total_a[i] <- terra::global(r_area, "sum", na.rm = TRUE)[[1]]
  reg$anthrop[i] <- terra::global(r_area * subr, "sum", na.rm = TRUE)[[1]]
}

as.data.frame(r_area * subr)
reg <- reg |> dplyr::mutate(prop_anthr = anthrop / total_a)

# readr::write_tsv(reg, file.path(fig_save, "land_use_proportion.txt"))

## %######################################################%##
#                                                          #
####            Figure anthropogenetic land             ####
####              use and species richness              ####
#                                                          #
## %######################################################%##
# Load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)

# Get world map data as an sf object
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Create a new column to define the fill color
# Argentina will be "black", other countries "grey"
world_map <- world_map %>%
  mutate(fill_color = ifelse(name == "Argentina", "black", "grey"))

world_map


# Create the ggplot map with an orthographic (spherical) projection
ar_fig <- ggplot(data = world_map) +
  geom_sf(aes(fill = fill_color), color = "white", size = 0.1) + # Use fill_color aesthetic
  coord_sf(crs = "+proj=ortho +lat_0=-30 +lon_0=-60") + # Spherical projection centered on Argentina
  scale_fill_manual(
    values = c("black" = "black", "grey" = "grey"),
    guide = "none"
  ) + # Manually set colors and hide legend
  theme_bw() +
  theme(element_rect(fill = "transparent", color = NA))
ar_fig

ggsave(
  plot = ar_fig,
  file.path(fig_save, "1 Arg in the continentt.png"),
  width = 8,
  height = 8,
  units = "cm",
  scale = 1.8
)


# Argentina polygon
require(pals)
require(biscale)
ar <- "Spatial data/Argentina.gpkg" |>
  terra::vect()
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions
ar_for$region <- ar_for$region2

w_vege <- terra::rast("Land use/woodded_vegetation_2024_habitat.tif") |>
  mask(ar_for)
w_vege_85 <- terra::rast("Land use/woodded_vegetation_1985_habitat.tif") |>
  mask(ar_for)

plot(w_vege - w_vege_85)
plot(w_vege_85)

rich <- terra::rast("species_richness.tif") |> mask(ar_for)

ar_woodchange <- (w_vege - w_vege_85) |> mask(rich)
plot(ar_woodchange)

# rich_w <- terra::rast("species_richness_wooded.tif") |> mask(ar_for) # Rich based on wodded vegetation

rich <- rich * w_vege # Richness X proportion of woded vegetation in a cell
# plot(rich-rich_w) # difference between two approaches

rich # this approach will be used
plot(rich)


reg <- ar_for

# ar_woodchange <- ifel(ar_woodchange<0, ar_woodchange, 0)
rich_anth <- c(rich, ar_woodchange)
plot(rich_anth)

maps_rich <- list()
maps_lnd <- list()
maps_richlnd <- list()

# Themes
thm <- theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank()
  )

thm2 <- thm |> theme(legend.position = "none")

n_class <- 3
pallet <- "BlueOr"

for (i in 1:nrow(ar_for)) {
  subr <- ar_for |> tidyterra::filter(region2 == ar_for$region[i])

  rr0 <- rich_anth |>
    crop(subr) |>
    mask(subr)
  names(rr0) <- c("r", "a")

  if (reg$region[i] == "Chaco") {
    # rr[[1]][rr[[1]] > 130] <- 130
    rr0[[1]][rr0[[1]] > 100] <- 100
  }

  if (reg$region[i] == "Monte") {
    # rr[[1]][rr[[1]]>50] <- 50
    rr0[[1]][rr0[[1]] > 22] <- 22
  }
  if (reg$region[i] == "Espinal") {
    rr0[[1]][rr0[[1]] > 50] <- 50
  }
  if (reg$region[i] == "Andean Patagonian Forest") {
    rr0[[1]][rr0[[1]] > 30] <- 30
  }
  rich_anth_eco <- rr0

  rr0[[1]] <- terra::scale_linear(rr0[[1]])

  rr_df <- as.data.frame(rr0, xy = TRUE)
  rr_df <- bi_class(rr_df, x = r, y = a, style = "equal", dim = n_class)

  maps_rich[[i]] <- ggplot() +
    geom_spatraster(data = rich_anth_eco[[1]]) +
    scale_fill_viridis_c(option = "inferno", na.value = "transparent") +
    theme_bw() +
    thm

  maps_lnd[[i]] <- ggplot() +
    geom_spatraster(data = rich_anth_eco[[2]]) +
    scale_fill_gradient2(
      low = "brown",
      mid = "gray90",
      high = "darkgreen",
      midpoint = 0,
      na.value = "transparent"
    ) +
    theme_bw() +
    thm +
    theme(legend.position = "none")

  maps_richlnd[[i]] <- ggplot() +
    theme_void() +
    geom_raster(
      data = rr_df,
      mapping = aes(x = x, y = y, fill = bi_class),
      show.legend = FALSE
    ) +
    bi_scale_fill(pal = pallet, dim = n_class) +
    geom_spatvector(data = ar_for[i, ], fill = "transparent", col = "gray20")
}

# Richness
combined_plot <-
  wrap_plots(maps_rich, nrow = 3, widths = 2)

ar_r <- ggplot() +
  geom_spatvector(data = ar, fill = "transparent") +
  geom_spatraster(data = rich_anth[[1]]) +
  geom_spatvector(data = ar_for, fill = "transparent", col = "gray") +
  scale_fill_viridis_c(option = "inferno", na.value = "transparent") +
  theme_void() +
  thm
# ar_r + combined_plot
ggsave(
  plot = ar_r + combined_plot,
  filename = file.path(fig_save, "1 Arg species richness.svg"),
  width = 18,
  height = 16,
  units = "cm",
  scale = 1.8
)
ggsave(
  plot = ar_r + combined_plot,
  filename = file.path(fig_save, "1 Arg species richness.png"),
  width = 18,
  height = 16,
  units = "cm",
  scale = 1.8
)

# Change in Wooded land
combined_plot <-
  wrap_plots(maps_lnd, nrow = 3, widths = 2)

ar_r <- ggplot() +
  geom_spatvector(data = ar, fill = "transparent") +
  geom_spatraster(data = rich_anth[[2]]) +
  geom_spatvector(data = ar_for, fill = "transparent", col = "black") +
  scale_fill_gradient2(
    low = "brown",
    mid = "gray90",
    high = "darkgreen",
    midpoint = 0,
    na.value = "transparent"
  ) +
  theme_void() +
  thm

ggsave(
  plot = ar_r + combined_plot,
  filename = file.path(fig_save, "Loss in Wooded vegetation_0.png"),
  # plot = ar_r + combined_plot, filename = file.path(fig_save, "Loss in Wooded vegetation_0.svg"),
  width = 18,
  height = 16,
  units = "cm",
  scale = 1.8
)


# Land use and Richness
rr <- rich_anth
names(rr) <- c("r", "a")
rr_df <- rr |>
  as.data.frame(xy = TRUE) |>
  bi_class(
    x = r,
    y = a,
    style = "equal",
    dim = n_class
  )

# Create the legend for the bivariate map
legend <- bi_legend(
  pal = pallet,
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = n_class,
  xlab = "Species richness",
  ylab = "Anthropogenic land-use",
  size = 10
)

ar_r <- ggplot() +
  theme_void() +
  geom_spatvector(data = ar, fill = "transparent") +
  geom_raster(
    data = rr_df,
    mapping = aes(x = x, y = y, fill = bi_class),
    show.legend = FALSE
  ) +
  bi_scale_fill(
    pal = pallet,
    dim = n_class,
    flip_axes = FALSE,
    rotate_pal = FALSE
  ) +
  geom_spatvector(data = ar_for, fill = "transparent", col = "gray20")
# Combine the map and legend using cowplot
finalPlot <- ggdraw() +
  draw_plot(ar_r, 0, 0, 1, 1) + # Draw the main map plot
  draw_plot(legend, 0.55, 0.05, 0.25, 0.25) # Draw the legend in the specified position


combined_plot <-
  wrap_plots(maps_richlnd, nrow = 3, widths = 2)

# finalPlot + combined_plot
ggsave(
  plot = finalPlot + combined_plot,
  filename = file.path(fig_save, "Arg div + change_wooded_0.png"),
  width = 18,
  height = 16,
  units = "cm",
  scale = 1.8
)


## %######################################################%##
#                                                          #
####            Calculate metric by species              ####
#                                                          #
## %######################################################%##
# Argentina polygon
ar <- terra::vect("Spatial data/Argentina.gpkg")
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions
ar_for$region <- NULL

# Protected areas
pa_r <- terra::rast("Protected areas.tif") |> mask(ar_for)
pa_I <- terra::rast("Protected areas+OTNB I.tif") |> mask(ar_for)
pa_I_II <- terra::rast("Protected areas+OTNB I-II.tif") |> mask(ar_for)

# Woodded vegetation
ar_woodded <- terra::rast("./Land use/woodded_vegetation_2024_habitat.tif") |>
  mask(pa_r)
ar_woodded_85 <- terra::rast(
  "./Land use/woodded_vegetation_1985_habitat.tif"
) |>
  mask(pa_r)
plot(ar_woodded) # woodded_vegetation
plot(ar_woodded_85) # woodded_vegetation

# Figure
require(tidyterra)
sp <- list.files(
  "Models/1-SDM/2_Outputs/3_FinalModels/WholeDist/",
  pattern = "tif$",
  full.names = TRUE
) # Whole species distriubtion
sp_1km <- list.files(
  "Models/1-SDM/2_Outputs/3_FinalModels/Argentina_1km",
  pattern = "tif$",
  full.names = TRUE
) # Only in Argentina
names(sp_1km) <- gsub(".tif$", "", basename(sp_1km))

sp <- data.frame(
  dir = sp,
  species = gsub(".tif$", "", basename(sp)),
  a_total = NA, # total species range
  a_cntry_all = NA, # species range within country
  a_cntry_forest_85 = NA, # species range within country 1985
  a_cntry_forest = NA, # species range within country 2024
  a_wthn_pa = NA, # species range within protected areas system based on 2024 range
  a_wthn_pa_I = NA, # species range within protected areas system + OTNB I based on 2024 range
  a_wthn_pa_I_II = NA # species range within protected areas system + OTNB I + OTNB II based on 2024 range
) |>
  as_tibble()

# sp <- readr::read_tsv(file.path(fig_save, "Sp_representativeness_Arg.txt"))
# sp <- sp |> dplyr::mutate(dir =list.files("Models/1-SDM/2_Outputs/3_FinalModels/WholeDist/", pattern = "tif$", full.names = TRUE))

# Calculate different areas
for (i in 1:nrow(sp)) {
  message(paste("Species", i))

  # Total area
  r <- terra::ifel(terra::rast(sp$dir[i])[[2]] > 0, 1, NA)
  sp$a_total[[i]] <- terra::expanse(r, unit = "km")[[2]]

  # Total area within country
  r <- r |>
    terra::crop(ar) |>
    terra::mask(ar)
  sp$a_cntry_all[[i]] <- terra::expanse(r, unit = "km")[[2]]

  # Total area within forest region and wooded vegetation area 1985
  r <- terra:::ifel(terra::rast(sp_1km[sp$species[i]])[[2]] > 0, 1, NA) |>
    terra::crop(ar_for) |>
    terra::mask(ar_for)
  r_v <- terra::as.polygons(r)
  r <- ar_woodded_85 |>
    terra::crop(r) |>
    terra::mask(r_v)
  r_area <- terra::cellSize(r, unit = "km") * r
  sp$a_cntry_forest_85[[i]] <- terra::global(r_area, "sum", na.rm = TRUE)[[1]]

  # Total area within forest region and wooded vegetation area 2024
  r <- terra:::ifel(terra::rast(sp_1km[sp$species[i]])[[2]] > 0, 1, NA) |>
    terra::crop(ar_for) |>
    terra::mask(ar_for)
  r_v <- terra::as.polygons(r)
  r <- ar_woodded |>
    terra::crop(r) |>
    terra::mask(r_v)
  r_area <- terra::cellSize(r, unit = "km") * r
  sp$a_cntry_forest[[i]] <- terra::global(r_area, "sum", na.rm = TRUE)[[1]]

  # Area within Protected areas system based on range of 2024
  r <- (pa_r |> terra::crop(r_area) |> terra::mask(r_area))
  r <- (r_area * r)
  sp$a_wthn_pa[[i]] <- terra::global(r, "sum", na.rm = TRUE)[[1]]

  # Area within PA+OTBN I
  r <- (pa_I |> terra::crop(r_area) |> terra::mask(r_area))
  r <- (r_area * r)
  sp$a_wthn_pa_I[[i]] <- terra::global(r, "sum", na.rm = TRUE)[[1]]

  # Area within PA+OTBN I+II
  r <- (pa_I_II |> terra::crop(r_area) |> terra::mask(r_area))
  r <- (r_area * r)
  sp$a_wthn_pa_I_II[[i]] <- terra::global(r, "sum", na.rm = TRUE)[[1]]
}


sp <- sp |>
  dplyr::mutate(
    endem_degr = a_cntry_all / a_total,
    prop_loss = (a_cntry_forest_85 - a_cntry_forest) / a_cntry_forest_85,
    prop_wthn_pa = a_wthn_pa / a_cntry_forest, # this is based on remaining area 2024
    prop_wthn_pa_I = a_wthn_pa_I / a_cntry_forest,
    prop_wthn_pa_I_II = a_wthn_pa_I_II / a_cntry_forest
  )

sp |>
  dplyr::filter(prop_loss > 0.1) |>
  arrange(prop_loss)
sp |>
  dplyr::filter(prop_loss > 0.1) |>
  arrange(prop_loss) |>
  pull(species)
sp$prop_loss[which(sp$prop_loss < 0)] |>
  sort() |>
  round(2)
sp$prop_loss[which(sp$prop_loss < -2)] <- -0.67

sp |> ggplot(aes(x = endem_degr)) + geom_histogram()
# sp |> ggplot(aes(x = prop_remain)) + # prop_remain is wrong
#   geom_histogram()
sp |> ggplot(aes(x = prop_wthn_pa)) + geom_histogram()
sp |> ggplot(aes(x = prop_wthn_pa_I)) + geom_histogram()
sp |> ggplot(aes(x = prop_loss)) + geom_histogram()
sp$prop_loss |> sort()

sp$prop_loss
sp |>
  ggplot(aes(prop_loss, prop_wthn_pa)) +
  geom_point(alpha = 0.5) +
  geom_point(
    data = sp,
    alpha = 0.5,
    aes(prop_loss, prop_wthn_pa_I),
    col = "blue"
  ) +
  geom_point(
    data = sp,
    alpha = 0.5,
    aes(prop_loss, prop_wthn_pa_I_II),
    col = "red"
  )

# readr::write_tsv(sp, file.path(fig_save, "Sp_representativeness_Arg.txt"))

## %######################################################%##
#                                                          #
####           Process for each forest region           ####
#                                                          #
## %######################################################%##
represent <- function(
  sp_list,
  region,
  wooded_veg,
  wooded_veg_prev,
  pa,
  pa_I,
  pa_I_II
) {
  sp <- data.frame(
    dir = sp_list,
    species = gsub(".tif$", "", basename(sp_list)),
    a_cntry = NA,
    a_cntry_forest_85 = NA, # species range within country 1985
    a_cntry_forest = NA, # species range within country 2024
    a_wthn_pa = NA,
    a_wthn_pa_I = NA,
    a_wthn_pa_I_II = NA
  ) |>
    as_tibble()

  # crop
  wooded_veg_prev <- wooded_veg_prev |>
    crop(region) |>
    mask(region)
  wooded_veg <- wooded_veg |>
    crop(region) |>
    mask(region)
  pa <- pa |>
    crop(region) |>
    mask(region)
  pa_I <- pa_I |>
    crop(region) |>
    mask(region)
  pa_I_II <- pa_I_II |>
    crop(region) |>
    mask(region)

  for (i in 1:nrow(sp)) {
    r <- (terra::rast(sp$dir[i])[[2]] > 0)
    r1_ext <- terra::ext(r)
    r2_ext <- terra::ext(region)
    y <- intersect(r1_ext, r2_ext)

    if (!is.null(y)) {
      # Crop
      r <- r |>
        terra::crop(region) |>
        terra::mask(region)
      r[r == 0] <- NA
      r <- resample(r, wooded_veg)

      if (terra::expanse(r, unit = "km")[[2]] > 0) {
        # message(paste("Species", i))

        sp$a_cntry[[i]] <- terra::expanse(r, unit = "km")[[2]]

        # Remaining area 1985
        r <- (wooded_veg_prev |> crop(r) |> mask(r))
        r_area <- terra::cellSize(r, unit = "km") * r
        sp$a_cntry_forest_85[[i]] <- terra::global(
          r_area,
          "sum",
          na.rm = TRUE
        )[[1]]

        # Remaining area 2024
        r <- (wooded_veg |> crop(r) |> mask(r))
        r_area <- terra::cellSize(r, unit = "km") * r
        sp$a_cntry_forest[[i]] <- terra::global(r_area, "sum", na.rm = TRUE)[[
          1
        ]]

        # Area within Protected areas system
        r <- (pa_r |> crop(r_area) |> mask(r_area))
        r <- (r_area * r)
        sp$a_wthn_pa[[i]] <- terra::global(r, "sum", na.rm = TRUE)[[1]]

        # Area within PA+OTBN I
        r <- (pa_I |> crop(r_area) |> mask(r_area))
        r <- (r_area * r)
        sp$a_wthn_pa_I[[i]] <- terra::global(r, "sum", na.rm = TRUE)[[1]]

        # Area within PA+OTBN I+II
        r <- (pa_I_II |> crop(r_area) |> mask(r_area))
        r <- (r_area * r)
        sp$a_wthn_pa_I_II[[i]] <- terra::global(r, "sum", na.rm = TRUE)[[1]]
      }
    }
  }
  return(sp)
}


# Load data
ar <- terra::vect("Spatial data/Argentina.gpkg")
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions
ar_for$region <- NULL
ar <- terra::vect("Spatial data/Argentina.gpkg")
pa_r <- terra::rast("Protected areas.tif")
pa_I <- terra::rast("Protected areas+OTNB I.tif")
pa_I_II <- terra::rast("Protected areas+OTNB I-II.tif")
ar_woodded <- terra::rast("./Land use/woodded_vegetation_2024_habitat.tif") |>
  mask(ar_for)
ar_woodded_85 <- terra::rast(
  "./Land use/woodded_vegetation_1985_habitat.tif"
) |>
  mask(ar_for)
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar)
ar_for$region <- NULL


# sp <- list.files("Models/1-SDM/2_Outputs/3_FinalModels/WholeDist/", pattern = "tif$", full.names = TRUE)
sp <- list.files(
  "Models/1-SDM/2_Outputs/3_FinalModels/Argentina_1km",
  pattern = "tif$",
  full.names = TRUE
)

# Calculate area for each species and regions
ar_for$region2
eco_repr <- list()
for (i in 1:nrow(ar_for)) {
  message(i)
  eco_repr[[i]] <- represent(
    sp_list = sp,
    region = ar_for[i, ],
    wooded_veg = ar_woodded,
    wooded_veg_prev = ar_woodded_85,
    pa = pa_r,
    pa_I = pa_I,
    pa_I_II = pa_I_II
  )
  eco_repr[[i]] <- eco_repr[[i]][!is.na(eco_repr[[i]]$a_cntry_forest), ]
}

names(eco_repr) <- ar_for$region2
eco_repr_2 <- bind_rows(eco_repr, .id = "region")
eco_repr_2$dir <- NULL
eco_repr_2$region |> table()
eco_repr_2 <- eco_repr_2 |>
  dplyr::mutate(
    prop_loss = (a_cntry_forest_85 - a_cntry_forest) / a_cntry_forest_85,
    prop_wthn_pa = a_wthn_pa / a_cntry_forest,
    prop_wthn_pa_I = a_wthn_pa_I / a_cntry_forest,
    prop_wthn_pa_I_II = a_wthn_pa_I_II / a_cntry_forest
  )

eco_repr_2$prop_loss |> hist()
eco_repr_2$prop_loss[which(eco_repr_2$prop_loss < 0)] <- 0
# readr::write_tsv(eco_repr_2, file.path(fig_save, "Sp_representativeness_by_region.txt"))

## %######################################################%##
#                                                          #
####          Figure species representativness          ####
#                                                          #
## %######################################################%##
# Argentina
rep_ar <- file.path(fig_save, "Sp_representativeness_Arg.txt") |>
  readr::read_tsv()
rep_ar$dir <- NULL
rep_ar$region <- "Argentina"
rep_ar$a_cntry_forest |>
  round(4) |>
  sort()
rep_ar[which(rep_ar$a_cntry_forest < 4), 1:5] # remove Escallonia myrtoidea


# Ecorregions
rep_eco <- file.path(fig_save, "Sp_representativeness_by_region.txt") |>
  readr::read_tsv()
rep_eco$dir <- NULL
rep_eco$region
rep_eco <- bind_rows(rep_ar, rep_eco)

rep_eco |>
  dplyr::filter(species %in% c("Alchornea castaneifolia", "Cordia alliodora"))

table(rep_eco$region)
rep_eco$region <- rep_eco$region |>
  factor(
    levels = c(
      "Argentina",
      "Andean Patagonian Forest",
      "Atlantic Forest",
      "Chaco",
      "Espinal",
      "Monte",
      "Paraná Delta",
      "Yungas"
    )
  )

rep_eco2 <- rep_eco |>
  dplyr::select(region, prop_wthn_pa, prop_wthn_pa_I, prop_wthn_pa_I_II) |>
  tidyr::gather("key", "value", -region)

labelss <-
  rep_eco2 |>
  dplyr::filter(key == "prop_wthn_pa") |>
  group_by(region) |>
  count()
labelss[1, 2] <- 565
labelss$labs <- paste0(labelss$region, paste0("\n(", labelss$n, ")"))
labelss$n
labelss_2 <- labelss$labs
names(labelss_2) <- labelss$region


# pals::pal.bands(pals::kelly(n = 22))
eco_violin <- rep_eco2 |>
  ggplot(aes(key, value * 100)) +
  geom_jitter(alpha = 0.3, width = 0.2, aes(color = key)) +
  geom_violin(alpha = 0.5, aes(fill = key)) +
  geom_boxplot(alpha = 0.5, aes(fill = key)) +
  theme_bw() +
  scale_fill_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  scale_color_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  facet_wrap(
    . ~ region,
    scales = "free_x",
    labeller = labeller(region = labelss_2),
    nrow = 2
  ) +
  scale_x_discrete(labels = c("PA", "PA +\nFTP-I ", "PA +\nFTP-I-II")) +
  labs(x = element_blank(), y = "Species range representativeness (%)") +
  theme(legend.position = "none")

eco_violin

ggsave(
  plot = eco_violin,
  filename = file.path(fig_save, "3_Sp_representativeness_Ecor.png"),
  width = 14,
  height = 10,
  units = "cm",
  scale = 1.4,
  dpi = 300
)

rep_eco2 <- na.omit(rep_eco2) |>
  mutate(value = value * 100)
rep_eco2 |>
  filter(key == "prop_wthn_pa") |>
  group_by(region) |>
  count()

table_rep <- rep_eco2 |>
  group_by(region, key) |>
  dplyr::summarise(
    rep_mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n_sp = length(value)
  )

table_rep$key
table_rep$key <- gsub("prop_wthn_", "", table_rep$key) |> toupper()
table_rep$val2 <-
  paste(
    round(table_rep$rep_mean, 2),
    "±",
    round(table_rep$sd, 2)
  )

# readr::write_tsv(table_rep, file.path(fig_save, "3_Sp_representativeness_Ecor.txt"))

## %######################################################%##
#                                                          #
####      Representativeness and protection degree      ####
#                                                          #
## %######################################################%##
# add to stable with speceis list those species with higher endemism degree
splist <- readxl::read_excel(
  file.path(getwd() |> dirname(), "2-Manuscript/Figures_2026/Tables.xlsx"),
  sheet = "Table S1"
)
rep_ar <- file.path(fig_save, "Sp_representativeness_Arg.txt") |>
  readr::read_tsv() |>
  dplyr::select(species, endem_degr)
names(rep_ar)[1] <- "Species"

splist <- left_join(splist, rep_ar, by = "Species")
splist$high_endem <- ifelse(splist$endem_degr >= 0.8, "Yes", "No")
splist |>
  dplyr::filter(high_endem == "Yes") |>
  nrow() # 26 species with high endemism degree
# readr::write_tsv(splist, file.path(fig_save, "0_modeled species_2.txt"))

# Process data for figure
rep_ar <- file.path(fig_save, "Sp_representativeness_Arg.txt") |>
  readr::read_tsv()
rep_ar$dir <- NULL
rep_ar <- rep_ar |>
  dplyr::select(endem_degr, prop_wthn_pa, prop_wthn_pa_I, prop_wthn_pa_I_II)
sum(rep_ar$endem_degr != 0) # TODO this is the number of especies analyzed
rep_ar <- rep_ar[rep_ar$endem_degr != 0, ]

rep_ar$endem_degr_c <- cut(
  rep_ar$endem_degr,
  c(-Inf, 0.2, 0.4, 0.6, 0.8, +Inf),
  labels = c("0-20", "20-40", "40-60", "60-80", "80-100")
)
rep_ar$endem_degr <- NULL
names(rep_ar)[1:3]
names(rep_ar)[1:3] <- gsub("prop_wthn_pa", "PA", names(rep_ar)[1:3])
names(rep_ar)[4] <- "endem"
rep_ar <- rep_ar |>
  tidyr::gather(
    key = "type",
    value = "value",
    -endem
  ) |>
  arrange(type, endem)

labelss <-
  rep_ar |>
  dplyr::filter(type == "PA") |>
  group_by(endem) |>
  count()
labelss[1, 2] <- 380
sum(labelss[, 2])
labelss$labs <- paste0(labelss$endem, paste0("\n(", labelss$n, ")"))
labelss_2 <- labelss$labs
names(labelss_2) <- labelss$endem

rep_ar$endem <- rep_ar$endem |> as.character()


ggplot(rep_ar, aes(type, value * 100)) +
  geom_jitter(alpha = 0.3, width = 0.2, aes(color = type)) +
  geom_violin(
    aes(fill = type),
    alpha = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_boxplot(
    aes(fill = type),
    alpha = 0.5,
    outlier.alpha = 0,
    position = position_dodge(width = 0.8)
  ) +
  theme_bw() +
  scale_x_discrete(labels = c("PA", "PA +\nFTP-I ", "PA +\nFTP-I-II")) +
  scale_fill_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  scale_color_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  facet_grid(
    . ~ endem,
    scales = "free_x",
    labeller = labeller(endem = labelss_2)
  ) +
  labs(
    subtitle = "Degree of endemism (%)",
    y = "Species range representativeness (%)",
    x = element_blank()
  ) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave(
  filename = file.path(fig_save, "5_Sp_representativeness_endemicity.png"),
  width = 12,
  height = 6,
  units = "cm",
  scale = 1.7,
  dpi = 300
)

rep_ar2 <- na.omit(rep_ar)
rep_ar2 <- rep_ar2 |>
  group_by(endem, type) |>
  dplyr::summarise(
    rep_mean = mean(value * 100, na.rm = T),
    sd = sd(value * 100, na.rm = T),
    n_sp = length(value)
  )

rep_ar2$val2 <-
  paste(
    round(rep_ar2$rep_mean, 2),
    "±",
    round(rep_ar2$sd, 2)
  )
# readr::write_tsv(rep_ar2, file.path(fig_save, "5_Sp_representativeness_endemicity.txt"))

## %######################################################%##
#                                                          #
####           Range loss vs representativeness.        ####
#                                                          #
## %######################################################%##
rep_ar <- file.path(fig_save, "Sp_representativeness_Arg.txt") |>
  readr::read_tsv()
rep_ar[which(rep_ar$a_cntry_forest < 4), "species"]
# rep_ar <- rep_ar |> dplyr::filter(species != "Escallonia myrtoidea")
rep_ar <- rep_ar |>
  dplyr::select(
    species,
    prop_wthn_pa,
    prop_wthn_pa_I,
    prop_wthn_pa_I_II,
    prop_loss
  )


# Histogram of species loss, add arrow from left to right indicating loss
hist(rep_ar$prop_loss * 100)
length(which(rep_ar$prop_loss > 0)) # species that gain range
length(which(rep_ar$prop_loss < 0)) # species that gain range
rep_ar$species[which(rep_ar$prop_loss < 0)] |> sort() # species that gain range
mean(rep_ar$prop_loss * 100, na.rm = TRUE)
sd(rep_ar$prop_loss * 100, na.rm = TRUE)

ggplot(rep_ar, aes(x = -1 * prop_loss * 100)) +
  geom_histogram(alpha = 0.5, col = "black") +
  labs(
    x = "Proportion of rage change between 1985-2024 (%)",
    y = "Number of species"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(
    aes(x = 0, y = 120, xend = 11, yend = 120), # Start (x, y) and end (xend, yend) coordinates
    arrow = arrow(length = unit(0.2, "cm")), # Add the arrow head
    color = "blue",
    size = 1.0
  ) +
  geom_segment(
    aes(x = 0, y = 120, xend = -11, yend = 120), # Start (x, y) and end (xend, yend) coordinates
    arrow = arrow(length = unit(0.2, "cm")), # Add the arrow head
    color = "red",
    size = 1.0
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw()

ggsave(
  file.path(fig_save, "Figure S6 sp range loss.png"),
  width = 11,
  height = 8,
  units = "cm",
  scale = 1.7,
  dpi = 300
)


# gather data
rep_ar <- rep_ar |>
  tidyr::gather("type", "value", -prop_loss, -species) |>
  na.omit()
# Clasify prop_loss  into categories
rep_ar$prop_loss2 <- cut(
  rep_ar$prop_loss,
  c(-Inf, 0.05, 0.10, 0.15, 0.20, +Inf),
  labels = c("0-5", "5-10", "10-15", "15-20", ">20")
)

labelss <-
  rep_ar |>
  dplyr::filter(prop_loss >= 0) |>
  dplyr::filter(type == "prop_wthn_pa") |>
  group_by(prop_loss2) |>
  count()
labelss$labs <- paste0(labelss$prop_loss2, paste0("\n(", labelss$n, ")"))
labelss_2 <- labelss$labs
names(labelss_2) <- labelss$prop_loss2


ggplot(
  rep_ar |> dplyr::filter(prop_loss >= 0),
  aes(prop_loss, value * 100)
) +
  geom_point(alpha = 0.5, aes(col = type)) +
  theme_bw() +
  facet_wrap(
    . ~ type,
    nrow = 1,
    labeller = labeller(
      type = c(
        prop_wthn_pa = "PA",
        prop_wthn_pa_I = "PA + TP-I",
        prop_wthn_pa_I_II = "PA + TP-I + TP-II"
      )
    )
  ) +
  scale_color_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  labs(
    y = "Species range protected (%)",
    x = "Proportional range loss between 1985-2024"
  ) +
  theme(legend.position = "none")

ggplot(
  rep_ar |> dplyr::filter(prop_loss >= 0),
  aes(type, value * 100)
) +
  geom_jitter(alpha = 0.5, width = 0.2, aes(color = type)) +
  geom_violin(alpha = 0.5, aes(fill = type)) +
  geom_boxplot(alpha = 0.5, aes(fill = type)) +
  theme_bw() +
  facet_wrap(
    . ~ prop_loss2,
    nrow = 1,
    labeller = labeller(prop_loss2 = labelss_2)
  ) +
  scale_color_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  scale_fill_manual(values = c("#E68FAC", "#8DB600", "#008856")) +
  labs(
    subtitle = "Classe of proportional range loss between 1985-2024",
    y = "Species range representativeness (%)",
    x = element_blank()
  ) +
  scale_x_discrete(labels = c("PA", "PA +\nFTP-I ", "PA +\nFTP-I-II")) +
  theme(legend.position = "none")

ggsave(
  filename = file.path(fig_save, "6_Sp_representativeness_range_loss.png"),
  width = 12,
  height = 6,
  units = "cm",
  scale = 1.7,
  dpi = 300
)


rep_ar2 <- na.omit(rep_ar)
rep_ar2 <- rep_ar2 |>
  group_by(prop_loss2, type) |>
  dplyr::summarise(
    rep_mean = mean(value * 100, na.rm = T),
    sd = sd(value * 100, na.rm = T),
    n_sp = length(value)
  )

rep_ar2$val2 <-
  paste(
    round(rep_ar2$rep_mean, 2),
    "±",
    round(rep_ar2$sd, 2)
  )
rep_ar2$type <- gsub("prop_wthn_pa", "PA", rep_ar2$type)

# readr::write_tsv(rep_ar2, file.path(fig_save, "6_Sp_representativeness_range_loss.txt"))

## %######################################################%##
#                                                          #
####            Richness and protected areas            ####
#                                                          #
## %######################################################%##
ar <- terra::vect("Spatial data/Argentina.gpkg")
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions
pa_r <- terra::rast("Protected areas.tif") |> mask(ar_for)
pa_I <- terra::rast("Protected areas+OTNB I.tif") |> mask(ar_for)
pa_I_II <- terra::rast("Protected areas+OTNB I-II.tif") |> mask(ar_for)
pa_all <- c(pa_r, pa_I, pa_I_II)
plot(pa_all)
plot(pa_all[[3]] >= .5)

rich <- terra::rast("species_richness.tif") |> mask(ar_for)
w_vege <- terra::rast("Land use/woodded_vegetation_2024_habitat.tif") |>
  mask(ar_for)
rich_2 <- rich * w_vege
plot(w_vege)
plot(rich)
plot(rich_2)

names(pa_r) <- "PA"
names(rich_2) <- "Rich"
rich_prot <- c(rich_2, pa_I_II)

# Process data for All argentina
rich_2 <- round(rich_2)
rich_3 <- terra::classify(
  x = rich_2,
  seq(0, 250, by = 40),
  include.lowest = TRUE,
  brackets = F
)

pa_all_3 <- terra::classify(
  x = (pa_all * 100),
  seq(0, 100, by = 25),
  include.lowest = TRUE,
  brackets = F
)
names(pa_all_3) <- c("PA", "PA_I", "PA_I_II")

freq(pa_all_3)
plot(pa_all_3)

all <- c(rich_3, pa_all_3) |>
  as.data.frame() |>
  as_tibble() |>
  tidyr::gather("Factor", "ProtClass", -Rich) |>
  na.omit()


ggplot(all, aes(x = Rich, fill = ProtClass)) +
  geom_bar(position = "fill", stat = "count") +
  facet_wrap(
    ~Factor,
    labeller = labeller(
      Factor = c(
        "PA" = "PA",
        "PA_I" = "PA + FTP-I",
        "PA_I_II" = "PA + FTP-I + FTP-II"
      )
    )
  ) +
  # geom_text(data = data.frame(Factor = c("PA", "PA_I", "PA_I_II"), label = c("a)", "b)", "c)")),
  #           aes(label = label, x = 0.6, y = 0.98), inherit.aes = FALSE,
  #           hjust = 0, vjust = 1, size = 5) +
  labs(
    x = "Species richness class",
    y = "Species richness representativeness (%)",
    fill = "Proportion of\ncell protected (%)"
  ) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  theme_bw() +
  scale_fill_manual(
    values = c(
      "#D6CFB7FF",
      "#E5AD4FFF",
      "#BD5630FF",
      "#6D8325FF"
    )
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = percent) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

pp <- dplyr::group_by(all, Rich, Factor, ProtClass) %>%
  count() %>%
  dplyr::group_by(Rich, Factor) %>%
  dplyr::mutate(prop = n / sum(n))

# ggsave(
#   filename = file.path(fig_save, "4_Richness_representativeness_arg.png"),
#   width = 13, height = 8, units = "cm", scale = 1.3, dpi = 300
# )
# readr::write_tsv(pp, file.path(fig_save, "4_Richness_representativeness_arg.txt"))

# Process for each forest region
rich_prot <- list()
prop_table <- list()
ar_for <- ar_for %>% tidyterra::arrange(region2)
for (i in 1:nrow(ar_for)) {
  rich_eco <- rich_2 |>
    mask(ar_for[i, ]) |>
    crop(ar_for[i, ])
  mx <- terra::global(rich_eco, "max", na.rm = TRUE)[1, ]
  rich_eco <- terra::classify(
    x = rich_eco,
    seq(0, mx, length.out = 6) |> round(),
    include.lowest = TRUE,
    brackets = F
  )
  pa_eco <- pa_all_3 |>
    mask(ar_for[i, ]) |>
    crop(ar_for[i, ])

  all <- c(rich_eco, pa_eco) |>
    as.data.frame() |>
    as_tibble() |>
    tidyr::gather("Factor", "value", -Rich) |>
    na.omit()

  # Plot
  rich_prot[[i]] <-
    ggplot(all, aes(x = Rich, fill = value)) +
    geom_bar(position = "fill") +
    facet_wrap(
      ~Factor,
      labeller = labeller(
        Factor = c(
          "PA" = "PA",
          "PA_I" = "PA + FTP-I",
          "PA_I_II" = "PA + FTP-I + FTP-II"
        )
      )
    ) +
    geom_hline(yintercept = 0.5, linetype = 2) +
    theme_bw() +
    scale_fill_manual(
      values = c(
        "#D6CFB7FF",
        "#E5AD4FFF",
        "#BD5630FF",
        "#6D8325FF"
      )
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), labels = percent) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    ) +
    if (i == 5) {
      labs(
        x = element_blank(),
        y = "Species richness representativeness (%)",
        subtitle = ar_for[i, 2] |> as.data.frame() |> pull(1),
        fill = "Proportion of\ncell protected (%)"
      )
    } else {
      labs(
        x = element_blank(),
        y = element_blank(),
        subtitle = ar_for[i, 2] |> as.data.frame() |> pull(1),
        fill = "Proportion of\ncell protected (%)"
      )
    }

  # Frequency Table
  all <- all |>
    dplyr::select(Type = Factor, Rich, Protection = value) |>
    arrange(Type, Rich, Protection)
  prop_table[[i]] <- all |>
    group_by(Type, Rich, Protection) |>
    count() |>
    group_by()
  names(prop_table[[i]]) <- c("Rich", "Type", "Protection", "n")
  prop_table[[i]] <- prop_table[[i]] |>
    group_by(Type, Rich) |>
    mutate(prop = n / sum(n))
}
names(prop_table) <- ar_for$region2
prop_table <- bind_rows(prop_table, .id = "regions")

combined_plot <-
  wrap_plots(rich_prot, ncol = 2)

ggsave(
  plot = combined_plot,
  filename = file.path(fig_save, "4_Richness_representativeness_regions.png"),
  width = 14,
  height = 14,
  units = "cm",
  scale = 1.6,
  dpi = 300
)

# readr::write_tsv(prop_table,
#    file.path(fig_save, "4_Richness_representativeness_regions.txt"))

## %######################################################%##
#                                                          #
####     PAs representativenes within forest region     ####
#                                                          #
## %######################################################%##
# Argentina polygon
ar <- "Spatial data/Argentina.gpkg" |>
  terra::vect()
# Fores ecoregions
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions

pa <- terra::rast("Protected areas.tif")
plot(pa)
plot(ar_for, add = TRUE)

df_area <- as.data.frame(ar_for) %>% dplyr::as_tibble()
df_area$area_km <- NA
df_area$area_pa_km <- NA
df_area$prop <- NA
df_area$prop <- NA
for (i in 1:nrow(df_area)) {
  pa_2 <- pa %>%
    crop(ar_for[ar_for$region2 == df_area$region2[i]]) %>%
    mask(ar_for[ar_for$region2 == df_area$region2[i]])
  df_area$area_km[i] <- terra::expanse(pa_2, unit = "km")[1, 2]
  pa_2[pa_2 <= 0] <- NA
  df_area$area_pa_km[i] <- terra::expanse(pa_2, unit = "km")[1, 2]
}

df_area <- df_area[-1]
df_area <- df_area %>% mutate(prop = area_pa_km / area_km * 100)
# readr::write_tsv(df_area, file.path(fig_save, "PAs coverage by regions.txt"), na = "")

## %######################################################%##
#                                                          #
####     Richness map with highest endemic species      ####
#                                                          #
## %######################################################%##
ar <- terra::vect("Argentina.gpkg")
base_ <- terra::rast("Protected areas.tif")
base_[base_ >= 0] <- 0


# List species raster
l <- list.files(
  path = "Models/1-SDM/2_Outputs/3_FinalModels/Argentina_1km",
  full.names = TRUE,
  pattern = "tif$"
)
names(l) <- l %>%
  basename() %>%
  gsub(".tif$", "", .)

db <- readr::read_tsv(
  getwd() |>
    dirname() |>
    file.path(
      "2-Manuscript/Figures_2026/0_modeled species_2.txt"
    )
)

# High degree of endemism (>= 80%)
db <- db |> filter(high_endem == "Yes")
db$Species %in% names(l)
l <- l[db$Species]


sp <- (l[1] %>%
  terra::rast())[[2]] %>%
  terra::crop(ar) %>%
  terra::mask(ar) %>%
  terra::resample(base_)
sp[is.na(sp) & !is.na(base_)] <- 0
rich <- sp
plot(rich)
for (i in 2:length(l)) {
  message(i)
  sp <- l[i] %>%
    terra::rast() %>%
    .[[2]] %>%
    crop(base_) %>%
    resample(base_) %>%
    mask(base_)
  sp[is.na(sp) & !is.na(base_)] <- 0
  rich <- rich + sp
}
plot(rich)
terra::writeRaster(
  rich,
  "species_richness_most_endemic_sp.tif",
  overwrite = TRUE
)

# Plot of richness
# Fores ecoregions
ar_for <- terra::vect("Spatial data/reg_ftales_2019_3857b.shp") |> project(ar) # Forest regions
# Plot maps
rich <- terra::rast("species_richness_most_endemic_sp.tif") |> mask(ar_for)
plot(rich)

en_ri <- ggplot(ar) +
  geom_spatvector() +
  geom_spatraster(data = rich) +
  scale_fill_viridis_c(option = "inferno", na.value = "transparent") +
  geom_spatvector(data = ar_for, fill = NA, col = "black") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

# ggsave(
#   plot = en_ri,
#   filename = file.path(fig_save, "Most endemic species richness.png"),
#   width = 8,
#   height = 12,
#   dpi = 300,
#   units = "cm",
#   scale = 1.5
# )
