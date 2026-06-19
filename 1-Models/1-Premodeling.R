## %######################################################%##
#                                                          #
####                   1-Pre-modeling                   ####
#                                                          #
## %######################################################%##
{
  require(dplyr)
  require(terra)
  require(flexsdm)
  require(here)
  require(progress)
  require(ape)
  require(ggplot2)
  require(corrplot)
  require(dismo)
  require(USE) # remotes::install_github("danddr/USE")
}
getwd()

## %######################################################%##
####             1-Create directory structure            ####
## %######################################################%##
fut <- "./1-SDM/1_Inputs/2_Predictors/2_Projection" %>%
  list.dirs(recursive = FALSE) %>%
  basename()
dir <- flexsdm::sdm_directory(
  main_dir = file.path(getwd(), "1-SDM"),
  projections = fut,
  calibration_area = TRUE,
  algorithm = c(
    "gam",
    "gau",
    "gbm",
    "glm",
    "max",
    "net",
    "raf",
    "svm",
    "median"
  ),
  ensemble = NULL,
  threshold = FALSE,
  return_vector = TRUE
)
# dir[1] %>% fs::dir_tree(., recurse = TRUE)
dir %>% head()


## %######################################################%##
####          2-Filtering on occurrence per cell          ####
## %######################################################%##
r <- "Variables/SoilGrids_v2/cfvo_100-200cm.tif" %>% terra::rast()
r[!is.na(r)] <- as.data.frame(r, row.names = TRUE) %>%
  rownames() %>%
  as.numeric()
names(r) <- "ncell"
plot(r)


unfilt_occ <-
  data.table::fread("species_records_final_unfiltered.gz") %>%
  as_tibble() %>%
  dplyr::select(x, y, db_id, species)

unflit_occ <- flexsdm::sdm_extract(
  unfilt_occ,
  x = "x",
  y = "y",
  env_layer = r,
  filter_na = TRUE
)
dim(unflit_occ)
unflit_occ <- unflit_occ %>%
  dplyr::group_by(species) %>%
  dplyr::filter(!duplicated(ncell)) %>%
  dplyr::group_by()
dim(unflit_occ)
# unflit_occ %>% dplyr::select(-ncell) %>% data.table::fwrite("species_records_final_OneByCell.gz")

## %######################################################%##
####                  3-Delimit training area            ####
## %######################################################%##
here()
unfilt_occ <-
  data.table::fread("species_records_final_OneByCell.gz") %>%
  as_tibble() %>%
  dplyr::select(x, y, db_id, species)

# # Ecoregion
eco <- terra::vect("./Ecoregions/Ecoregions.gpkg")
# eco

# Process species
sp <- unfilt_occ$species %>%
  table()
sp <- names(sp[sp >= 3]) %>% sort()
unfilt_occ <- unfilt_occ %>% dplyr::filter(species %in% sp)

i <- 1
for (i in 1:length(sp)) {
  message("species ", i, " ", round(i / length(sp) * 100, 2), "%")
  # x2 <-
  #   flexsdm::calib_area(
  #     data = unfilt_occ[unfilt_occ$species == sp[i], ],
  #     x = "x",
  #     y = "y",
  #     method = c("mask", eco, "ECO_ID")
  #   )
  x2 <- flexsdm::calib_area(
    data = unfilt_occ[unfilt_occ$species == sp[i], ],
    x = "x",
    y = "y",
    method = c("bmcp", width = "500000"), # buffer of 500 km
    crs = crs(eco)
  )
  x2$ECO_NAME <- NULL
  plot(x2, add = T)
  terra::writeVector(
    x2,
    file.path("1-SDM/1_Inputs/3_Calibration_area", paste0(sp[i], ".gpkg")),
    overwrite = TRUE
  )
}


## %######################################################%##
####     4-Producing plots for each species            ####
## %######################################################%##
require(sf)
# Select the region (in this case only South America)
w <-
  sf::st_as_sf(rnaturalearth::ne_countries(continent = c("South america")))

# Create directory for saving figures
dirs <- "1-SDM/1_Inputs/3_Calibration_area_plot"
dir.create(dirs)

# List of polygons
dirs_2 <- list.files(
  "1-SDM/1_Inputs/3_Calibration_area",
  full.names = TRUE,
  pattern = ".gpkg"
)
names(dirs_2) <- gsub(".gpkg", "", basename(dirs_2))

# List of species names
spl <- unfilt_occ$species %>%
  unique() %>%
  sort()

# Loop for saving figures for each species
for (i in 1:length(spl)) {
  message(i)
  pol_1 <- sf::st_read(dirs_2[spl[i]])
  p <- ggplot(w) +
    geom_sf() +
    geom_sf(data = pol_1, alpha = 0.5, fill = "red") +
    scale_fill_manual(values = pals::viridis(5)[c(3, 4)]) +
    # {if(nrow(pol_2)>0){
    #   geom_sf(data = pol_2, aes(group = gid), fill="green", alpha=0.5)
    # }} +
    geom_point(
      data = unfilt_occ %>% dplyr::filter(species == spl[i]),
      aes(x, y),
      alpha = 0.8
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
      title = spl[i],
      col = element_blank(),
      fill = element_blank(),
      x = element_blank(),
      y = element_blank()
    )
  p

  p2 <- ggplot() +
    geom_sf(data = pol_1, alpha = 0.5, fill = "red") +
    scale_fill_manual(values = pals::viridis(5)[c(3, 4)]) +
    geom_point(
      data = unfilt_occ %>% dplyr::filter(species == spl[i]),
      aes(x, y),
      alpha = 0.3
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(
      title = spl[i],
      col = element_blank(),
      fill = element_blank(),
      x = element_blank(),
      y = element_blank()
    )

  spn <- paste0(spl[i], ".png") # name of the png figure
  ggsave(
    plot = p + p2,
    filename = file.path(dirs, spn),
    width = 40,
    height = 20,
    units = "cm",
    dpi = 150
  )
  rm(pol_1)
}


## %######################################################%##
####               5-Average edaphic data                 ####
## %######################################################%##
stat_var <- "./Variables/SoilGrids_v2/" %>%
  list.files(full.names = TRUE, pattern = ".tif$")
stat_var <- data.frame(
  dir = stat_var,
  var = basename(stat_var) %>% stringr::str_split_fixed("_", 2) %>% .[, 1]
)
stat_var <- stat_var[-1, ]
var <- stat_var$var %>% unique()
# calculate mean for each variable
i <- 1
for (i in 1:length(var)) {
  print(i)
  r <- stat_var$dir[stat_var$var == var[i]] %>%
    terra::rast() %>%
    mean()
  names(r) <- var[i]
  terra::writeRaster(
    r,
    file.path("./Variables/SoilGrids_v2/", paste0("mean_", var[i], ".tif")),
    overwrite = TRUE
  )
}

## %######################################################%##
####             5-Correct NA cell of cities             ####
## %######################################################%##
r <- "./Variables/SoilGrids_v2/bdticm.tif" %>% terra::rast()
r0 <- "./Variables/Elevation/elevation.tif" %>% terra::rast()
r <- mask(r, r0)
m <- "./Variables/SoilGrids_v2/" %>%
  list.files(full.names = TRUE, pattern = "mean")

for (i in 1:length(m)) {
  message(i / length(m))
  mr <- m[i] %>% terra::rast()
  mr <- (is.na(mr) & !is.na(r))
  mr <- mask(mr, r)
  mr[!mr] <- NA
  mdf <- as.data.frame(mr, xy = TRUE, cells = TRUE) %>%
    dplyr::select(x, y, cell)
  # mdf <- mdf[is.na(mdf[, 5]), ] %>% dplyr::select(x, y, cell)
  mdf_vect <- vect(
    mdf,
    geom = c("x", "y"),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  ) %>%
    terra::buffer(10000)
  m_fill <- m[i] %>% terra::rast()

  mdf_vect2 <- terra::extract(
    x = m_fill,
    y = mdf_vect,
    fun = mean,
    na.rm = TRUE
  )
  m_fill[mdf_vect$cell] <- mdf_vect2[, 2]

  terra::writeRaster(m_fill, gsub(".tif$", "_xxx.tif", m[i]), overwrite = TRUE)
  rm(mr)
  rm(m_fill)
}

m <- "./Variables/SoilGrids_v2/" %>%
  list.files(full.names = TRUE, pattern = "mean")
file.rename(m, gsub("_xxx.tif", ".tif", m))

## %######################################################%##
####    6-Combine static variables with climate data      ####
## %######################################################%##
dirss <- "./Variables/Allvariables/" %>% list.dirs(recursive = FALSE)

stat_var <- "./Variables/SoilGrids_v2/" %>%
  list.files(full.names = TRUE, pattern = "mean")
stat_var <- grep("mean", stat_var, value = TRUE)
stat_var <- grep(
  paste(c("cec", "itrogen", "phh", "ocs", "soc"), collapse = "|"),
  stat_var,
  invert = T,
  value = T
)
stat_var <- c(
  stat_var,
  "./Variables/SoilGrids_v2/bdticm.tif",
  "./Variables/Elevation/elevation.tif",
  "./Variables/Geomorpho/slope.tif"
)
i <- 1
for (i in 1:length(dirss)) {
  print(i)
  file.copy(stat_var, file.path(dirss[i], basename(stat_var)))
}

sapply(dirss, function(x) length(list.files(x))) %>% unique()

# Remove nitrogen, CEC, pH, organic carbon
stat_var
# sapply(dirss, function(x) list.files(x, patter = "cec.tif", full.names = TRUE)) %>% unlist() %>% file.remove()
# sapply(dirss, function(x) list.files(x, patter = "itrogen.tif", full.names = TRUE)) %>% unlist() %>% file.remove()
# sapply(dirss, function(x) list.files(x, patter = "phh2o.tif", full.names = TRUE)) %>% unlist() %>% file.remove()
# sapply(dirss, function(x) list.files(x, patter = "ocs.tif", full.names = TRUE)) %>% unlist() %>% file.remove()
# sapply(dirss, function(x) list.files(x, patter = "soc.tif", full.names = TRUE)) %>% unlist() %>% file.remove()

## %######################################################%##
####         7-PCA for each calibration area         ####
## %######################################################%##
save_dir <- "./1-SDM/1_Inputs/4_Predictors_by_sp"
dir.create(save_dir)
save_dir

# Environmental condition for baseline period
env_variables <- list.files(
  "./1-SDM/1_Inputs/2_Predictors/1_Current/1981-2010",
  pattern = ".tif",
  full.names = TRUE
) %>%
  terra::rast()

# List of gpkg files for each calibration area
cal_area <- file.path(getwd(), "1-SDM/1_Inputs/3_Calibration_area") %>%
  list.files(pattern = ".gpkg$", full.names = TRUE)

i <- 1
list.files(
  "./1-SDM/1_Inputs/4_Predictors_by_sp/2071-2100_UKESM1-0-LL_ssp585"
) %>%
  gsub(".tif", "", .) %>%
  length()

# Check species 4, 5, 103 they could be changed because the problme of using two loops
i <- 1

for (i in 1:length(cal_area)) {
  message("species ", i, " ", Sys.time())
  v <- cal_area[i] %>% terra::vect()

  env_new <- correct_colinvar(
    env_layer = env_variables,
    method = c("pca"),
    restric_to_region = v,
    restric_pca_proj = TRUE,
    save_proj = save_dir
  )

  # Save tables
  sp_name <- basename(cal_area[i]) %>%
    gsub(".gpkg$", "", .)

  env_new$cumulative_variance %>%
    readr::write_tsv(
      file.path(save_dir, "1981-2010", paste0(sp_name, "-cum_var.txt"))
    )

  env_new$coefficients %>%
    readr::write_tsv(
      file.path(save_dir, "1981-2010", paste0(sp_name, "-coefficients.txt"))
    )

  # Saver raster for current conditions
  terra::writeRaster(
    env_new$env_layer,
    file.path(save_dir, "1981-2010", "pcs.tif"),
    overwrite = TRUE
  )

  terra:::tmpFiles(current = TRUE, orphan = TRUE, old = FALSE, remove = TRUE)
}


n <- NA
save_dir <- file.path(save_dir, "1981-2010") %>%
  list.files(pattern = ".tif$", full.names = TRUE)
for (i in 1:length(save_dir)) {
  n[i] <- terra::rast(gsub(".gpkg", ".tif", save_dir[i])) %>% terra::nlyr()
}
table(n)
# n
# 7   8   9  10
# 61 205 291  12

## %######################################################%##
#                                                          #
####       8-Correcting sampling bias occurrence        ####
#                                                          #
## %######################################################%##
db0 <- data.table::fread("species_records_final_OneByCell.gz") %>%
  as_tibble()

nocc <- db0 %>%
  group_by(species) %>%
  count() %>%
  # arrange(desc(n)) %>%
  # mutate(need_bias_corr = ifelse(n >= 100, 1, 0))
  mutate(need_bias_corr = 0)

# Save in txt and check visually the species that need bias correction
# readr::write_tsv(nocc, "need_bias_corr.txt")
nocc <- readxl::read_xlsx("need_bias_corr.xlsx", sheet = 1)
sum(nocc$species %in% "Neltuma × vinalillo")
tail(nocc)

sp <- nocc %>%
  dplyr::filter(need_bias_corr == TRUE) %>%
  pull(species)


cal_area <- file.path(
  getwd(),
  "1-SDM/1_Inputs/4_Predictors_by_sp/1981-2010"
) %>%
  list.files(pattern = ".tif$", full.names = TRUE)
names(cal_area) <- gsub(".tif$", "", basename(cal_area))


filter_prop <- list_filt_occ <- list()
for (i in 1:length(sp)) {
  message("species ", i, " ", round(i / length(sp) * 100, 2), "%")

  env <- terra::rast(cal_area[sp[i]])
  suppressMessages(
    filt_occ_0 <-
      flexsdm::occfilt_geo(
        data = db0[db0$species == sp[i], ],
        x = "x",
        y = "y",
        env_layer = env,
        method = c("defined", d = 3, 6, 8, 10, 12, 14, 16, 18, 20), # 6 km
        prj = crs(env),
        reps = 3
      )
  )
  filt_occ_0 <- filt_occ_0 %>%
    flexsdm::occfilt_select(
      x = "x",
      y = "y",
      env_layer = env,
      filter_prop = TRUE
    )
  filt_occ_0
  filter_prop[[i]] <- filt_occ_0$filter_prop %>%
    dplyr::filter(grepl("[*]", filt_value)) %>%
    dplyr::select(filt_value:n_records) %>%
    dplyr::mutate(
      n_original = nrow(db0[db0$species == sp[i], ]),
      secies = sp[i]
    )

  list_filt_occ[[i]] <- filt_occ_0$occ
}

# dind data.frames
list_filt_occ_final <- bind_rows(list_filt_occ)
filter_prop_final <- bind_rows(filter_prop)

# merge unfiltered species with filtered one
db1 <- db0 %>% filter(!species %in% sp)

# # remove occ in a same cell
filt_occ <- bind_rows(db1, list_filt_occ_final)
data.table::fwrite(filt_occ, "occurrences_cleaned_final_FILTERED.gz")
readr::write_tsv(
  filter_prop_final,
  "1-SDM/1_Inputs/1_Occurrences/filt_prop_selected.txt"
)

# data.table::fwrite(bind_rows(data.table::fread("occurrences_cleaned_final_FILTERED.gz"), filt_occ) , "occurrences_cleaned_final_FILTERED.gz")

# Filter species with >=5 occurrences and between 2-4 occurrences
occ <- data.table::fread("occurrences_cleaned_final_FILTERED.gz") %>%
  as_tibble()
filt <- occ %>%
  group_by(species) %>%
  count()
filt <- filt %>%
  filter(n >= 5) %>%
  pull(species)

occ_more5 <- occ %>% filter(species %in% filt) # filtra mayor que 5
data.table::fwrite(
  occ_more5,
  here("occurrences_cleaned_final_FILTERED_more5.gz")
)

# Species less than 5 occurrences will be used only for current projetion
occ_less5 <- occ %>% filter(!species %in% filt) # filtra menor que 5
data.table::fwrite(
  occ_less5,
  here("occurrences_cleaned_final_FILTERED_less5.gz")
)


## %######################################################%##
#                                                          #
####           9-Pseudo-absences, background points      ####
#                                                          #
## %######################################################%##

# Protocol
# Account with two occurrence dataset
# 1- unfiltered occurrences
# 2- with filtered occurrence in geographical space

# Dataset 1 will be used to calculate Mahalanobis and maskout predictors
# Dataset 2 will be used to define the number of pseudo-absence sampled in the environmental space (using kmean)

occ_unfilt <- data.table::fread("species_records_final_OneByCell.gz") %>%
  as_tibble()
occ <- data.table::fread(here(
  "occurrences_cleaned_final_FILTERED_more5.gz"
)) %>%
  as_tibble()
occ_unfilt <- occ_unfilt %>% dplyr::filter(species %in% unique(occ$species))
occ$pr_ab <- 1
sp <- occ$species %>%
  unique() %>%
  sort()

# List of PCA of each training area
clibarea <- list.files(
  "./1-SDM/1_Inputs/4_Predictors_by_sp/1981-2010/",
  pattern = ".tif$",
  full.names = TRUE
)
names(clibarea) <- clibarea %>%
  basename() %>%
  gsub(".tif$", "", .)
clibarea <- clibarea[sp]

# Calculate gower distance for each species
"./1-SDM/1_Inputs/3_Gower_dist" %>% dir.create()
max_dg <- c()
for (i in 1:length(sp)) {
  message(i)

  r <- terra::rast(clibarea[[sp[i]]]) # PCA for a specific calibration area
  coord <- occ_unfilt %>%
    dplyr::filter(species == sp[i]) %>%
    flexsdm::sdm_extract(data = ., x = "x", y = "y", env_layer = r)

  # gd <- env_dist(
  #    training_data = coord,
  #    projection_data = r,
  #    n_cores = 15,
  #    metric = "gower"
  #  )

  gd <- flexsdm::map_env_dist(
    training_data = coord,
    projection_data = r,
    # n_cores = 15,
    metric = "domain"
  )
  gd <- (1 - gd)
  terra::writeRaster(
    gd,
    file.path("./1-SDM/1_Inputs/3_Gower_dist", paste0(sp[i], ".tif")),
    overwrite = TRUE
  )

  max_dg[i] <- global(gd, max, na.rm = T)[1, ]

  gd2 <- classify(
    gd,
    c(0, 0.05, 0.10, 0.15, 0.20, Inf),
    include.lowest = F,
    brackets = TRUE
  )

  # Save in png
  png(
    file.path("./1-SDM/1_Inputs/3_Gower_dist", paste0(sp[i], ".png")),
    width = 25,
    height = 20,
    units = "cm",
    res = 200
  )
  plot(
    gd2,
    main = paste(sp[i], "max Gd", round(max_dg[i], 2)),
    col = pals::kovesi.rainbow(6)
  )
  points(coord[, c("x", "y")], col = "white", pch = 19, cex = 0.5)
  dev.off()
}
names(max_dg) <- sp
max_dg2 <- data.frame(MaxGower = max_dg, species = names(max_dg))

readr::write_tsv(
  max_dg2,
  file.path("./1-SDM/1_Inputs/3_Gower_dist/0_maxGower.txt")
)


## %######################################################%##
#                                                          #
####         10-Sample pseudo-absences constrained        ####
####                 by Gower distance                  ####
#                                                          #
## %######################################################%##
# List of PCA of each training area
clibarea <- list.files(
  "./1-SDM/1_Inputs/4_Predictors_by_sp/1981-2010/",
  pattern = ".tif$",
  full.names = TRUE
)
names(clibarea) <- clibarea %>%
  basename() %>%
  gsub(".tif$", "", .)

# Gower dissimilarity matrix
gower_r <- list.files(
  "./1-SDM/1_Inputs/3_Gower_dist",
  pattern = ".tif",
  full.names = TRUE
)
names(gower_r) <- gsub(".tif$", "", basename(gower_r))

# Occurrence database
occ <- data.table::fread(here(
  "occurrences_cleaned_final_FILTERED_more5.gz"
)) %>%
  as_tibble()
occ$pr_ab <- 1

occ %>%
  group_by(species) %>%
  count() %>%
  arrange(n)

db_bg <- db_pa <- list() # object to store backgroud points (db_bg) and pseudo-absences (db_pa)
sp <- occ$species %>%
  unique() %>%
  sort()

for (i in 1:length(sp)) {
  message("species ", i)
  coord <- dplyr::filter(occ, species == sp[i])
  pred <- terra::rast(clibarea[[sp[i]]]) # PCA for a specific calibration area
  pred_gow <- terra::rast(gower_r[[sp[i]]]) > 0.05
  pred_gow <- terra::mask(pred, pred_gow, maskvalue = FALSE) %>%
    terra::aggregate(2) # aggregate value to speed up

  db_pa[[i]] <-
    set.seed(15) %>%
    flexsdm::sample_pseudoabs(
      data = coord,
      x = "x",
      y = "y",
      n = nrow(coord) * 2, # absence double than presences
      method = c("kmeans", env = pred_gow), # environmentally restricted based on Gower distance - pred_gow object -
      rlayer = pred_gow
    ) %>%
    dplyr::bind_rows(coord, .)

  # Sample background points
  db_bg[[i]] <-
    flexsdm::sample_background(
      data = coord,
      x = "x",
      y = "y",
      n = 10000,
      method = "random",
      rlayer = pred,
    )

  plot(pred_gow[[1]])
  points(db_pa[[i]][, c("x", "y")])
  points(db_pa[[i]][db_pa[[i]]$pr_ab == 0, c("x", "y")], col = "red", pch = 19)

  # Data partitioning
  number_of_variables <- terra::nlyr(pred)
  if (nrow(coord) >= (number_of_variables * 2)) {
    # Blocks
    par_data <-
      flexsdm::part_sblock(
        env_layer = pred,
        data = db_pa[[i]],
        x = "x",
        y = "y",
        pr_ab = "pr_ab",
        n_part = 5, # four partitions
        min_res_mult = 20,
        max_res_mult = 300,
        num_grids = 60,
        min_occ = 5,
        prop = 0.9
      )
    # Latitudinal bands
    if (length(par_data) < 3) {
      par_data <-
        flexsdm::part_sband(
          env_layer = pred,
          data = db_pa[[i]],
          x = "x",
          y = "y",
          pr_ab = "pr_ab",
          type = "lat",
          n_part = 5,
          min_bands = 4,
          max_bands = 60,
          min_occ = 5,
          prop = 0.9
        )
    }
    # Longittudinal bands
    if (length(par_data) < 3) {
      par_data <-
        flexsdm::part_sband(
          env_layer = pred,
          data = db_pa[[i]],
          x = "x",
          y = "y",
          pr_ab = "pr_ab",
          type = "lon",
          n_part = 5,
          min_bands = 4,
          max_bands = 60,
          min_occ = 5,
          prop = 0.9
        )
    }

    # K-fold
    if (length(par_data) < 3) {
      db_pa[[i]] <- flexsdm::part_random(
        data = db_pa[[i]],
        pr_ab = "pr_ab",
        method = c(
          method = "kfold",
          folds = 5
        )
      )
      db_bg[[i]] <- flexsdm::part_random(
        data = db_bg[[i]],
        pr_ab = "pr_ab",
        method = c(
          method = "kfold",
          folds = 5
        )
      )
    }

    if (any("best_part_info" == names(par_data))) {
      terra::writeRaster(
        par_data$grid,
        file.path(
          "./1-SDM/1_Inputs/1_Occurrences",
          paste0(sp[i], "_best_part.tif")
        ),
        overwrite = TRUE
      )
      readr::write_tsv(
        par_data$best_part_info,
        file.path(
          "./1-SDM/1_Inputs/1_Occurrences",
          paste0(sp[i], "_best_part.txt")
        )
      )

      db_pa[[i]] <- par_data$part
      db_bg[[i]] <-
        flexsdm::sdm_extract(
          data = db_bg[[i]],
          x = "x",
          y = "y",
          env_layer = par_data$grid
        )
    }
  } else {
    # Partition for Ensemble of Small Model

    db_pa[[i]] <- db_pa[[i]] %>%
      flexsdm::part_random(
        # partition k-fold
        data = .,
        pr_ab = "pr_ab",
        method = c(
          method = "rep_kfold",
          folds = 5,
          replicates = 5
        )
      )
    db_bg[[i]] <- db_bg[[i]] %>%
      flexsdm::part_random(
        # partition k-fold
        data = .,
        pr_ab = "pr_ab",
        method = c(
          method = "rep_kfold",
          folds = 5,
          replicates = 5
        )
      )
  }
}

length(db_bg)
names(db_pa) <- names(db_bg) <- sp
db_pa <- bind_rows(db_pa, .id = "species")
db_bg <- bind_rows(db_bg, .id = "species")

data.table::fwrite(
  db_pa,
  file.path("./1-SDM/1_Inputs/1_Occurrences", "1_occ_presabs_randompart_1.gz")
)
data.table::fwrite(
  db_bg,
  file.path("1-SDM/1_Inputs/1_Occurrences", "1_occ_bkground_randompart_1.gz")
)


# Merge _1 and _2 datasets
db_pa1 <- data.table::fread(file.path(
  "./1-SDM/1_Inputs/1_Occurrences",
  "1_occ_presabs_randompart_1.gz"
))
db_pa2 <- data.table::fread(file.path(
  "./1-SDM/1_Inputs/1_Occurrences",
  "1_occ_presabs_randompart_2.gz"
))
db_bg1 <- data.table::fread(file.path(
  "./1-SDM/1_Inputs/1_Occurrences",
  "1_occ_bkground_randompart_1.gz"
))
db_bg2 <- data.table::fread(file.path(
  "./1-SDM/1_Inputs/1_Occurrences",
  "1_occ_bkground_randompart_2.gz"
))

db_paall$species %>%
  unique() %>%
  length()
db_paall <- bind_rows(db_pa1, db_pa2)
db_bgall <- bind_rows(db_bg1, db_bg2)

data.table::fwrite(
  db_paall,
  file.path("./1-SDM/1_Inputs/1_Occurrences", "1_occ_presabs_randompart.gz")
)
data.table::fwrite(
  db_bgall,
  file.path("1-SDM/1_Inputs/1_Occurrences", "1_occ_bkground_randompart.gz")
)

# data.table::fwrite(bind_rows(data.table::fread(file.path("./1-SDM/1_Inputs/1_Occurrences", "1_occ_bkground_randompart.gz")), db_bg),
#
#                    file.path("./1-SDM/1_Inputs/1_Occurrences", "1_occ_bkground_randompart.gz"))
