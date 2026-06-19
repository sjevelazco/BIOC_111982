## %######################################################%##
#                                                          #
####            Ensemble of small models                  ####
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
  require(ggplot2)
}
memory.limit(1000000)


## %######################################################%##
#                                                          #
####             1-Read occurrences databases             ####
#                                                          #
## %######################################################%##
occ <- data.table::fread(here(
  "1-SDM/1_Inputs/1_Occurrences",
  "1_occ_presabs_randompart.gz"
)) %>%
  tibble()
bkg <- data.table::fread(here(
  "1-SDM/1_Inputs/1_Occurrences",
  "1_occ_bkground_randompart.gz"
)) %>%
  tibble()


# Count number of presences
n_occ <- occ %>%
  dplyr::filter(pr_ab == 1) %>%
  dplyr::pull(species) %>%
  table() %>%
  sort()


## %######################################################%##
#                                                          #
####        2-Filter species with < 15 occurrences        ####
#                                                          #
## %######################################################%##
sp <- names(n_occ[n_occ < 15]) %>% sort()


occ <- occ %>% dplyr::filter(species %in% sp)
bkg <- bkg %>% dplyr::filter(species %in% sp)

## %######################################################%##
#                                                          #
####               3-Loop to model species                ####
#                                                          #
## %######################################################%##
perf_dir <- here("1-SDM/2_Outputs/0_Model_performance")

# List of rasters with environmental variable
env <- "./1-SDM/1_Inputs/3_Calibration_area/" %>%
  list.files(full.names = TRUE, patter = ".tif$")
names(env) <- basename(env) %>% gsub(".tif$", "", .)
env <- env[sp]
error_sp <- NA

# Loop para procesar las especies
i <- 1
length(sp) / 3
for (i in 1:56) {
  tryCatch(
    {
      message(paste("\nModeling sp", i, sp[i]))
      pa <- occ %>% dplyr::filter(species == sp[i])
      b <- bkg %>% dplyr::filter(species == sp[i])

      # Extract environmental variables
      env_r <- terra::rast(env[sp[i]])

      pa <- sdm_extract(
        pa,
        x = "x",
        y = "y",
        env_layer = env_r,
        filter_na = TRUE
      )
      b <- sdm_extract(b, x = "x", y = "y", env_layer = env_r, filter_na = TRUE)

      pa <- pa[apply(pa, 2, function(x) !all(is.na(x)))]
      b <- b[apply(b, 2, function(x) !all(is.na(x)))]

      # Read species raster
      env_r <- terra::rast(env[sp[i]])

      env_used <- env_r %>%
        names()

      #### Boosted regression trees ####
      m_gbm <- flexsdm::esm_gbm(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        thr = "max_sens_spec"
      )

      #### Maximum entropy ####
      try(
        m_max <- flexsdm::esm_max(
          data = pa,
          response = "pr_ab",
          predictors = env_used,
          background = b,
          partition = ".part",
          thr = "max_sens_spec"
        )
      )

      #### Neural Network ####
      m_net <- esm_net(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        thr = "max_sens_spec"
      )

      #### Random forest ####
      m_raf <- tune_raf(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        grid = expand.grid(mtry = seq(1, length(env_used), 1)),
        thr = "max_sens_spec",
        metric = "SORENSEN",
        n_cores = 5
      )

      if (length(m_raf) > 1) {
        h <- m_raf$hyper_performance
        readr::write_tsv(
          x = h,
          file = here(perf_dir, paste0(sp[i], " hyp_raf.txt"))
        )
      }

      #### Support Vector Machine ####
      m_svm <- esm_svm(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        thr = "max_sens_spec"
      )

      #### Generalized Additive Model ####
      n_t <- flexsdm:::n_training(data = pa, partition = ".part")

      candidate_k <- 20
      while (
        any(
          n_t <
            flexsdm:::n_coefficients(
              data = pa,
              predictors = env_used,
              k = candidate_k
            )
        )
      ) {
        candidate_k <- candidate_k - 3
      }

      m_gam <- esm_gam(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        thr = "max_sens_spec",
        k = candidate_k
      )

      #### Generalized Linear Models ####
      if (sum(pa$pr_ab == 1) >= 2) {
        m_glm <- esm_glm(
          data = pa,
          response = "pr_ab",
          predictors = env_used,
          partition = ".part",
          thr = "max_sens_spec",
          poly = 2
        )
      }

      models <- grep("m_", ls(), value = TRUE)
      filt <- sapply(models, function(x) {
        length(get(x))
      })
      models <- models[filt > 1]

      # Filter by performance
      filt <- flexsdm::sdm_summarize(lapply(models, get))
      filt_perf <-
        round(filt$SORENSEN_mean, 1) >= 0.7 &
        filt$thr_value != 0 &
        filt$thr_value != 1
      best_models <- models[filt_perf]

      # Model performance
      performance <- flexsdm::sdm_summarize(lapply(models, function(x) {
        if (length(get(x)) > 0) {
          get(x)
        }
      }))

      readr::write_tsv(
        x = performance,
        file = here(perf_dir, paste0(sp[i], "_best_modelsormance.txt"))
      )

      if (length(best_models) >= 1) {
        ## %######################################################%##
        #                                                          #
        #### Predict individual models only for best algorithm  ####
        #                                                          #
        ## %######################################################%##

        message("Predicting models for species ", sp[i], " ", i)
        for (iii in 1:length(best_models)) {
          prd <-
            flexsdm::sdm_predict(
              models = get(best_models[iii]),
              pred = env_r,
              thr = c("max_sens_spec"),
              con_thr = TRUE,
              clamp = TRUE,
              pred_type = "cloglog",
              predict_area = NULL
            )

          terra::writeRaster(
            prd[[1]],
            file.path(
              "1-SDM/2_Outputs/1_Current",
              "Algorithm",
              gsub("esm_", "", names(prd[1])[1]),
              paste0(sp[i], ".tif")
            ),
            overwrite = TRUE
          )
          rm(prd)
        }

        ## %######################################################%##
        #                                                          #
        ####           Calculate ensemble only using             ####
        ####           rasters based on median approach          ####
        #                                                          #
        ## %######################################################%##

        prd_cont <-
          file.path(
            "1-SDM/2_Outputs/1_Current",
            "Algorithm",
            gsub("m_", "", best_models),
            paste0(sp[i], ".tif")
          ) %>%
          terra::rast()
        prd_cont <- prd_cont[[grep(
          "max_sens_spec",
          names(prd_cont),
          invert = TRUE,
          value = TRUE
        )]] %>%
          terra::app(., fun = median, cores = 1)

        # calculate threshold
        # pa_2 <- flexsdm::sdm_extract(
        #   data =
        #     dplyr::select(pa, x, y, pr_ab),
        #   x = "x",
        #   y = "y",
        #   env_layer = prd_cont
        # ) %>%
        #   as.data.frame()
        #
        # pa_2 <- split(x = pa_2[, "median"], f = pa_2$pr_ab)

        thr_val <- performance[filt_perf, ] %>% pull("thr_value") %>% median() # median of threshold are calculated
        prd_bin <- prd_cont >= thr_val
        prd_bin <- prd_cont * prd_bin
        prd <- c(prd_cont, prd_bin)
        names(prd) <- c("median", "max_sens_spec")

        # if (sum(pa$pr_ab == 1) <= 10) {
        #
        #   thr_val <- flexsdm::sdm_eval(
        #     p = pa_2$`1`, a = pa_2$`0`,
        #     thr = c("lpt")
        #   ) %>% pull(thr_value)
        #   prd_bin <- prd_cont >= thr_val
        #   prd_bin <- prd_cont * prd_bin
        #   prd <- c(prd_cont, prd_bin)
        #   names(prd) <- c("median", "lpt")
        #
        # } else if (sum(pa$pr_ab == 1) > 10) {
        #
        #   thr_val <- flexsdm::sdm_eval(
        #     p = pa_2$`1`, a = pa_2$`0`,
        #     thr = c("sensitivity", sens = "0.90")
        #   ) %>% pull(thr_value)
        #   prd_bin <- prd_cont >= thr_val
        #   prd_bin <- prd_cont * prd_bin
        #   prd <- c(prd_cont, prd_bin)
        #   names(prd) <- c("median", "sensitivity")
        #
        # }

        terra::writeRaster(
          prd,
          file.path(
            "1-SDM/2_Outputs/1_Current",
            "Algorithm",
            "median",
            paste0(sp[i], ".tif")
          ),
          overwrite = TRUE
        )
        # plot(prd, main=paste("sp", i))
      }
    },
    # if an error occurs, tell me the error
    error = function(e) {
      message("An error occurred with", sp[i])
      error_sp[i] <- sp[i]
    }
  )
  rm(list = grep("m_", ls(), value = TRUE))
}
