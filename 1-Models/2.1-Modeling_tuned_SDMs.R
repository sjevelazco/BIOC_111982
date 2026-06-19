## %######################################################%##
#                                                          #
####                       Modeling                      ####
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
source("sdm_predict3.R")

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
length(n_occ)

## %######################################################%##
#                                                          #
####        2-Select species with > 15 occurrences        ####
#                                                          #
## %######################################################%##
sp <- names(n_occ[n_occ >= 15]) %>% sort()

occ <- occ %>% dplyr::filter(species %in% sp)
bkg <- bkg %>% dplyr::filter(species %in% sp)


##%######################################################%##
#                                                          #
####            3-Environmental variables               ####
#                                                          #
##%######################################################%##
# Environmental variables - Current conditions
# list of rasters with environmental variable
current_foldn <- "1981-2010"
env <- file.path("./1-SDM/1_Inputs/4_Predictors_by_sp", current_foldn) %>%
  list.files(full.names = TRUE, patter = ".tif$")
names(env) <- basename(env) %>% gsub(".tif$", "", .)


# Future predictors
env_fut <- "./1-SDM/1_Inputs/4_Predictors_by_sp" %>%
  list.dirs(recursive = FALSE) %>%
  grep(current_foldn, ., invert = TRUE, value = TRUE)

names(env_fut) <- basename(env_fut)
env_fut <- as.list(env_fut)
# Avoid projecting to 2011-2040
env_fut <- env_fut[!grepl("2011-2040", names(env_fut))]

## %######################################################%##
#                                                          #
####              5-Loop to model species                ####
#                                                          #
## %######################################################%##
perf_dir <- here("1-SDM/2_Outputs/0_Model_performance")

length(sp)
i <- 1
error_sp <- NA

# Check species without model
modeled <- "./1-SDM/2_Outputs/0_Model_performance" %>%
  list.files(pattern = "_models_performance.txt")
modeled <- gsub("_models_performance.txt", "", modeled)
sp <- sp[!sp %in% modeled]
for (i in 1:length(sp)) {
  tryCatch(
    {
      message(paste("\nModeling sp", i, sp[i]))
      pa <- occ %>% dplyr::filter(species == sp[i])
      b <- bkg %>% dplyr::filter(species == sp[i])
      # remove columns filled with NAs
      pa <- pa[!apply(pa, 2, function(x) all(is.na(x)))]
      b <- b[!apply(b, 2, function(x) all(is.na(x)))]

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

      # Read species raster
      env_used <- env_r %>%
        names()

      #### Boosted regression trees ####

      m_gbm <- flexsdm::tune_gbm(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        grid = expand.grid(
          n.trees = seq(10, 200, 20),
          shrinkage = seq(0.1, 1.5, 0.2),
          n.minobsinnode = seq(1, 5, 1)
        ),
        thr = "max_sens_spec",
        metric = "SORENSEN",
        n_cores = 8 # length(pa$.part %>% unique())
      )

      if (length(m_gbm) > 1) {
        h <- m_gbm$hyper_performance
        readr::write_tsv(
          x = h,
          file = here(perf_dir, paste0(sp[i], " hyp_gbm.txt"))
        )
      }

      #### Maximum entropy ####
      tryCatch(
        {
          m_max <- flexsdm::tune_max(
            data = pa,
            response = "pr_ab",
            predictors = env_used,
            background = b,
            partition = ".part",
            grid = expand.grid(
              regmult = seq(0.1, 5, 0.2),
              classes = c("lq", "lqh", "lqhp", "lqhpt")
            ),
            thr = "max_sens_spec",
            metric = "SORENSEN",
            n_cores = 4
          )

          if (length(m_max) > 1) {
            h <- m_max$hyper_performance
            readr::write_tsv(
              x = h,
              file = here(perf_dir, paste0(sp[i], " hyp_max.txt"))
            )
          }
        },
        # if an error occurs, tell me the error
        error = function(e) {
          message("Error in maxent")
          error_sp[i] <- sp[i]
        }
      )

      #### Neural Network ####
      m_net <- tune_net(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        grid = expand.grid(
          size = (2:length(env_used)),
          decay = c(seq(0.01, 1, 0.05), 1, 3, 4, 5, 6)
        ),
        thr = "max_sens_spec",
        metric = "SORENSEN",
        n_cores = 5
      )

      if (length(m_net) > 1) {
        h <- m_net$hyper_performance
        readr::write_tsv(
          x = h,
          file = here(perf_dir, paste0(sp[i], " hyp_net.txt"))
        )
      }

      #### Random forest ####
      m_raf <- tune_raf(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        grid = expand.grid(
          mtry = seq(1, length(env_used), 1),
          ntree = c(200, 400, 600, 800, 1000, 1200)
        ),
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
      m_svm <- tune_svm(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        grid = expand.grid(
          C = seq(2, 60, 5),
          sigma = c(seq(0.001, 0.2, 0.002))
        ),
        thr = "max_sens_spec",
        metric = "SORENSEN",
        n_cores = 5
      )

      if (length(m_svm) > 1) {
        h <- m_svm$hyper_performance
        readr::write_tsv(
          x = h,
          file = here(perf_dir, paste0(sp[i], " hyp_svm.txt"))
        )
      }

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

      m_gam <- fit_gam(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        thr = "max_sens_spec",
        k = candidate_k
      )

      #### Generalized Linear Models ####
      if (nrow(pa) >= length(env_used) * 5) {
        m_glm <- fit_glm(
          data = pa,
          response = "pr_ab",
          predictors = env_used,
          partition = ".part",
          thr = "max_sens_spec",
          poly = 2
        )

        nparm <- sum(ncol(combn(env_used, 2)), length(env_used) * 2)
        if (nrow(pa) >= (nparm * 5)) {
          m_glm <- fit_glm(
            data = pa,
            response = "pr_ab",
            predictors = env_used,
            partition = ".part",
            thr = "max_sens_spec",
            poly = 2,
            inter_order = 1
          )
        }
      }

      #### Gaussian Process ####
      m_gau <- fit_gau(
        data = pa,
        response = "pr_ab",
        predictors = env_used,
        partition = ".part",
        # background = b,
        thr = "max_sens_spec"
      )

      models <- grep("^m_", ls(), value = TRUE)
      filt <- sapply(models, function(x) {
        length(get(x))
      })
      models <- models[filt > 0]

      # Filter by performance
      filt <- flexsdm::sdm_summarize(lapply(models, get))
      filt_perf <-
        round(filt$SORENSEN_mean, 1) >= 0.7 &
        filt$thr_value != 0 &
        filt$thr_value != 1
      models_perf <- models[filt_perf]

      #### Ensemble ####
      if (length(models_perf) > 1) {
        m_ensemble <-
          flexsdm::fit_ensemble(
            lapply(models_perf, get),
            ens_method = c("median"),
            thr_model = "max_sens_spec",
            thr = "max_sens_spec"
          )
      } else if (length(models_perf) == 1) {
        m_ensemble <-
          flexsdm::fit_ensemble(
            lapply(c(models_perf, models_perf), get),
            ens_method = c("median"),
            thr_model = "max_sens_spec",
            thr = "max_sens_spec"
          )
      }

      models <- grep("m_", ls(), value = TRUE)
      filt <- sapply(models, function(x) {
        length(get(x))
      })
      models <- models[filt > 0]

      # Model performance
      performance <- flexsdm::sdm_summarize(lapply(models, function(x) {
        if (length(get(x)) > 0) {
          get(x)
        }
      }))

      readr::write_tsv(
        x = performance,
        file = here(perf_dir, paste0(sp[i], "_models_performance.txt"))
      )

      if (length(models_perf) >= 1) {
        ## %######################################################%##
        #                                                          #
        ####             Predict individual models              ####
        #                                                          #
        ## %######################################################%##
        # models <- models[models!="m_ensemble"]

        message("Predicting models for species ", sp[i], " ", i)

        # models_object <- lapply(models, function(x) {
        #   get(x)
        # })

        # prd <-
        #   flexsdm::sdm_predict(
        #     models = models_object,
        #     pred = env_r,
        #     thr = c("max_sens_spec"),
        #     con_thr = TRUE,
        #     clamp = TRUE,
        #     pred_type = 'cloglog',
        #     predict_area = NULL
        #   )
        #
        # for(mm in 1:length(prd)){
        #   terra::writeRaster(prd[[mm]],
        #                      here('1-SDM/2_Outputs/1_Current',
        #                           'Algorithm',
        #                           names(prd[mm]),
        #                           paste0(sp[i], '.tif'))
        #                      , overwrite=TRUE)
        # }
        # rm(prd)

        prd <-
          predict2(
            models = m_ensemble,
            pred = env_r,
            thr = c("max_sens_spec"),
            con_thr = TRUE,
            clamp = TRUE,
            pred_type = "cloglog",
            predict_area = NULL,
            uncertainty = TRUE
          )
        terra::writeRaster(
          prd[[1]],
          here(
            "1-SDM/2_Outputs/1_Current",
            "Algorithm",
            names(prd),
            paste0(sp[i], ".tif")
          ),
          overwrite = TRUE
        )

        ##### Model projection for future conditions #####
        for (f in 1:length(env_fut)) {
          print(f)

          env_f_r <- file.path(env_fut[[f]], paste0(sp[i], ".tif")) %>%
            terra::rast()

          # prd <-
          #   sdm_predict(
          #     models = models_object,
          #     pred = env_f_r,
          #     thr = c("max_sens_spec"),
          #     con_thr = TRUE,
          #     clamp = TRUE,
          #     pred_type = "cloglog"
          #     # predict_area = range_predict
          #   )
          #
          # for (mm in 1:length(prd)) {
          #   terra::writeRaster(prd[[mm]],
          #                      here(
          #                        dirr, "2_Outputs/2_Projection",
          #                        names(env_fut[f]),
          #                        "Algorithm",
          #                        names(prd[mm]),
          #                        paste0(sp[i], ".tif")
          #                      ),
          #                      overwrite = TRUE
          #   )
          # }

          # prd_ens <- terra::rast(lapply(prd, function(x) x[[1]]))
          # prd_ens <- median_ens(m = prd_ens, thr = thr_ens)

          prd <-
            predict2(
              models = m_ensemble,
              pred = env_f_r,
              thr = c("max_sens_spec"),
              con_thr = TRUE,
              clamp = TRUE,
              pred_type = "cloglog",
              predict_area = NULL,
              uncertainty = TRUE
            )

          terra::writeRaster(
            prd[[1]],
            here(
              "1-SDM/2_Outputs/2_Projection",
              names(env_fut[f]),
              "Algorithm",
              "median",
              paste0(sp[i], ".tif")
            ),
            overwrite = TRUE
          )

          rm(prd)
        }
      }
    },
    # if an error occurs, tell me the error
    error = function(e) {
      rm(list = grep("m_", ls(), value = TRUE))
      message("An error occurred with", sp[i])
      error_sp[i] <- sp[i]
    }
  )
  rm(list = grep("m_", ls(), value = TRUE))
}


modeled <- "./1-SDM/2_Outputs/0_Model_performance" %>%
  list.files(pattern = "_models_performance.txt") %>%
  gsub("_models_performance.txt", "", .)
sp_pred <- "./1-SDM/2_Outputs/1_Current/Algorithm/median" %>%
  list.files(pattern = '.tif') %>%
  gsub(".tif$", "", .)
modeled[!modeled %in% sp_pred]
