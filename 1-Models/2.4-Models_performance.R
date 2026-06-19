##%######################################################%##
#                                                          #
####         Final performance table and plots          ####
#                                                          #
##%######################################################%##
require(ggplot2)
require(dplyr)

thr <- "./1-SDM/2_Outputs/0_Model_performance/" %>%
  list.files(pattern = "_models_performance.txt", full.names = TRUE)
thr2 <- lapply(thr, data.table::fread)

names(thr2) <- stringr::str_split_fixed(basename(thr), "_", 2)[, 1]
thr2 <- bind_rows(thr2, .id = "species")
thr2 <- as_tibble(thr2)
# readr::write_tsv(thr2,  "./1-SDM/2_Outputs/0_Model_performance/00_performance_all_models.txt")

# Calculate number of times a species presented a model with Sorensen >= 0.7
perf <- readr::read_tsv(
  "./1-SDM/2_Outputs/0_Model_performance/00_performance_all_models.txt"
)

perf %>% filter(SORENSEN_mean >= 0.7)

count_models <- perf %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(n = sum(SORENSEN_mean >= 0.7))
sum(count_models$n > 0)

without_model <- count_models %>% dplyr::filter(n == 0) %>% pull(species)
