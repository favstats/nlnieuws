

library(newsanchor)
library(tidyverse)

ts <- Sys.time()

get_the_nieuws <- function(.x) {
  res <- get_headlines_all(country = "nl", category = .x) 
  res$results_df %>% 
    mutate(category = .x)
}

get_the_nieuws <- possibly(get_the_nieuws, otherwise = NULL, quiet = F)

the_nieuws <- terms_category %>% 
  unlist() %>% as.character() %>% 
  map_dfr(get_the_nieuws) %>% 
  group_by(url) %>% 
  mutate(category = paste0(category, collapse = ", ")) %>% 
  ungroup() %>% 
  distinct(url, .keep_all = T) %>% 
  mutate(tstamp = ts)


arrow::write_parquet(the_nieuws, "nieuws.parquet")


# arrow::read_parquet("nieuws.parquet")
