
library(tidyverse)
library(newsanchor)
library(piggyback)

source("utils.R")

ts <- Sys.time()



thiry$results_df %>% filter(str_detect(name, "be")) %>% 
  distinct(name) %>% pull(name) %>% dput()

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


old_data <- arrow::read_parquet("https://github.com/favstats/nlnieuws/releases/download/nl-nieuws/nieuws.parquet") 

belgian_sites <- old_data %>% 
  filter(str_detect(name, "\\.be")) %>% 
  distinct(name) %>% 
  pull(name) %>% dput()
  c(c("Redactie24.be", "Elle.be", "Www.hln.be", "Demorgen.be", "apache.be", "tijd.be",
      "Www.vrt.be", "Nieuwsblad.be", "Hbvl.be", "Www.gva.be", "Kw.be", "Standaard.be", 
      "Www.tvl.be", "Knack.be", "Www.vrt.be", "Pal.be")) %>% 
  unique() %>% as.character() %>% str_to_lower() 

the_query <- "politiek OR regering OR partij OR poldermodel OR coalitie OR fractie OR verkiezingen"

every_dat <- get_everything(query = the_query,
                            language = "nl", 
                            exclude_domains =  belgian_sites,
                            from = ts - 60*60*24*3) %>% 
  mutate(category = the_query) %>% 
  mutate(tstamp = ts)

new_nieuws <- the_nieuws %>% 
  bind_rows(every_dat)  %>% 
  distinct(url, .keep_all = T) %>% 
  anti_join(old_data %>% select(url))

if(nrow(new_nieuws)!=0){
  
  print(paste0("New rows: ", nrow(new_nieuws)))
  
  fin_nieuws <- old_data %>% 
    bind_rows(the_nieuws) %>% 
    distinct(url, .keep_all = T)
  
  arrow::write_parquet(fin_nieuws, "nieuws.parquet")
  
  releases <- pb_releases()
  
  try({
    pb_upload_file_fr(
      "nieuws.parquet",
      repo = "favstats/nlnieuws",
      tag = "nl-nieuws",
      releases = releases
    )   
  })

} else {
  print("No new nieuws!")
}


