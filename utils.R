

pb_upload_file_fr <- function (file, repo, tag, .token = gh::gh_token(), releases, dir = NULL, skip  = F) {
  # Construct the file path
  file_path <- do.call(file.path, compact(list(dir, file)))
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  # full_repos$file_name
  # Obtain release information
  # releases <- pb_releases(repo = repo, .token = .token)
  upload_url <- releases$upload_url[releases$tag == tag][1]
  # print(upload_url)
  
  if(is.na(upload_url)){
    return(NULL)
  }
  # Set up the request
  rsd <- httr::POST(
    # "POST",
    url = sub("\\{.+$", "", upload_url),
    query = list(name = basename(file_path)),
    httr::add_headers(Authorization = paste("token", .token)),
    body = httr::upload_file(file_path)
  )
  
  r <- piggyback:::parse_repo(repo)
  
  if(!is.null(httr::content(rsd)$errors[[1]]$code)){
    # tag <- "EE-last_7_days"
    print(httr::content(rsd)$errors[[1]])
    
    df <- releases[releases$tag == tag,]
    
    if(!skip){
      
      delete_asset_by_filename(owner = r[1], repo = r[2], release_id = df$release_id, filename = file_path)
      
      
      rsd <- httr::POST(
        # "POST",
        url = sub("\\{.+$", "", upload_url),
        query = list(name = basename(file_path)),
        httr::add_headers(Authorization = paste("token", .token)),
        body = httr::upload_file(file_path)
      )
      
      # } else {
      #   print("already there so we skip")
      # }
      
      httr::warn_for_status(rsd)
      # invisible(rsd)
      
      return(NULL)
      
    }
    
    
    
    
  }
  
  print(paste0("Status CODE: ", httr::status_code(rsd)))
  # Handle response
  httr::warn_for_status(rsd)
  invisible(rsd)
}


pb_info_fr <- function(repo = guess_repo(),
                       tag = NULL,
                       .token = gh::gh_token()) {
  
  r <- piggyback:::parse_repo(repo)
  
  # get all releases
  releases <- piggyback::pb_releases(repo = repo, .token = .token, verbose = FALSE)
  
  # if no releases return empty df
  if(nrow(releases) == 0) {
    return(
      data.frame(
        file_name = "",
        size = 0L,
        timestamp = .as_datetime(0),
        tag = "",
        owner = r[[1]],
        repo = r[[2]],
        upload_url = "",
        browser_download_url = "",
        api_download_url = "",
        id = "",
        state = "",
        stringsAsFactors = FALSE
      ))
  }
  
  # if tag is "latest" (and no tag is literally named "latest"), set tag to
  # GitHub's idea of latest release tag
  if(identical(tag, "latest") && !"latest" %in% releases$tag_name) {
    tag <- releases$tag_name[releases$latest]
  }
  
  # if tag is present, filter the releases to search to just the tags requested
  if(!is.null(tag)) releases <- releases[releases$tag_name %in% tag,]
  
  # get release assets and metadata for each release
  info <- piggyback:::get_release_assets(releases = releases, r = r, .token = .token)  %>% 
    bind_rows(releases %>% 
                select(tag = release_name,
                       id = release_id,
                       upload_url)) %>% 
    distinct(tag, id, upload_url, .keep_all = T)
  
  return(info)
}

# Function to delete a release asset by filename
delete_asset_by_filename <- function(owner, repo, release_id, filename, .token  = gh::gh_token()) {
  # Retrieve all assets for the specified release
  assets <- gh::gh("GET /repos/:owner/:repo/releases/:release_id/assets",
                   owner = owner, repo = repo, release_id = release_id, .token = .token)
  
  print(assets)
  
  # Find the asset by filename
  asset <- purrr::keep(assets, ~ .x$name == filename)
  
  print(glimpse(asset))
  
  # Check if the asset was found
  if (length(asset) == 1) {
    # Extract the asset ID
    asset_id <- asset[[1]]$id
    
    # Delete the asset
    gh::gh("DELETE /repos/:owner/:repo/releases/assets/:id",
           owner = owner, repo = repo, id = asset_id, .token = .token)
    
    message("Asset deleted successfully.")
  } else {
    stop("Asset not found or multiple assets with the same name exist.")
  }
}

get_full_release <- function() {
  full_repos <- pb_info_fr("favstats/nlnieuws") %>% as_tibble()
  
  return(full_repos)  
}
