# download data 

# load packages
  library(dplyr)
  library(XML)
  library(purrr)
  
  # check if folder already exists to put downloaded files in
  # if it does, read in files already there
  # if not make folder  
  if(file.exists("expenditure")) {
    message("Folder already exists")
    already_downloaded <- list.files("./expenditure")
  } else{
  dir.create("expenditure")  
  }
  
url <- "https://www.bolton.gov.uk/downloads/download/196/expenditure_reports"
  
# check what's on web page to download files
  doc <- htmlParse(readLines(url), 
                   asText = TRUE)
  links <- xpathSApply(doc, "//a/@href")
  free(doc)
  
  links2 <- data.frame(link = links) %>%
    # get list of what's on the page, filter out anything except downloads
    filter(grepl("/downloads/file/", link)) %>%
    # get rid of duplicates
    unique() %>%
    # get rid of folder structure bits to just get filename
    mutate(expenditure_filename =   stringr::str_replace(link, 
                                          "/downloads/file/\\d+/",
                                          ""
                                          ),
      # add in if they're already downloaded
      file_downloaded = ifelse(expenditure_filename %in% already_downloaded,
                               TRUE, FALSE)
    )
  
# check if anything's actually new
  message(paste("NUmber of new files available to download:", 
                length(links2$file_downloaded[FALSE])))

# go through links to download & download if not done already 
  for(i in 1:nrow(links2)){
    if(links2$file_downloaded[i] == FALSE) {
    download.file(paste0("https://www.bolton.gov.uk", links2$link[i]),
                  destfile = paste0("./expenditure/", links2$expenditure_filename[i]),
                  )
    }
  }

 