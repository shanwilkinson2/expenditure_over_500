# download data 

# load packages
  library(dplyr)
  library(readxl)
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
           # get date part of filename
           file_date = stringr::str_replace(expenditure_filename, 
              "expenditure-over-500-for-",
              ""),
           # turn date as text into date as date
           file_date2 = lubridate::fast_strptime(file_date, 
                                                 format = "%B-%Y"),
           file_date2 = as.POSIXct(file_date2),
      # add in if they're already downloaded
      file_downloaded = ifelse(expenditure_filename %in% already_downloaded,
                               TRUE, FALSE)
    )
  
# check if anything's actually new
  message(paste("NUmber of new files available to download:", 
                length(links2$file_downloaded[FALSE])))

# need to download files first then read
# go through links to download & download if not done already 
  for(i in 1:nrow(links2)){
    if(links2$file_downloaded[i] == FALSE) {
    download.file(paste0("https://www.bolton.gov.uk", links2$link[i]),
                  destfile = paste0("./expenditure/", links2$expenditure_filename[i]),
                  )
    }
  }

 # read in downloaded files ################################

  # already_downloaded2 <- data.frame(expenditure_filename = already_downloaded) %>%
  #   mutate(
  #     # get date part of filename
  #     file_date = stringr::str_replace(expenditure_filename, 
  #                                      "expenditure-over-500-for-",
  #                                      ""),
  #     # turn date as text into date as date
  #     file_date2 = lubridate::fast_strptime(file_date, 
  #                                           format = "%B-%Y")
  #   ) 

# read in contents of all files as a list   
  files_list <- map(.x = paste0("./expenditure/", already_downloaded), 
                     .f = ~.x %>%
                      data.table::fread() %>%
                      janitor::clean_names()
                    )
 
 # applying a different date to each element, just looks like the wrong one
 files_list2 <- mapply(cbind, 
                       files_list, 
                       "file_date" = links2$file_date2,
                       SIMPLIFY = FALSE)
 

  
  # variables not all same type....
  all_expenditure <- for(i in 1:nrow(already_downloaded2)){
    filepath <- paste0("./expenditure/", already_downloaded2$expenditure_filename[i])
    
    process_file <- data.table::fread(filepath) %>%
      mutate(file_date = already_downloaded2$file_date2[i])
      
      if(already_downloaded2$expenditure_filename == "expenditure-over-500-for-june-2021"){
        process_file <- process_file[-1,]
      }
        
    }
    
    #if first file 
      if(i == 1){
        expenditure <- process_file
          # data.table::fread(filepath) %>%
          # mutate(file_date = links2$file_date2[i])
      } else {
        expenditure2 <- 
          # data.table::fread(filepath) %>%
          # mutate(file_date = links2$file_date2[i])
        expenditure <- bind_rows(expenditure, 
                  expenditure2 
                    
        )
        # return(expenditure)
      }
      
    }
 