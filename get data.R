# download data 

# load packages
  library(dplyr)
  library(readxl)
  library(XML)
  
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
  
# setup to download files
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
    # add in if they're already downloaded
      file_downloaded = ifelse(expenditure_filename %in% already_downloaded,
                               TRUE, FALSE)
    )
  
  

# need to download files first then read
# go through links to download & download if not done already  
  for(i in 1:nrow(links2)){
    if(links2$file_downloaded[i] == FALSE) {
    download.file(paste0("https://www.bolton.gov.uk", links2$link[i]),
                  destfile = paste0("./expenditure/", links2$expenditure_filename[i]),
                  )
    }
  }

# read in downloaded files 
  # variables not all same type....
  all_expenditure <- for(i in 1:nrow(links2)){
    filepath <- paste0("./expenditure/", links2$expenditure_filename[i])
    #if first file 
      if(i == 1){
        expenditure <- data.table::fread(filepath) # %>%
          #mutate(file_date = links2$file_date2[i])
      } else {
        expenditure <- bind_rows(expenditure, 
                  data.table::fread(filepath) #%>%
                    #mutate(file_date = links2$file_date2[i])
                    
        )
        # return(expenditure)
      }
      
    }
 