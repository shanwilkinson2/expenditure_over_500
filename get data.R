# download data 

# load packages
  library(dplyr)
  library(readxl)
  library(XML)
  
  # make new folder to put them in (only need to do once)   
  # delete folder & contents if it already exists
  if(file.exists(expenditure)) {
    unlink(expenditure)
    cat("files deleted")
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
    mutate(new_filename =   stringr::str_replace(link, 
                                          "/downloads/file/\\d+/",
                                          ""
                                          ),
           # get date part of filename
           file_date = stringr::str_replace(new_filename, 
              "expenditure-over-500-for-",
              ""),
           # turn date as text into date as date
           file_date2 = lubridate::fast_strptime(file_date, 
                                                 format = "%B-%Y")
    )

# need to download files first then read
for(i in 1:nrow(links2)){
  
  # read.csv(paste0("https://www.bolton.gov.uk/", links2$link[i]))
  
  download.file(paste0("https://www.bolton.gov.uk", links2$link[i]),
                #remove file dividers from file location NOT WORKING YET
                destfile = paste0("./expenditure/", links2$new_filename[i]),
                #mode = "wb"
                )

}

# get list of filenames & file types that have been downloaded
excel_files <- list.files(path="./licenses", pattern = ".xls") %>%
  as.data.frame()

names(excel_files) = "filename"

excel_files <- excel_files %>%
  mutate(filetype = stringr::str_extract(filename, "\\.xlsx?$"))
