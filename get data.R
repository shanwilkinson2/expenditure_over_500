# get data 

# load packages
  library(dplyr)
  library(readxl)
  library(XML)
  
url <- "https://www.bolton.gov.uk/downloads/download/196/expenditure_reports"
  
# setup to download files
  doc <- htmlParse(readLines(url), 
                   asText = TRUE)
  links <- xpathSApply(doc, "//a/@href")
  free(doc)
  
  links2 <- data.frame(link = links) %>%
    filter(grepl("/downloads/file/", link))

# make new folder to put them in      
dir.create("expenditure")

# need to download files first then read
for(i in 1:nrow(links2)){
  
  # read.csv(paste0("https://www.bolton.gov.uk/", links2$link[i]))
  
  download.file(paste0("https://www.bolton.gov.uk/", links2$link[i]),
                #links2$link[i],
                #remove file dividers from file location NOT WORKING YET
                destfile = paste0(getwd(), "/expenditure", substr(links2$link[i], "expenditure-over-500", $) ),
                #mode = "wb"
                )

}

# get list of filenames & file types that have been downloaded
excel_files <- list.files(path="./licenses", pattern = ".xls") %>%
  as.data.frame()

names(excel_files) = "filename"

excel_files <- excel_files %>%
  mutate(filetype = stringr::str_extract(filename, "\\.xlsx?$"))
