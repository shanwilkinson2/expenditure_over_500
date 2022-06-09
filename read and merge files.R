# import & merge downloaded datafiles

library(dplyr)
library(purrr)
library(janitor)

# read in downloaded files ################################

# get list of files downloaded
  downloaded_files <- data.frame(expenditure_filename = list.files("./expenditure")) %>%
    mutate(
      # get date part of filename
      file_date = stringr::str_replace(expenditure_filename,
                                       "expenditure-over-500-for-",
                                       ""),
      # turn date as text into date as date
      file_date2 = lubridate::fast_strptime(file_date,
                                            format = "%B-%Y") %>%
        as.POSIXct()
    )

# read in contents of all files as a list   
  files_list <- map(.x = paste0("./expenditure/", 
                                downloaded_files$expenditure_filename), 
                    .f = ~.x %>%
                      data.table::fread() %>%
                      janitor::clean_names() 
  )

# add file date as a column to each list element
  files_list2 <- mapply(cbind, 
                        files_list, 
                        "file_date" = downloaded_files$file_date2,
                        SIMPLIFY = FALSE)

  names(files_list2) <- downloaded_files$file_date
  
# compare variables in list elements
  colname_mismatches <- compare_df_cols(files_list2,
                  return = "mismatch") %>%
    data.table::transpose()
  names(colname_mismatches)<- colname_mismatches[1,]
  colname_mismatches <- colname_mismatches[-1,] 
  colname_mismatches <- colname_mismatches %>%
    mutate(file_date = names(files_list2))

  colname_mismatches %>% filter(!is.na(v1))
  
# cleaning
  # has an extra row at the top
  # june-2011 
  # january-2018
  
  
  delete_extra_top_row <- function(filename) {
    
  }
  
  names(files_list2[["june-2021"]]) <- head(files_list2[["june-2021"]],1)
head(files_list2[["june-2021"]],1)

df <- files_list2[["june-2021"]]
names(df) <- as.character(df[1,])
df <- df[-1,]
  
                  