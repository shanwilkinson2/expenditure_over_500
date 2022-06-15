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
  # january-2018 - 1st col unnamed, seems to be an id number
  # june-2021 - first row blank, probably empty columns at the end
  
  
  files_list3 <- map(.x = files_list2,
                     .f = ~janitor::remove_empty(.x, "cols"))

  
  # identify if all col names are v1, v2, v3 etc
  current_names <- names(files_list3[[1]])
  current_names <- current_names[1:(length(current_names)-1)]
  
  test_names <- 1:(length(current_names))
  test_names <- paste0("v", test_names)
  
  test_2 <- data.frame(current_names = current_names, 
                       test_names = test_names) %>%
    mutate(match = ifelse(current_names == test_names, TRUE, FALSE))
  

  if(sum(test_2$mismatch > 0)) {
    message("blank names")
  }
  
  my_df <- files_list3[["june-2021"]] %>%
    janitor::row_to_names(1) %>%
    clean_names()
    # last is file_date, but has removed this because it did have a colname
    names(my_df)[length(my_df)] <- "file_date"
 

  