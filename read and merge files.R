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

# compare variables in list elements - not comparing, looking at each seperately
  # compare_df_cols(df1, df2, df3, return = "mismatch")
  colname_mismatches <- map(.x = files_list2,
                            .f = ~janitor::compare_df_cols(
                              .x, 
                              return= "mismatch")
                            )

  # not working not lists of dataframes
  colname_mismatches <- compare_df_cols(c(files_list2[[1]], 
                                          files_list2[[2]], 
                                          files_list2[[3]], 
                                          files_list2[[4]]),
                  return = "mismatch")

# # variables not all same type....
# all_expenditure <- for(i in 1:nrow(downloaded_files)){
#   filepath <- paste0("./expenditure/", already_downloaded2$expenditure_filename[i])
#   
#   process_file <- data.table::fread(filepath) %>%
#     mutate(file_date = downloaded_files$file_date2[i])
#   
#   if(already_downloaded2$expenditure_filename == "expenditure-over-500-for-june-2021"){
#     process_file <- process_file[-1,]
#   }
#   
# }
# 
# #if first file 
# if(i == 1){
#   expenditure <- process_file
#   # data.table::fread(filepath) %>%
#   # mutate(file_date = links2$file_date2[i])
# } else {
#   expenditure2 <- 
#     # data.table::fread(filepath) %>%
#     # mutate(file_date = links2$file_date2[i])
#     expenditure <- bind_rows(expenditure, 
#                              expenditure2 
#                              
#     )
#   # return(expenditure)
# }
# 
# }
