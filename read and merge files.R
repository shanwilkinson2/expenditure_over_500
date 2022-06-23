# import & merge downloaded datafiles

library(dplyr)
library(purrr)
library(janitor)
library(lubridate)

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
    ) %>%
    arrange(desc(file_date2))

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
  compare_mismatches <- function(objectname){
      colname_mismatches <- compare_df_cols(objectname,
                      return = "mismatch") %>%
        data.table::transpose()
      names(colname_mismatches)<- colname_mismatches[1,]
      colname_mismatches <- colname_mismatches[-1,] 
      colname_mismatches <- colname_mismatches %>%
        mutate(file_date = names(objectname))
      View(colname_mismatches)
  }

# compare_mismatches(files_list2)
  
# cleaning
  # january-2018 - 1st col unnamed, seems to be an id number
  # june-2021 - first row blank, probably empty columns at the end
  # payment date is a mix of number (date squished into number)& text (date with -)
  # payment_date - latest, date_paid, payment_data, date_paid, payment_dates
  
  
  # remove empty columns
  files_list2 <- map(.x = files_list2,
                     .f = ~janitor::remove_empty(.x, "cols"))

 # check for blank names 
  check_toprow_blank <- function(filename, i){
    test_names <- 1:100
    test_names <- paste0("v", test_names) 
    
    # identify if all col names are v1, v2, v3 etc
    current_names <- names(filename[[i]])
    current_names <- current_names[1:(length(current_names)-1)]
    
    test_2 <- data.frame(current_names = current_names, 
                         test_names = test_names[1:length(current_names)]) %>%
      mutate(match = ifelse(current_names == test_names, TRUE, FALSE))
    return(sum(test_2$match))
  }
 
  # # change blank names- not working
  # change_blank_names <- function(filename, i){
  #   filename[[i]] <- filename[[i]] %>%
  #     janitor::row_to_names(1) %>%
  #     clean_names()
  #   # last is file_date, but has removed this because it did have a colname
  #   names(filename[[i]])[length(filename[[i]])] <- "file_date"
  # }
  
  files_list3 <- files_list2
  
for(i in 1:length(files_list3)){
  if(check_toprow_blank(files_list3, i) > 0) {
    files_list3[[i]] <- files_list3[[i]] %>%
      janitor::row_to_names(1) %>%
      clean_names() %>%
      
    # last is file_date, but has removed this because it did have a colname
    names(files_list3[[i]])[length(files_list3[[i]])] <- "file_date"
  }
  
  if(class(files_list3[[i]]$paid_date) == "character") {
    files_list3[[i]]$paid_date <- dmy(files_list3[[i]]$paid_date)
  }
  
  if(class(files_list3[[i]]$paid_date) == "integer") {
    files_list3[[i]]$paid_date <- ymd(files_list3[[i]]$paid_date)
  }
  
  if(sum(names(files_list3[[i]]) %in% "body_name")>0){
    data.table::setnames(files_list3[[i]], "body_name", "authority",
                         skip_absent = TRUE)
  }
  
  if(sum(names(files_list3[[i]]) %in% "payment_data")>0){
    data.table::setnames(files_list3[[i]], "payment_data", "payment_date")
  } %>%
  
  if(sum(names(files_list3[[i]]) %in% "serivce_area")>0){
    data.table::setnames(files_list3[[i]], "serivce_area", "service_area")
  }
  
  if(sum(names(files_list3[[i]]) %in% "decription")>0){
    data.table::setnames(files_list3[[i]], "decription", "description")
  }
  
  if(sum(names(files_list3[[i]]) %in% "payment_date")>0){
    files_list3[[i]]$payment_date2 <- ymd(files_list3[[i]]$payment_date)
    #files_list3[[i]]$paid_date <- files_list3[[date_paid]]
    #files_list3[[i]] <- subset (files_list3[[i]], select = -c(date_paid))
  }
  
  files_list3[[i]]$invoice_id <- as.character(files_list3[[i]]$invoice_id)
  files_list3[[i]]$supplier_id <- as.character(files_list3[[i]]$supplier_id)
}

  compare_colnames <- compare_df_cols(files_list3) %>% 
    data.table::transpose() %>%
    row_to_names(1) %>%
    cbind(names_from = downloaded_files$file_date2) %>%
    relocate(names_from)
compare_colnames <- compare_colnames %>%
  mutate(file_date = names(objectname))
View(colname_mismatches)
  
  
# turn into df
  files_df <- bind_rows(files_list3)
    
  
# test rename
  test_df <- data.frame (x1 = 1:10, x2 = 21:30)
  test_df <- rename(test_df, any_of(
                  c(var1 = "x1", var2 = "x2", var3 = "x3")))
  
  test_list <- list(a = data.frame (x1 = 1:10, x2 = 21:30),
                    b = data.frame (x1 = 1:10, xx2 = 31:40)
                    )

  test_list[[2]] <- rename(test_list[[2]], 
                           any_of(c(var1 = "x1", var2 = "xx2"))
  )

  test_list[[2]] <- data.table::setnames(test_list[[2]],
                                         old = c("x1", "xx2"),
                                         new = c("var1", "var2") ,
                                         skip_absent = TRUE
  )
  
  test_df <- data.table::setnames(test_df,
                                  old = c("x1", "x3"),
                                  new = c("var1", "var3"),
                                  skip_absent = TRUE
  )
  