# import & merge downloaded datafiles

library(dplyr)
library(purrr)
library(janitor)
library(lubridate)

# read in output
 files_df <- readRDS("./app/expenditure_over_500.RDS")

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
        as.Date()
    ) %>%
    arrange(desc(file_date2))

# read in contents of all files as a list   
  files_list <- map(.x = paste0("./expenditure/", 
                                downloaded_files$expenditure_filename), 
                    .f = ~.x %>%
                      data.table::fread() %>%
                      mutate(across(cols= everything, .fns = ~stringr::str_replace(., "<NA>", NA_character_))) %>%
                      janitor::remove_empty(which = c("rows", "cols")) %>%
                      janitor::clean_names() 
  )

# add file date as a column to each list element
  files_list2 <- mapply(cbind, 
                        files_list, 
                        "file_date" = downloaded_files$file_date2,
                        SIMPLIFY = FALSE)

  names(files_list2) <- downloaded_files$file_date
 
# remove empty columns
  files_list2 <- map(.x = files_list2,
                     .f = ~janitor::remove_empty(.x, "cols"))  
   
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
  # january-2018 - 1st col unnamed (v1), seems to be an id number
  # june-2021 - first row blank, probably empty columns at the end
  # payment date is a mix of number (date squished into number yyyymmdd)
    #  & text (ddmmyyyy) & text (yyyymmdd) which reads as date
  # july-2019- invoice_date only date
  # april-2016 - date_invoiced as well as payment_date
  # august-2016 - invoice = invoiced amount
  # april-2016 has all of: supplier_id, invoice_id, supplier_invoice_id
  # invoice date in april-2016 dd/mm/yyyy, august 2019 dd-mmm-yy
  # january-2018 'date' is among files with only payment_date, so assumed it is this not invoice date

# function - check for blank names 
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
  
  
  files_list3 <- files_list2
  
for(i in 1:length(files_list3)){
    # if >5 variable names match those given to blank colnames,
    # take colnames from top row
  if(check_toprow_blank(files_list3, i) >5) {
    files_list3[[i]] <- files_list3[[i]] %>%
      janitor::row_to_names(1) %>%
      clean_names()
    # last is file_date, but has removed this because it did have a colname
    names(files_list3[[i]])[length(files_list3[[i]])] <- "file_date"
  }
  
  # one is date as a number as yymmdd, convert to date
  if(class(files_list3[[i]]$paid_date) == "integer") {
    files_list3[[i]]$paid_date <- ymd(files_list3[[i]]$paid_date)
  }
  
  # rename col names
  files_list3[[i]] <- files_list3[[i]] %>%
    rename_at(vars(matches(c("body_name", "boday_name"))), ~"authority") %>%
    rename_at(vars(matches(c("servcie_area", "serivce_area", "directorate"))), ~"service_area") %>%
    rename_at(vars(matches(c("body_name", "boday_name"))), ~"authority") %>%
    rename_at(vars(matches(c("invoice_line_amount", "^invoice$", 
                             "^invoiced$", "^invoiced_a$", "invoice_distribution_amount",
                             "spend_exc_vat"))), ~"amount") %>%
    rename_at(vars(matches(c("^expense$", "expenses_type"))), ~"expense_type") %>%
    rename_at(vars(matches(
      c("payment_dates", "payment_data", "paid_date", 
        "^date$", "date_paid"))), ~"payment_date") %>%
    rename_at(vars(matches(c("date_invoiced"))), ~"invoice_date") %>%
    rename_at(vars(matches(
      c("subjective_description", "subjective_descr", "subjective_name",
        "^description$", "decription", "gl_desc", "expense_type"))), ~"subject_description") %>%
    rename_at(vars(matches(c("^cost_centre$"))), ~"cost_centre_code") %>%
    rename_at(vars(matches(c("supp_id"))), ~"supplier_id") %>%
    rename_at(vars(matches(c("^subjective_codes$", "gl_code"))), ~"subjective_code") %>%
    rename_at(vars(matches(c("^cost_centre_desc$"))), ~"cost_centre_name") %>%
    rename_at(vars(matches(c("supplier_name"))), ~"supplier") %>%
    rename_at(vars(matches(c("^v1$"))), ~"unknown_id") 
  
   # payment date - change date as a number as yyyymmdd, to date
  if(class(files_list3[[i]]$payment_date) == "integer") {
    files_list3[[i]]$payment_date <- ymd(files_list3[[i]]$payment_date)
  }
  
  # payment date - change date as character to date as date
  if(is.character(files_list3[[i]]$payment_date) & 
     files_list3[[i]]$file_date != "2010-12-01") {
    files_list3[[i]]$payment_date <- dmy(files_list3[[i]]$payment_date)
  }
  
  # payment date - change december-2010 date as character to date as date
  # yyyymmdd numbers but stored as text
  if(files_list3[[i]]$file_date == "2010-12-01") {
    files_list3[[i]]$payment_date <- ymd(files_list3[[i]]$payment_date)
  }
    
    # invoice date - change date as character to date as date
  if(is.character(files_list3[[i]]$invoice_date)) {
     files_list3[[i]]$invoice_date <- dmy(files_list3[[i]]$invoice_date)
  }
   
  # convert ids to all character, some are number  
  files_list3[[i]]$invoice_id <- as.character(files_list3[[i]]$invoice_id)
  files_list3[[i]]$supplier_id <- as.character(files_list3[[i]]$supplier_id)

}  
  
  compare_colnames <- compare_df_cols(files_list3) %>% 
    data.table::transpose() %>%
    row_to_names(1) %>%
    cbind(from_filedate = downloaded_files$file_date2) %>% 
    cbind(from_filename = downloaded_files$file_date) %>%
    relocate(c(from_filedate, from_filename))

########################################################### 
   
# turn into df
  files_df <- bind_rows(files_list3) %>%
 # turn amount to number by removing , £
    mutate(
      amount = stringr::str_remove(amount, "Â"),
      amount = stringr::str_remove(amount, "£"),
      amount = stringr::str_remove_all(amount, ","),
      amount = stringr::str_remove(amount, ".$"),
      amount = as.numeric(amount),
      
      supplier = stringr::str_replace(supplier, "^$", NA_character_),
      service_area = stringr::str_replace(service_area, "^$", NA_character_),
      subject_description = stringr::str_replace(subject_description, "^$", NA_character_),
      ) %>% 
    # get rid of rows with blank amount - they are empty rows except for the file date
    filter(!is.na(amount),
           !is.na(supplier))
  

     
#####################################
  
# save cleaned df
  data.table::fwrite(files_df, "expenditure_over_500.csv")
    
# save for app
  saveRDS(files_df, "./app/expenditure_over_500.RDS")
  