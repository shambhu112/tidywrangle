
library(R6)
library(tidyverse)

tidy_loader <- R6Class("tidy_load",
                       public = list(
                           dic = NULL ,
                           source_df = NULL,
                           data = NULL,
                           initialize = function(dictionary) {
                               self$dic <- dictionary
                           },
                           prep_df = function(source_df){
                               self$source_df <- source_df
                               data_cols <- colnames(source_df)
                               dic_colnames <- self$dic$original_colnm
                               
                               ###  DQ check 1 : match dictionary to dataframr for col names ---- 
                               cli::cli_h3("Step 1: check 1: match col names with dictionary)")
                               
                               x <- list(dictionary = dic_colnames , data = data_cols)
                               inter <- intersect(dic_colnames , data_cols)
                               dic_only <-  dic_colnames[dic_colnames %notin%  inter]
                               data_only <-  data_cols[data_cols %notin%  inter]
                               
                               if(length(dic_only) >0 ){
                                   cli::cli_alert_warning(" {length(dic_only)} Columns found in dictionary but not in data ")
                                   cli::cli_ol()
                                   for(x in 1:length(dic_only)){
                                       cli::cli_li("\t  {dic_only[x]}")  
                                   }
                                   cli::cli_end()
                               } else if(length(data_only) >0){
                                   cli::cli_alert_warning("Columns found in data but not in dictionary ")
                                   cli::cli_ol()
                                   for(x in 1:length(data_only)){
                                       cli::cli_li("\t  {data_only[x]}")  
                                   }
                                   cli::cli_end()
                                   
                               } else {
                                   cli::cli_alert_success(" Columns Names match in dataset and dictionary  ")
                               }
                               
                               
                               ###  DQ Check 2: pull data ignoring cols marked as ignore ----
                               cli::cli_h3("Step 2: check2 - Drop cols marked for drop in dictionary")
                               
                               data <- source_df
                               ignore_cols <- which(as.logical(self$dic$drop))
                               if(length(ignore_cols) > 0){
                                   data <- data[-c(ignore_cols)]
                                   cli::cli_alert_success(" columns dropped count = {length(ignore_cols)} : {names(source_df)[ignore_cols]} dropped from dataset")
                               } else{
                                   cli::cli_alert_success(" No columns droped")
                               }
                               
                               
                               ###  DQ Check 3: Rename Cols ----
                               # Note: this is name based look at each col name level
                               cli::cli_h3("Step 3 - rename columns")
                               new_names <- sapply(colnames(data), function(x){
                                   new_name <- self$dic[self$dic$original_col == x,]$target_col
                                   if(is_valid_str(new_name))
                                       return(new_name)
                                   else
                                       return(x)
                               })
                               colnames(data) <- new_names
                               self$data <- data
                               cli::cli_alert_success("Target df set with cols = {ncol(self$data)} , rows = {nrow(self$data)}")
                               
                           },
                           print = function() {
                               cli::cli_alert_success("dictionary loaded , variable count = {nrow(self$dic)}")
                               if(is.null(self$source_df)){
                                   cli::cli_alert_info(" Soure data is not yet loaded")   
                               }else{
                                   cli::cli_alert_success("Source data : Rows = {nrow(self$source_df)},column count= {ncol(self$source_df)}")
                               }
                               
                               
                               if(is.null(self$data)){
                                   cli::cli_alert_info(" Target dataset is not created")   
                               }else{
                                   cli::cli_alert_success("Target data : Rows = {nrow(self$data)},column count= {ncol(self$data)}")
                               }
                           }
                       )
)

# Utility methods

'%notin%' <- Negate('%in%')

# Create dictionary based on a data_frame
create_dictory_df <- function(sourcedf){
    old_names <- colnames(sourcedf)
    new_names <- janitor::make_clean_names(old_names)
    
    new_dic <- data.frame("original_colnm" = old_names , "newcol_nm" = new_names , "drop" = NA , "na_threshhold" = 0.0 )
    
}

# write the dictionary to csv or xlsx or google
write_dictionary <- function( dict , to = "csv" , filename = "dictionary"){
    ret <- "NULL"
    if(to == "google"){
        ret <- googlesheets4::write_sheet(data = dict , ss = filename , sheet = "dictionary")
    }else if(to == "excel"){
        nm <- paste(filename , ".xlsx" , sep = "")
        write_excel_csv(x = dict , file = nm)
        ret <- nm
    }else {
        nm <- paste(filename , ".csv" , sep = "")
        ret <- write_csv(x = dict , file = nm)
        ret <- nm
    }
    
    cli::cli_alert_info("dictionary file written to {to} - {ret}")
    ret
    
}

is_valid_str <- function(str){
    if(length(str) == 0) return(FALSE)
    if(is.null(str)) return(FALSE)
    if(is.na(str)) return(FALSE)
    return(nzchar(str))
}


cli_list <- function(l){
    cli::cli_ol()
    for(x in l){
        cli::cli_li(x)
    }
    cli::cli_end()
}


changeColumName <- function(df , original , newname){
    names(df)[names(df) == original] <-  newname
    return(df)
}

replace_na_with_zero <- function(df){
    df[is.na(df)] <- 0
    return(df)
}

replace_zero_with_str <- function(vec , str){
    vec[which(vec == 0)] <- str
    return(vec)
}

