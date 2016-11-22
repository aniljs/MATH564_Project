



# turn a date into a 'monthnumber' relative to an origin
monthToNumber <- function(d) 
{ 
  lt <- as.POSIXlt(as.Date(paste("01-", d, sep = ""), "%d-%b-%Y"))
  return (lt$year*12 + lt$mon) 
  
  
}


# compute a month difference as a difference between two monthToNumber's
monthDifference <- function(d1, d2) 
{ 
  return (monthToNumber(d2) - monthToNumber(d1)) 
}




clean <- function(data_in)
{
  #Interest-Rate Convert from factor to string, replace % and convert to numeric
  data_in$int_rate <- as.numeric(sub("%","",sapply(data_in$int_rate, as.character)))
  #data_in$int_rate <- NULL
  
  #Enploylength -- Keep only the digits and convert to digit
  data_in$emp_length <- as.numeric(gsub("\\D","",sapply(data_in$emp_length,as.character)))
  #data_in$emp_length <- NULL
  
  # Zip code replacing xx
  #print (data_in$zip_code)
  data_in$zip_code <- as.character(sub("xx","",sapply(data_in$zip_code,as.character)))
  #data_in$zip_code <- NULL
  
  # Revol_util -- Replace % and convert to float
  data_in$revol_util <- as.numeric(sub("%","",sapply(data_in$revol_util, as.character)))
  #data_in$revol_util <- NULL
  
  # Date Transformation
  data_in$earliest_cr_line <- monthDifference(data_in$earliest_cr_line, "Dec-2016")
  
  # Remove all the columns that have more than 85% of NA
  col_names <- names(data_in)[colSums(is.na(data_in)) >= 0.85 * dim(data_in)[1]]
  
  
  return (drop_columns(data_in, col_names))
}




drop_columns <- function(data_in, col_names)
{
  for (cn in col_names) {
    data_in[,paste(cn)]<-NULL
  }
  return(data_in)
}




var_cleaner = function(data_in)
{
  data_in$loan_status.new <- data_in$loan_status == 'Fully Paid'
  data_in['loan_status.new'] <- lapply(data_in['loan_status.new'], as.integer)
  
  data_in$annual_inc <- as.integer(data_in$annual_inc)
  data_in$dti <- as.numeric(data_in$dti)
  data_in$installment <- as.numeric(data_in$installment)
  data_in$last_pymnt_amnt <- as.numeric(data_in$last_pymnt_amnt)
  data_in$installment <- as.numeric(data_in$installment)
  data_in$pymnt_plan <- as.numeric(data_in$pymnt_plan)
  data_in$out_prncp <- as.numeric(data_in$out_prncp)
  data_in$out_prncp_inv <- as.numeric(data_in$out_prncp_inv)
  data_in$recoveries <- as.numeric(data_in$recoveries)
  data_in$total_pymnt <- as.numeric(data_in$total_pymnt)
  data_in$total_pymnt_inv <- as.numeric(data_in$total_pymnt_inv)
  data_in$total_rec_int <- as.numeric(data_in$total_rec_int)
  data_in$total_rec_late_fee <- as.numeric(data_in$total_rec_late_fee)
  data_in$total_rec_prncp <- as.numeric(data_in$total_rec_prncp)
  return(data_in)
}






