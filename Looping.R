# Loading Libraries

library(readxl)
library(tidyr)
library(stringr)
library(dplyr)

# Setting Up Directory and Gathering a List

list_files <- list.files(path = "CSV.Files")

list_files_loans_6 <- list_files <- list.files(path = "CSV.Files/Loans.6")

# Looping for Bringing in Excel Files

# Loan.6 Looping

## Loop Length

loan_length_6 <- length(list_files_loans_6)
for (i in 1:loan_length_6) {

## i is what cycle the loop is on
  
loan_filename_6 <- list_files_loans_6[i] 

## Choosing Excel Files to Loop and Creating Path Names

path_name_loans_6 <-  paste0("CSV.Files/Loans.6/", loan_filename_6)
print(path_name_loans_6)

temp_dataframe_6 <- read_excel(path_name_loans_6, sheet = 2, col_names = TRUE, col_types = NULL, na = "",
                            skip = 5) 

## Changing Column Names

colnames(temp_dataframe_6) <- c("OPE.ID", "School", "State", "Zip.Code", "School.Type", "Recipients.1", "Loans.No.1", "Loans.Sum.1", "Disbursements.No.1", "Disbursements.Sum.1", "Recipients.2", "Loans.No.2", "Loans.Sum.2", "Disbursements.No.2", "Disbursements.Sum.2",
                               "Recipients.3", "Loans.No.3", "Loans.Sum.3", "Disbursements.No.3", "Disbursements.Sum.3", "Recipients.4", "Loans.No.4", "Loans.Sum.4", "Disbursements.No.4", "Disbursements.Sum.4", "Recipients.5", "Loans.No.5", "Loans.Sum.5", "Disbursements.No.5", "Disbursements.Sum.5",
                               "Recipients.6", "Loans.No.6", "Loans.Sum.6", "Disbursements.No.6", "Disbursements.Sum.6")

temp_dataframe_6 <- temp_dataframe_6[c("School", "State", "School.Type", "Disbursements.Sum.1", "Disbursements.Sum.2", "Disbursements.Sum.3", "Disbursements.Sum.4", "Disbursements.Sum.5", "Disbursements.Sum.6")]
                                   
temp_dataframe_6$Total.Disbursements <- temp_dataframe_6$Disbursements.Sum.1 + temp_dataframe_6$Disbursements.Sum.2 + temp_dataframe_6$Disbursements.Sum.3 + temp_dataframe_6$Disbursements.Sum.4 + temp_dataframe_6$Disbursements.Sum.5 + temp_dataframe_6$Disbursements.Sum.6

temp_dataframe_6 <- temp_dataframe_6[c("School", "State", "School.Type", "Total.Disbursements")]

#temp_dataframe <- subset(temp_dataframe, State=="CT")

temp_dataframe_6$Year <- loan_filename_6

temp_dataframe_6$Year <- gsub("DL_Dashboard_AY", "", temp_dataframe_6$Year)
temp_dataframe_6$Year <- gsub("_Q4.xls", "", temp_dataframe_6$Year)

if (i == 1) {
mega_dataframe_6 <- temp_dataframe_6  
} else (
  mega_dataframe_6 <- rbind(mega_dataframe_6, temp_dataframe_6)
)



}

## Exporting

write.csv(mega_dataframe_6, "loans.10-12.csv")


# Gathering a List

list_files <- list.files(path = "CSV.Files")

list_files_loans_5 <- list_files <- list.files(path = "CSV.Files/Loans.5")

# Loan.5 Looping

## Loop Length

loan_length_5 <- length(list_files_loans_5)
for (i in 1:loan_length_5) {
  
  ## i is what cycle the loop is on
  
  loan_filename_5 <- list_files_loans_5[i] 
  
  ## Choosing Excel Files to Loop and Creating Path Names
  
  path_name_loans_5 <- paste0("CSV.Files/Loans.5/", loan_filename_5)
  print(path_name_loans_5)
  
  temp_dataframe_5 <- read_excel(path_name_loans_5, sheet = 2, col_names = TRUE, col_types = NULL, na = "",
                                 skip = 5) 
  
  ## Changing Column Names
  
  colnames(temp_dataframe_5) <- c("OPE.ID", "School", "State", "Zip.Code", "School.Type", "Recipients.1", "Loans.No.1", "Loans.Sum.1", "Disbursements.No.1", "Disbursements.Sum.1", "Recipients.2", "Loans.No.2", "Loans.Sum.2", "Disbursements.No.2", "Disbursements.Sum.2",
                                  "Recipients.3", "Loans.No.3", "Loans.Sum.3", "Disbursements.No.3", "Disbursements.Sum.3", "Recipients.4", "Loans.No.4", "Loans.Sum.4", "Disbursements.No.4", "Disbursements.Sum.4", "Recipients.5", "Loans.No.5", "Loans.Sum.5", "Disbursements.No.5", "Disbursements.Sum.5")
                                
  temp_dataframe_5 <- temp_dataframe_5[c("School", "State", "School.Type", "Disbursements.Sum.1", "Disbursements.Sum.2", "Disbursements.Sum.3", "Disbursements.Sum.4", "Disbursements.Sum.5")]
  
  temp_dataframe_5$Total.Disbursements <- temp_dataframe_5$Disbursements.Sum.1 + temp_dataframe_5$Disbursements.Sum.2 + temp_dataframe_5$Disbursements.Sum.3 + temp_dataframe_5$Disbursements.Sum.4 + temp_dataframe_5$Disbursements.Sum.5
  temp_dataframe_5 <- temp_dataframe_5[c("School", "State", "School.Type", "Total.Disbursements")]
  
  #temp_dataframe <- subset(temp_dataframe, State=="CT")
  
  temp_dataframe_5$Year <- loan_filename_5
  
  temp_dataframe_5$Year <- gsub("DL_Dashboard_AY", "", temp_dataframe_5$Year)
  temp_dataframe_5$Year <- gsub("_Q4.xls", "", temp_dataframe_5$Year)
  
  if (i == 1) {
    mega_dataframe_5 <- temp_dataframe_5  
  } else (
    mega_dataframe_5 <- rbind(mega_dataframe_5, temp_dataframe_5)
  )
  
  
  
}

## Exporting

write.csv(mega_dataframe_5, "loans.12-16.csv")





# Grant.3 Looping

## File Names

list_files_grants_3 <- list_files <- list.files(path = "CSV.files/Grants.3")

## Loop Length

grant_length_3 <- length(list_files_grants_3)
for (i in 1:grant_length_3) {
  
  ## i is what cycle the loop is on
  
  grant_filename_3 <- list_files_grants_3[i] 
  
  ## Choosing Excel Files to Loop and Creating Path Names
  
  path_name_grants_3 <-  paste0("CSV.Files/Grants.3/", grant_filename_3)
  print(path_name_grants_3)
  
  temp_dataframe_3 <- read_excel(path_name_grants_3, sheet = 3, col_names = TRUE, col_types = NULL, na = "",
                               skip = 5) 
  
  ## Changing Column Names

  colnames(temp_dataframe_3) <- make.names(colnames(temp_dataframe_3))
  
  colnames(temp_dataframe_3) <- c("OPE.ID", "School", "State", "Zip.Code", "School.Type", "YTD.Recipients.1", "YTD.Disbursements.1", "YTD.Recipients.2", "YTD.Disbursements.2", "YTD.Recipients.3", "YTD.Disbursements.3")

temp_dataframe_3 <- temp_dataframe_3[c("School", "State", "School.Type", "YTD.Disbursements.1", "YTD.Disbursements.2", "YTD.Disbursements.3")]
  
  temp_dataframe_3$Total.Disbursements <- temp_dataframe_3$YTD.Disbursements.1 + temp_dataframe_3$YTD.Disbursements.2 + temp_dataframe_3$YTD.Disbursements.3
  
  temp_dataframe_3 <- temp_dataframe_3[c("School", "State", "School.Type", "Total.Disbursements")]
  
  temp_dataframe_3$Year <- grant_filename_3
  
  temp_dataframe_3$Year <- gsub("Q4", "", temp_dataframe_3$Year)
  temp_dataframe_3$Year <- gsub("AY.xls", "", temp_dataframe_3$Year)
  
  if (i == 1) {
    mega_dataframe_3 <- temp_dataframe_3  
  } else (
    mega_dataframe_3 <- rbind(mega_dataframe_3, temp_dataframe_3)
  )
  
  
  
}

## Exporting

write.csv(mega_dataframe_3, "grants.11-16.csv")






# Grant.5 Looping

## File Names

list_files_grants_5 <- list_files <- list.files(path = "CSV.files/Grants.5")

## Loop Length

grant_length_5 <- length(list_files_grants_5)
  
  ## i is what cycle the loop is on
  
  grant_filename_5 <- list_files_grants_5 
  
  ## Choosing Excel Files to Loop and Creating Path Names
  
  path_name_grants_5 <- paste0("CSV.Files/Grants.5/", grant_filename_5)
  print(path_name_grants_5)
  
  temp_dataframe.5 <- read_excel(path_name_grants_5, sheet = 3, col_names = TRUE, col_types = NULL, na = "",
                                 skip = 5) 
  
  ## Changing Column Names
  
  colnames(temp_dataframe.5) <- make.names(colnames(temp_dataframe.5))
  
  colnames(temp_dataframe.5) <- c("OPE.ID", "School", "State", "Zip.Code", "School.Type", "YTD.Recipients.1", "YTD.Disbursements.1", "YTD.Recipients.2", 
                                  "YTD.Disbursements.2", "YTD.Recipients.3", "YTD.Disbursements.3", "YTD.Recipients.4", "YTD.Disbursements.4",
                                  "YTD.Recipients.5", "YTD.Disbursements.5") 
  
  temp_dataframe.5 <- temp_dataframe.5[c("School", "State", "School.Type", "YTD.Disbursements.1", "YTD.Disbursements.2", "YTD.Disbursements.3", "YTD.Disbursements.4", "YTD.Disbursements.5")]
  
  temp_dataframe.5$Total.Disbursements <- temp_dataframe.5$YTD.Disbursements.1 + temp_dataframe.5$YTD.Disbursements.2 + temp_dataframe.5$YTD.Disbursements.3 + temp_dataframe.5$YTD.Disbursements.4 + temp_dataframe.5$YTD.Disbursements.5
  
  temp_dataframe.5 <- temp_dataframe.5[c("School", "State", "School.Type", "Total.Disbursements")]
  
  #temp_dataframe2 <- subset(temp_dataframe2, State=="CT")
  
  #temp_dataframe2 <- temp_dataframe2[!grepl("HIGH", temp_dataframe2$School),]
  
  temp_dataframe.5$Year <- grant_filename_5
  
  temp_dataframe.5$Year <- gsub("Q4", "", temp_dataframe.5$Year)
  temp_dataframe.5$Year <- gsub("AY.xls", "", temp_dataframe.5$Year)
  

## Exporting

write.csv(temp_dataframe.5, "grants.10-11.csv")


# Gathering a List

list_files <- list.files(path = "CSV.Files")

list_files_campus <- list_files <- list.files(path = "CSV.Files/Campus")

campus_length <- length(list_files_campus)
for (i in 1:campus_length) {
  
  ## i is what cycle the loop is on
  
  campus_filename <- list_files_campus[i] 
  
  ## Choosing Excel Files to Loop and Creating Path Names
  
  path_name_campus <-  paste0("CSV.Files/Campus/", campus_filename)
  print(path_name_campus)
  
  temp_dataframe_campus <- read_excel(path_name_campus, sheet = 1, col_names = TRUE, col_types = NULL, na = "",
                               skip = 3) 
  
  
  ## Changing Column Names
  
  colnames(temp_dataframe_campus) <- c("OPE.ID", "School", "State", "Zip.Code", "School.Type", "Recipients.1", "Federal.Award.1", "Disbursements.1", "Recipients.2", "Federal.Award.2", "Disbursements.2", 
                                  "Recipients.3", "Federal.Award.3", "Disbursements.3")
  
  temp_dataframe_campus <- temp_dataframe_campus[c("School", "State", "School.Type", "Disbursements.1", "Disbursements.2", "Disbursements.3")]
  
  temp_dataframe_campus$Total.Disbursements <- temp_dataframe_campus$Disbursements.1 + temp_dataframe_campus$Disbursements.2 + temp_dataframe_campus$Disbursements.3
  
  temp_dataframe_campus <- temp_dataframe_campus[c("School", "State", "School.Type", "Total.Disbursements")]
  
  #temp_dataframe <- subset(temp_dataframe, State=="CT")
  
  temp_dataframe_campus$Year <- campus_filename
  
  temp_dataframe_campus$Year <- gsub("CBData.xls", "", temp_dataframe_campus$Year)
 
  
  if (i == 1) {
    mega_dataframe_campus <- temp_dataframe_campus  
  } else (
    mega_dataframe_campus <- rbind(mega_dataframe_campus, temp_dataframe_campus)
  )
  
  
  
}

## Exporting

write.csv(mega_dataframe_campus, "campus.10-15.csv")

