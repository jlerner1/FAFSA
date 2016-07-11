# Gathering a List

list.files(path = "Enrollment")
list_files <- list.files(path = "Enrollment")

# Loading Libraries

library(readxl)

library(tidyr)

# Looping for Bringing in Excel Files

# Enrollment Looping

## File Names

list_files_enroll <- list_files <- list.files(path = "Enrollment")

## Loop Length

enroll_length <- length(list_files_enroll)
for (i in 1:enroll_length) {
  
  ## i is what cycle the loop is on
  
  enroll_filename <- list_files_enroll[i] 
  
  ## Choosing Excel Files to Loop and Creating Path Names
  
  path_name_enroll <-  paste0("Enrollment/", enroll_filename)
  print(path_name_enroll)
  
  temp_enroll <- read_excel(path_name_enroll, sheet = 1, col_names = TRUE, col_types = NULL, na = "") 
  
  ## Changing Column Names
  
  colnames(temp_enroll) <- make.names(colnames(temp_enroll))
  
  temp_enroll$Year <- enroll_filename
  
  temp_enroll$Year <- gsub("2010-11.enroll.CT.xls", "2010-11", temp_enroll$Year)
  temp_enroll$Year <- gsub("2011-12.enroll.CT.xls", "2011-12", temp_enroll$Year)
  temp_enroll$Year <- gsub("2012-13.enroll.CT.xls", "2012-13", temp_enroll$Year)
  temp_enroll$Year <- gsub("2013-14.enroll.CT.xls", "2013-14", temp_enroll$Year)
  
  if (i == 1) {
    mega_enroll <- temp_enroll 
  } else (
    mega_enroll <- rbind(mega_enroll, temp_enroll)
  )
  
  
  
}

## Exporting

write.csv(mega_enroll, "enroll.10.14.final.csv")

# Importing

enroll.10.14 <- read.csv(file = "enroll.10.14.csv")

setwd(list.dirs(path = "/Users/jessicalerner/Documents/CT Mirror/FAFSA/"))

college.name <- read_excel(path = "college.names.final.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "")


# Changing Column Names

enroll.10.14 <- enroll.10.14[c("School", "Enrollment", "Year")]
college.name <- college.name[c("School", "Type")]
college.name <- subset(college.name, !is.na(college.name$School))

# Fixing Cases

library(stringr)

college.name$School <- str_to_title(college.name$School)
enroll.10.14$School <- str_to_title(enroll.10.14$School)
college.name$School <- str_trim(college.name$School)
enroll.10.14$School <- str_trim(enroll.10.14$School)
enroll.10.14$School <- as.character(enroll.10.14$School)

# Joining

library(dplyr)

enroll.join <- full_join(enroll.10.14, college.name)

# Cleaning

enroll.join <- subset(enroll.join, !is.na(enroll.join$Type))
enroll.join <- subset(enroll.join, Enrollment!="NA")
enroll.join$Enrollment <- as.numeric(enroll.join$Enrollment)

# Final Dataframe

enroll.final <- enroll.join %>%
  select(School, Enrollment, Type, Year) %>%
  group_by(Year, Type) %>%
  summarise(Enroll.Total=sum(Enrollment, na.rm=T)) %>%
  mutate(Year.Total=sum(Enroll.Total), Percent=round(Enroll.Total/Year.Total, 4)) %>%
  select(Type, Year, Percent) %>%
  spread(Year, Percent)

#Exporting

write.csv(enroll.final, "enroll.final.csv")
