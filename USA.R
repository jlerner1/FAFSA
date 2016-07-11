# Loading Libraries

library(dplyr)
library(tidyr)
library(stringr)


# Importing CSV Files

setwd(list.dirs(path = "Documents/CT Mirror/FAFSA/"))

grants.10.11 <- read.csv(file = "grants.10-11.csv")
grants.11.16 <- read.csv(file = "grants.11-16.csv")
loans.10.12 <- read.csv(file = "loans.10-12.csv")
loans.12.16 <- read.csv(file = "loans.12-16.csv")
campus.10.15 <- read.csv(file = "campus.10-15.csv")

# Joining

loans.10.16 <- full_join(loans.10.12, loans.12.16)
grants.10.16 <- full_join(grants.10.11, grants.11.16)

# Exporting

loans.10.16 <- write.csv(loans.10.16, "loans.10.16.csv")
grants.10.16 <- write.csv(grants.10.16, "grants.10.16.csv")

# Grouping

loan.state.summary <- loans.10.16 %>%
  group_by(State, School.Type, Year) %>%
  summarise(Sum.Type.loan=sum(Total.Disbursements))

grant.state.summary <- grants.10.16 %>%
  group_by(State, School.Type, Year) %>%
  summarise(Sum.Type.grant=sum(Total.Disbursements))

campus.state.summary <- campus.10.15 %>%
  group_by(State, School.Type, Year) %>%
  summarise(Sum.Type.campus=sum(Total.Disbursements))

loan.state.summary <- subset(loan.state.summary, !is.na(loan.state.summary$State))
grant.state.summary <- subset(grant.state.summary, !is.na(grant.state.summary$State))
campus.state.summary <- subset(campus.state.summary, !is.na(campus.state.summary$State))

# Fixing Cases

library(stringr)

loan.state.summary$School.Type <- str_to_title(loan.state.summary$School.Type)

grant.state.summary$School.Type <- str_to_title(grant.state.summary$School.Type)

campus.state.summary$School.Type <- str_to_title(campus.state.summary$School.Type)

loan.state.summary$School.Type <- gsub("Private-Nonprofit", "Private", loan.state.summary$School.Type)

grant.state.summary$School.Type <- gsub("Private-Nonprofit", "Private", grant.state.summary$School.Type)

campus.state.summary$School.Type <- gsub("Private/Non-Profit", "Private", campus.state.summary$School.Type)

loan.state.summary$School.Type <- gsub("Foreign*", "", loan.state.summary$School.Type)

grant.state.summary$School.Type <- gsub("Foreign*", "", grant.state.summary$School.Type)

campus.state.summary$School.Type <- gsub("Foreign*", "", campus.state.summary$School.Type)

loan.state.summary$Year <- gsub("2010_2011", "1011", loan.state.summary$Year)

loan.state.summary$Year <- gsub("2011_2012", "1112", loan.state.summary$Year)

loan.state.summary$Year <- gsub("2012_2013", "1213", loan.state.summary$Year)

loan.state.summary$Year <- gsub("2013_2014", "1314", loan.state.summary$Year)

loan.state.summary$Year <- gsub("2014_2015", "1415", loan.state.summary$Year)

loan.state.summary$Year <- gsub("2015_2016", "1516", loan.state.summary$Year)

campus.state.summary$Year <- gsub("2010-11", "1011", campus.state.summary$Year)

campus.state.summary$Year <- gsub("2011-12", "1112", campus.state.summary$Year)

campus.state.summary$Year <- gsub("2012-13", "1213", campus.state.summary$Year)

campus.state.summary$Year <- gsub("2013-14", "1314", campus.state.summary$Year)

campus.state.summary$Year <- gsub("2014-15", "1415", campus.state.summary$Year)

campus.state.summary$Year <- str_trim(campus.state.summary$Year)

grant.state.summary$Year <- as.character(grant.state.summary$Year)

campus.state.summary$Year <- as.character(campus.state.summary$Year)

# Joining

loans_grants_join <- full_join(loan.state.summary, grant.state.summary)

# Subsetting

loans_grants_join <- subset(loans_grants_join, !is.na(loans_grants_join$State))

loans_grants_join <- subset(loans_grants_join, State!="FC")
loans_grants_join <- subset(loans_grants_join, State!="PR")
loans_grants_join <- subset(loans_grants_join, State!="GU")
loans_grants_join <- subset(loans_grants_join, State!="AS")
loans_grants_join <- subset(loans_grants_join, State!="FM")
loans_grants_join <- subset(loans_grants_join, State!="MH")
loans_grants_join <- subset(loans_grants_join, State!="MP")
loans_grants_join <- subset(loans_grants_join, State!="VI")
loans_grants_join <- subset(loans_grants_join, State!="PW")

loans_grants_join$Total.Disbursement <- loans_grants_join$Sum.Type.loan + loans_grants_join$Sum.Type.grant


campus.state.summary <- subset(campus.state.summary, !is.na(campus.state.summary$State))

campus.state.summary <- subset(campus.state.summary, State!="FC")
campus.state.summary <- subset(campus.state.summary, State!="PR")
campus.state.summary <- subset(campus.state.summary, State!="GU")
campus.state.summary <- subset(campus.state.summary, State!="AS")
campus.state.summary <- subset(campus.state.summary, State!="FM")
campus.state.summary <- subset(campus.state.summary, State!="MH")
campus.state.summary <- subset(campus.state.summary, State!="MP")
campus.state.summary <- subset(campus.state.summary, State!="VI")
campus.state.summary <- subset(campus.state.summary, State!="PW")

# Exporting

loans_grants_join <- write.csv(loans_grants_join, "loans.grants.join.csv")
campus.state.summary <- write.csv(campus.state.summary, "campus.csv")

# Importing

loans.grants.campus.join <- read.csv(file = "loans.grants.campus.join.csv")

# Fixing Cases

loans.grants.campus.join$State <- str_trim(loans.grants.campus.join$State)
loans.grants.campus.join$Sum.Type.campus <- as.numeric(loans.grants.campus.join$Sum.Type.campus)
loans.grants.campus.join$Total.Disbursement <- as.numeric(loans.grants.campus.join$Total.Disbursement)

# Final Dataframe

loans.grants.campus.total <- loans.grants.campus.join %>%
  select(State, School.Type, Year, Total.Disbursement) %>%
  group_by(State, Year) %>%
  mutate(Year.Total=sum(Total.Disbursement), Percent=round(Total.Disbursement/Year.Total, 4)) %>%
  select(State, School.Type, Year, Percent) %>%
spread(Year, Percent)

loans.grants.campus.flipped.full <- loans.grants.campus.join %>%
  select(State, School.Type, Year, Total.Disbursement) %>%
  group_by(State, Year) %>%
  mutate(Year.Total=sum(Total.Disbursement), Percent=round(Total.Disbursement/Year.Total*100, 4)) %>%
  select(State, School.Type, Year, Percent) %>%
  spread(School.Type, Percent)

loans.grants.campus.USA <- loans.grants.campus.join %>%
  select(School.Type, Year, Total.Disbursement) %>%
  group_by(Year, School.Type) %>%
  summarise(Total.Disbursement=sum(Total.Disbursement, na.rm=T)) %>%
  mutate(Year.Total=sum(Total.Disbursement), Percent=round(Total.Disbursement/Year.Total, 4)) %>%
  select(School.Type, Year, Percent) %>%
  spread(Year, Percent)

loans.total <- loans.grants.campus.join %>%
  select(State, School.Type, Year, Sum.Type.loan) %>%
  group_by(State, Year) %>%
  mutate(Year.Total=sum(Sum.Type.loan), Percent=round(Sum.Type.loan/Year.Total, 4)) %>%
  select(State, School.Type, Year, Percent) %>%
  spread(Year, Percent)

loans.USA <- loans.grants.campus.join %>%
  select(School.Type, Year, Sum.Type.loan) %>%
  group_by(Year, School.Type) %>%
  summarise(Sum.Type.loan=sum(Sum.Type.loan, na.rm=T)) %>%
  mutate(Year.Total=sum(Sum.Type.loan), Percent=round(Sum.Type.loan/Year.Total, 4)) %>%
  select(School.Type, Year, Percent) %>%
  spread(Year, Percent)

grants.total <- loans.grants.campus.join %>%
  select(State, School.Type, Year, Sum.Type.grant) %>%
  group_by(State, Year) %>%
  mutate(Year.Total=sum(Sum.Type.grant), Percent=round(Sum.Type.grant/Year.Total, 4)) %>%
  select(State, School.Type, Year, Percent) %>%
  spread(Year, Percent)

grants.USA <- loans.grants.campus.join %>%
  select(School.Type, Year, Sum.Type.grant) %>%
  group_by(Year, School.Type) %>%
  summarise(Sum.Type.grant=sum(Sum.Type.grant, na.rm=T)) %>%
  mutate(Year.Total=sum(Sum.Type.grant), Percent=round(Sum.Type.grant/Year.Total, 4)) %>%
  select(School.Type, Year, Percent) %>%
  spread(Year, Percent)

campus.total <- loans.grants.campus.join %>%
  select(State, School.Type, Year, Sum.Type.campus) %>%
  group_by(State, Year) %>%
  mutate(Year.Total=sum(Sum.Type.campus), Percent=round(Sum.Type.campus/Year.Total, 4)) %>%
  select(State, School.Type, Year, Percent) %>%
  spread(Year, Percent)

campus.total <- campus.total[c("State", "School.Type", "1011", "1112", "1213", "1314", "1415")]

campus.USA <- campus.total[c("State", "School.Type", "1011", "1112", "1213", "1314", "1415")]

campus.USA <- loans.grants.campus.join %>%
  select(School.Type, Year, Sum.Type.campus) %>%
  group_by(Year, School.Type) %>%
  summarise(Sum.Type.campus=sum(Sum.Type.campus, na.rm=T)) %>%
  mutate(Year.Total=sum(Sum.Type.campus), Percent=round(Sum.Type.campus/Year.Total, 4)) %>%
  select(School.Type, Year, Percent) %>%
  spread(Year, Percent)

campus.USA <- campus.total[c("State", "School.Type", "1011", "1112", "1213", "1314", "1415")]

#Exporting

loans.grants.campus.total <- write.csv(loans.grants.campus.total, "loans.grants.campus.by.state.csv")
loans.grants.campus.flipped.full <- write.csv(loans.grants.campus.flipped.full, "loans.grants.campus.flipped.full.csv")
loans.grants.campus.USA <- write.csv(loans.grants.campus.USA, "loans.grants.campus.USA.csv")

loans.total <- write.csv(loans.total, "loans.by.state.csv")
loans.USA <- write.csv(loans.USA, "loans.USA.csv")

grants.total <- write.csv(grants.total, "grants.by.state.csv")
grants.USA <- write.csv(grants.USA, "grants.USA.csv")

campus.total <- write.csv(campus.total, "campus.by.state.csv")
campus.USA <- write.csv(campus.USA, "campus.USA.csv")
