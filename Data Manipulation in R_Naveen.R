# Session 5: Data Manipulation in R::::::::

# Creating col_names array for creating variable names

col_names<-c("Athlete", "Age", "Country", "Year", "Closing Date", "Sport", "Gold Medals", "Silver Medals",
          "Bronze Medals", "Total Medals");
View(col_names)

# colnames(olympicdata)

# Reading Olympic data file into R:::::::::

setwd("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS")

olympicdata <-read.csv("olympic_data.csv",stringsAsFactors = F, col.names = col_names, sep = "\t", na.strings = "")
View(olympicdata)

# 1) Consider only those participants who have all the data points

new_olympic_data <- olympicdata[complete.cases(olympicdata),  ]
View(new_olympic_data)

# 2) Rank the participants in terms : . Swimming . Table Tennis . Shooting . Gymnastics . Total
# Medal

install.packAges("dplyr")
library(dplyr)

# Ranking Swimming participants
 swimming_rank <- aggregate(Total.Medals ~ Athlete + Sport ,data=new_olympic_data,sum) %>% 
  filter(Sport=="Swimming") %>% arrange(-Total.Medals) %>% 
  mutate(Rank = 1:length(Total.Medals))
 View(swimming_rank)

# Ranking Table Tennis participants
  TT_rank <- aggregate(Total.Medals ~ Athlete + Sport ,data=new_olympic_data,sum) %>% 
  filter(Sport=="Table Tennis") %>% arrange(-Total.Medals) %>% 
  mutate(Rank = 1:length(Total.Medals))
  View(TT_rank)

# Ranking Shooting Participants
  Shooting_rank <- aggregate(Total.Medals ~ Athlete + Sport ,data=new_olympic_data,sum) %>% 
  filter(Sport=="Shooting") %>% arrange %>% 
  mutate(Rank = 1:length(Total.Medals))
  View(Shooting_rank)

# Ranking Gymnastics Participants
  Gymnastics_rank <- aggregate(Total.Medals ~ Athlete + Sport ,data=new_olympic_data,sum) %>% 
  filter(Sport=="Gymnastics") %>% arrange(-Total.Medals) %>% 
  mutate(Rank = 1:length(Total.Medals))
  View(Gymnastics_rank)

# Ranking all participants in terms of total medals.
  Total_medals_rank <- aggregate(Total.Medals ~ Athlete ,data=new_olympic_data,sum) %>% 
  arrange(-Total.Medals) %>% 
  mutate(Rank = 1:length(Total.Medals))
  View(Total_medals_rank)
    
# 3) Rank the Categories in terms of Age.(Higher the Age,Higher the Rank)
  
   quest_5.3<- newdata %>% select (Sport, Age) %>% 
    group_by(Sport) %>%
    filter(Age == max(Age)) %>%
    distinct() %>% 
    summarise(Age = max(Age)) %>% 
    arrange (desc(Age)) %>% 
    mutate(Rank = row_number(desc(Age)))
    View(quest_5.3)

# 4) Identify Year wise top participants in terms of : . Swimming . Table Tennis . Shooting 
#    .Gymnastics . Total Medal

# Identifying Year wise top participants in terms of Swimming  
  Swimming_year <- aggregate(Total.Medals ~ Athlete+Sport+Year,data = new_olympic_data,max) %>% 
  filter(Sport=="Swimming") %>% 
  arrange(Year,-Total.Medals) %>% group_by(Year)
  View(Swimming_year)
  
# Identifying Year wise top participants in terms of Table Tennis 
  TT_year <- aggregate(Total.Medals ~ Athlete+Sport+Year,data = new_olympic_data,max) %>% 
  filter(Sport=="Table Tennis") %>% 
  arrange(Year,-Total.Medals) %>% group_by(Year)
  View(TT_year)

# Identifying Year wise top participants in terms of Shooting  
  Shooting_year <- aggregate(Total.Medals ~ Athlete+Sport+Year,data = new_olympic_data,max) %>% 
  filter(Sport=="Shooting") %>% 
  arrange(Year,-Total.Medals) %>% group_by(Year)
  View(Shooting_year)

# Identifying Year wise top participants in terms of Gymnastics  
  Gymnastics_year<- aggregate(Total.Medals ~ Athlete+Sport+Year,data = new_olympic_data,max) %>% 
  filter(Sport=="Gymnastics") %>% 
  arrange(Year,-Total.Medals) %>% group_by(Year)
  View(Gymnastics_year)

# Identifying Year wise top participants in terms of Total Medal  
  totalmedals_year<- aggregate(Total.Medals ~ Athlete+Year,data = new_olympic_data,sum) %>% 
  arrange(Year,-Total.Medals) %>% group_by(Year)
  View(totalmedals_year)
