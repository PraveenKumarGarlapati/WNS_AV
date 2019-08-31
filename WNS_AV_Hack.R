#WNS Analytics Vidya hackathon


# Installing Libraries ----------------------------------------------------
library(tidyverse)
library(lubridate)
library(prophet)
library(data.table)
library(DataExplorer)
library(readr)
library(randomForest)
library(caret)
library(xgboost)
library(InformationValue)
library(dummies)




# Importing Data ----------------------------------------------------------

train <- read_csv("C:/Users/u278677/Downloads/WNS AV Hackathon/train_NA17Sgz/train.csv")
itemdata <- read_csv("C:/Users/u278677/Downloads/WNS AV Hackathon/train_NA17Sgz/item_data.csv")
viewlog <- read_csv("C:/Users/u278677/Downloads/WNS AV Hackathon/train_NA17Sgz/view_log.csv")
test <-  read_csv("C:/Users/u278677/Downloads/WNS AV Hackathon/test_aq1FGdB/test.csv")
sample <- read_csv("C:/Users/u278677/Downloads/WNS AV Hackathon/sample_submission_IPsBlCT/sample_submission.csv")

dim(train)  
dim(itemdata)  
dim(viewlog)
dim(test)
dim(sample)

train%>%View()
test%>%View()
viewlog%>%View()
itemdata%>%View()


#Personal PC

viewlog <- view_log
rm(view_log)

itemdata <- item_data
rm(item_data)



# Train Data Manipulations ------------------------------------------------

train <- train%>%
  group_by(user_id,app_code)%>%
  mutate(freq_user_website = rank(impression_time,ties.method = "first"))

train <- train%>%
  group_by(user_id)%>%
  mutate(freq_userlevel = rank(impression_time,ties.method = "first"))

test <- test%>%
  group_by(user_id,app_code)%>%
  mutate(freq_user_website = rank(impression_time,ties.method = "first"))

test <- test%>%
  group_by(user_id)%>%
  mutate(freq_userlevel = rank(impression_time,ties.method = "first"))

train%>%
  arrange(user_id, impression_time)%>%View()

train <- train%>%
  mutate(date = date(impression_time),
         year = year(impression_time),
         day = day(impression_time),
         month = month(impression_time),
         hour = hour(impression_time),
         mins = minute(impression_time))

test <- test%>%
  mutate(date = date(impression_time),
         year = year(impression_time),
         day = day(impression_time),
         month = month(impression_time),
         hour = hour(impression_time),
         mins = minute(impression_time))


train <- train%>%
  ungroup(user_id)

# train%>%
#   count(day, month)%>%
#   print(n=100)

train%>%
  count(mins)%>%
  ggplot(aes(x = mins, y=n)) + geom_point()
train%>%
  count(hour)%>%
  ggplot(aes(x = hour, y=n)) + geom_point()

#Check percentage conversion at minute and hour level and add them to
#train sheet

####
train%>%
  count(freq_userlevel, is_click)%>%
  filter(is_click == 1)%>%
  ggplot(aes(x = freq_userlevel, y=n)) + geom_point()

#Percentage Conversion         
train%>%
  count(freq_userlevel, is_click)%>%
  dcast(freq_userlevel ~ is_click, value.var = "n")%>%
  mutate(perc_conv = (`1`/ (`1` + `0`)*100))%>%
  ggplot(aes(x = freq_userlevel, y=perc_conv)) + geom_point()

fq_userlevel_perc <- train%>%
  count(freq_userlevel, is_click)%>%
  dcast(freq_userlevel ~ is_click, value.var = "n")%>%
  mutate(perc_conv = (`1`/ (`1` + `0`)*100))

#####
train%>%
  count(freq_user_website, is_click)%>%
  filter(is_click == 1)%>%
  ggplot(aes(x = freq_user_website, y=n)) + geom_point()

#Percentage Conversion 
train%>%
  count(freq_user_website, is_click)%>%
  dcast(freq_user_website ~ is_click, value.var = "n")%>%
  mutate(perc_conv = (`1`/ (`1` + `0`)*100))%>%
  ggplot(aes(x = freq_user_website, y=perc_conv)) + geom_point()

fq_user_website_perc <- train%>%
  count(freq_user_website, is_click)%>%
  dcast(freq_user_website ~ is_click, value.var = "n")%>%
  mutate(perc_conv = (`1`/ (`1` + `0`)*100))

#Add these perc conversion rates to the train sheet

train%>%View()





# Viewlog Manipulations ---------------------------------------------------


viewlog <- viewlog%>%
  mutate(date = date(server_time),
         day = day(server_time),
         month = month(server_time),
         hour = hour(server_time),
         mins = minute(server_time))

#Total Views by a user on a given date
viewlog%>%
  group_by(user_id, date)%>%
  summarise(totalviews_givendate = n())

#Total views by a given user of a particular product  
views_user_item <- viewlog%>%
  group_by(user_id, date, item_id)%>%
  summarise(views = n())%>%
  group_by(user_id)%>%
  mutate(cum_viewsAll = cumsum(views))%>%
  ungroup()%>%
  group_by(user_id, item_id)%>%
  mutate(cum_views_OnItem = cumsum(views))

#Instead of rleid, use label the data
viewlog%>%
  group_by(user_id, date, item_id)%>%
  summarise(views = n())%>%
  ungroup()%>%
  mutate(rleid(item_id))



