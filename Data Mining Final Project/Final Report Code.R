# Final Report Code SI 671


# X = fread("Game_comments_final.txt", sep = ";") # it works!

library(RedditExtractoR)
library(tidytext)
library(tm)
library(topicmodels)
library(dplyr)
library(rvest)
library(lubridate)
library(data.table)
library(tidyr)
setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/SI 671/Project")
# Sales_data for 2015, Sales_data2.csv for 2016
sales_data = read.csv2("Sales_data2.csv", header = T, sep = ",") 
sales_data = sales_data[is.na(sales_data$Pos) == FALSE,]
# This gets us the publishers separated.
sales_data$Publisher = lapply(strsplit(as.character(sales_data$Publisher), ","), "[", 1)
sales_data$Genre = lapply(strsplit(as.character(sales_data$Publisher), ","), "[", 2)
sales_data$Game3 = gsub("\xff.*"," ",as.character(sales_data$Game2))
sales_data$Platform = gsub(".*\xff","",as.character(sales_data$Game2))


#sales_data$Game3 = gsub("\xca.*","",as.character(sales_data$Game2))
sales_data$Platform = gsub(".*\xca","",as.character(sales_data$Game2))
sales_data$Weekly = as.integer(gsub(",", "", as.character(sales_data$Weekly))) 
sales_data$Total = as.integer(gsub(",", "",as.character(sales_data$Total)))

#lapply(strsplit(as.character(sales_data$Game2), "\xca"), "[", 2)
# remove the <ca>, then (XOne, 3DS, PS4, WiiU, X360, )
#sales_dataY1 = sales_data[sales_data$Week..<=12,]
#sales_dataM1 = sales_data[sales_data$Week..==4,]
#sales_dataM1_1 = sales_data[sales_data$Week..==5,]
sales_dataW1 = sales_data[sales_data$Week..==1,]

#sales_publisher_list = data.frame(unique(sales_data$Publisher))
# Now make a giant loop for the  things.
#sales_dataW1$Week_1_Sales = NA
total_sales = sales_dataW1 %>% group_by(Game3) %>% summarize(Week_1_Sales = sum(Weekly))
total_sales = inner_join(total_sales, sales_dataW1,by ="Game3")
colnames(total_sales)[2] = "Week_1_Sales"
#colnames(total_sales)[6] = "Platform_sales"
total_sales = total_sales[!duplicated(total_sales$Game3),]

#total_sales$Game3[148] = "Yo-kai Watch 2 Shinuchi"
sales_data$Week..[1]

Week_2_sales = sales_data[sales_data$Week.. == 2,]
Week_2_sales = Week_2_sales  %>% group_by(Game3) %>% summarize(Week_2_Sales = sum(Weekly))

Week_3_sales = sales_data[sales_data$Week.. == 3,]
Week_3_sales = Week_3_sales  %>% group_by(Game3) %>% summarize(Week_3_Sales = sum(Weekly))

Week_4_sales = sales_data[sales_data$Week.. == 4,]
Week_4_sales = Week_4_sales  %>% group_by(Game3) %>% summarize(Week_4_Sales = sum(Weekly))


total_sales= full_join(total_sales, Week_2_sales, by = "Game3", copy = TRUE)
total_sales = full_join(total_sales, Week_3_sales, by = "Game3", copy = TRUE)
total_sales = full_join(total_sales, Week_4_sales, by = "Game3", copy = TRUE)


# Total_sales are the total Week1 sales across all platforms(released in that year) for each game.
# Week1 sales are total week1 sales for each platform for each game

# initial total list to Rbind to, only run once.
total_list = matrix(NA, nrow = 1, ncol = 29)
colnames(total_list) = c("id", "structure","post_date","comm_date", "num_comments", "subreddit", "upvote_prop","post_score","author", "user",            
                       "comment_score", "controversiality", "comment", "title", "post_text",       
                        "link", "domain", "URL", "Publisher", "W1Sales", "W2Sales","W3Sales","W4Sales",
                         "Platform", "Game_Name", "Release_Date", "Year", "CDays_since_release", "PDays_since_release")

# set MIN and L to the same value, so you can then check the comments to see that they work.
# do this manually. yes it sucks, but looping will waste even more time if the total list is corrupted
# and we have to go back through the algorithm


# This contains the release date data for each game that has worked.
# Can update by rerunning as we do it again.
release_dates = read.csv2("Game_release_dates.csv", sep = ",", header =T)
release_dates$Game_Name = as.character(release_dates$Game_Name)
release_dates$Release_Date = as.character(release_dates$Release_Date)
release_dates$Release_Date = gsub('\"', "", release_dates$Release_Date)
release_dates$Release_Date = as.Date(release_dates$Release_Date, "%d-%m-%y")

# Do game release date's manually?


# Keep a list of the games you've missed. (don't have comments listed.)
# 2015 Missed
# Missed: 1,2, 6, 13, 22, 24, 25, 31, 32, 36, 42, 45, 46, 51, 
# 55, 56, 59, 61, 62, 66, 73, 82, 83, 86, 87, 89, 90, 96, 101, 104, 106, 111, 113, 115, 124, 125, 130
# 131, 134, 135, 137, 142, 143, 146, 148
# Something wrong with 30 (Dying Light), 33 (Evolve), 58 (Just Cause 3)
# Something wrong with Rise of the Tomb Raider 98

# 2016 Missed
#  11, 14, 15, 18, 30, 31, 34, 36, 46, 49, 51, 52, 61, 68, 70, 73, 74, 75, 76, 85, 89, 93, 95, 103, 104
# 106, 107, 109, 110, 111, 116, 120, 121, 124, 126, 129, 130, 131
j = 2016
i = 136
game_name = as.character(total_sales$Game3[i])
Platform = as.character(total_sales$Platform[i])
Publisher = as.character(total_sales$Publisher[i])
comments = get_reddit(search_terms = game_name, 
                      regex_filter = "", subreddit = "gaming", 
                      cn_threshold = 5, page_threshold = 100, 
                      sort_by = "relevance", wait_time = 2)

# check to see if anything runs first!!!!
print(game_name)
print(i)


comments$Publisher = Publisher
comments$W1Sales = total_sales$Week_1_Sales[i]
comments$W2Sales = total_sales$Week_2_Sales[i]
comments$W3Sales = total_sales$Week_3_Sales[i]
comments$W4Sales = total_sales$Week_4_Sales[i]
comments$Platform = Platform
comments$Game_Name = game_name
#comments$release_date = total_sales$Week..
  # put dates in dat format
comments$post_date = as.Date(comments$post_date, "%d-%m-%y")
comments$comm_date = as.Date(comments$comm_date, "%d-%m-%y")

# If the comments run and they're not corrupted, attach a release date to the comments data:
comments$Release_Date = as.Date("2016-10-11") # put in the game's release date, then run this line.
#comments$Release_Date = as.Date(comments$Release_Date, "%d-%m-%y") # make sure the date works.
comments$CDays_since_release = (comments$Release_Date) - (comments$comm_date)
comments$PDays_since_release = (comments$Release_Date) - (comments$post_date)
comments$Year = j

#View(comments)
#### Only run this line when you're sure this iteration of the comments is good.
total_list_test = rbind(total_list, comments)
fwrite(total_list_test, file = "Game_comments_test.txt", sep = ";")

#total_list_test = total_list_test[,-c(2, 6, 16, 17, 18)]



# For a bad one...
#total_list_bad = rbind(total_list_test, comments)

## Then repeat for each game that works
# You'll get a large list of comments by this point in total_list.

total_list = rbind(total_list, comments)
#When you've gotten all the comments on as many games as possible:
# Then see that it properly works on Python

fwrite(total_list, file = "Game_comments_final2016.txt", sep = ";")
