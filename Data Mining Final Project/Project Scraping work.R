library(RedditExtractoR)
library(tidytext)
library(tm)
library(topicmodels)
library(dplyr)
library(rvest)
library(lubridate)
setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/SI 671/Project")
# Documentation
# https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf

# Paper we're basing it all on.
# http://sdsu-dspace.calstate.edu/bitstream/handle/10211.10/1073/Ehrenfeld_Steven.pdf?sequence=1
#reddit_content
#get_reddit (wrapper for first two)

# get a dataframe of Publisher names, release date, and video games.
publisher = read.csv2("Publishers.csv", header =TRUE, sep = ",")
publisher$Date = gsub("2015","15",publisher$Date)
publisher$Date = gsub("/","-",publisher$Date)

game_name_data_frame = publisher

end_data = data.frame(x = NA)
colnames(end_data)=c("id")
end_data$structure = NA
end_data$post_date=NA
end_data$comm_date = NA
end_data$num_comments = NA
end_data$subreddit = NA
end_data$upvote_prop = NA
end_data$post_score = NA
end_data$author = NA
end_data$user = NA
end_data$comment_score = NA
end_data$controversiality = NA
end_data$comment = NA
end_data$title = NA
end_data$post_text = NA
end_data$link = NA
end_data$domain = NA
end_data$URL = NA
end_data$Game_Release_Date = NA
end_data$Game_Name = NA

# These have no results:
# 29, 35, 36, 37, 40, 41, 44, 45, 46, 47, 52, 
# 54, 57, 58, 60, 61, 62, 63, 67, 69, 75, 76, 
# 78, 80, 82, 84, 85, 86, 89, 90, 91, 93, 98,
# 100, 106, 107, 110, 112, 118, 124, 129, 131,
# 136, 137, 139, 140, 145, 151, 152, 163, 175,
# 181, 200, 202, 205, 216, 222, 223, 224, 231,
# 233, 238, 239, 240, 244, 251, 

skipped = c(29, 35, 36, 37, 40, 41, 44, 45, 46, 47,
            52, 54, 57, 58, 60, 61, 62, 63, 67, 69,
            75, 76, 78, 80, 82, 84, 85, 86, 89, 90,
            91, 93, 98, 100, 106, 107, 110, 112, 118, 
            124, 129, 131, 136, 137, 139, 140, 145, 151, 152, 
            159, 163, 175,
            181, 200, 202, 205, 216, 222, 223, 224, 231, 233, 238, 239, 240, 244, 251)
game_name_data_frame = game_name_data_frame[-skipped,]

# Find something to do with these.
skipped_games = publisher[skipped,]

# 28, 52, 104, 143, 173 didn't work.
begin =174  #move these up and down as needed 
end =  length(game_name_data_frame$Game) # length(game_name_data_frame) #move these up and down as needed
for (i in begin:end){
  game_name = game_name_data_frame$Game[i]
  comments = get_reddit(search_terms = as.character(game_name), regex_filter = "", subreddit = "gaming", cn_threshold = 5, page_threshold = 100, sort_by = "relevance", wait_time = 2)
  comments$Game_Release_Date = game_name_data_frame$Date[i]
  comments$Game_Name = game_name_data_frame$Game[i]
  end_data = rbind(end_data, comments)
  print(i)
}

games = data.frame(unique(end_data$Game_Name))
missed = data.frame(setdiff(game_name_data_frame$Game, games$unique.end_data.Game_Name.))

missed_index_in_publisher = c(28, 72, 153, 196, 235)

new_missed = publisher[missed_index_in_publisher,]

Missed_Games = rbind(new_missed, skipped_games)

# 2, 5, worked


# Run through for all missed data.
begin =72 #move these up and down as needed 
end =  length(Missed_Games$Game) # length(game_name_data_frame) #move these up and down as needed
for (i in begin:end){
  game_name = Missed_Games$Game[i]
  comments = get_reddit(search_terms = as.character(game_name), regex_filter = "", subreddit = "gaming", cn_threshold = 5, page_threshold = 100, sort_by = "relevance", wait_time = 2)
  comments$Game_Release_Date = Missed_Games$Date[i]
  comments$Game_Name = Missed_Games$Game[i]
  end_data = rbind(end_data, comments)
  print(i)
}

# List of games to run through again:


write.csv2(end_data, file = "comment_data_prototype.csv")

data = read.csv2("comment_data_prototype.csv", header = T)

#begin =241 #move these up and down as needed 
#end = 255 #move these up and down as needed
#name = paste("comment_data_", as.character(begin), "_", as.character(end), ".csv", sep = "", collapse = NULL)
#write.csv2(end_data, file = name)


sales_data = read.csv2("Sales_data.csv", header = T, sep = ",")
sales_data = sales_data[is.na(sales_data$Pos) == FALSE,]
# This gets us the publishers separated.
sales_data$Publisher = lapply(strsplit(as.character(sales_data$Publisher), ","), "[", 1)
sales_data$Genre = lapply(strsplit(as.character(sales_data$Publisher), ","), "[", 2)

sales_data$Platform = lapply(strsplit(as.character(sales_data$Game), ), "[", 2)
# remove the <ca>, then (XOne, 3DS, PS4, WiiU, X360, )
sales_dataY1 = sales_data[sales_data$Week..<=12,]
sales_dataM1 = sales_data[sales_data$Week..==4,]
sales_dataM1_1 = sales_data[sales_data$Week..==5,]

sales_dataW1 = sales_data[sales_data$Week..==1,]

sales_publisher_list = data.frame(unique(sales_data$Publisher))
comment_publisher_list = t(data.frame("Nintendo", "Take-Two Interactive", "Microsoft", "Sony", "EA", "Square Enix", "Ubisoft", "Sega", "Bandai Namco", "Capcom"))

sales_data2 = filter(sales_data, sales_data$Publisher == comment_publisher_list[1]
                                              | sales_data$Publisher == comment_publisher_list[2]
                                              | sales_data$Publisher == comment_publisher_list[3]
                                   | sales_data$Publisher == comment_publisher_list[4]
                                   | sales_data$Publisher == comment_publisher_list[5]
                                   | sales_data$Publisher == comment_publisher_list[6]
                                   | sales_data$Publisher == comment_publisher_list[7]
                                   | sales_data$Publisher == comment_publisher_list[8]
                                   | sales_data$Publisher == comment_publisher_list[9]
                                   | sales_data$Publisher == comment_publisher_list[10])

sales_data2$Game2 = as.character(sales_data2$Game2) 
sales_data2$Game2[1]
sales_data2$Game2 = gsub("\xca", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(PS4)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(XOne)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(PS3)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(WiiU)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(PC)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(3DS)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(Wii)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(X360)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("(PSV)", "", sales_data2$Game2)
sales_data2$Game2 = gsub("()", "", sales_data2$Game2)
sales_data2$Game2 = substr(sales_data2$Game2, 1, nchar(sales_data2$Game2) - 2)



# Now that the data is clean:
sales_dataY1 = sales_data2[sales_data2$Week..==12,]
sales_dataY1_1 =  sales_data2[sales_data2$Week..==13,]
sales_dataM1 = sales_data2[sales_data2$Week..==4,]
sales_dataM1_1 = sales_data2[sales_data2$Week..==5,]
sales_dataW1 = sales_data2[sales_data2$Week..==1,]

games_unique = data.frame(unique(data$Game_Name))
sales_uniqe = data.frame(unique(sales_data2$Game2))

sales_we_have = data.frame(intersect(sales_uniqe$unique.sales_data2.Game2., games_unique$unique.data.Game_Name.))
sales_we_have = c("Grand Theft Auto V", "Captain Toad: Treasure Tracker", "Assassin's Creed Syndicate", "Just Cause 3",                                 
"Super Mario Maker", "Splatoon", "NBA 2K16", "Animal Crossing: Happy Home Designer", "Just Dance 2016", "Kirby and the Rainbow Curse",                   
"The Legend of Zelda: Majora's Mask 3D", "Monster Hunter 4 Ultimate", "Evolve", "Total War: Attila", "Yakuza 0", "Mario Party 10",                                
"Code Name: S.T.E.A.M.", "Final Fantasy Type-0 HD", "Resident Evil: Revelations 2", "Borderlands: The Handsome Collection",          
"Xenoblade Chronicles X", "Puzzle & Dragons Z + Super Mario Bros. Edition", "Yoshi's Woolly World", "Dragon Quest VIII: Journey of the Cursed King", 
"Dragon's Dogma Online", "Hatsune Miku: Project Mirai DX", "Pokemon Super Mystery Dungeon", "WWE 2K16",                                      
"Anno 2205", "Rise of the Tomb Raider")   

# Now make a dataframe with the filtered games.
data$Game_Name = as.character(data$Game_Name)
new_data =  filter(data, Game_Name == sales_we_have)
library(data.table)
sales_dataW1$Weekly = as.character(as.factor(sales_dataW1$Weekly))
sales_dataW1$Weekly = gsub(",", "", sales_dataW1$Weekly)
sales_dataW1$Weekly = as.integer(sales_dataW1$Weekly)
total_sales = aggregate(sales_dataW1, by=list("Game2"), FUN = sum)

total_sales = sales_dataW1 %>% group_by(Game2) %>% summarize(Weekly = sum(Weekly))

X = intersect(total_sales$Game2, sales_we_have$intersect.sales_uniqe.unique.sales_data2.Game2...games_unique.unique.data.Game_Name..)

data$Game_Name = as.character(data$Game_Name)
games_with_sales = data[1,]
games_with_sales$W1 = NA
games_with_sales$X = NULL
i = 1
for (i in 1:length(X) ) {
  temp = filter(end_data, Game_Name == X[i])
  temp$W1 = total_sales$Weekly[total_sales$Game2==X[i]]
  games_with_sales = rbind(games_with_sales, temp)
}

# Need the file encoding for python.
write.csv2(games_with_sales, file = "games_with_sales.csv",row.names = FALSE, fileEncoding = "UTF-8")

write.csv2(games_with_sales, file = "games_with_sales.csv")



idea2 = get_reddit(search_terms = "Super Smash Bros. for Nintendo 3DS", subreddit = "gaming", regex_filter = "", cn_threshold = 5, page_threshold = 10, sort_by = "relevance", wait_time = 2)

total = fread("Sales_data.csv")
library(dplyr)
library(data.table)
# Ok, let's redo this from another angle

# get the Sales data first for as many games as possible
# then clean the data (get W1-W2 data, and maybe Y1 (W12-W13)) and extract all names of games

# Then go down each one and get the data.
total = read.csv2("games_with_sales.csv", sep = ";", header = T)
total$post_date = as.Date(total$post_date, "%d-%m-%y")
total$comm_date = as.Date(total$comm_date, "%d-%m-%y")
total$Game_Release_Date = as.Date(total$Game_Release_Date, "%d-%m-%y")


# Figured out the date thing.
# Might be too slow, so you'll need to make a loop and just loop through it.
new = head(total)
new$post_date = as.Date(new$post_date, "%d-%m-%y")
new$comm_date = as.Date(new$comm_date, "%d-%m-%y")
new$Game_Release_Date = as.Date(new$Game_Release_Date, "%d-%m-%y")




X = "23-06-17"
X = as.Date(X, "%d-%m-%y")
Y = "3-11-15"
Y = as.Date(Y, "%d-%m-%y")





#######
# Code Redo
#
######

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
sales_data = read.csv2("Sales_data.csv", header = T, sep = ",")
sales_data = sales_data[is.na(sales_data$Pos) == FALSE,]
# This gets us the publishers separated.
sales_data$Publisher = lapply(strsplit(as.character(sales_data$Publisher), ","), "[", 1)
sales_data$Genre = lapply(strsplit(as.character(sales_data$Publisher), ","), "[", 2)
sales_data$Game3 = gsub("\xca.*","",as.character(sales_data$Game2))
sales_data$Platform = gsub(".*\xca","",as.character(sales_data$Game2))
sales_data$Weekly = as.integer(gsub(",", "", as.character(sales_data$Weekly))) 
sales_data$Total = as.integer(gsub(",", "",as.character(sales_data$Total)))

  #lapply(strsplit(as.character(sales_data$Game2), "\xca"), "[", 2)
# remove the <ca>, then (XOne, 3DS, PS4, WiiU, X360, )
sales_dataY1 = sales_data[sales_data$Week..<=12,]
sales_dataM1 = sales_data[sales_data$Week..==4,]
sales_dataM1_1 = sales_data[sales_data$Week..==5,]
sales_dataW1 = sales_data[sales_data$Week..==1,]

sales_publisher_list = data.frame(unique(sales_data$Publisher))
# Now make a giant loop for the  things.
#sales_dataW1$Week_1_Sales = NA
total_sales = sales_dataW1 %>% group_by(Game3) %>% summarize(Week_1_Sales = sum(Weekly))
total_sales = inner_join(total_sales, sales_dataW1,by ="Game3")
colnames(total_sales)[2] = "Week_1_Sales"
colnames(total_sales)[6] = "Platform_sales"
total_sales = total_sales[!duplicated(total_sales$Game3),]



# Not worth the platform thing at the moment.
# total_sales$Platform2 = paste(total_sales$Platform[duplicated(total_sales$Game3)], collapse = ',')

# Missed numbers in total_sales: 6, 13, 22, 24, 25, 31, 32, 36, 42, 45, 46, 51, 55, 56, 59, 61, 62,66, 73

L = length(total_sales$Pos)
L =4
for(i in 4:L){
  game_name = as.character(total_sales$Game3[i])
  Platform = as.character(total_sales$Platform[i])
  Publisher = as.character(total_sales$Publisher[i])
  Sales = total_sales$Week_1_Sales[i]
  comments = get_reddit(search_terms = game_name, 
                        regex_filter = "", subreddit = "gaming", 
                        cn_threshold = 5, page_threshold = 100, 
                        sort_by = "relevance", wait_time = 2)
  comments$Publisher = Publisher
  comments$Sales = Sales
  comments$Platform = Platform
  comments$Game_Name = game_name
  comments$post_date = as.Date(comments$post_date, "%d-%m-%y")
  comments$comm_date = as.Date(comments$comm_date, "%d-%m-%y")
  print(game_name)
  print(i)
  #total_list = rbind(total_list, comments)
}

comments$Release_Date = release_dates$Release_Date[2]

write.table(total_list, file = "game_sales_pres.csv", sep = "<;;;;>")

total_list = read.table(file = "game_sales_pres.csv")

#, fileEncoding = "utf-8"

release_dates = read.csv2("Game_release_dates.csv", sep = ",", header =T)
release_dates$Game_Name = as.character(release_dates$Game_Name)
release_dates$Release_Date = as.character(release_dates$Release_Date)
release_dates$Release_Date = gsub('\"', "", release_dates$Release_Date)
release_dates$Release_Date = as.Date(release_dates$Release_Date, "%d-%m-%y")
# Get a list of unique games, then get their release dates
# Now merge with names to the giant dataset

Ttotal_list = data.table(total_list, key = "Game_Name")
Trelease_dates = data.table(release_dates, key = "Game_Name")
new_list = merge(Ttotal_list, Trelease_dates, all.x = TRUE)
new_list$Weekly = NULL
new_list$post_date = as.Date(new_list$post_date, "%d-%m-%y")
new_list$comm_date = as.Date(new_list$comm_date, "%d-%m-%y")
new_list$post_date[1] - new_list$Release_Date[1]

new_list$comment = gsub("\n\n", "",new_list$comment)
new_list$post_text = gsub("\n\n", "", new_list$post_text)

write.table(new_list, file = "game_sales_pres2.csv", sep = "<;>")

# Worry later:
new_list_pruned = new_list[new_list$comm_date< new_list$Release_Date,]

# Find top-rated comment:
max(new_list$comment_score)
min(new_list$comment_score)
# Add release date to CSV. (through merge?)

# Then read Total_list into a CSV
# total_list