# NOAH GALE CODE APRIL 1, 2017
# CODE FROM DATA CLEANING UP TO LOGIT MODEL AND ODDS FROM CHAPTERS

setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/Biostat 682/Project")

#setwd("~/Dropbox/Course On Doing/BIOSTAT 682/Project/dataset") 
data = read.csv2("character-deaths.csv", header = T, sep = ",")



# Some of the data is wrong, here are fixes:
# Cressen is intro'd and dies in book 2
data$Book.of.Death[157] = 2
# Shyra Errol Died in Book 3, did not appear


# add on 1 to the death chapter number and intro chapter number, for prologues.
data$Book.Intro.Chapter = data$Book.Intro.Chapter + 1
data$Death.Chapter = data$Death.Chapter + 1

# Make a book intro variable
data$Book.of.Intro = 0
#data$Book.of.Intro = data$GoT
X1 = which(data$GoT == 1)
X2 = which(data$CoK == 1 & data$GoT == 0)
X3 = which(data$SoS== 1 & data$CoK == 0 & data$GoT == 0)
X4 = which(data$FfC ==1 & data$SoS == 0 & data$CoK == 0 & data$GoT == 0)
X5 = which(data$DwD == 1 & data$FfC == 0 & data$SoS == 0 & data$CoK == 0 & data$GoT == 0)
data$Book.of.Intro[X1] = 1
data$Book.of.Intro[X2] = 2
data$Book.of.Intro[X3] = 3
data$Book.of.Intro[X4] = 4
data$Book.of.Intro[X5] = 5

# these are the characters that die in the same book they're introduced.
Same_book_death = which(data$Book.of.Intro == data$Book.of.Death)
Dies_same_book_1 = sum(data$Book.of.Intro[Same_book_death] == 1)
Dies_same_book_2 = sum(data$Book.of.Intro[Same_book_death] == 2)
Dies_same_book_3 = sum(data$Book.of.Intro[Same_book_death] == 3)
Dies_same_book_4 = sum(data$Book.of.Intro[Same_book_death] == 4)
Dies_same_book_5 = sum(data$Book.of.Intro[Same_book_death] == 5)

mean(c(Dies_same_book_1,Dies_same_book_2,Dies_same_book_3,Dies_same_book_4,Dies_same_book_5))
# 38.4 average
# So we can assume that around 38.4 people will be introduced and die within the Winds of Winter.

# Book 1 has 73 chapters
# book 2 has 70 chapters
# Book 3 has 82 chapters
# book 4 has 46 chapters
# book 5 has 73 chapters

# Make a character lives variable

Total_possible_Life = 73 + 70 + 82 + 46 + 73
book1chap = 73
book2chap = 70
book3chap = 82
book4chap = 46
book5chap = 73
#the total chapter of book 1-4 is 271

data$Chapters_featured = data$GoT*book1chap + data$CoK*book2chap + data$SoS*book3chap + data$FfC*book4chap + data$DwD*book5chap

# subtract the chapters before a character appears in GoT
data$Chapters_featured[X1] = data$Chapters_featured[X1] - data$Book.Intro.Chapter[X1] + 1
data$Chapters_featured[X2] = data$Chapters_featured[X2] - data$Book.Intro.Chapter[X2] + 1 
data$Chapters_featured[X3] = data$Chapters_featured[X3] - data$Book.Intro.Chapter[X3] + 1
data$Chapters_featured[X4] = data$Chapters_featured[X4] - data$Book.Intro.Chapter[X4] + 1
data$Chapters_featured[X5] = data$Chapters_featured[X5] - data$Book.Intro.Chapter[X5] + 1


data$Death.Chapter2 = data$Death.Chapter
data$Death.Chapter2[is.na(data$Death.Chapter)] = 0
Not_Dead = is.na(data$Death.Chapter)
Isnt_Dead = sum(Not_Dead)

# subtract the difference in chapters before death and total # of chapters in book.
data$In_Book = data$Chapters_featured
data = data.frame(data)
data = data[which(data$In_Book > 0),]


D1 = which(data$Book.of.Death == 1 & !is.na(data$Death.Chapter))
D2 = which(data$Book.of.Death == 2 & !is.na(data$Death.Chapter))
D3 = which(data$Book.of.Death == 3 & !is.na(data$Death.Chapter))
D4 = which(data$Book.of.Death == 4 & !is.na(data$Death.Chapter))
D5 = which(data$Book.of.Death == 5 & !is.na(data$Death.Chapter))

data$In_Book[D1] = data$Chapters_featured[D1] - (73 - data$Death.Chapter2[D1]) 
data$In_Book[D2] = data$Chapters_featured[D2] - (70 - data$Death.Chapter2[D2])
data$In_Book[D3] = data$Chapters_featured[D3] - (82 - data$Death.Chapter2[D3])
data$In_Book[D4] = data$Chapters_featured[D4] - (46 - data$Death.Chapter2[D4]) 
data$In_Book[D5] = data$Chapters_featured[D5] - (73 - data$Death.Chapter2[D5])


which(data$In_Book < 0)
data$Name[which(data$In_Book < 0)]

hist(data$In_Book, breaks = 50, main = "Chapters Surviving in Book 1-5", xlab= "# of chapters")


# In_Book is now good enough. whatever.
# Average number of chapters a character lives if they're introduced and killed in same book:
summary(data$In_Book[Same_book_death])



# So how do we start running analysis:



#### NOW DOING THIS FOR JUST BOOKS 1-4:



data2 = read.csv2("character-deaths.csv", header = T, sep = ",")
# drop the DwD variable, and then drop all chatactes e
data2$DwD = NULL


# Some of the data is wrong, here are fixes:
# Cressen is intro'd and dies in book 2
data2$Book.of.Death[157] = 2
# Shyra Errol Died in Book 3, did not appear


# add on 1 to the death chapter number and intro chapter number, for prologues.
data2$Book.Intro.Chapter = data2$Book.Intro.Chapter + 1
data2$Death.Chapter = data2$Death.Chapter + 1

X_data2 = which(data2$FfC + data2$SoS + data2$CoK + data2$GoT == 0)
data2 = data2[-X_data2,]
data2$Book.of.Intro = 0
X1_data2 = which(data2$GoT == 1)
X2_data2 = which(data2$CoK == 1 & data2$GoT == 0)
X3_data2 = which(data2$SoS== 1 & data2$CoK == 0 & data2$GoT == 0)
X4_data2 = which(data2$FfC ==1 & data2$SoS == 0 & data2$CoK == 0 & data2$GoT == 0)

# this one isn't working jsut yet.
data2$Book.of.Intro[X1_data2] = 1
data2$Book.of.Intro[X2_data2] = 2
data2$Book.of.Intro[X3_data2] = 3
data2$Book.of.Intro[X4_data2] = 4

Same_book_death_data2 = which(data$Book.of.Intro == data$Book.of.Death)
Dies_same_book_1_data2 = sum(data2$Book.of.Intro[Same_book_death_data2] == 1)
Dies_same_book_2_data2 = sum(data2$Book.of.Intro[Same_book_death_data2] == 2)
Dies_same_book_3_data2 = sum(data2$Book.of.Intro[Same_book_death_data2] == 3)
Dies_same_book_4_data2 = sum(data2$Book.of.Intro[Same_book_death_data2] == 4)


# Average number of deaths of characters that are introduced and die in the same book.
mean(c(Dies_same_book_1,Dies_same_book_2,Dies_same_book_3,Dies_same_book_4))
# Average of 41.5 deaths.

data2$Chapters_featured = data2$GoT*book1chap + data2$CoK*book2chap + data2$SoS*book3chap + data2$FfC*book4chap

# subtract the chapters before a character appears in GoT
data2$Chapters_featured[X1_data2] = data2$Chapters_featured[X1_data2] - data2$Book.Intro.Chapter[X1_data2] + 1
data2$Chapters_featured[X2_data2] = data2$Chapters_featured[X2_data2] - data2$Book.Intro.Chapter[X2_data2] + 1 
data2$Chapters_featured[X3_data2] = data2$Chapters_featured[X3_data2] - data2$Book.Intro.Chapter[X3_data2] + 1
data2$Chapters_featured[X4_data2] = data2$Chapters_featured[X4_data2] - data2$Book.Intro.Chapter[X4_data2] + 1


data2$Death.Chapter2 = data2$Death.Chapter
data2$Death.Chapter2[is.na(data2$Death.Chapter)] = 0
Not_Dead2 = is.na(data2$Death.Chapter)
Isnt_Dead2 = sum(Not_Dead2)

# subtract the difference in chapters before death and total # of chapters in book.
data2$In_Book = data2$Chapters_featured
data2 = data.frame(data2)
data2 = data2[which(data2$In_Book > 0),]


D1_data2 = which(data2$Book.of.Death == 1 & !is.na(data2$Death.Chapter))
D2_data2 = which(data2$Book.of.Death == 2 & !is.na(data2$Death.Chapter))
D3_data2 = which(data2$Book.of.Death == 3 & !is.na(data2$Death.Chapter))
D4_data2 = which(data2$Book.of.Death == 4 & !is.na(data2$Death.Chapter))
D5_data2 = which(data2$Book.of.Death == 5 & !is.na(data2$Death.Chapter))

data2$In_Book[D1_data2] = data2$Chapters_featured[D1_data2] - (73 - data2$Death.Chapter2[D1_data2]) 
data2$In_Book[D2_data2] = data2$Chapters_featured[D2_data2] - (70 - data2$Death.Chapter2[D2_data2])
data2$In_Book[D3_data2] = data2$Chapters_featured[D3_data2] - (82 - data2$Death.Chapter2[D3_data2])
data2$In_Book[D4_data2] = data2$Chapters_featured[D4_data2] - (46 - data2$Death.Chapter2[D4_data2])

hist(data2$In_Book, breaks = 50, main = "Chapter survival book 1-4")

###
# Now get a list / chapter # who survive to book 5

data3 = data[data$Book.of.Death ==5 | is.na(data$Book.of.Death),]
hist(data3$In_Book, breaks = 50, main = "Chapter survival book 5")



### VARIABLES

# For Book 5, did the characters appear in book 4?
data$Appear45 = 0
data$Appear45[data$FfC == 1 & data$DwD == 1] = 1 

# For Book 4, did the characters appear in book 3?

data$Appear34 = 0
data$Appear34[data$SoS == 1 & data$FfC == 1] = 1 

data$is.Dead = 0
data$is.Dead[data$Death.Chapter2 > 0] = 1

data2$is.Dead = 0
data2$is.Dead[data2$Death.Chapter2 > 0] = 1

data3$is.Dead = 0
data3$is.Dead[data3$Death.Chapter2 > 0] = 1



data = data[which(data$In_Book > 0),]
data2 = data2[which(data2$In_Book > 0),]



### For this part, we may have to change to dead, isn't dead. But we'll keep empirical median for now.

### For this part, we may have to change to dead, isn't dead. But we'll keep empirical median for now.

# For books 1-4
Binary_Matrix14 = matrix(rep(0,16), nrow = 4, ncol = 4)
colnames(Binary_Matrix14) = c("Nobility", "Gender", "Dies", "Lives")
Binary_Matrix14[,1] = c(1,1,0,0)
Binary_Matrix14[,2] = c(0,1,0,1)
Binary_Matrix14[1,3] = length(which(data2$is.Dead == 1 & data2$Gender ==0 & data2$Nobility == 1))
Binary_Matrix14[1,4] = length(which(data2$is.Dead == 0 & data2$Gender ==0 & data2$Nobility == 1))
Binary_Matrix14[2,3] = length(which(data2$is.Dead == 1 & data2$Gender ==1 & data2$Nobility == 1))
Binary_Matrix14[2,4] = length(which(data2$is.Dead == 0 & data2$Gender ==1 & data2$Nobility == 1))
Binary_Matrix14[3,3] = length(which(data2$is.Dead == 1 & data2$Gender ==0 & data2$Nobility == 0))
Binary_Matrix14[3,4] = length(which(data2$is.Dead == 0 & data2$Gender ==0 & data2$Nobility == 0))
Binary_Matrix14[4,3] = length(which(data2$is.Dead == 1 & data2$Gender ==1 & data2$Nobility == 0))
Binary_Matrix14[4,4] = length(which(data2$is.Dead == 0 & data2$Gender ==1 & data2$Nobility == 0))

# For books 1-5
Binary_Matrix15 = matrix(rep(0,16), nrow = 4, ncol = 4)
colnames(Binary_Matrix15) = c("Nobility", "Gender", "Dies", "Lives")
Binary_Matrix15[,1] = c(1,1,0,0)
Binary_Matrix15[,2] = c(0,1,0,1)
Binary_Matrix15[1,3] = length(which(data$is.Dead == 1 & data$Gender ==0 & data$Nobility == 1))
Binary_Matrix15[1,4] = length(which(data$is.Dead == 0 & data$Gender ==0 & data$Nobility == 1))
Binary_Matrix15[2,3] = length(which(data$is.Dead == 1 & data$Gender ==1 & data$Nobility == 1))
Binary_Matrix15[2,4] = length(which(data$is.Dead == 0 & data$Gender ==1 & data$Nobility == 1))
Binary_Matrix15[3,3] = length(which(data$is.Dead == 1 & data$Gender ==0 & data$Nobility == 0))
Binary_Matrix15[3,4] = length(which(data$is.Dead == 0 & data$Gender ==0 & data$Nobility == 0))
Binary_Matrix15[4,3] = length(which(data$is.Dead == 1 & data$Gender ==1 & data$Nobility == 0))
Binary_Matrix15[4,4] = length(which(data$is.Dead == 0 & data$Gender ==1 & data$Nobility == 0))



# Now we can do a logistic regression much like the death panel data:


# For the WinBUGS Code, we need CSV text files of Nobility, Gender, Is.Dead, and In_Book for books1-5 and books1-4
# beforehand, we'll need to omit the NA In_Books, since they're not 'on stage'
# We'll also need to eliminate characters with negative or NA chapter numbers.

write.table(data$Nobility, file = "Nobility Books 1-5", sep = ",", row.names = F, col.names = F, eol = ", ")
write.table(data$Gender, file = "Gender Books 1-5", sep = ",", row.names = F, col.names = F, eol = ", ")
write.table(data2$Nobility, file = "Nobility Books 1-4", sep = ",", row.names = F, col.names = F, eol = ", ")
write.table(data2$Gender, file = "Gender Books 1-4", sep = ",", row.names = F, col.names = F, eol = ", ")
write.table(data$In_Book, file = "In_Book Books 1-5", sep = ",", row.names = F, col.names = F, eol = ", ")
write.table(data2$In_Book, file = "In_Book Books 1-4", sep = ",", row.names = F, col.names = F, eol = ", ")





# randomize sample for Hierarchical Model

data15 = data[which(data$In_Book>0),] 

id11 = which(data15$Nobility==1 & data15$Gender==1)
id10 = which(data15$Nobility==1 & data15$Gender==0)
id01 = which(data15$Nobility==0 & data15$Gender==1)
id00 = which(data15$Nobility==0 & data15$Gender==0)
mat = matrix(0,nrow = 20, ncol=8)
set.seed(2017)
for (i in 1:20){
  m = round(runif(1,20,40))
  id1 = id11[sample(1:length(id11),m)]
  t1 = sum(data15[id1,"is.Dead"]==0)
  t2 = m
  
  m = round(runif(1,20,40))
  id2 = id10[sample(1:length(id10),m)]
  t3 = sum(data15[id2,"is.Dead"]==0)
  t4 = m
  
  m = round(runif(1,20,40))
  id3 = id01[sample(1:length(id01),m)]
  t5 = sum(data15[id3,"is.Dead"]==0)
  t6 = m
  
  m = round(runif(1,20,40))
  id4 = id00[sample(1:length(id00),m)]
  t7 = sum(data15[id4,"is.Dead"]==0)
  t8 = m
  
  mat[i,]=c(t1,t2,t3,t4,t5,t6,t7,t8)
}

mat

y.s = c(mat[,1],mat[,3],mat[,5],mat[,7])
n.s = c(mat[,2],mat[,4],mat[,6],mat[,8])

xn = c(rep(1,40),rep(0,40))
xg = c(rep(1,20),rep(0,20),rep(1,20),rep(0,20))

write.table(t(y.s),file = "y.s_15.txt",sep = ",",quote = F,row.names = F,col.names = F)
write.table(t(n.s),file = "n.s_15.txt",sep = ",",quote = F,row.names = F,col.names = F)
write.table(t(xn),file = "xn_15.txt",sep = ",",quote = F,row.names = F,col.names = F)
write.table(t(xg),file = "xg_15.txt",sep = ",",quote = F,row.names = F,col.names = F)





###### IGNORE FOR NOW. #########

## Let's see if we can do 8 with 25 percentile chapter survival?
median(data15$In_Book)
quantile(data15$In_Book, 0.25)
mean(data15$In_Book)

# try 25-50 range for quantile.
# 50 chapters for median
# 23 for (7)25%th percentile
# 70 for average-ish length of 1 books?
data15 = data[which(data$In_Book>0),] 

id110 = which(data15$Nobility==1 & data15$Gender==1 & data15$In_Book < 23 | data15$In_Book > 107)
id100 = which(data15$Nobility==1 & data15$Gender==0 & data15$In_Book < 23 | data15$In_Book > 107)
id010 = which(data15$Nobility==0 & data15$Gender==1 & data15$In_Book < 23 | data15$In_Book > 107)
id000 = which(data15$Nobility==0 & data15$Gender==0 & data15$In_Book < 23 | data15$In_Book > 107)
id111 = which(data15$Nobility==1 & data15$Gender==1 & data15$In_Book >= 23 & data15$In_Book <= 107)
id101 = which(data15$Nobility==1 & data15$Gender==0 & data15$In_Book >= 23 & data15$In_Book <= 107)
id011 = which(data15$Nobility==0 & data15$Gender==1 & data15$In_Book >= 23 & data15$In_Book <= 107)
id001 = which(data15$Nobility==0 & data15$Gender==0 & data15$In_Book >= 23 & data15$In_Book <= 107)

mat = matrix(0,nrow = 20, ncol=16)
set.seed(2017)
for (i in 1:20){
  m = round(runif(1,20,40))
  id1 = id110[sample(1:length(id110),m)]
  t1 = sum(data15[id1,"is.Dead"]==0)
  t2 = m
  
  m = round(runif(1,20,40))
  id2 = id100[sample(1:length(id100),m)]
  t3 = sum(data15[id2,"is.Dead"]==0)
  t4 = m
  
  m = round(runif(1,20,40))
  id3 = id010[sample(1:length(id010),m)]
  t5 = sum(data15[id3,"is.Dead"]==0)
  t6 = m
  
  m = round(runif(1,20,40))
  id4 = id000[sample(1:length(id000),m)]
  t7 = sum(data15[id4,"is.Dead"]==0)
  t8 = m
  
  m = round(runif(1,20,40))
  id5 = id111[sample(1:length(id111),m)]
  t9 = sum(data15[id1,"is.Dead"]==0)
  t10 = m
  
  m = round(runif(1,20,40))
  id6 = id101[sample(1:length(id101),m)]
  t11 = sum(data15[id2,"is.Dead"]==0)
  t12 = m
  
  m = round(runif(1,20,40))
  id7 = id011[sample(1:length(id011),m)]
  t13 = sum(data15[id3,"is.Dead"]==0)
  t14 = m
  
  m = round(runif(1,20,40))
  id8 = id001[sample(1:length(id001),m)]
  t15 = sum(data15[id4,"is.Dead"]==0)
  t16 = m
  
  mat[i,]=c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16)
}

mat

y.s1 = c(mat[,1],mat[,3],mat[,5],mat[,7])
n.s1 = c(mat[,2],mat[,4],mat[,6],mat[,8])

y.s2 = c(mat[,9],mat[,11],mat[,13],mat[,15])
n.s2 = c(mat[,10],mat[,12],mat[,14],mat[,16])

y.15 = c(y.s1, y.s2)
n.15 = c(n.s1, n.s2)

xn1 = c(rep(1,40),rep(0,40))
xg1 = c(rep(1,20),rep(0,20),rep(1,20),rep(0,20))

xn2 = c(rep(1,40),rep(0,40))
xg2 = c(rep(1,20),rep(0,20),rep(1,20),rep(0,20))

xc = c(rep(1,10),rep(0,10),rep(1,10),rep(0,10),rep(1,10),rep(0,10),rep(1,10),rep(0,10))

xn = c(xn1, xn2)
xg = c(xg1, xg2)
xc = c(xc,xc)




length(xn)
length(y.15)

write.table(t(y.s),file = "y.s_15.txt",sep = ",",quote = F,row.names = F,col.names = F)
write.table(t(n.s),file = "n.s_15.txt",sep = ",",quote = F,row.names = F,col.names = F)
write.table(t(xn),file = "xn_15.txt",sep = ",",quote = F,row.names = F,col.names = F)
write.table(t(xg),file = "xg_15.txt",sep = ",",quote = F,row.names = F,col.names = F)


