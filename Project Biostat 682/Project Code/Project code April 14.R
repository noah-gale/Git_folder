# NOAH GALE CODE APRIL 14, 2017
# CODE FROM DATA CLEANING UP TO LOGIT MODEL AND ODDS FROM CHAPTERS


#setwd("~/Dropbox/Course On Doing/BIOSTAT 682/Project/dataset") 
setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/Biostat 682/Project")

data = read.csv2("character-deaths.csv", header = T, sep = ",")
data = data[which(data$Book.Intro.Chapter >=0),]


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
data2 = data2[which(data$Book.Intro.Chapter >=0),]
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

# For books 1-4
Binary_Matrix14 = matrix(rep(0,16), nrow = 4, ncol = 4)
colnames(Binary_Matrix14) = c("Nobility", "Gender", "Dies", "Lives")
Binary_Matrix14[,1] = c(1,1,0,0)
Binary_Matrix14[,2] = c(0,1,0,1)
Binary_Matrix14[1,3] = length(which(data2$is.Dead == 1 & data2$Gender ==0 & data2$Nobility == 1))
Binary_Matrix14[1,4] = length(which(data2$is.Dead == 0 & data2$Gender ==0 & data2$Nobility == 1))
Binary_Matrix14[2,3] = length(which(data2$Iis.Dead == 1 & data2$Gender ==1 & data2$Nobility == 1))
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




## Then we use rjags to check convergence in each of the variables from our WinBugs Code.
# Let's try jags
In_Book14 = as.integer(data2$In_Book)
In_Book15 = as.integer(data$In_Book)



# SIYU'S LOGIT CODE WORKS--------------------------------------------------

prior_model_Logit = "model{
for(i in 1:M){
death.yes[i] ~ dbin(p[i], n[i])
logit(p[i]) <- alpha + beta1*v.Gen[i] + beta2*v.Nob[i]
}

alpha ~ dnorm(0, 1.0E-6) 
beta1 ~ dnorm(0, 1.0E-6)
beta2 ~ dnorm(0, 1.0E-6)

ORgen <- exp(beta1)
ORnob <- exp(beta2)
prb1 <- step(0-beta1)
prb2 <- step(0-beta2)
}"

prior_model.con_L<-textConnection(prior_model_Logit)

death.yes.15 = Binary_Matrix15[,4]
death.no.15 = Binary_Matrix15[,3]
n.15 = death.no.15 + death.yes.15
v.Gen.15 = Binary_Matrix15[,2]
v.Nob.15 = Binary_Matrix15[,1]
M.15 = 4

jags15.l = jags.model(prior_model.con_L, n.chains = 1, n.adapt = 200, data = list('death.yes' = death.yes.15, 'n' = n.15,'v.Gen' = v.Gen.15, 'v.Nob' = v.Nob.15, 'M' = M.15))
sample15.l = coda.samples(jags15.l, c("alpha","beta1","beta2", "ORgen", "ORnob", "prb1", "prb2"), 10000)
quartz()
plot(sample15.l, trace = FALSE)
summary(sample15.l)

# Put in the summary, and say what beta1 and beta2 are:
# Use median beta1, beta2 for our distribution, since more robust?

#1. Empirical mean and standard deviation for each variable,
#plus standard error of the mean:
  
#  Mean             SD  Naive SE Time-series SE
#ORgen  0.5145 0.10883 0.0010883      0.0050837
#ORnob  1.8682 0.27346 0.0027346      0.0051406
#alpha  1.0497 0.20975 0.0020975      0.0103166
#beta1 -0.6871 0.21342 0.0021342      0.0104747
#beta2  0.6143 0.14632 0.0014632      0.0027611
#prb1   0.9998 0.01414 0.0001414      0.0001414
#prb2   0.0000 0.00000 0.0000000      0.0000000

#2. Quantiles for each variable:
  
#  2.5%     25%     50%     75%   97.5%
#ORgen  0.3274  0.4379  0.5067  0.5789  0.7562
#ORnob  1.3753  1.6797  1.8487  2.0396  2.4463
#alpha  0.6493  0.9088  1.0437  1.1864  1.4657
#beta1 -1.1165 -0.8257 -0.6798 -0.5467 -0.2795
#beta2  0.3187  0.5186  0.6145  0.7128  0.8946
#prb1   1.0000  1.0000  1.0000  1.0000  1.0000
#prb2   0.0000  0.0000  0.0000  0.0000  0.0000


median1 = -0.8257
median2 = 0.5186
sd1 = 0.21342
sd2 = 0.14632

### -----------------------------------------------------
# SIYU'S CODE IT WORKS

require(rjags)

# Weibull IT WORKS
#n.15 = sum(death.no.15 + death.yes.15)
Is.Dead15 = data$is.Dead
Gender15 = data$Gender
Nobility15 = data$Nobility
In_Book_15 = data$In_Book
#In_Book_152 = data$In_Book
N_15 =  length(data$In_Book)
prior_model_Weibull = "model{

#alpha ~ dnorm(0, 1.0E-6) 
beta1 ~ dnorm(0, 1.0E-6)
beta2 ~ dnorm(0, 1.0E-6)

for (i in 1:N){
#Is.Dead[i] <- Is.Dead[i]
#Gender[i] <- Gender[i]
#Nobility[i] <- Nobility[i]
In_Book[i] ~ dweib(alpha,beta)
Is.Dead[i] ~ dbern(Prob[i])
logit(Prob[i]) <- log(exp(-beta*pow(In_Book[i], alpha)) - exp(-beta*pow(In_Book[i] + 1, alpha)) /(1 -exp(-beta*pow(In_Book[i], alpha)) - exp(-beta*pow(In_Book[i] + 1, alpha))  ) )     #+ beta1*Gender[i] + beta2*Nobility[i]
}
# Given -> c(120, 1, 0) What's the probability this person lives for 1 more chapter?

alpha1 ~ dnorm(0, 1.0E-6) 
#beta_nu ~dunif(0,100)
alpha ~ dunif(0,100)
beta ~ dunif(0,100)
#beta1 ~ dnorm(median1, sd1)
#beta2 ~ dnorm(median2, sd2)
}"

prior_model.con_W<-textConnection(prior_model_Weibull)

jags15 = jags.model(prior_model.con_W, n.chains = 1, n.adapt = 10000, 
                    data = list('In_Book' = In_Book_15, 'N' = N_15, 'Is.Dead15' = Is.Dead15,
                                'Gender' = Gender15, 'Nobility' = Nobility15, 'median1' = median1,
                                'median2' = median2, 'sd1' = sd1, 'sd2' = sd2))
sample15 = coda.samples(jags15, c("alpha", "beta"), 50000)

quartz()
plot(sample15, trace = FALSE)
summary(sample15)

## Summary

#Iterations = 201:50200
#Thinning interval = 1 
#Number of chains = 1 
#Sample size per chain = 50000 

#1. Empirical mean and standard deviation for each variable,
#plus standard error of the mean:
  
#  Mean       SD  Naive SE Time-series SE
#BetaC1  -0.01678 0.002096 9.373e-06      5.002e-05
#BetaC70 -0.96762 0.034041 1.522e-04      3.806e-04
#alpha    0.95603 0.025560 1.143e-04      6.148e-04
#beta     0.01678 0.002096 9.373e-06      5.002e-05
#lambda   0.13497 0.009224 4.125e-05      4.013e-05
#theta   49.42510 2.018179 9.026e-03      3.061e-02

#2. Quantiles for each variable:
  
#  2.5%      25%      50%      75%    97.5%
#alpha    0.90639  0.93870  0.95588  0.97317  1.00686
#beta     0.01301  0.01532  0.01666  0.01812  0.02123
#lambda   0.11745  0.12865  0.13477  0.14106  0.15362
#theta   45.52642 48.06692 49.40457 50.76452 53.43730


# Note: Every time we plug in different data, we need to re-compile the model

WEIB_PRIOR15 = sample15
geweke.diag(WEIB_PRIOR15, frac1=0.1, frac2=0.5)
geweke.plot(WEIB_PRIOR15, frac1=0.1, frac2=0.5)

# These Work!



# chance of living to x chapters from Midterm:
# exp(-Beta*X^Alpha)

# Which means, chance of living to x + y chapters, after living x chapters:
# exp(-Beta*X^Alpha) - exp(-Beta*(X+Y)^Alpha)
# Can say Y = 1, or Y = 70


# Find a way to do it in R, or let it be done in Winbugs.

# Logit
# I'm going to pretend that we have the results for this.

prior_model_Logit = "model{
for(i in 1:M){
logit(p[i]) <- alpha + beta1*v.Gen[i] + beta2*v.Nob[i]
death.yes14[i] ~ dbin(p[i], n14[i])
}
alpha ~dnorm(0, 1.0E-6) # give parameters diffuse proper priors
beta1 ~dnorm(0, 1.0E-6)
beta2 ~dnorm(0, 1.0E-6)
ORgen <-exp(beta1)
ORnob <-exp(beta2)
prb1 <- step(0-beta1)
prb2 <- step(0-beta2)
}"


# Values for Logit15 (can directly link them to Matrixes later)
death.yes15 = c(154, 31, 219, 42)
death.no15 = c(211, 31, 195, 31)
v.Gen15 = c(1,1,0,0)
v.Nob15 = c(1,0,1,0)
n15 = death.yes15 + death.no15
M = 4

# Values for Logit14 (can directly link them to Matrixes later)
death.yes14 = c(154, 31, 219, 42)
death.no14 = c(211, 31, 195, 31)
v.Gen14 = c(1,1,0,0)
v.Nob14 = c(1,0,1,0)
n14 = death.yes14 + death.no14
M = 4


prior_model.con_L<-textConnection(prior_model_Logit)
jags14L = jags.model(prior_model.con_L, n.chains = 1, n.adapt = 10000, 
                    data = list("death.yes" = death.yes14, n14 = "n14", 'v.Gen' = v.Gen14, 'v.Nob' = v.Nob14, 'M' = M))
sample14 = coda.samples(jags14L, c("beta1","beta2", "alpha", "ORgen", "ORnob", "prb1", "prb2"), 50000)


# put the tests here.
# Gelman Rubin 1-4 and 1-5 


LOGIT_PRIOR = mcmc.list("Honbos file from WinBUGS")
gelman.diag(LOGIT_PRIOR, confidence = 0.95, transform = FALSE, multivariate =  TRUE)

# google it.

# Geweke's 1-4 and 1-5
WEIB_PRIOR14 = sample14
geweke.diag(WEIB_PRIOR14, frac1=0.1, frac2=0.5)
geweke.plot(WEIB_PRIOR14, frac1=0.1, frac2=0.5)

# Geweke plot? what does it do?


# We can try and merge these 4 plots to one graph to make things easier on ourselves.

# Put in our WINBUGS CODE HERE (could possibly do the whole thing in R)

# Gelman and Rubin for multiple chains (logit for Nobility and Gender)
# Geweke's conversion for single chains (Weibull dist In_Book). page 69.
# Variables: A, B, med, alpha, beta1, beta2, beta3, sd1, sd2, sd3,


### FULL MODEL USING ALL PIECES
# Let's use for books 1-5

# Using BetaC1
median1 = -0.01666
sd1 = 0.002096
median2 = .
sd2 = .
median3 = .
sd3 = .


In_Book = data$In_Book
Nobility15 = data$Nobility
Gender15 = data$Gender
N = length(data$In_Book)
DF = N - 1
post_model_full14 = "model{
  for(i in 1:N){
    logit(p[i]) ~ alpha + beta1*In_Book14 + beta2*Gender14 + beta3*Nobility14
  }
  alpha ~ dunif(0, 1000)
  beta1 ~ dt(median1, pow(1/sd1, 2), DF)
  beta2 ~ dnorm(median2, sd2)
  beta3 ~ dnorm(median3, sd3)
  prb1 <- exp(beta1)
  prb2 <- exp(beta2)
  prb3 <- exp(beta3)
}"

post_model.con<-textConnection(prior_model_Logit)
jags14POST = jags.model(post_model.con, n.chains = 1, n.adapt = 200, 
                     data = list("In_Book" =In_Book, "Nobility14" = Nobility14, "Gender14" = Gender14,
                                 "median1" = median1, "sd1" = sd1, "median2" = median2, "sd2" = sd2,
                                 "median3" = median3, "sd3" = sd3))
sample14POST = coda.samples(jags14L, c("beta1","beta2", "alpha", "ORgen", "ORnob", "prb1", "prb2"), 50000)







##### CODE WE PROBABLY DON'T NEED.

### Try to do bayes analysis now

# Example

events.0=0   # for X = 0
events.1=5   # for X = 1
x = c(rep(0,100), rep(1,100))
y = c(rep(0,100-events.0), rep(1,events.0),
      rep(0, 100-events.1), rep(1, events.1))

library(MCMCpack)
logmcmc = MCMClogit(y~as.factor(x), burnin=1000, mcmc=21000, b0=0, B0=.04)

# try with the data

functionW = function(x){
  sum(dweibull(shape = 1, x))
}

### Generalized Linear Models

# Books 1-5
logmcmc_idea = MCMClogit(is.Dead ~ In_Book + as.factor(Gender) + as.factor(Nobility), data= data, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz() 
plot(logmcmc_idea)

# Books 1-6
logmcmc_idea2 = MCMClogit(is.Dead ~ In_Book + as.factor(Gender) + as.factor(Nobility), data= data2, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz()
plot(logmcmc_idea2)

# Book 5
logmcmc_idea3 = MCMClogit(is.Dead ~ In_Book + as.factor(Gender) + as.factor(Nobility), data= data3, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz()
plot(logmcmc_idea3)

# Retry with logmcmc
logmcmc_idea_retry = MCMClogit(is.Dead ~ In_Book + as.factor(Nobility), data= data, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz() 
plot(logmcmc_idea_retry)
summary(logmcmc_idea_retry)

CI_In_Book_MCMC = c(-0.01392, -0.008265)
Median_In_Book_MCMC = -0.01087 # Median of Beta log odds in book.




(which(data$In_Book <= 49 & data$Gender ==0 & data$Nobility == 1))


# What are the mean estimated odds of a character for books 1-6, given the number of chapters they've lived?
data$Odds_Chapter = -0.01099*data$In_Book

# Characters with odds less than -2 from # of Chapters:
print(na.omit(data$Name[data$Odds_Chapter < -2 & data$is.Dead == 0]))

quantile(data$Odds_Chapter, probs = c(0.975, 0.025), na.rm = TRUE)

quantile(data$Odds_Chapter, probs = c(0.90, 0.10), na.rm = TRUE)
quantile(data$Odds_Chapter, probs = 0.1, na.rm = TRUE)

print(na.omit(data$Name[data$Odds_Chapter < quantile(data$Odds_Chapter, probs = 0.05, na.rm = TRUE) 
                        & data$is.Dead == 0]))


### Try Linear Regression
require(LearnBayes)
Y = data2$is.Dead
X = cbind(1, data2$In_Book, as.factor(data2$Gender), as.factor(data2$Nobility));X
m = 10000
result = blinreg(Y, X, m)

hist(result$beta[,2], main = "Histogram of Beta In_Book", xlab = " ")
hist(result$beta[,3], main = "Histogram of Beta Gender", xlab = " ")
hist(result$beta[,4], main = "Histogram of Beta Nobility", xlab = " ")
hist((result$sigma)^2,main = "Histogram of Sigma", xlab = " " )

apply(result$beta, 2, mean)
mean(result$sigma)

# Try to predict
covariates <- matrix(c(1, 215, 1, 1),1,4)
predicted <- blinregpred(covariates, result)
mean(predicted)
c(quantile(predicted,0.025),quantile(predicted,0.975))
hist(predicted)


### Another Linear Regression
data.f = cbind(data2$is.Dead, data2$In_Book, data2$Gender, data2$Nobility)
data.f = data.frame(data.f)
data.f = data.f[complete.cases(data.f),]
colnames(data.f) = c('is.Dead', 'In_Book', 'Gender', 'Nobility')

set.seed(321)
n = length(data.f$is.Dead)
Y = data.f$is.Dead
X = cbind(rep(1,n), data.f$In_Book, as.factor(data.f$Gender),  as.factor(data.f$Nobility))
colnames(X) = c('obs', 'In_Book', 'Gender', 'Nobility')
# reusing code / updating code from 1E
beta = matrix(rep(0,4*10000), ncol = 4)
beta_hat = solve (t(X) %*% X) %*% t(X) %*% Y
tau = runif(1,0,1)
sigma = 1/ tau
S = sigma * solve(t(X) %*% X)
tau_samples = 1:10000
for (i in 1:10000){
  beta[i,] = mvrnorm(1, beta_hat, S)
  tau = rgamma(1, (n-k)/2, 0.5*t(Y - X %*% beta_hat) %*% (Y - X %*% beta_hat))
  sigma = 1/tau
  S = sigma*solve(t(X) %*% X)
  tau_samples[i] = tau
}
new_person = c(1, 215, 1, 1)
var = sigma*(1 + t(new_person)%*%solve(t(X)%*%X)%*%new_person)
x_bar = t(new_person) %*% beta_hat
# Mean Predicted Value
x_bar
# Lower Bound of Prediction Interval
qt(0.025, n-k)*sqrt(var) + t(new_person) %*% beta_hat
# Upper Bound of Prediction Interval
qt(0.975, n-k)*sqrt(var) + t(new_person) %*% beta_hat

### Hanbo's Linear regression

x = as.matrix(cbind(1,data2[7:12]))
y = as.matrix(data2$In_Book)
y[which(is.na(y))] =3

n=nrow(x);
k=7;
set.seed(2017)
N = 10000

betah = solve(t(x)%*%x)%*%t(x)%*%y 
betah
pre = rep(0,N)
beta <- matrix(rep(0,k*N),nrow=N)

for (i in 1:N){
  pre[i] = rgamma(1,shape = (n-k)/2, rate = t(y-x%*%betah)%*%(y-x%*%betah)/2)
  phi = solve(t(x)%*%x)/pre[i]
  beta[i,] = mvrnorm(1,mu = betah,Sigma = phi)
}

mean(beta[,1])
mean(beta[,2])
mean(beta[,3])
mean(beta[,4])
mean(beta[,5])
mean(beta[,6])
mean(beta[,7])

p1 <- ggplot(data.frame(t=1:N,x=beta[,1]),aes(x=t,y = x))+
  geom_line()+ggtitle("Trace plot of beta_1")

par(mfrow=c(1,1))
acf(beta[,1])

quantile(beta[,1],c(0.025,0.975))
quantile(beta[,2],c(0.025,0.975))
quantile(beta[,3],c(0.025,0.975))
quantile(beta[,4],c(0.025,0.975))
quantile(beta[,5],c(0.025,0.975))
quantile(beta[,6],c(0.025,0.975))
quantile(beta[,7],c(0.025,0.975))


rownames(data)
rownames(data2)


