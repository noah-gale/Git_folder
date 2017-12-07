# 682 project code

setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/Biostat 682/Project")
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


data$North = 0
data$North[data$Allegiances == "Wildling" |
           data$Allegiances == "Nights's Watch" |
           data$Allegiances == "Stark"] = 1


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


# Books 1-5
logmcmc_idea = MCMClogit(is.Dead ~ In_Book + as.factor(Gender) + as.factor(Nobility), data= data, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz() 
plot(logmcmc_idea)

# Books 1-4
logmcmc_idea2 = MCMClogit(is.Dead ~ In_Book + as.factor(Gender) + as.factor(Nobility), data= data2, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz()
plot(logmcmc_idea2)
summary(logmcmc_idea2)

# Book 5
logmcmc_idea3 = MCMClogit(is.Dead ~ In_Book + as.factor(Gender) + as.factor(Nobility), data= data3, burnin=10000, tune = 1, seed = 123, mcmc = 30000, user.prior.density = functionW, logfun = TRUE)
quartz()
plot(logmcmc_idea3)



# What are the mean estimated odds of a character for books 1-6, given the number of chapters they've lived?
data$Odds_Chapter = exp(-0.01099*data$In_Book)


# Characters with odds less than -2 from # of Chapters:
print(na.omit(data$Name[data$Odds_Chapter < -2 & data$is.Dead == 0]))

quantile(data$Odds_Chapter, probs = c(0.975, 0.025), na.rm = TRUE)

quantile(data$Odds_Chapter, probs = c(0.90, 0.10), na.rm = TRUE)
quantile(data$Odds_Chapter, probs = 0.1, na.rm = TRUE)

print(na.omit(data$Name[data$Odds_Chapter < quantile(data$Odds_Chapter, probs = 0.05, na.rm = TRUE) 
                        & data$is.Dead == 0]))




#### NUMBER OF DEATHS PER BOOK:

# We want to have a distribution of the # of people who are introduced and die in the same book:

prior_dies = c(48, 54, 55, 9, 26)
N = 5
X = rep(0, 10000)
X[1] = mean(prior_dies)

Y_bar = mean(prior_dies)
for(i in 2:length(X)){
  X[i] = rt(1, 4, (mean(X[1:i-1]) - Y_bar)*sqrt(N)/sd(prior_dies) )
}



# Predicting for single characters:
install.packages("LearnBayes")
library(LearnBayes)

Y = data2$is.Dead
X_Bayes = cbind(1, data2$In_Book, as.factor(data2$Gender), as.factor(data2$Nobility));X_Bayes
m = 10000
result = blinreg(Y, X_Bayes, m)


par(mfrow= c(2,2))
hist(result$beta[,2], main = "Histogram of Beta In_Book", xlab = " ")
hist(result$beta[,3], main = "Histogram of Beta Gender", xlab = " ")
hist(result$beta[,4], main = "Histogram of Beta Nobility", xlab = " ")
hist((result$sigma)^2,main = "Histogram of Sigma", xlab = " " )

apply(result$beta, 2, mean)
mean(result$sigma)
# Covariates Orde: Intercept, In_Book, Gender, Nobility 
covariates = matrix(c(-0.41269, 30, 1, 0), 1, 4)
predicted <- blinregpred(covariates, result)
mean(predicted)
c(quantile(predicted,0.025),quantile(predicted,0.975))





# HANBO'S IDEA
library(ggplot2)
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







# Predicting Total number of character deaths per book:
# 
death_number_total = c(length(D1), length(D2), length(D3), length(D4), length(D5))
death_number_total_sum = c(length(D1), length(D2)+length(D1), 
                           length(D3)+length(D2)+length(D1), 
                           length(D3)+length(D2)+length(D1)+length(D4), 
                           length(D3)+length(D2)+length(D1)+length(D4)+length(D5))
death_same_book = c(Dies_same_book_1, Dies_same_book_2, Dies_same_book_3, Dies_same_book_4, Dies_same_book_5)

# Idea Death_6 = beta1 + Beta2*death_number_total + Beta3*death_same_book_sum + E




