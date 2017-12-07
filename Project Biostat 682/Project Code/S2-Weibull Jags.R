
#### Use sample15


# hazard function alpha*beta(-beta*x)^alpha-1
# pdf 

# try exp((1 - lambda(x= beginning number)), exp(xb))

 #1 - exp(exp(beta*t1, alpha) - exp(beta*t2, alpha), 1)
# Jsnow <- 1 - exp(pow(beta*In_Book[410], alpha)) - pow(beta*(In_Book[410]+70), alpha))

require(rjags)

prior_model_Weibull = "model{
for (i in 1:N){
In_Book[i] ~ dweib(alpha,beta)
}
alpha ~ dunif(0,100)
beta ~ dunif(0,100)

theta <- pow(-log(0.5)/beta,1/alpha)
lambda1 <- exp(-beta*pow(70,alpha))
lambda2 <- exp(-beta*pow(140,alpha))
lambda3 <- exp(-beta*pow(210,alpha))
lambda4 <- exp(-beta*pow(280,alpha))
lambda5 <- exp(-beta*pow(350,alpha))
Jsnow <- 1 - exp(beta*pow(343, alpha) - beta*pow((343+70), alpha))
Asha <- 1 - exp(beta*pow(178, alpha) - beta*pow((178+70), alpha))
JonCon<- 1 - exp(beta*pow(68, alpha) - beta*pow((68+70), alpha))
Davos <- 1 - exp(beta*pow(225, alpha) - beta*pow((225+70), alpha))
Arianne <- 1 - exp(beta*pow(117, alpha) - beta*pow((117+70), alpha))
}"

prior_model.con_W<-textConnection(prior_model_Weibull)

In_Book_15 = as.integer(data$In_Book)
N_15 = length(data$In_Book)
jags15 = jags.model(prior_model.con_W, n.chains = 8, n.adapt = 200, data = list('In_Book' = In_Book_15, 'N' = N_15))
sample15 = coda.samples(jags15, c("alpha","beta","theta", "lambda1", "lambda2", "lambda3", "lambda4", "lambda5", "Asha", "Jsnow", "Arianne", "JonCon", "Davos"), 10000)
#quartz()
#plot(sample15, trace = FALSE)
summary(sample15)
gelman.diag(sample15) # Gelman and Rubin Cinvergence Diagnostic
#quartz()
#gelman.plot(sample15)


###  PLOTTING OUR CHARACTER LIVES


Names = c("Areo", "Arianne", "Arya", "Asha", "Barristan", "Bran", "Cersie", "Daenerys", "Davos",
          "Jon Snow", "Jon Con", "Melisandre", "Samwell", "Sansa", "Theon", "Tyrion", "Victarion")
LCI = c(0.21431,0.14818,0.27899,0.21137,0.43001,0.18429,0.49299,0.21431,0.28291,0.37662,0.13112,
        0.09211,0.07931,0.36052,0.11604,0.18684,0.24267)
UCI = c(0.23994,0.16860,0.30905,0.23681,0.46513,0.20761,0.52916,0.23994,0.31323,0.41073,0.15022,
        0.10815,0.09433,0.39425,0.13396,0.21037,0.27040)
Mean = c(0.22683,0.15822,0.29365,0.22380,0.44748,0.19571,0.51089,0.22683,0.29771,0.39338,0.14051,
         0.10001,0.08671,0.37705,0.12485, 0.19835, 0.25619)


library(plyr)
#library(gg2themes)

predicts = data.frame(cbind(Names, UCI, LCI, Mean))
predicts = predicts[order(predicts$Mean, decreasing = FALSE),]

quartz()
plot(1:17, range(c(predicts$LCI, predicts$Mean, predicts$UCI)),
      xlab="Character)",
      ylab="% Chance of Death", type="n",
      main=expression("Chance of Death in within another Book"), mgp=c(1.5,.5,0))
abline (0,0,lty=2,lwd=.5)
points (freq, diff, pch=20)
for (i in 1:nrow(chickens))
  lines (rep(freq[i],2), diff[i]+1.96*se.diff[i]*c(-1,1), lwd=.5)




quartz()
G = ggplot(predicts, aes(y =Mean, x= Names)) +
  geom_point(aes(y= Mean, x = Names)) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Character") + ylab("Posterior Chance of Living Another 70 Chapters")
###### -----------------------------------------------------------------------------
G

#not working:

# Chance of living 70 more chapters from characters who are still alive? For our Point of View Characters.
Arya <-(exp(-beta*pow(In_Book[56],alpha)) - exp(-beta*pow(In_Book[56]+70,alpha)))/(exp(-beta*pow(In_Book[56],alpha))*70)
Asha <-(exp(-beta*pow(In_Book[58],alpha)) - exp(-beta*pow(In_Book[58]+70,alpha)))/(exp(-beta*pow(In_Book[58],alpha))*70)
Cersie <-(exp(-beta*pow(In_Book[131],alpha)) - exp(-beta*pow(In_Book[131]+70,alpha)))/(exp(-beta*pow(In_Book[131],alpha))*70)
Samwell <-  (exp(-beta*pow(In_Book[741],alpha)) - exp(-beta*pow(In_Book[741]+70,alpha)))/(exp(-beta*pow(In_Book[741],alpha))*70)
Victarion <-  (exp(-beta*pow(In_Book[857],alpha)) - exp(-beta*pow(In_Book[857]+70,alpha)))/(exp(-beta*pow(In_Book[857],alpha))*70)
Theon <-  (exp(-beta*pow(In_Book[801],alpha)) - exp(-beta*pow(In_Book[801]+70,alpha)))/(exp(-beta*pow(In_Book[801],alpha))*70)
Barristan <- (exp(-beta*pow(In_Book[67],alpha)) - exp(-beta*pow(In_Book[67]+70,alpha)))/(exp(-beta*pow(In_Book[67],alpha))*70)
Davos <- (exp(-beta*pow(In_Book[177],alpha)) - exp(-beta*pow(In_Book[177]+70,alpha)))/(exp(-beta*pow(In_Book[177],alpha))*70)
Tyrion <- (exp(-beta*pow(In_Book[834],alpha)) - exp(-beta*pow(In_Book[834]+70,alpha)))/(exp(-beta*pow(In_Book[834],alpha))*70)
Arianne <- (exp(-beta*pow(In_Book[45],alpha)) - exp(-beta*pow(In_Book[45]+70,alpha)))/(exp(-beta*pow(In_Book[45],alpha))*70)
Sansa <-  (exp(-beta*pow(In_Book[744],alpha)) - exp(-beta*pow(In_Book[744]+70,alpha)))/(exp(-beta*pow(In_Book[744],alpha))*70)
JonCon <- (exp(-beta*pow(In_Book[408],alpha)) - exp(-beta*pow(In_Book[408]+70,alpha)))/(exp(-beta*pow(In_Book[408],alpha))*70)
Bran <- (exp(-beta*pow(In_Book[107],alpha)) - exp(-beta*pow(In_Book[107]+70,alpha)))/(exp(-beta*pow(In_Book[107],alpha))*70)
Daenerys <-  (exp(-beta*pow(In_Book[163],alpha)) - exp(-beta*pow(In_Book[163]+70,alpha)))/(exp(-beta*pow(In_Book[163],alpha))*70)
Areo <- (exp(-beta*pow(In_Book[44],alpha)) - exp(-beta*pow(In_Book[44]+70,alpha)))/(exp(-beta*pow(In_Book[44],alpha))*70)
Melisandre <-  (exp(-beta*pow(In_Book[540],alpha)) - exp(-beta*pow(In_Book[540]+70,alpha)))/(exp(-beta*pow(In_Book[540],alpha))*70)
JSnow <- (exp(-beta*pow(In_Book[410],alpha)) - exp(-beta*pow(In_Book[410]+70,alpha)))/(exp(-beta*pow(In_Book[410],alpha))*70)


