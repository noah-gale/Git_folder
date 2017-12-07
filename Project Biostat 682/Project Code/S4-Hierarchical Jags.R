
#### Use sample.RElogit.15.2

require(rjags)

# Model：random effects logit model

# New Data Book 1-5 (5-70)
RE_logit_model = "model {
for (i in 1:N) {
y[i] ~ dbin(p[i],n[i])
mu[i] <- alpha1 + alpha2*x1[i] + alpha3*x2[i] + alpha4*x1[i]*x2[i]
beta[i] ~ dnorm(mu[i],tau)
logit(p[i]) <- beta[i]
}
alpha1 ~ dnorm(0.0,1.0E-6)
alpha2 ~ dnorm(0.0,1.0E-6)
alpha3 ~ dnorm(0.0,1.0E-6)
alpha4 ~ dnorm(0.0,1.0E-6)
tau ~ dgamma(0.001,0.001)
sd <- sqrt(1.0/tau)
}"

RE_logit_model.con <- textConnection(RE_logit_model)
# Data from Book 1-5 (5-70). Much better than 30-50.
y.15.2 = c(31,20,20,20,23,18,23,24,26,22,17,27,26,15,23,15,21,23,25,19,24,33,21,21,29,27,23,31,22,32,24,29,29,23,27,32,25,24,28,25,24,18,17,21,13,24,12,24,23,18,16,22,20,15,10,17,20,15,16,14,22,27,17,25,17,24,26,15,24,27,24,22,22,19,19,21,27,25,22,28)
n.15.2 = c(38,29,28,30,28,23,33,31,36,36,27,39,39,23,34,23,27,37,39,28,31,35,24,26,35,34,27,39,25,38,30,36,33,28,31,36,31,26,31,29,34,37,29,38,27,40,21,37,35,34,25,33,33,26,25,25,30,27,25,25,30,36,23,36,24,31,37,21,35,34,32,26,33,30,27,27,33,29,32,34)
x1.15.2 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
x2.15.2 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
N.15.2 = length(y.15.2)

jags.RElogit.15.2 = jags.model(RE_logit_model.con, n.chains = 2, n.adapt = 200, data = list('y'= y.15.2, 'n'= n.15.2, 'x1'= x1.15.2, 'x2'= x2.15.2, 'N'= N.15.2 ))
sample.RElogit.15.2 = coda.samples(jags.RElogit.15.2, c("alpha1","alpha2","alpha3", "alpha4"), 10000)
#quartz()
plot(sample.RElogit.15.2, trace = FALSE)
summary(sample.RElogit.15.2)
gelman.diag(sample.RElogit.15.2) # Gelman and Rubin Cinvergence Diagnostic
#quartz()
gelman.plot(sample.RElogit.15.2) 


#--------------------- Code For potential median survivability ----------------------
# Data from Book 1-5 (30-50)
RE_logit_model = "model {
for (i in 1:N) {
y[i] ~ dbin(p[i],n[i])
mu[i] <- alpha1 + alpha2*x1[i] + alpha3*x2[i] + alpha4*x3[i] + alpha5*x1[i]*x2[i] + alpha6*x1[i]*x3[i]+ alpha7*x2[i]*x3[i] + alpha8*x1[i]*x2[i]*x3[i]
beta[i] ~ dnorm(mu[i],tau)
logit(p[i]) <- beta[i]
}
alpha1 ~ dnorm(0.0,1.0E-6)
alpha2 ~ dnorm(0.0,1.0E-6)
alpha3 ~ dnorm(0.0,1.0E-6)
alpha4 ~ dnorm(0.0,1.0E-6)
alpha5 ~ dnorm(0.0,1.0E-6)
alpha6 ~ dnorm(0.0,1.0E-6)
alpha7 ~ dnorm(0.0,1.0E-6)
alpha8 ~ dnorm(0.0,1.0E-6)
tau ~ dgamma(0.001,0.001)
sd <- sqrt(1.0/tau)
}"

RE_logit_model.con <- textConnection(RE_logit_model)

y.15 = y.15
n.15 = n.15
x1.15 = xg
x2.15 = xn
x3.15 = xc
N.15 = length(y.15)

jags.RElogit.15 = jags.model(RE_logit_model.con, n.chains = 3, n.adapt = 10000, data = list('y'= y.15, 'n'= n.15, 'x1'= x1.15, 'x2'= x2.15, 'x3' = x3.15, 'N'= N.15 ))
sample.RElogit.15 = coda.samples(jags.RElogit.15, c("alpha1","alpha2","alpha3", "alpha4", "alpha5", "alpha6", "alpha7", "alpha8"), 50000)
quartz()
plot(sample.RElogit.15, trace = FALSE)
summary(sample.RElogit.15)
gelman.diag(sample.RElogit.15) # Gelman and Rubin Cinvergence Diagnostic
#quartz()
gelman.plot(sample.RElogit.15) 


 

