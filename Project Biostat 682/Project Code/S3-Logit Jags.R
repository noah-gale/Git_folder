
#### Use Both sample15.l

require(rjags)

# Note: Every time we plug in different data, we need to re-compile the model

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
#prb1 <- step(0-beta1)
#prb2 <- step(0-beta2)
}"

prior_model.con_L<-textConnection(prior_model_Logit)

death.yes.15 = Binary_Matrix15[,3]
death.no.15 = Binary_Matrix15[,4]
n.15 = death.no.15 + death.yes.15
v.Gen.15 = Binary_Matrix15[,2]
v.Nob.15 = Binary_Matrix15[,1]
M.15 = 4

jags15.l = jags.model(prior_model.con_L, n.chains = 3, n.adapt = 200, data = list('death.yes' = death.yes.15, 'n' = n.15,'v.Gen' = v.Gen.15, 'v.Nob' = v.Nob.15, 'M' = M.15))
sample15.l = coda.samples(jags15.l, c("alpha","beta1","beta2", "ORgen", "ORnob"), 10000)
#quartz()
plot(sample15.l, trace = FALSE)
summary(sample15.l)
gelman.diag(sample15.l) # Gelman and Rubin Cinvergence Diagnostic
#quartz()
gelman.plot(sample15.l) 