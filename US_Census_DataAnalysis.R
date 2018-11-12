usdemo <- read.csv("C:/acs2015_county_data.csv", header=T)

StateNames <- data.frame(state=unique(usdemo$State)) 
StateNames$Income <-""
StateNames$Poverty <-""
StateNames$Unemployment <-""


StateNames$state

#############Processing Data to show state wise##################

for(i in 1:nrow(StateNames)){
  StateNames$Income[i] <- round((sum((usdemo$Income[usdemo$State == StateNames$state[i]]) * (usdemo$TotalPop[usdemo$State == StateNames$state[i]])) / sum(usdemo$TotalPop[usdemo$State == StateNames$state[i]])),3)
  StateNames$Poverty[i] <- round((sum((usdemo$Poverty[usdemo$State == StateNames$state[i]]) * (usdemo$TotalPop[usdemo$State == StateNames$state[i]])) / sum(usdemo$TotalPop[usdemo$State == StateNames$state[i]])),3)
  StateNames$Unemployment[i] <- round((sum((usdemo$Unemployment[usdemo$State == StateNames$state[i]]) * (usdemo$TotalPop[usdemo$State == StateNames$state[i]])) / sum(usdemo$TotalPop[usdemo$State == StateNames$state[i]])),3)
}

#############################################################
hist(as.numeric(StateNames$Poverty), ylim =c(0,35), breaks=10, main="Histogram of Numerical Data")
barplot (as.numeric(StateNames$Poverty), xlab = "Poverty per state", ylab = "Percent", names.arg = StateNames$state, col=c("aquamarine3"))
boxplot(as.numeric(StateNames$Income))



###############CENTRAL LIMIT########################

library(sampling)
library(plotly)

samples <- 10000

Sample10 <- numeric(samples)
Sample20 <- numeric(samples)
Sample30 <- numeric(samples)
Sample50 <- numeric(samples)

set.seed(123)

for(i in 1:samples){
  Sample10[i]<- mean(sample(usdemo$Poverty , size=10, replace = T))
  Sample20[i]<- mean(sample(usdemo$Poverty , size=20, replace = T))
  Sample30[i]<- mean(sample(usdemo$Poverty, size=30, replace = T))
  Sample50[i]<- mean(sample(usdemo$Poverty, size=50, replace = T))
}

par(mfrow=c(2,2))
hist(Sample10, prob=TRUE, breaks = 50)
hist(Sample20, prob=TRUE, breaks = 50)
hist(Sample30, prob=TRUE, breaks = 50)
hist(Sample50, prob=TRUE, breaks = 50)

###############TYPES OF SAMPLING#################
library(sampling)

# srswr
set.seed(123)

sampling_wr <- srswr(100, nrow(usdemo))

#sampling_wr[sampling_wr!=0]
#head(MS_dataset[sampling_wr!=0])

selected_data <- (1:nrow(usdemo))[sampling_wr!=0]
selected_data <- rep(selected_data,sampling_wr[sampling_wr!=0])

#selected_data

sample_srswr <- usdemo[selected_data,] 

#table(sample_srswr$ttl_victim)
#hist(MS_dataset$ttl_victim, breaks=100)
#hist(sample_srswr$ttl_victim,breaks=100)

original_data <- plot_ly(x=usdemo$Citizen, type='histogram' ,nbinsx=100, color = "", colors = c("Greys"), name="Citizen Percent")
hist_srswr <- plot_ly(x=sample_srswr$Citizen, type='histogram' ,nbinsx=100, color = "", colors = c("Oranges"), name="SRSWR")


######################################################

#srswor 
set.seed(123)

sampling_swor <- srswor(100, nrow(usdemo))

sample_srswor <- usdemo[sampling_swor!=0,]

#table(MS_dataset$ttl_victim)
#table(sample_srswor$ttl_victim)
#hist(sample_srswor$ttl_victim, breaks = 100)
#hist(MS_dataset$ttl_victim, breaks=100)

hist_srswor <- plot_ly(x=sample_srswor$Citizen, type='histogram' ,nbinsx=100, color = "", colors = c("Blues"), name="SRSWOR")

#####################################################

#Systematic Smapling

set.seed(123)

N <- nrow(usdemo)
n <- 100

k <- ceiling(N / n)
k

r <- sample(k, 1)
r

# select every kth item

s <- seq(r, by = k, length = n)

sample_systematic <- usdemo[s, ]
head(sample_systematic[c(2,4,14,17,22)])

table(sample_systematic$Citizen)
hist_systematic <- plot_ly(x=sample_systematic$Citizen, type='histogram' ,nbinsx=100, color = "", colors = c("Reds"), name="Systematic")


#############################################
#Unequal Systematic
set.seed(123)

pik <- inclusionprobabilities(usdemo$Citizen, 100)

s <- UPsystematic(pik)

sample.unsystematic <- usdemo[s != 0, ]
hist_unsystematic <- plot_ly(x=pik, type='histogram' ,nbinsx=100, color = "", colors = c("Greens"), name="UnEqual Systematic")


#################################


subplot(nrows=5,original_data, hist_srswr,hist_srswor, hist_systematic, hist_unsystematic)

#################Confidence interval #########3###########

pop.mean <- mean(usdemo$Poverty)
pop.sd <- sd(usdemo$Poverty)
conf <- c(80, 90)
alpha <- 1 - conf/100

cat("Poverty Percent : mean =",pop.mean," and sd =",pop.sd)


for (i in alpha) {
  str_srswr <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                       100*(1-i), i, 
                       pop.mean - qnorm(1-i/2) * pop.sd,
                       pop.mean + qnorm(1-i/2) * pop.sd)
  cat(str_srswr,"\n")
}

#srswr#

sd.sample.means_srswr <- pop.sd/sqrt(nrow(sample_srswr))
xbar_srswr <- mean(sample_srswr$Poverty)
cat("SRSWR : mean =",xbar_srswr," and sd =",sd.sample.means_srswr)

for (i in alpha) {
  str_srswr <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                       100*(1-i), i, 
                       xbar_srswr - qnorm(1-i/2) * sd.sample.means_srswr,
                       xbar_srswr + qnorm(1-i/2) * sd.sample.means_srswr)
  cat(str_srswr,"\n")
}

#srswor#
sd.sample.means_srswor <- pop.sd/sqrt(nrow(sample_srswor))
xbar_srswor <- mean(sample_srswor$Poverty)
cat("SRSWOR : mean =",xbar_srswor," and sd =",sd.sample.means_srswor)

for (i in alpha) {
  str_srswor <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                        100*(1-i), i, 
                        xbar_srswor - qnorm(1-i/2) * sd.sample.means_srswor,
                        xbar_srswor + qnorm(1-i/2) * sd.sample.means_srswor)
  cat(str_srswor,"\n")
}


#systematic#

sd.sample.means_systematic <- pop.sd/sqrt(nrow(sample_systematic))
xbar_systematic <- mean(sample_systematic$Poverty[!is.na(sample_systematic$Poverty)])
cat("SRSWOR : mean =",xbar_systematic," and sd =",sd.sample.means_systematic)

for (i in alpha) {
  str_systematic <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                            100*(1-i), i, 
                            xbar_systematic - qnorm(1-i/2) * sd.sample.means_systematic,
                            xbar_systematic + qnorm(1-i/2) * sd.sample.means_systematic)
  cat(str_systematic,"\n")
}

#Unequalsystematic#
sd.sample.means_upsystematic <- pop.sd/sqrt(nrow(sample.unsystematic))
xbar_upsystematic <- mean(sample.unsystematic$Poverty[!is.na(sample.unsystematic$Poverty)])
cat("UPSystematic : mean =",xbar_upsystematic," and sd =",sd.sample.means_upsystematic)

for (i in alpha) {
  str_upsystematic <- sprintf("%2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",
                              100*(1-i), i, 
                              xbar_upsystematic - qnorm(1-i/2) * sd.sample.means_upsystematic,
                              xbar_upsystematic + qnorm(1-i/2) * sd.sample.means_upsystematic)
  cat(str_upsystematic,"\n")
}


