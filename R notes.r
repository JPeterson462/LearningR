#
# Practice
#

# Mean of 7 numbers
t <- c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
mean(t)

# Sum of i^2 from i=1 to 25
t = c()
for (index in 1:25) { t <- c(t, index * index) }
sum(t)

# Average distance of cars dataset
mean(cars[,2])

# Which row index of cars has a dist of 85
which(cars[,2] == 85, arr.ind=TRUE)

#
# Gettting Started
#
femaleMiceWeights <- read.csv(file="femaleMiceWeights.csv")
femaleMiceWeights[12,2]
femaleMiceWeights$Bodyweight[11]
length(femaleMiceWeights$Bodyweight)
hf <- femaleMiceWeights[femaleMiceWeights$Diet == 'hf',]
mean(hf[,2])
set.seed(1)
sample(13:24, size = 1) # == 21
femaleMiceWeights[21, 2]

library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

#
# dplyr
#
msleep <- read.csv("msleep_ggplot2.csv")
class(msleep)
nrow(msleep %>% filter(order == "Primates"))
msleep %>% filter(order == "Primates") %>% select(sleep_total)
msleep %>% filter(order == "Primates") %>% select(sleep_total) %>% unlist(use.names = FALSE) %>% mean()
msleep %>% filter(order == "Primates") %>% summarize(mean = mean(sleep_total))

#
# qqnorm
#
par(mfrow = c(3,3))
for (i in 1:9) { qqnorm(dat[,i]); qqline(dat[,i]) }

#
# boxplot
#
median(split(InsectSprays, InsectSprays$spray)$A[,1])
boxplot(split(InsectSprays, InsectSprays$spray)$C[,1])
boxplot(nym.2002$time~nym.2002$gender)

#
# random variables
#
mean(x)
set.seed(1)
mean(sample(x, size = 5))
abs(mean(x) - 23.564)
set.seed(5)
y <- sample(x, size = 5)
abs(mean(y) - mean(x))

#
# Null distributions
#
set.seed(1)
a <- mean(x)
res <- c()
for (i in 1:1000) { res <- c(res, mean(sample(x, size = 5))) }
count <- 0
for (avg in res) { if (abs(avg - a) > 1) { count = count + 1 } }

#
# Probability distributions
#
x <- gapminder %>% filter(year == 1952) %>% .$lifeExp
mean(x <= 40)
mean(x > 40 & x <= 60)

#
# Normal distributions
#
pnorm(25, mean = 23.9, sd = 0.43) - pnorm(23, mean = 23.9, sd = 0.43)

#
# Population, samples, and estimates
#
males <- split(dat, dat$Sex)$M
controlled <- split(males, males$Diet)$chow
mean(controlled[,3])
popsd(controlled[,3])

#
# Central limit theorem
#
stdev = popsd(males_control[,3])
avg = mean(males_control[,3])
weights = males_control[,3]
mean(weights >= avg - stdev & weights <= avg + stdev)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist

#
# T distribution
#
simulate_runs = function(n) {
x = sample(1:6, n, replace=TRUE)
p = 1 / 6
z = (mean(x == 6) - p) / sqrt(p * (1 - p) / n)
z
}
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
stderror = sqrt((sd(Y) * sd(Y))/12 + (sd(X) * sd(X))/12)
t = (mean(Y) - mean(X)) / stderror
# p(observing value as large as t-value)
2 * (1 - pnorm(t))
t.test(X, Y)

#
# Power calculations
#
set.seed(1)
X = sample(bwt.nonsmoke, 5)
Y = sample(bwt.smoke, 5)
t.test(X, Y)

N=90
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.01
})
mean(rejects)

#
# Monte carlo simulations
#
set.seed(1)
X = rnorm(5)
t = sqrt(5) * mean(X) / sd(X)

set.seed(1)
par(mfrow = c(3,2))
for (n in seq(5,30,5)) {
	ts = replicate(1000,{
		X = rnorm(n)
		sqrt(n) * mean(X) / sd(X)
	})
	qqplot(qs, ts)
}

set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*median(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)

#
# Permuations exercise
#

# code from course
obs = median(smokers) - median(nonsmokers)
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

#
# Association tests
#
chisq.test(table(d))
fisher.test(table(d))

men = filter(nym.2002, gender == "Male") %>% select(age, time)
men_p = cor.test(men$age, men$time, method="pearson")

#
# Median MAD and Spearman Correlation
#
(dim(chick)[1] * mean(chick$weight.4) + 3000) / (dim(chick)[1] + 1) / mean(chick$weight.4)
chick_plus_day4 = c(3000, chick$weight.4)
chick_plus_day21 = c(3000, chick$weight.21)
cor_plus = cor(chick_plus_day4, chick_plus_day21)
cor_plus / cor(chick$weight.4, chick$weight.21)

#
# Wilcox test
#
x_xtra = c(x, 200)
wilcox.test(x_xtra, y)$p.value
t.test(x, y+10)$p.value - t.test(x, y + 100)$p.value