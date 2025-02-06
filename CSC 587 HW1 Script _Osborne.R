#Reads file
su <- read.delim("Su_raw_matrix.txt")

#Gets mean and standard deviation of Liver_2.CEL column
liver2_mean <- mean(su$Liver_2.CEL)
liver2_sd <- sd(su$Liver_2.CEL)

#Column means and sums
col_means <- colMeans(su)
col_sums <- colSums(su)

print(liver2_mean)
print(liver2_sd)
print(col_means)
print(col_sums)

randomNum1 <- rnorm(10000, mean=0, sd=0.2)
hist(randomNum1, xlim=c(-5,5), main="mean=0, sd=0.2", xlab="Value", col="blue")

randomNum2 <- rnorm(10000, mean=0, sd=0.5)
hist(randomNum2, xlim=c(-5,5), main="mean=0, sd=0.5", xlab="value", col="green")

#Only had to run install line once; left in for clarity
#install.packages("ggplot2")

library(ggplot2)


# 3a dat 3b - 3e
dat <- data.frame(cond = factor(rep(c("A", "B"), each=200)), 
                  rating = c(rnorm(200),rnorm(200, mean=.8)))

# 3b dat Overlaid histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

# 3c dat Interleaved histograms
ggplot(dat, aes(x=rating, fill=cond)) + 
  geom_histogram(binwidth=.5, position="dodge")

# 3d dat Density plots
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()

# 3e dat Density plots with semitransparent fill
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)



# 3f diabetes 3b - 3e
diabetes <- read.csv("diabetes_train.csv")

# 3b diabetes Overlaid histograms
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

# 3c diabetes Interleaved histograms
ggplot(diabetes, aes(x=mass, fill=class)) + 
  geom_histogram(binwidth=.5, position="dodge")

# 3d diabetes Density plots
ggplot(diabetes, aes(x=mass, colour=class)) + geom_density()

# 3e diabetes Density plots with semitransparent fill
ggplot(diabetes, aes(x=mass, fill=class)) + geom_density(alpha=.3)


#Only had to run install line once; left in for clarity
#install.packages("tidyverse")
library(tidyverse)

passengers <- read.csv("titanic.csv")

passengers %>% drop_na() %>% summary()

passengers %>% filter(Sex == "male")

passengers %>% arrange(desc(Fare))

passengers %>% mutate(Famsize = Parch + SibSp)

passengers %>% group_by(Sex) %>% 
  summarise(meanFare = mean(Fare), numSurv = sum(Survived))

percentiles <- quantile(diabetes$skin, probs = c(0.1, 0.3, 0.5, 0.6))
print(percentiles)