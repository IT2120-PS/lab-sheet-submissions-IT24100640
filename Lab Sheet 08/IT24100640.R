setwd("C:\\Users\\it24100640\\Downloads\\Lab 08-20250925")

data<-read.table("Data - Lab 8.txt",header = TRUE)
fix(data)
attach(data)

popmn<-mean(Nicotine)
popvar<-var(Nicotine)

samples<-c()
n<-c()

for(i in 1:30){
  s<-sample(Nicotine,5,replace = TRUE)
  samples<-cbind(samples,s)
  n<-c(n,paste('S',i))
}
colnames(samples)=n
s.means<-apply(samples,2,mean)
s.vars<-apply(samples,2,var)

#Following commands will calculate mean and variance of sample means stored in "s.means" variable.
    samplemean<-mean(s.means)
    samplevars<-var(s.means)
#Compare the population mean and mean of sample means.
    popmn
    samplemean
#Compare the population variance and variance of sample means.
    truevar=popvar/5
    samplevars

 
    
       
data<-read.table("Exercise - LaptopsWeights.txt",header = TRUE)
fix(data)
attach(data)

# Calculate population mean and population standard deviation
pop_mean <- mean(Weight.kg.)
pop_sd_population <- sqrt(sum((Weight.kg. - pop_mean)^2) / length(Weight.kg.))

print("Population Mean:")
print(pop_mean)

print("Population Standard Deviation:")
print(pop_sd_population)

# Draw 25 random samples of size 6 (with replacement) and calculate sample mean and sd
set.seed(123)  # For reproducibility
n_samples <- 25
sample_size <- 6

sample_means <- numeric(n_samples)
sample_sds <- numeric(n_samples)

for (i in 1:n_samples) {
  sample <- sample(Weight.kg., size = sample_size, replace = TRUE)
  sample_means[i] <- mean(sample)
  sample_sds[i] <- sd(sample)
  
  print(paste("Sample", i, "Mean:"))
  print(sample_means[i])
  print(paste("Sample", i, "SD:"))
  print(sample_sds[i])
}

#  Calculate mean and sd of the 25 sample means
mean_of_sample_means <- mean(sample_means)
sd_of_sample_means <- sd(sample_means)

print("Mean of Sample Means:")
print(mean_of_sample_means)

print("Standard Deviation of Sample Means:")
print(sd_of_sample_means)

