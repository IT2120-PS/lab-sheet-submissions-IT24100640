setwd("C:\\Users\\it24100640\\Downloads\\Lab 05-20250828")
data<-read.table("Data.txt",header = TRUE, sep=",")
fix(data)
attach(data)


names(data)<-c("X1","X2")
attach(data)
hist(X2,main = "histogram for no of stakeholders")

histogram<-hist(X2,main = "histogram for no of stakeholders", breaks = seq(130,270,length=8),right =FALSE)
?hist

breaks<-round(histogram$breaks)
freq<-histogram$counts
mids<-histogram$mids
classes<-c()
for(i in 1:length(breaks)-1){
  classes[i]<-paste0("[",breaks[i],",",breaks[i+1],")")
}
cbind(Classes=classes,Frequency=freq)



lines(mids,freq)
plot(mids,freq, type='1',main = "frequency polygon for shareholders",xlab = "shareholder",ylab = "frequency",ylim = c(0,max(freq)))


cum.freq<-cumsum(freq)
new<-c()
for(i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}
plot(breaks,new,main="cumaltive frequency polygon for shareholders",xlab = "shareholder",ylab = "cumculative frequency",ylim = c(0,max(cum.freq)))
cbind(Upper=breaks,CumFreq=new)













#Exercise
#1. Import the dataset (’Exercise – Lab 05.txt’) into R and store it in a data frame called ”Delivery Times”.
        # Load the dataset
        delivery_times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep=",")

        fix(delivery_times)

#2. Draw a histogram for deliver times using nine class intervals where the lower limit is 20 and upper limit is 70. Use right open intervals.
        # Create class breaks with lower limit 20 and upper limit 70
        breaks <- seq(20, 70, by = (70 - 20) / 9)
        
        # Draw the histogram
        
        hist(delivery_times$Delivery_Time, 
             breaks = breaks, 
             right = TRUE,
             main = "Histogram of Delivery Times", 
             xlab = "Delivery Times", 
             ylab = "Frequency")

        
#4. Draw a cumulative frequency polygon (ogive) for the data in a separate plot.
        # Calculate cumulative frequency
        cum_freq <- cumsum(freq_table$counts)
        
        # Plot the cumulative frequency polygon (ogive)
        plot(freq_table$mids, cum_freq, 
             xlab = "Delivery Times", ylab = "Cumulative Frequency", 
             main = "Cumulative Frequency Polygon")
        











