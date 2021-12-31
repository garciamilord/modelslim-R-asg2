#=============================================================================
# 
# PROGRAMMER: Garcia Milord
# PANTHER ID: 6168616
#
# CLASS: CAP4830
# SECTION: U01
# SEMESTER: Spring 2021
# CLASSTIME: T/TH 9:00-10:15 am
#
# CERTIFICATION: I understand FIU’s academic policies, and I certify that this 
#                work is my own and that none of it is the work of any other person.
#
#=============================================================================



library(triangle)

#1) Create a data frame named inputs with the following column names (q1, q2, p,s)
#and each column has 2000 rows with the following data:
#.q1-> 2000 random variables that have a triangle distribution with A = 0, B = 1500 , C=1200
#.q2-> 2000 random variables that have a triangle distribution with A = 0, B = 3500 , C=1000
#.p-> 2000 random variables that have a triangle distribution with A = 10, B = 17.50, C=12.50
#.s-> 2000 random variables that have an exponential distribution with l=10


inputs <- data.frame(q1 = rtriangle(2000,a=0,b=1500,c=1200),
                     q2 = rtriangle(2000,a=0,b=3500,c=1000), 
                     p = rtriangle(2000,a=10,b=17.50,c=12.50),
                     s = rexp(2000,10))

#2) Plot the histogram in a single window of each column of the inputs data frame.
#Hint use par(mfrow=c(4, 1)

par(mfrow=c(4,1))

hist(inputs$q1)
hist(inputs$q2)
hist(inputs$p)
hist(inputs$s)

#3) Create a data frame named outputs with a column name value that stores the 
#output of the following mode:
#f(q1, q2, p, s) = (2700 - q1- q2) *p - (s*p)
outputs <- data.frame(q1 = double(), q2 = double(), p = double(), s = double(), value = double())


#4) Run a Monte Carlo Simulation 1000 times of the model shown in #3, and store 
#the results in the value
#column of the data frame outputs



for(i in 1:1000){
  # store the results with inputs 
  q1 <- sample(inputs$q1)
  q2 <- sample(inputs$q2) 
  p <- sample(inputs$p)
  s <- sample(inputs$s)
  value <- (2700 - q1- q2) *p - (s*p)
  
  outputs <- rbind(outputs, data.frame(q1, q2, p, s, value))  
}

#5) Plot the histogram of outputs$value
par(mfrow=c(4,1))
hist(outputs$value)



#6) Create the empirical CDF of outputs$value and plot this CDF

par(mfrow=c(1, 1)) 
empiricalCDF <- ecdf(outputs$value)
plot(empiricalCDF)


#7) Calculate the P0, P10, P20, P30, . P90, P100 and output these values on the R-Console


px <- data.frame (value = quantile(outputs$value, probs = c(seq(0,1,by = 0.10))))
summary(px)





#8) Find the P20 to P80 interval values from the empirical CDF.

sprintf("Interval of Interest: [ %.2f , %.2f ]",  px$value[3], px$value[9])



#9) Create a matrix named storage that stores 100 samples of the Monte Carlo simulation 
#of the model in #3. Each Monte Carlo simulation will have 250 realizations. 

storage <- matrix(ncol = 100, nrow = 250)
for(i in 1:100){
  outputs <- data.frame(q1 = double(), q2 = double(), p = double(), s = double(), value = double())
  for(j in 1:250){
    q1 <- sample(inputs$q1,1)
    q2 <- sample(inputs$q2,1) 
    p <- sample(inputs$p,1)
    s <- sample(inputs$s,1)
    value <- (2700 - q1- q2) *p - (s*p)
    
    outputs <- rbind(outputs, data.frame(q1, q2, p, s, value))  
  }
  storage[ , i] <- outputs$value
}


#10) Convert the storage matrix into a data frame named storage.
storage <- data.frame(storage)


#11) Create a data frame named cltData that stores all the means of each column of the storage data
#frame.
cltData <- data.frame( mean = colMeans(storage))


#12) Plot the histograms of the data within cltData

par(mfrow=c(1, 1))
hist(cltData$mean)


#13) Check if the data within cltData is normally distributed. Hint use a normal test for this.

shapiro.test(cltData$mean)

#14) State in your code using a comment if the data in cltData is normal distributed or not and why.

#cltData is not normal distributed because p<0.05

#15) Calculate the 80% confidence internal and output the lower and upper bounds of this interval on the
#R-console

n <- nrow(cltData)
cltMean <- mean(cltData$mean)
cltSD <- sd(cltData$mean)

margin <- qt(0.80, df = n-1 )* cltSD/sqrt(n)

lowerBound <- cltMean - margin
higherBound <- cltMean + margin

sprintf("Interval of Interest: [ %.2f , %.2f ]",  lowerBound, higherBound)







