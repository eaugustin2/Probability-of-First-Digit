#Function for obtaining the first digit of any number greater than 9
Modulo <- function(x){
  mod = 10
  if(x<10 && x>=1){
    return(x)
  }else{
    return(Modulo(x/mod))
  }
  
}

#Specifing the path of my file/files
#variable mydata = whatever is read from csv file
#Header = if you have column names
#sep = if the data is separated by commans or anything else ex \t
path <- file.path("/","Users", "elisee", "Downloads", "StatePopulation.csv")
mydata = read.csv(path,header=TRUE,sep =",")
x <-mydata$Population

x

#Putting the value of population into ListData and then getting the first value as integer
ListData<-list()
for (i in 1:length(x)){
  ListData[[i]]<-x[[i]]
}

 Values<-list()
for(i in 1:length(ListData)){
  Values[[i]]<-as.integer(Modulo(ListData[[i]]))
}
 
 Values
 
 
 
 
 #Probability
 #Made an array of size 9 and filled each spot with 9 zeroes
 #To fill the array with how many times that number appeared
 frequency <- seq(0,0,length.out=9)
 frequency
 
 
 
 #Which counts how many times that number is seen throughout the whole array
 for(i in 1:9){
   for(j in 1:length(Values)){
     if(Values[j] == i)
       frequency[i] = frequency[i] + 1
   }
 }
 
 frequency
 
 
 
 #Array has values of how many times each value showed up, now replaces it with the probability
 #Even if it is a decimal it is the percentage
 #Calculating the percentage/Probability of each number over the length of the array
 for(i in 1:9){
   frequency[i] = (frequency[i]/length(Values))*100
 }
 
 #Probability of that number appearing out of all the numbers
 frequency
 
 

 
 plot(frequency, type = "o",main = "Probability of Population First Digit", xlab="First Digit", ylab="Probability", col="black", xlim=c(0,10), las=1)

 
 # basic straight line of fit
 x<-1:9
 fit <- glm(frequency~x)
 co <- coef(fit)
 abline(fit, col="blue", lwd=1)
 
 # exponential line of fit
 x<-1:9
 f <- function(x,a,b) {a * exp(b * x)}
 fit <- nls(frequency ~ f(x,a,b), start = c(a=10, b=0)) 
 co <- coef(fit)
 curve(f(x, a=co[1], b=co[2]), add = TRUE, col="green", lwd=2) 
 
 
 # polynomial
 f <- function(x,a,b,d) {(a*x^2) + (b*x) + d}
 fit <- nls(frequency ~ f(x,a,b,d), start = c(a=1, b=1, d=1)) 
 co <- coef(fit)
 curve(f(x, a=co[1], b=co[2], d=co[3]), add = TRUE, col="pink", lwd=2)
 
 legend("topright",legend=c("linear","Probability", "Exponential", "Polynomial"),col=c("blue","black","green","pink"),lwd=2 )
 
 
 




