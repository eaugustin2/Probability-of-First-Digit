#Function for obtaining the first digit in numbers less than 1
RModulo <- function(x){
  Rmod = 10
  if (x>=1 && x<10){
    return(x)
  }else{
    return (RModulo(x*10))
  }
  
  
}


y<-runif(70,0,1)

UniformValues<-list()
for(i in 1:length(y)){
  UniformValues[[i]]<-as.integer(RModulo(y[[i]]))
  
}

UniformValues

#Probability
#Made an array of size 9 and filled each spot with 9 zeroes
#To fill the array with how many times that number appeared
frequency <- seq(0,0,length.out=9)
frequency


for(i in 1:9){
  for(j in 1:length(UniformValues)){
    if(UniformValues[j] == i)
      frequency[i] = frequency[i] + 1
  }
}

counter<-frequency
frequency
counter


#Array has values of how many times each value showed up, now replaces it with the probability
#Even if it is a decimal it is the percentage
for(i in 1:9){
  frequency[i] = (frequency[i]/length(UniformValues))*100
}

#Probability of that number appearing out of all the numbers
frequency

plot(frequency, type = "o",main = "Probability of Uniform Rand First Digit", xlab="First Digit", ylab="Probability", col="black", xlim=c(0,10), las=1)

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





