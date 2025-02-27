c<-c(5,10,15,20,25,30)
max_value<-max(c)
min_value<-min(c)
cat("Max value is ",max_value,"\n")
cat("Min value is ",min_value,"\n")

n<-as.integer(readline(prompt = "Enter a Number:  "))
if(n>0){
  f<-1
  for(i in 1:n){
    f<-f*i
  }
  cat("Factorial is:  ", f)
}else if(n<0){
  cat("Not valid")
}else{
  cat("Factorial of 0 is 1")
}

n <- as.integer(readline(prompt = "Enter a number: "))
if (n > 0) {
  a <- 0
  b <- 1
  if (n == 1) {
    cat(a)
  } else if (n == 2) {
    cat(a, b, "  ")
  } else {
    cat(a,"  ",b, "  ")
    for (i in 3:n) {
      c <- a + b
      cat("  ", c)
      a <- b
      b <- c
    }
  }
} else {
  cat("Enter a positive number")
}


a<-as.integer(readline("Enter first number:"))
b<-as.integer(readline("Enter second number:"))
op<-readline("Enter operation")
{
  if(op=="+"){
    cat(a+b)
  }
  if(op=="-"){
    cat(a-b)
  }
  if(op=="*"){
    cat(a*b)
  }
  if(op=="/"){
    cat(a/b)
  }
}

x=c(5,10,15,20,25)
y=c(1,2,3,4,5)
pie(x)
plot(x,y)
barplot(y)

x<-c(rep("Gold",20),rep("Silver",30),rep("Bronze",10))
cat(sample(x,10))

x<-c("success","failure")
cat(sample(x,10,replace= TRUE, prob=c(0.9,0.1)))

rainy_cloudy=function(cloudy,rainy,cloudy_rainy){
  result=rainy*cloudy_rainy/cloudy
  return(result)
}
cat("probability thaat it will rain on that day is: ", rainy_cloudy(0.4,0.2,0.85))



data(iris)
#printing first few rows
print(head(iris))
#printing structure
print(str(iris))

sepal_length_range <- range(iris$Sepal.Length)
cat("Range of Sepal Length:", sepal_length_range[1], "to", sepal_length_range[2])

sepal_length_mean<-mean(iris$Sepal.Length)
cat("Mean is", sepal_length_mean)

sepal_length_median <- median(iris$Sepal.Length)
cat("Median of Sepal Length:", sepal_length_median)

sepal_length_quantile<-quantile(iris$Sepal.Length, probs=c(0.25, 0.75))
sepal_length_iqr<-IQR(iris$Sepal.Length)
cat("1ST QUANTILE", sepal_length_quantile[1])
cat("2ndQUANTILE", sepal_length_quantile[2])
cat("IQR IS", sepal_length_iqr)

sepal_lenth_sd<-sd(iris$Sepal.Length)
sepal_lenth_var<-var(iris$Sepal.Length)
cat("Standard Deviation is:  ",sepal_lenth_sd)
cat("Variance is:  ", sepal_lenth_var)
cat(summary(iris))


#MODE
get_mode=function(x){
  fre=tabulate(x)
  mode=which.max(fre)
  return(mode)
}
cat("Mode of sepal length",get_mode(iris$Sepal.Length))

n=12
p=1/6
x=pbinom(6,n,p)
y=pbinom(9,n,p)
print(y-x)

mean<-72
sd<-15.2
x<-84

prob<-1- pnorm(x,mean,sd)
cat(prob*100)

prob_no_cars<-dpois(0,5)
cat(prob_no_cars)
prob_between<-ppois(47,50)
prob_between1<-ppois(50,50)
cat(prob_between1-prob_between)

success<-233
defect<-17
prob<-dhyper(3,defect,success,5)
cat(prob)

x=c(0,1,2,3,4)
p=c(0.41,0.37,0.16,0.05,0.01)
f1=sum(x*p)
print(f1)
f2=weighted.mean(x,p)
print(f2)
f3=c(x%*%p)
print(f3)


f<-function(t){0.1*exp(-0.1*t)}
expected_value<-integrate(function(t) f(t)*t,lower=0,upper=Inf)
cat(expected_value$value)

Y <- c(-12, -2, 8, 18)  
P_X <- c(0.1, 0.2, 0.2, 0.5)  
expected_Y <- sum(Y * P_X)
cat("Expected value of Y (Net Revenue):", expected_Y, "\n")

f<- function(x){0.5*exp(-abs(x))}
first_moment<-integrate(function(x) x*f(x),lower=1,upper=10)$value
second_moment<-integrate(function(x) x^2*f(x),lower=1,upper=10)$value
variance<-second_moment-(first_moment)^2

cat("Mean is: ", first_moment)
cat("Variance is: ", variance)


