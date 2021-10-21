###   Задача 1  ###

meanCI = function(x,sigma=sd(x),confLevel=0.95){
  if (confLevel>=1 || confLevel<=0)
  {return ("The value of confLevel should be in the interval (0,1)")}
  else{
    n = length(x)
    meanX = mean(x)
    alpha = 1 - confLevel
    se = sigma/sqrt(n)
    if (missing(sigma))       
    {cv = qt(1-alpha/2,df=n-1)}
    else {cv=qnorm(1-alpha/2)}
    return(c(lowerBound=meanX-cv*se,upperBound=meanX+cv*se))
    
  }
}
### now test it
x=rnorm(100,0,1.5)
meanCI(x)
meanCI(x,sd(x))                 

###   Задача 2  ###

mtcars

hist(mtcars$mpg,probability = TRUE)
qqnorm(mtcars$mpg,main = "MPG Dist")
qqline(mtcars$mpg,col=2)
shapiro.test(mtcars$mpg)

###   Задача 3  ###

install.packages("HSAUR") 
data(water,package = "HSAUR")
??water
str(water)
edit(water)

morSouth=water$mortality[water$location=="South"]
morNorth=water$mortality[water$location=="North"]
hardSouth=water$hardness[water$location=="South"]
hardNorth=water$hardness[water$location=="North"]
ks.test(morSouth,morNorth)
ks.test(hardSouth,hardNorth)

cor.test(~mortality + hardness, data = water)
t.test(mortality~location,data=water)
t.test(hardness~location,data=water)
row1=tapply(water$mortality,water$location,mean)
row2=tapply(water$hardness,water$location,mean)
tab=rbind(row1,row2)
rownames(tab)=c("mortality","hardness")
tab

###   Задача 4  ###

data(waves,package = "HSAUR")
??waves
str(waves)
edit(waves)
t.test(waves$method1,waves$method2,paired = TRUE)
ks.test(waves$method1,waves$method2)
cor.test(~method1 + method2, data = waves)
