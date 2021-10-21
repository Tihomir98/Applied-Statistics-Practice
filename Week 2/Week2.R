### Задача 1 ### 
?rnorm # повече за rnorm, който не знае
normData<-rnorm(10000,0,1) # генерираме: n=10000, mean=0, sd=1

hist(normData,prob=TRUE,xlab = 'Simulated observations') #добра практика е да видите как изглеждат нашите данни на графика
curve(dnorm(x, mean=mean(normData), sd=sd(normData)), add=TRUE, col=2,lwd=2) # Изчертава крива, съответстваща на функция през интервала.

?dnorm # Density, distribution function, quantile function and random generation for the normal distribution with mean equal to mean and standard deviation equal to sd.
?curve #Draws a curve corresponding to a function over the interval [from, to]. curve can plot also an expression in the variable xname, default x.

mean(normData) #Generic function for the (trimmed) arithmetic mean.
median(normData) 
mad(normData) #Median Absolute Deviation
sd(normData) #Standard Deviation


cutoff<-median(normData)+3*mad(normData)

### Задача 2 ### 

p=runif(1000,0,1)>9/10;
simData=rnorm(1000,0+(10-0)*p,sqrt(4)+(sqrt(9)-sqrt(4))*p)

hist(simData,prob=TRUE,xlab = 'Observations')
curve(dnorm(x, mean=mean(simData), sd=sd(simData)), add=TRUE, col=2,lwd=2)
d=density(simData,bw="SJ")
lines(d,col='blue',lwd=2)

any(abs((simData-median(simData)))>3*mad(simData))  

### Задача 3 ### 

a<-c(8,3,8,7,15,9,12,4,9,10,5,1)
matrr<-matrix(a,6,2) # create matrix 6x2
matrr
rownames(matrr)<-c("r1","r2","r3","r4","r5","r6") # named rows
matrr 
coll=seq(1,12,2) # create a vector from 1 to 12 with step 2 
# so this seq. contains odd numbers 
matr2=cbind(matrr,coll) 
matr2  #create matrix 6x3 where the last column is named

matr2=cbind(matrr,seq(1,12,2)) # create a matrix 6x3 where thee 3column is without name
matr2
ord=order(matr2[,1]) 
matr2[ord,] # sorting   by 1 coloumn

### Задача 4 ###

rand.Circle <- function(n,x_0=0,y_0=0,r=1){
  theta = 2*pi*runif(n,0,1);
  R = r*sqrt(runif(n,0,1));
  x = x_0 + R*cos(theta);
  y = y_0 + R*sin(theta);
  return(cbind(x,y))
}
plot(rand.Circle(1000,0,0,4),asp=1)
