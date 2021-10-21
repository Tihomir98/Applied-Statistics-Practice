### Задача 1 ###

square.approx.Pi<- function(trials){
  x=runif(trials,min = -1,max = 1);
  y=runif(trials,min = -1,max = 1);
  return(4*sum(sqrt(x^2+y^2)<=1)/trials);
}

square.approx.Pi(100000)


### Задача 2 ###
data(sleep)
?sleep # extra - increase in hours of sleep; group - drug given; ID - patient ID
sleep_wide <- data.frame(
  ID=1:10, 
  group1=sleep$extra[1:10],
  group2=sleep$extra[11:20]
)
count.plus=sum((sleep_wide$group1-sleep_wide$group2)>=0);
n=length(sleep_wide$group1);
binom.test(count.plus,n,0.5,alternative = c("two.sided"))

### Задача 3 ###

install.packages("UsingR")
data("pi2000",package = "UsingR")
str(pi2000)
t=table(pi2000)
n=length(t)
barplot(t/length(pi2000),
        col=rgb(1:n,1:n,1:n,maxColorValue=n),cex.axis=1.5, cex.lab=1.5,cex.names=1.5,
        xlab = "Digit",ylab = "Probability", main = "First 2000 Digits in PI")
abline(h=1/n,col=2,lty=2,lwd=3)
chisq.test(t,p=rep(1/n,n))

### Задача 4 ### (Chi-square test for goodness-of-fit)

roll=as.factor(sample(1:6,size=100,replace=TRUE))
(freq = table(roll))
probs = c(1,1,1,1,1,1)/6 # or use rep(1/6,6)
chisq.test(freq,p=probs)

dieFair = as.factor(sample(1:6,100,p=c(1,1,1,1,1,1)/6,replace=T))
dieBias = as.factor(sample(1:6,100,p=c(0.5,0.5,0.5,1.5,1.5,1.5)/6,replace=T))
chisq.test(dieBias,dieFair) # WRONG!!!!
tFair = table(dieFair)
tBias = table(dieBias)
chisq.test(rbind(tFair,tBias))

### Задача 5 ### (Chi-square test for independence)
titanic <- read.csv("https://github.com/natigeorgieva/Applied_Statistics_2021/blob/main/week3/Data/Titanic.csv")
head(titanic)
sum(table(titanic$Survived)) #Преброяване на общия брой пътници

#Преброяване на броя на хората, оцелели от потъващия титаник
t=data.frame(table(titanic$Survived))
names(t)[1]= 'Survived'
t 
#Виждаме, че 340 души са оцелели след трагедията. 

#Изчисляваме процента на хората, оцелели след трагедията
y=table(titanic$Survived)
prop.table(y)*100
#Както се вижда, 38,24% от хората са оцелели.

#Сега нека да преброим броя на първокласните пътници, оцелели от потъването на Титаник.
mytable <- xtabs(~Survived + Pclass,data= titanic[titanic$Pclass=="1", ])
mytable
#По този начин броят на оцелелите от 1-ви клас е 134

#Да намерим процента на първокласните пътници, оцелели от потъването на Титаник. 
prop.table(mytable)*100

#Нека използваме R, за да преброим броя на жените от Първа класа, които са оцелели от потъването на Титаник
mytable1=(subset(titanic,Pclass=='1' & Survived=='1',select=c(Pclass,Survived,Sex)))
ftable(mytable1)
#Така 89 жени от 1-ви клас оцелели след потъването на титаника

#Нека използваме R, за да измерим процента на оцелелите, които са жени, т.е. процент само от оцелелите хора, 
#влюбени както в мъже, така и в жени.

q = subset(titanic,Survived=='1',select=c(Sex,Survived))
mytable2 <- xtabs(~Survived + Sex, data=q)
prop.table(mytable2)*100
#Така от оцелелите хора 67% от тях са жени

#Нека използваме R, за да измерим процента на жените на борда на Титаник, които са оцелели, т.е. от общия брой пътници на борда
p=xtabs(~Survived + Sex,data = titanic)
prop.table(p)*100
#По този начин 26% от жените и 12% от мъжете могат да бъдат спасени само

#Нека да проведем тест на Pearson’s Chi-squared test, за да проверим следната хипотеза: Делът на жените на борда, които са оцелели от потъването 
#на Титаник, е по-висок от дела на мъжете на борда, които са оцелели от потъването на Титаник.
chisq.test(p)
assocstats(p)
#Тъй като р-стойността е много малка, ние отхвърляме нулевата хипотеза и приемаме алтернативната хипотеза и следователно делът на жените на 
#борда, оцелели от потъването на Титаник, е по-висок от дела на мъжете на борда, оцелели от потъването на Титаник.
