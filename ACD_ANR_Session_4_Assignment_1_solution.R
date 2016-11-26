#1
v1 <- c(2:30)
v2 <- seq(30,2,-1)
v2
v3 <- c(1:30,29:1)
dev <- c(4,6,3)
o1 <- rep(c(5, 6, 7), 10)
o2 <- c(rep(c(5,6,7),10),5)
o3 <- c(rep(c(4),each=10),rep(c(6),each=20),rep(c(3),each=30))

#2
x<-seq(3,6,0.1)
eX <- sin(x)

#3
set.seed(100) #Q1. how seed use?
x <- sample(0:999, 250, replace=T) 
y <- sample(0:999, 250, replace=T) 
x
y
#a.
y[y>500]
#b.
which(y>700)
#c.
x[which(y>400)]
#d Question not able to understand. Plese clear the question or please give answer below
#e.
y[y%%2==0]
result<-table(y%%2==0)["TRUE"]
result
#f
sort(x)
order(y)
#g,h
n<-length(x)
(x*n???2 + 2*x*n???1 - x*n)

#4
paste("Label",1:30,sep=" ")
paste("Fn",1:30,sep="")
#5
p=1000
n=15
rate=11.5
total <- sum(p*(1+rate/100)*(n:1))
total
#6
matrix(c(1,101,102,103,2,201,202,203,3,301,302,303,4,401,402,403),ncol = 4, byrow = TRUE)

marathontime