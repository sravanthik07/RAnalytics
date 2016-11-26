#1
numbers <- c(1:100)
oddNumVector <- numbers[numbers%%2 != 0]
oddNumVector
#2
v1 <- c(1,2,3,4,5,8,6,2,11)
#3
matv1 <- matrix(v1,nrow = 3,ncol=3)
#4
a<-c(NA,11:15,NA,NA)
mean(a,na.rm = T)
#5
x=c("apple","banana","grape")
x<- chartr("a",'$',x)
x
