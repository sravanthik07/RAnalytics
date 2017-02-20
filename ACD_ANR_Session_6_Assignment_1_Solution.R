#1. PRIME or NOT PRIME 
flag <- 1
vec <- c(1,2,3,6,7,103,82,179)
for(n in vec){
if(n == 2){
  flag <- 1
}else if(n%%2:(n-1) == 0){
  flag <- 0
}else {
  falg <- 0}
if(flag==1){
  print(paste(n," is a Prime number"))
  cat("\n")
}  else{
  print(paste(n," is NOT a Prime number")) }
}

print(paste("4","\n"," is a Prime number"))

#2. letter u and a both occur 
words <- c("above","unit","Under", "umberella")
for(word in words){
  a <- grep("a",word)
  u <- grep("u",word)
  if(length(a) > 0 && length(u) >0){
    print(paste("Word ",word," has letter u and a both"))
  }
}

#3. BMI

yourbmi <- function(w,h){
  if(!is.numeric(w)){
    if(!is.numeric(h)){ print("Please enter numeric weight and height")
      }  else{print("Please enter numeric weight")}
  }else if(!is.numeric(h)){print("Please enter numeric heigth")}
  bmirange <- w/(h*h)
  cat(bmirange)
  if(bmirange < 15 ){
    print("You are very severely underweight")
  } else if(bmirange >= 15.0 && bmirange < 16.0){
    print("You are very severely underweight")
  } else if(bmirange >= 16.0 && bmirange < 18.5){
    print("You are Underweight")
  }else if(bmirange >= 18.5 && bmirange < 25){
    print("You are Normal (healthy weight) ")
  }else if(bmirange >= 25.0 && bmirange> 30.0){
    print("You are Overweight ")
  }else if(bmirange >= 30 && bmirange> 35){
    print("You are Obese Class I (Moderately obese) ")
  }else if(bmirange >= 35 && bmirange> 40){
    print("You are Obese Class II (Severely obese) ")
  }else if(bmirange >= 40){
    print("You are Obese Class III (Very severely obese) ")
  }
}

yourbmi(170,3)

#4.------------- sum of cubes of the first n natural numbers
cubeSum <- function(n){
  cubesum <- 0
  for(i in (1:n)){
    cubesum <- cubesum + (i^3)
    i <- i+1
  }
  print(paste("sum of cubes of the first ", n ," natural numbers is ",cubesum))
}
cubeSum(3)

#5.------------- calculate the mode (highest frequency) 
x = c(2,3,3,4,4,5,6,7,9,10) 
temp <- table(as.vector(x))
names(temp)[temp == max(temp)]
#function
modeFun <- function(x){
  onex <- unique(x)
  print(onex[which.max(tabulate(match(x,onex)))])
}
modeFun(x)  

#6.----------- calculate the no. of prime numbers 
x = c(2,2,3,3,4,5,7,11,15,19,24,29)
primeCount <- function(x){
  pc <- 0
  for(i in x){
    print(i)
    falg <- 1
    if(i==2){
      pc <- pc + 1
      print(paste("Prime number: ",i))
    }  else if(i%%2:(i-1) == 0){
        flag <- 0
        break
      }
    if(flag == 1){
      print(paste("Prime number: ",i))
      pc <- pc + 1
    }
  }
  print(paste("Prime Count: ",pc))
}

primeCount(x) #Q2.Not working

#7.Create a R package for calculating the count of prime numbers , name it as “CountPrime” 
#8.----Data.frame and Data.table---
#a)
library(xlsx)
f1 <- read.xlsx("C:\\BigData\\Analytics\\R\\datasets\\f1.xlsx",1)
fix(f1)
f2 <- read.xlsx("C:\\BigData\\Analytics\\R\\datasets\\f2.xlsx",1)

merge(f1,f2,by="empno",all=F) #b)equijoin
leftjoin <- merge(f1,f2,by="empno",all.x = T) #c)leftjoin
rightjoin <- merge(f1,f2,by="empno",all.y = T) #d)rightjoin
merge(f1,f2,by="empno",all=T) #e)fulljoin

#f.
library(dplyr)
leftjoin%>%select(empno,deptId,mgrId,sal,DOJ)%>%filter(sal>300000)

#g.
rightjoin%>%group_by(empno,deptId)%>%
  select(empno,deptId,mgrId,sal,DOJ)%>%summarise(sumsal=sum(sal),avgsal=mean(sal))
f1
f2
f2$empname <- NULL
f2-f1

#i.
f2$empname <- c("name1","name2","name3","name4","name5","name6","name7")

#j.Q4. Create a “working” test that example 
write.csv(equijoin,"C:\\BigData\\Analytics\\R\\datasets\\equijoin.csv")
write.csv(leftjoin,"C:\\BigData\\Analytics\\R\\datasets\\leftjoin.csv")

#9.
#a.
library(sqldf)
sqldf("select count(distinct(empno)) from f1")
sqldf("select count(distinct(empno)) from f1 group by empno,deptId,mgrId")
#Q5.How can I add new row in the f1 table in source editor
f2
newdata <- data.frame(empno="102",sal="400000",DOJ="2012-01-10",empname="name2")
newdata <- data.frame(empno="102",sal="400000",DOJ="2012-01-10",empname="name2")
f4<- rbind(f3,newdata)
sqldf("select * from f4 group by empno,sal,DOJ,empname having count(empno) > 1 ")

#10.
#a.
findnulls <- function(datatable){
  #newdata <- subset(datatable, is.na(datatable))
  newdata  <- datatable[ rowSums(is.na(datatable)) > 0,]
  cat("Following records have null values...\n\n")
  newdata
}
findnulls(leftjoin)

#b. Doing in spark instead of hadoop
library(sparklyr)
sc <- spark_connect(master="local")
library(dplyr)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
copy_to(sc,"C:\\BigData\\Analytics\\R\\datasets\\f2.xslx","equijoin.csv")

#Q1. Unable to copy normal datasets...

src_tbls(sc)
#"batting" "flights" "iris"    "mtcars"
flights_tbl

#11.
#a.
vec <- c(2,2,3,4,2,3,5,6,7)
uniquevec <- function(vec){
    unique(vec)
}
uniquevec(vec)
#b.
library(dplyr)
uniqueVecCount <- function(datatable){
  count(unique(group_by(datatable)))
}
uniqueVecCount(f4)
#c.
concate.fn <- function(s1,s2){
  return (paste(s1,s2))
}
concate.fn("hi","how r u?")
#d
x <- cbind(1:5,5:1)
rowsum <- apply(x,1,sum)
colsum <- apply(x,2,sum)
#e
tempfile <- tempfile(fileext = ".csv")
tempfile1 <- "C:\\BigData\\Analytics\\R\\datasets\\flights1.csv"
write.csv(nycflights13::flights, tempfile1, row.names = FALSE, na = "")
tempfile1


#12.
#a.hadoop
#b.hadoop
#c.rename column names
x <- cbind(1:5,5:1)
dimnames(x)[[2]] <- LETTERS[1:2]
dimnames(x)[[2]] <- paste("Obv",1:2,sep=" ")
#d.
f4$empname<-NA
#e.
#NA is missing data - Not Available
#Nan is also missing data that is produced by the numerical computation
#NULL is doesnt exists

#f

