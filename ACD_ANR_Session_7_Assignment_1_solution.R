#1.
x <- 1:100
plot(x,x^2)
plot(x,x^3)
plot(x,101-x)
plot(x,500/x)

#2.
cars <- c(1, 3, 6, 4, 9) 
plot(cars)
plot(cars,col="red")
title("samle of 5 cars")
plot(cars,type="o",col="blue",main="Autos",
     col.main="red",font.main=4)

cars <- c(1, 3, 6, 4, 9) 
trucks <- c(2, 5, 4, 5, 12) 

plot(cars, type="o", col="blue", ylim=c(0,12))
lines(trucks, type="o", pch=22, lty=2, col="red") 
title(main="Autos", col.main="red", font.main=4)
