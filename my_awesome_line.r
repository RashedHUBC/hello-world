a <- 2
d<-90
b <- 3

sig_sq <- 1

x <- runif(40)

y <- a + b*x + rnorm(40, sd= sqrt(sig_sq))

(avg_x <- mean(x))

write(avg_x, "avg_x.txt")

plot(x,y)

abline(a=a, b=b, col="hotpink", lwd=2)

my_fit <- lm(y~x)

####hello...

summary(my_fit)

