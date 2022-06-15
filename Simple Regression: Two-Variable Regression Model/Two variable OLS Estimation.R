study <- read.csv("C:\\Users\\User\\Desktop\\STK 310 (Stats - regression analysis)\\section a\\code\\datasets\\education-vs-wages.csv")

summary(study)

x <- study$Years.of.Schooling
y <- study$Hourly.Wage

cov(x,y)
cor(x,y)

lrm <- lm(y ~ x, data = study)

coef(lrm)
summary(lrm)
vcov(lrm)

plot(x, y,
     main = "Scatter diagram of hourly wage againts years of schooling",
     pch = 20,
     col = "blue",
     xlab = 'Years of schooling',
     ylab = 'Hourly wage ($)')
abline(lrm, col = "midnightblue")
