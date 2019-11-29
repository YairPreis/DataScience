
## linear regression

data <- Orange
head(data)
mod <- lm(age ~ circumference ,data = data)
summary(mod)
yhat <- predict(mod,data)
plot(yhat ~ data$age)
scatter.smooth(data$age,yhat)


## logistic regression


data <- iris
head(data)
data$y <- ifelse(data$Species == "versicolor",1,0) 
data$species <- NULL
mod1 <- glm(y ~ Petal.Length ,data = data , family = "binomial")
summary(mod1)
pred <- predict(mod1,data,type = "response")
hist(pred)
yhat <- ifelse()
table(yhat = yhat, y = data$y)
accuracy <-
accuracy.
vif