system.time(source("1-setup.R"))

f <- count ~ daypart + season + temp + yr
fit <- lm(f,data = training)

predict.validation <- predict(fit, validation)

#how well did the model do with validation set
RMSE(predict.validation,validation$count)
R2(predict.validation,validation$count)
rmsle(predict.validation,validation$count)

summary(fit)

par(mfrow = c(2,2))
plot(fit)

