source("1-setup.R")
# modeling


####### Random Forest
system.time(rf.fit <- randomForest(count ~.,data = select(training,-datetime)))

# prediction
rfpred <- predict(rf.fit,newdata=testing)
rfpredv <- predict(rf.fit,newdata=validation)

varImpPlot(rf.fit)
testing.results <- tbl_df(data.frame(testing,rfpred))

#build a dataframe with our results
submit.rf <- data.frame(datetime = testing$datetime, count=rfpred)
#write results to .csv for submission
write.csv(submit.rf, file="submit_rf_v2.csv",row.names=FALSE)

####### try ctree and party
#install.packages("party")

f.bike <- count ~ hr + weather + atemp + mth + wdy + season + holiday

system.time(fit.ctree <- ctree(f.bike, data = training))
fit.ctree
#run model against test data set
predictv.ctree <- predict(fit.ctree, validation)
predict.ctree <- predict(fit.ctree, testing)

#how well did the model do with validation set
RMSE(predictv.ctree,validation$count)
RMSE(rfpredv,validation$count)
R2(predictv.ctree,validation$count)
R2(rfpredv,validation$count)

#build a dataframe with our results
submit.ctree <- data.frame(datetime = testing$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v1.csv",row.names=FALSE)


#################### another ctree v2
f.bike <- count ~ hr + weather  + atemp + mth + wdy + season + holiday + workingday + 
  humidity + daypart + sunday


#################### another ctree v3
f.bike <- count ~ yr + season + holiday + workingday + weather + temp + atemp + humidity + 
  hr + daypart + sunday

#################### another ctree v4
formula <- count ~ yr + season + holiday + workingday + weather + temp + 
  atemp + humidity + hr + daypart + sunday


system.time(fit <- ctree(formula, data = training))

############ try caret train # best so far rmse = 48.7095
formula <- count ~ yr + season + holiday + workingday + weather + temp + 
  atemp + humidity + hr + daypart + sunday
# try interaction for temp and humidity
formula <- count ~ yr + season + holiday + workingday + weather + temp + 
  atemp + humidity + hr + daypart + sunday + temp:humidity
# much worse rmse = 118
#formula <- count ~ yr + season + holiday + workingday + weather + 
#  atemp + humidity + daypart + sunday

system.time(fit <- train(formula,data = training,method = "ctree",tuneLength = 3))

######## try caret with random forest, rf took 5911 seconds to run but rmse of 23.47212, new run 5887 time,rmse 23.34
######### rmsle .204547
system.time(fit <- train(formula,data = training,method = "rf",tuneLength = 3))
# try greater tuneLength, was not good rmse = 48.7095,rmsel = .35
system.time(fit <- train(formula,data = training,method = "rf",tuneLength = 4)) 
# try less tuneLength, a lot more time but no better performance: 14319 s and rmse 25.06,rmsel .231
system.time(fit <- train(formula,data = training,method = "rf",tuneLength = 5))

######### try simple linear model, this was a dud
#fit <- lm(count  ~ yr + atemp + daypart +sunday ,data = training)
#fit.ctree
#run model against test data set
predict.validation <- predict(fit, validation)
predict.testing<- predict(fit, testing)

RMSE(predict.validation,validation$count)
R2(predict.validation,validation$count)
rmsle(predict.validation,validation$count)

save(fit,file ="rf_v7.rda")

#build a dataframe with our results
submit <- data.frame(datetime = testing$datetime, count=predict.testing)

#write results to .csv for submission
write.csv(submit, file="submit_rf_v7.csv",row.names=FALSE)
