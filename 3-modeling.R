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


system.time(fit.ctree <- ctree(f.bike, data = training))
#fit.ctree
#run model against test data set
predictv.ctree <- predict(fit.ctree, validation)
predict.ctree <- predict(fit.ctree, testing)

RMSE(predictv.ctree,validation$count)
R2(predictv.ctree,validation$count)

#build a dataframe with our results
submit.ctree <- data.frame(datetime = testing$datetime, count=predict.ctree)

#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v4.csv",row.names=FALSE)
