source("1-setup.R")

# load training data


# histograms of count
ggplot(data = training,aes(x = count)) + geom_histogram(fill = "blue")
ggplot(data = training,aes(x = count)) + geom_histogram(fill = "blue") + facet_wrap(~ weather)
ggplot(data = training,aes(x = count)) + geom_histogram() + facet_wrap(~ workingday)

ggplot(data = training,aes(x = atemp)) + geom_histogram()

# boxplots
ggplot(data = training,aes(x = season,y = count,color = season)) + geom_boxplot()
ggplot(data = training,aes(x = workingday,y = count,color = workingday)) + geom_boxplot()
ggplot(data = training,aes(x = hr,y = count,color = hr)) + geom_boxplot()
ggplot(data = training,aes(x = holiday,y = count)) + geom_boxplot(color = "green")
ggplot(data = training,aes(x = wdy,y = count,color = wdy)) + geom_boxplot()
ggplot(data = training,aes(x = mth,y = count)) + geom_boxplot(color = "blue")
ggplot(data = training,aes(x = yr,y = count)) + geom_boxplot(color = "blue")
ggplot(data = training,aes(x = weather,y = count,color = weather)) + geom_boxplot()
ggplot(data = training,aes(x = windy,y = count)) + geom_boxplot()

# scatter plots
g.t <- ggplot(data = training,aes(x = atemp,y = count)) + 
  geom_point(alpha = .2,color = "orange") + geom_smooth()
g.t

g.h <- ggplot(data = training,aes(x = humidity,y = count)) + 
  geom_point(alpha = .2,color = "blue") + geom_smooth()
g.h

g.w <- ggplot(data = training,aes(x = windspeed,y = count)) + 
  geom_point(alpha = .2,color = "green") + geom_smooth()
g.w

grid.arrange(g.t,g.h)


training %>% group_by(wdy) %>% summarize(mean.count = mean(count))

training.agg <- training %>% group_by(yr,mth) %>% summarize(mean.count = mean(count))
training.agg$yrmon <- ymd(paste(training.agg$yr, training.agg$mth,'01', sep = "-"))

g <- ggplot(data = training.agg,aes(x = yrmon,y = mean.count)) + geom_line()
