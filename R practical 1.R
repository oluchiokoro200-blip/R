getwd()
v1<- seq(from = 1, to = 4, by = 0.2)
v1
rep(1,5)
rep(2,10)
# This command creates a single vector containing the number 1 repeated 5 times,
#followed by the number 2 repeated 10 times.
c(rep(1, 5), rep(2, 10))
rep(1:5, times = 2)
crop_yield<- read.csv("C:/Users/Stephen/Desktop/OLUCHI/Example-01 (2).csv")
crop_yield

ncol(crop_yield)
nrow(crop_yield)
y_values<- read_xlsx("C:/Users/Stephen/Desktop/OLUCHI/Example-03 (1).xlsx")  
y_values
str(y_values)
summary(y_values)
skim(y_values)
