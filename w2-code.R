##Question 4.2

my_data = read.table(file.choose(),header=TRUE)

#Use four numeric columns for the clustering of Species

data_num <- my_data[,1:4]

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- kmeans(data_num, centers = k, nstart = 10)$tot.withinss
}
plot(1:10, wss, type = "b")

model <- kmeans(data_num,centers=3)
table(model$cluster, my_data$Species)

#Visualize how clean and accurate the k-means clustering result
plot(data_num$Petal.Length, data_num$Petal.Width,
     col = model$cluster,
     pch=19,
     xlab="Petal Length",
     ylab="Petal Width",
     main = "K-means Clustering (k=3)")

#Comparing with the Actual data. 
#The chart is only 2-dimensional, so I firstly picked the Petal cols.
plot(data_num$Petal.Length, data_num$Petal.Width,
     col = as.numeric(my_data$Species),
     pch=19,
     xlab="Petal Length",
     ylab="Petal Width",
     main = "Actual Species")
# Error occured in col --> I corrected this by converting the Species column
#as a factor and a numeric one.

str(data_num)

species_num <- as.numeric(as.factor(my_data$Species))
plot(data_num$Petal.Length,data_num$Petal.Width,
     col=species_num,
     pch=19,
     xlab="Petal Length",
     ylab = "Petal Width",
     main = "Actual Species")

# Error fixed. And Compare the right answer and the k-means result again.
par(mfrow = c(1,2))

# K-means result
plot(data_num$Petal.Length, data_num$Petal.Width,
     col = model$cluster,
     pch = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "K-means Clusters")

# Actual species
plot(data_num$Petal.Length, data_num$Petal.Width,
     col = as.numeric(as.factor(my_data$Species)),
     pch = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "Actual Species")

# I was curious about the result when using Sepal for 2D chart
#Visualize with Sepal columns
par(mfrow = c(1,2))

plot(data_num$Sepal.Length, data_num$Sepal.Width,
     col = model$cluster,
     pch = 19,
     xlab = "Sepal Length",
     ylab = "Sepal Width",
     main = "K-means Clusters (Sepal view)")

plot(data_num$Sepal.Length, data_num$Sepal.Width,
     col = as.numeric(as.factor(my_data$Species)),
     pch = 19,
     xlab = "Sepal Length",
     ylab = "Sepal Width",
     main = "Actual Species (Sepal view)")

# The result shows Petal shows cleaner clustering than Sepal. 
# I assumed the sepal could lead a noisy clustering,
#so this time, I will only use Petal for k-means.

data_petal <- my_data[,3:4]
model_petal <- kmeans(data_petal,centers=3)
table(model_petal$cluster, my_data$Species)

# This clearly shows cleaner clustering result. 
# Visulization of the result
par(mfrow = c(1,2))

plot(data_petal$Petal.Length, data_petal$Petal.Width,
     col = model$cluster,
     pch = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "K-means Clusters (Using Petal)")

plot(data_petal$Petal.Length, data_petal$Petal.Width,
     col = as.numeric(as.factor(my_data$Species)),
     pch = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "Actual Species")

#the plots of two different k-means models look the same, so to verify it:
table(model$cluster, model_petal$cluster)
#Seems like it has different cluster number labeling per model for the same cluster
#so I matched them 
mapped_cluster <- model$cluster
mapped_cluster[model$cluster == 1] <- 3
mapped_cluster[model$cluster == 2] <- 1
mapped_cluster[model$cluster == 3] <- 2

#let's compare again

diff_points_correct <- model$cluster != model_petal$cluster
sum(diff_points_correct)

points(data_petal$Petal.Length[diff_points_correct],
       data_petal$Petal.Width[diff_points_correct],
       col = "red",
       pch = 4,
       cex = 3,      # bigger
       lwd = 3)      # thicker

plot(data_petal$Petal.Length, data_petal$Petal.Width,
     col = "lightgray",   # instead of cluster colors
     pch = 19,
     main = "Differences Highlighted")

points(data_petal$Petal.Length[diff_points_correct],
       data_petal$Petal.Width[diff_points_correct],
       col = "red",
       pch = 4,
       cex = 3,
       lwd = 3)

##Question 5.1ß

crime <- read.table(file.choose(),header=TRUE)
str(crime)
ncol(crime)
crime_rate <- crime[, 16]
crime_rate

install.packages("outliers")   # run once
library(outliers)
grubbs.test(crime_rate)

# Test for highest outlier
grubbs.test(crime_rate, type = 10)

# Test for lowest outlier
grubbs.test(crime_rate, type = 11)

boxplot(crime_rate,
        main = "Boxplot of Crime Rate",
        ylab = "Crimes per 100,000",
        col = "lightblue")

# highlight max value
points(1, max(crime_rate), col="red", pch=19)

plot(sort(crime_rate), type = "b",
     main = "Sorted Crime Rates",
     xlab = "Index",
     ylab = "Crime Rate")

# highlight max
points(length(crime_rate), max(crime_rate), col="red", pch=19)

# median
medians <- apply(crime, 2, median)
# top 3
top3 <- crime[order(crime_rate, decreasing = TRUE), ][1:3, ]

# combine
comparison <- rbind(Median = medians, top3)
comparison_clean<-round(comparison, 2)

# transpose for readability
t(comparison_clean)

## Question 6.2
temps <- read.table(file.choose(), header = TRUE, sep="\t", check.names=FALSE)

days <- temps[,1]

results <- data.frame()

for (yr in names(temps)[-1]) {
    x <- temps[[yr]]
  mu <- mean(x)
  
  cusum <- cumsum(x - mu)
  
  cp <- which.max(cusum)
  
  results <- rbind(results,
                   data.frame(
                     Year = yr,
                     EndOfSummer = days[cp]
                   ))
}

results
#draw the example cusum result 
x <- temps[["2006"]]

cusum <- cumsum(x - mean(x))
cp <- which.max(cusum)

plot(cusum,
     type = "l",
     lwd = 2,
     xlab = "Day",
     ylab = "CUSUM",
     main = "CUSUM for Atlanta Temperatures (2006)")

abline(v = cp, lty = 2)

# 6.2.2
# Average temperature from Jul 1 to EndOfSummer
summer.avg <- numeric(nrow(results))

for(i in 1:nrow(results)) {
  
  yr <- as.character(results$Year[i])
  
  # Find the change-point index
  cp <- match(results$EndOfSummer[i], days)
  
  # Average temperature from Jul 1 through end of summer
  summer.avg[i] <- mean(temps[[yr]][1:cp])
}

results$SummerAvgTemp <- summer.avg

results

plot(results$Year,
     results$SummerAvgTemp,
     type = "b",
     pch = 19,
     xlab = "Year",
     ylab = "Average Summer High (°F)",
     main = "Average Summer Temperature\n(Jul 1 to End of Summer)")

abline(lm(SummerAvgTemp ~ Year, data = results),
       col = "blue",
       lwd = 2)

#CUSUM approach
cusum.temp <- cumsum(results$SummerAvgTemp -
                       mean(results$SummerAvgTemp))

plot(results$Year,
     cusum.temp,
     type = "b",
     pch = 19,
     xlab = "Year",
     ylab = "CUSUM",
     main = "CUSUM of Average Summer Temperature")

abline(h = 0, lty = 2)


#Period of summer each year
results$SummerLength <- sapply(results$EndOfSummer,
                               function(x) match(x, days))

results[, c("Year", "EndOfSummer", "SummerLength")]

plot(results$Year,
     results$SummerLength,
     type = "b",
     pch = 19,
     xlab = "Year",
     ylab = "Summer Length (Days)",
     main = "Atlanta Summer Length by Year")