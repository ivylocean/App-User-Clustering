day2 <- read.table("D:/PKU/mobiledata/day02.txt", head = F, sep = ",")
head(day2)
day2 <- day2[, c(1, 2, 8)]
index <- rep(2, dim(day2)[1])
day2 <- cbind(day2, index)
day3 <- read.table("D:/PKU/mobiledata/day03.txt", head = F, sep = ",")
day3 <- day3[, c(1, 2, 8)]
index <- rep(3, dim(day3)[1])
day3 <- cbind(day3, index)
day4 <- read.table("D:/PKU/mobiledata/day04.txt", head = F, sep = ",")
day4 <- day4[, c(1, 2, 8)]
index <- rep(4, dim(day4)[1])
day4 <- cbind(day4, index)
day5 <- read.table("D:/PKU/mobiledata/day05.txt", head = F, sep = ",")
day5 <- day5[, c(1, 2, 8)]
index <- rep(5, dim(day5)[1])
day5 <- cbind(day5, index)
day6 <- read.table("D:/PKU/mobiledata/day06.txt", head = F, sep = ",")
day6 <- day6[, c(1, 2, 8)]
index <- rep(6, dim(day6)[1])
day6 <- cbind(day6, index)

cols <- c("uid", "appid", "duration", "day")
data <- rbind(day2, day3, day4, day5, day6)
colnames(data) <- cols
dim(data)
head(data)
app_class <- read.csv("D:/PKU/mobiledata/app_class.csv", header = F)
colnames(app_class) <- c("appid", "appclass")
mobile <- merge(x = data, y = app_class, by = "appid", all.x = T, incomparables = NA)
head(mobile)
unique(mobile$appclass)
dim(mobile)
mobile <- mobile[!is.na(mobile$appclass),]
table(mobile$appclass)
# save(mobile, file = "D:/PKU/mobiledata/mobile.RData")
# write.csv(mobile, "D:/PKU/mobile.txt")
###
load("D:/PKU/mobiledata/mobile.RData")

library(tidyr)
library(tidyverse)
# install.packages("dplyr")
mobile$day <- as.integer(mobile$day)
mobile$duration <- as.numeric(mobile$duration)
app_use <-  mobile %>% 
              group_by(uid, appclass) %>%
                summarise(sum_duration = sum(duration))
head(app_use)
app_use <- spread(app_use, appclass, sum_duration)
app_use[2:21] <- log(app_use[2:21])
app_use[is.na(app_use)] = 0
save(app_use , file = "D:/PKU/mobiledata/app_use.RData")
# write.csv(app_use, "D:/PKU/app_use.txt")
ptm <- proc.time()
cc <- 1:14
for(i in 2:15){
  k <- kmeans(app_use[2:21], i, nstart = 20, algorithm = "Lloyd", iter.max = 400)
  cc[i-1] <- k$bet/k$totss
}
names(cc) <- 2:15
round(cc, 2)
plot(1:14, cc, ylim = c(0, 0.5), type = "b", col = "#336699",
     main = "K值選擇",
     ylab = "組間方差/總方差", xlab = "k值")
k <- kmeans(app_use[2:21], 7, nstart = 20, algorithm = "Lloyd", iter.max = 400)
k$centers
k$size
k$size/dim(app_use)[1]
proc.time() - ptm

# plot(app_use[2:21], col = k$cluster, main = "K-means: app user")
app.pca <- princomp(app_use[, 2:21], cor = TRUE, scores = TRUE)
pca.dim1 <- app.pca$scores[, 1]
pca.dim2 <- app.pca$scores[, 2]
pca.dim3 <- app.pca$scores[, 3]

plot(pca.dim1, pca.dim2, main = "PCA for Mobile Data
     with K-means", xlab = "PCA-1", ylab = "PCA-2",
     col = k$cluster)
legend("topright", legend = paste("Group", 1:7), col = 1:7, pch = 20)
scatterplot3d::scatterplot3d(pca.dim1, pca.dim2, pca.dim3, pch = 20,
                             color = alpha(k$cluster, 0.5))
