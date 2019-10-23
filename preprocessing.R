library(tidyverse)

## load data
data <- read.csv("./311_Customer_Service_Requests.csv", nrow = 3000)

## subsample data realted to litter
data_dirty <- data %>% filter(SRType == "SW-Dirty Alley")

## sample 4 rows of data
data_sample <- data_dirty[c(1, 6, 34, 59),]

## distance between each pair using google maps
dist <- matrix(NA, nrow(data_sample) + 1, nrow(data_sample) + 1)
rownames(dist) <- c("JHSPH", as.character(data_sample$SRRecordID))
colnames(dist) <- c("JHSPH", as.character(data_sample$SRRecordID))
dist[1, ] <- c(0, 1.4, 1.9, 2.7, 1.9)
dist[2, ] <- c(1.4, 0, 3.2, 4.0, 3.3)
dist[3, ] <- c(1.9, 3.2, 0, 0.9, 2.7)
dist[4, ] <- c(2.7, 4.0, 0.9, 0, 2.7)
dist[5, ] <- c(1.9, 3.3, 2.7, 2.7, 0)

save(data_sample, dist, file = "data_sample.RData")

