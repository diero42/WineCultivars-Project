knitr::opts_chunk$set(echo = TRUE)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(class)
library(gmodels)

#Download wine dataset from UCI website
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

#Read the table into a list, flatten the list, convert data to characters, and split each row along the commas
wine <- strsplit(as.character(unlist(read.table(url))),split=",")

#Create a dataframe with the names of each column from the data source website
#Insert zeroes as placeholders
wineDat <- data.frame("Grape" = 0, "Alcohol" = 0, "Ash" = 0, "AlcalinityOfAsh" = 0, "Magnesium" = 0, "TotalPhenols" = 0, "Flavanoids" = 0, "NonflavanoidPhenols" = 0, "Proanthocyanins" = 0, "ColorIntensity" = 0, "Hue" = 0, "ProteinConcentration" = 0, "Proline" = 0)

#Insert each row of the list into the dataframe
for(i in 1:length(wine)){
  wineDat <- rbind(wineDat,wine[[i]])
}

#Remove the row of zeroes
#Remove the unused columns
#Renumber the rows
#Convert all values to numeric using sapply
wineDat <- wineDat[-1,]
wineDat <- wineDat[,-c(4:14)]
row.names(wineDat) <- 1:nrow(wineDat)
wineDat <- as.data.frame(sapply(wineDat, as.numeric))

#Set a standard normalization function gotten from the internet
norm <- function(x){
  (x -min(x))/(max(x)-min(x))
}

#Normalize the data by applying the norm function to all columns
normWine <- as.data.frame(lapply(wineDat[,1:3], norm))

#66% and 33% have been selected as the respective sizes of the training and test sets. This is an arbitrary decision influenced by observed custom
#Calculate 66% of the normalized data entries
#Sample the normalized data to select a test index
#Divide normalized data along test index
train_size <- floor(0.66 * nrow(normWine))
test_index <- sample(seq_len(nrow(normWine)), size = train_size)
train_set <- normWine[test_index,]
test_set <- normWine[-test_index,]

#Isolate the column of the value to be predicted (grape cultivar)
trainGrape <- wineDat[test_index,1]
testGrape <- wineDat[-test_index,1]

#Apply the knn function from the class package to the train and test sets in order to predict the grape type
#13 regions will be generated as this is roughly the square root of the total number of observations (178)
kWine <- knn(train=train_set,test=test_set,cl=trainGrape,k=13)
CrossTable(x=testGrape,y=kWine,prop.chisq = FALSE)
wineDat %>% ggplot(aes(Ash,Alcohol,color=as.factor(Grape))) + geom_point() + geom_smooth(method = glm) + labs(color="Grape")