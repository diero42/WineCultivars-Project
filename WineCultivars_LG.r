# title: "Wine Cultivar Flavor Profile Analysis"
# author: "Loren Grooms"
# date: "05/18/2021"

knitr::opts_chunk$set(echo = TRUE)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(class)

# Download wine dataset from UCI website.
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
# url <- "https://raw.githubusercontent.com/diero42/WineCultivars-Project/main/wine.data"

# Read the table into a list, flatten the list, convert data to characters, and
# split each row along the commas.
wine <- strsplit(as.character(unlist(read.table(url))),split=",")

# Create a dataframe with the names of each column from the data source website.
# Insert zeroes as placeholders.
wineDat <- data.frame("Grape" = 0, "Alcohol" = 0, "MalicAcid" = 0, "Ash" = 0,
                      "AlcalinityOfAsh" = 0, "Magnesium" = 0, "TotalPhenols" = 0,
                      "Flavonoids" = 0, "NonflavonoidPhenols" = 0, "Proanthocyanins" = 0,
                      "ColorIntensity" = 0, "Hue" = 0, "ProteinConcentration" = 0,
                      "Proline" = 0)

# Insert each row of the list into the dataframe.
for(i in 1:length(wine)){
  wineDat <- rbind(wineDat,wine[[i]])
}

# Remove the row of zeroes.
# Renumber the rows.
# Convert all values to numeric using sapply.
wineDat <- wineDat[-1,]
row.names(wineDat) <- 1:nrow(wineDat)
wineDat <- as.data.frame(sapply(wineDat, as.numeric))

head(wineDat)

cat("Rows: ",nrow(wineDat))
cat("Columns: ",ncol(wineDat))

# Set random seed so results are uniform.
set.seed(4)

# Set a standard normalization function.
norm <- function(x){
  (x -min(x))/(max(x)-min(x))
}

# Normalize the data by applying the norm function to all columns.
normWine <- as.data.frame(lapply(wineDat[,2:14], norm))

# 66% and 33% have been selected as the respective sizes of the training and
# test sets.
# This is an arbitrary decision influenced by observed custom.
# Calculate 66% of the normalized data entries.
# Sample the normalized data to select a test index.
# Divide normalized data along test index.
train_size <- floor(0.66 * nrow(normWine))
test_index <- sample(seq_len(nrow(normWine)), size = train_size)
train_set <- normWine[test_index,]
test_set <- normWine[-test_index,]

# Isolate the column of the value to be predicted (grape cultivar).
trainGrape <- wineDat[test_index,1]
testGrape <- wineDat[-test_index,1]

# Set function to determine accuracy of each prediction.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

# Apply the knn function from the class package to the column of each constituent
# in the train and test sets in order to predict the grape type.
# 13 regions will be generated as this is roughly the square root of the
# total number of observations (178).
kResults <- " Accuracy:\n"
for (x in 2:13){
  kWine <- knn(train=train_set[,c(1,x)],test=test_set[,c(1,x)],cl=trainGrape,k=13)
  tab <- table(kWine,testGrape)
  kResults <- c(kResults,paste(names(train_set)[x],"=",accuracy(tab),"%\n"))
}

cat(kResults)

# Graphing AC vs each constituent, with a GLM slope predicting constituent from AC (y ~ x).
wineDat %>% ggplot(aes(Alcohol,Flavonoids,color=as.factor(Grape))) + geom_point() + geom_smooth(method = glm) + labs(color="Grape", y="Flavonoids (g/L)", x="Alcohol (% ABV)")

wineDat %>% ggplot(aes(Alcohol,ProteinConcentration,color=as.factor(Grape))) + geom_point() + geom_smooth(method = glm) + labs(color="Grape", y="Protein Concentration (g/L)", x="Alcohol (% ABV)")

wineDat %>% ggplot(aes(Alcohol,ColorIntensity,color=as.factor(Grape))) + geom_point() + geom_smooth(method = glm) + labs(color="Grape", y="Color Intensity (Chroma)", x="Alcohol (% ABV)")

wineDat %>% ggplot(aes(Alcohol,Hue,color=as.factor(Grape))) + geom_point() + geom_smooth(method = glm) + labs(color="Grape", y="Hue (°)", x="Alcohol (% ABV)")

# Manually creating a list of our results, then converting to a more readable format.
newList <- c('Alcohol Content (% ABV)','Flavonoids (g/L)','Protein Concentration (g/L)',
             'Color Intensity (Chroma)','Hue (°)',12.8,2.7,3.2,4.5,1.1,'^',
             '^','-','^','-',14.8,3.3,3.2,6.5,1.1)
newArray <- array(newList,dim=c(5,4,1))
newFrame <- as.data.frame(newArray)
reName <- newFrame[,-1]
rownames(reName) <- newFrame[,1]
colnames(reName) <- c("Low AC"," - ","High AC")
cat("Grape 1")
reName