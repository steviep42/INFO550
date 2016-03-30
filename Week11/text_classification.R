###
# mtcars knn primer

# We want to predict what cars are automatic or manual based on 
# other attributes from the data

percent <- ceiling(nrow(mtcars)*0.80)

train.idx <- sample(1:nrow(mtcars),percent,F)

train.mtcars <- mtcars[train.idx,] 
test.mtcars  <- mtcars[-train.idx,]

preds <- knn(train.mtcars[,-9],test.mtcars[,-9],mtcars[train.idx,]$am)

table("Predictions" = preds, Actual= test.mtcars[,"am"])


mytrainer <- function(fraction=0.80, iterations=10) {
  
  retlist <- list()
  
  for (ii in 1:iterations) {
    percent <- ceiling(nrow(mtcars)*fraction)
    train.idx <- sample(1:nrow(mtcars),percent,F)
    
    train.mtcars <- mtcars[train.idx,] 
    test.mtcars  <- mtcars[-train.idx,]
    
    preds <- knn(train.mtcars[,-9],test.mtcars[,-9],mtcars[train.idx,]$am)
    
    out <- table("Predictions" = preds, Actual= test.mtcars[,"am"])
    if (prod(dim(out)) != 4) {
      accuracy <- 0
    } else {
      accuracy <- (out[1,1]+out[2,2])/sum(out)
    }
    
    retlist[[ii]] <- list(percent=fraction,
                          train=train.mtcars,
                          test=test.mtcars,
                          preds=preds,
                          table=out,
                          accuracy=accuracy)
  }
  return(retlist)
}

mypreds <- mytrainer()

sapply(mypreds, function(x) x$accuracy)


###


library(tm)
library(SnowballC)
library(dplyr)

setwd("~/Downloads")

# Read in the data

tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)

str(tweets)

tweets %>% filter(Avg == -1) %>% select(Tweet)


# Create dependent variable

tweets$Negative <- as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Create corpus

tweets$Tweet[1]

corpus <- Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus

corpus[[1]]


# Convert to lower-case

corpus <- tm_map(corpus, tolower)

corpus[[1]]$content

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus <- tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus <- tm_map(corpus, removePunctuation)

corpus[[1]]$content

# Look at stop words 

stopwords("english")[1:10]

# Remove stopwords and apple

corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]$content

# Stem document 

corpus <- tm_map(corpus, stemDocument)

corpus[[1]]$content


# Create matrix

frequencies <- DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

sparse <- removeSparseTerms(frequencies, 0.995)
sparse

inspect(sparse[1000:1005,10:20])

# Convert to a data frame

tweetsSparse <- as.data.frame(as.matrix(sparse))
rownames(tweetsSparse) <- make.names(rownames(tweetsSparse),unique=TRUE)
# Make all variable names R-friendly

colnames(tweetsSparse) <- make.names(colnames(tweetsSparse),unique=TRUE)

# Add dependent variable

tweetsSparse$Negative <- tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse  <- subset(tweetsSparse, split==FALSE)

# Look at KNN

preds <- knn(trainSparse[,-310],testSparse[,-310],trainSparse[,310])

knnout <- table("Predictions" = preds, Actual= testSparse[,310])

(knnacc <- round(sum(diag(knnout))/sum(knnout),2))

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART <- predict(tweetCART, newdata=testSparse, type="class")

cartout <- table(actual=testSparse$Negative, predicted=predictCART)
(cartacc <- round(sum(diag(cartout))/sum(cartout),2))

# Compute accuracy

# (294+18)/(294+6+37+18)

# Baseline accuracy 

table(actual=testSparse$Negative)

# 300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF <- randomForest(Negative ~ ., data=trainSparse)

# Make predictions:

predictRF <- predict(tweetRF, newdata=testSparse)

RFout <- table(actual=testSparse$Negative, predicted=predictRF)
(RFacc <- round(sum(diag(RFout))/sum(RFout),2))

# Accuracy:
# (293+21)/(293+7+34+21)

