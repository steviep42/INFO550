# Load up some libraries that will be useful here

library(tm)
library(SnowballC)
library(plyr)
library(class)
library(wordcloud)
library(XML)
library(RCurl)
library(RColorBrewer)


### MLK Speech

# See http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know


# Read the text file
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
# docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
# docs <- tm_map(docs, stemDocument)

# Create the Term Document Matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Create Document Term Matrix is just as easy - you can tranpose it also

dtm <- DocumentTermMatrix(docs)
m <- t(as.matrix(dtm))
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Next generate a Word cloud although this is more eye candy

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Find words mentioned at least 4 times
findFreqTerms(dtm, lowfreq = 4)

# find associations with a given word

findAssocs(dtm, terms = "freedom", corlimit = 0.3)

head(d, 10)


#######
# mtcars knn primer

# We want to predict what cars are automatic or manual based on 
# other attributes from the data

(percent <- ceiling(nrow(mtcars)*0.80))

train.idx <- sample(1:nrow(mtcars),percent,F)

train.mtcars <- mtcars[train.idx,] 
test.mtcars  <- mtcars[-train.idx,]

preds <- knn(train.mtcars[,-9],test.mtcars[,-9],mtcars[train.idx,]$am)

(out <- table("Predictions" = preds, Actual= test.mtcars[,"am"]))

sum(diag(out))/sum(out)

# Let's set up a function that will help us do this a few times

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
      accuracy <- (out[1,1]/sum(out))
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


#### GET PRESIDENTIAL CANDIDATE SPEECHES
# Using the URL 
# http://www.presidency.ucsb.edu/2012_election_speeches.php?candidate=44&doctype=1150
# This program will download and parse each of the speech links there. We'll keep it basic
# and download 53 speeches for each of Romney and Obama from the 2012 campaigns
#
#### 

# First get the OBAMA campaign speeches from 2012
setwd("~/Downloads/Candidates/OBAMA")

# Create a list / vector to house all the text from the sppech
nlinks <- getHTMLLinks("http://www.presidency.ucsb.edu/2012_election_speeches.php?candidate=44&doctype=1150")

nlinks <- nlinks[39:143]  # These represent just the 2012 speeches

# nlinks <- nlinks[sample(1:length(nlinks),35,F)]

# Let's get 53 speeches listed in the main body

for (ii in 1:53) {
  lks <- nlinks[ii]
  lks <- paste("http://www.presidency.ucsb.edu",substr(lks,3,26),sep="")
  cat("Processing ",lks,"\n")
  speech <- getURL(lks)
  
  doc <- htmlParse(speech,asText=TRUE)
  
  plain.text <- xpathSApply(doc,"//p",xmlValue)
  plain.text <- gsub("applause","",plain.text)
  plain.text <- gsub("\"","",plain.text)
  txt2write <- paste(plain.text,collapse=" ")
  tmpname <- paste("obama_speech",ii,sep="_")
  
  print(tmpname)
  
  write(txt2write,file=tmpname)
}


## Get speeches for Romney - We'll also get 53 of those

setwd("~/Downloads/Candidates/ROMNEY")

# Create a list / vector to house all the text from the sppech

nlinks <- getHTMLLinks("http://www.presidency.ucsb.edu/2012_election_speeches.php?candidate=79&campaign=2012ROMNEY&doctype=5000")

nlinks <- nlinks[39:92]  # These represent just the 2012 speeches

# 

for (ii in 1:length(nlinks)) {
  lks <- nlinks[ii]
  lks <- paste("http://www.presidency.ucsb.edu",substr(lks,3,26),sep="")
  cat("Processing ",lks,"\n")
  speech <- getURL(lks)
  
  doc <- htmlParse(speech,asText=TRUE)
  
  plain.text <- xpathSApply(doc,"//p",xmlValue)
  
  txt2write <- paste(plain.text,collapse=" ")
  txt2write <- gsub("ROMNEY:|APPLAUSE|Laughter","",txt2write)
  tmpname <- paste("romney_speech",ii,sep="_")
  
  print(tmpname)
  
  write(txt2write,file=tmpname)
}

####
# Next let's create Document Term Matricies for both sets of candidate speeches
# Are the different from each other ? Let's find out

# So here we want to prcoess each of the 53 speeches from each candidate.

#Set Parameters to find the speech data

candidates <- c("ROMNEY","OBAMA")
pathname <- "/Users/fender/Downloads/Candidates"

# Create a function to Clean text before we make the DTM

cleanCorpus <- function(docs) {
  # Convert the text to lower case
  c.docs <- tm_map(docs, content_transformer(tolower))
  
  # Remove numbers
  c.docs <- tm_map(c.docs, removeNumbers)
  
  # Remove english common stopwords
  c.docs <- tm_map(c.docs, removeWords, c(stopwords("english"),"obama","romney"))
  
  c.docs <- tm_map(c.docs, removePunctuation)
  
  # Eliminate extra white spaces
  c.docs <- tm_map(c.docs, stripWhitespace)
  
  # Text stemming
  c.docs <- tm_map(c.docs, stemDocument)
  
  return(c.docs)
}

# Build Document Term Matrix

gererateTDM <- function(cand,path){
  s.dir <- sprintf("%s/%s",path,cand)

  # Next up let's get the speeches from the correct directory/folder
  # We also clean it from various things like whitespace, punctuation, etc
  
  s.cor <- Corpus(DirSource(directory = s.dir, encoding= "Windows-1254"))
  s.cor.cl <- cleanCorpus(s.cor)
  
  # Next we make the actual Dcoument Term Matrix
  
  s.tdm <- DocumentTermMatrix(s.cor.cl,
                              control=list(tolower=TRUE,
                                           removePunctuation = TRUE))
  
  # Let's kick out terms that are too sparse - for example here we remove terms that
  # have at least a 70% "sparsity" - 70% of the time they don't show up in any of the docs 
  
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = cand, tdm = s.tdm)
  
}

# The lapply let's us loop over the candidates

tdm <- lapply(candidates,gererateTDM, path=pathname)

# This gives us a 2 element list with the first elment being for Romney
# and the second being for Obama

# Let's pull out some of the words from romney speeches.
# Find Terms mentioned in the species over 100 times

findFreqTerms(tdm[[1]]$tdm, lowfreq=100)

# Let's look at some of associations with the word tax from the romney speeches

findAssocs(tdm[[1]]$tdm, terms="tax",corlimit=0.45)

# Now Let's build a word cloud. First convert the TDM to a matrix. We'll also get 
# the transpose to accommodate the requirements of the word cloud command

rom <- t(as.matrix(tdm[[1]]$tdm))

#Get word counts in decreasing order
word_freqs <- sort(rowSums(rom), decreasing=TRUE) 

#Create data frame with words and their frequencies
drom <- data.frame(word=names(word_freqs), freq=word_freqs)


#Plot wordcloud
wordcloud(drom$word, drom$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

## Let's do the same for Obama 

# Let's pull out some of the words from romney speeches.
# Find Terms mentioned in the species over 200 times

findFreqTerms(tdm[[2]]$tdm, lowfreq=200)

# Let's look at associations to the word tax

findAssocs(tdm[[2]]$tdm, terms="tax",corlimit=0.45)

# At the same level as romney there are lots more assocations

findAssocs(tdm[[2]]$tdm, terms="tax",corlimit=0.55)

oba <- t(as.matrix(tdm[[2]]$tdm))

#Get word counts in decreasing order
word_freqs <- sort(rowSums(oba), decreasing=TRUE) 

#Create data frame with words and their frequencies
doba <- data.frame(word=names(word_freqs), freq=word_freqs)

#Plot wordcloud
wordcloud(doba$word, doba$freq, random.order=FALSE, max.words=200, colors=brewer.pal(8, "Dark2"))

###

# Create a data frame out of the DTMs and attach candidate's name 
bindCandidateToTDM <- function(tdm) {
  s.mat <- (data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringAsFactors = FALSE)
  s.df <-cbind (s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetCandiate"
  return(s.df)
}

candTDM <- lapply(tdm, bindCandidateToTDM)

# Stack
tdm.stack <- do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)] <- 0

# Side discussion on rbind.fill

mtcars.1 <- mtcars[1:10,]
mtcars.2 <- mtcars[20:30,]
mtcars.2$name <- rownames(mtcars[20:30,])
mylist <- list(df1=mtcars.1,df2=mtcars.2)
stacked <- do.call(rbind.fill,mylist)
stacked[is.na(stacked)] <- 0



# Hold-Out
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.8))
test.idx <- (1:nrow(tdm.stack))[-train.idx]

# Model - KNN
tdm.cand <- tdm.stack[, "targetCandiate"]

# Let's get rid of the candidate name from the data frame

col2leaveout <- grep("target",colnames(tdm.stack))

tdm.stack.nl <- tdm.stack[,-col2leaveout]

knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])

# Accuracy
(conf.mat <- table("Predictions" = knn.pred, Actual= tdm.cand[test.idx]))
(accuracy <- sum(diag(conf.mat)) / length(test.idx)* 100) 

