# INFO 550 Wk 13 - some considerations

rn <- rnorm(1000,10)

x <- 1.003234
mysummmary <- function(x=rnorm(100)) {

  # mysummary - to compute some summaries
  # INPUT: x - a vector of some values
  # OUTPUT: retvec - a vector of a mean and a sd

  newfunc <- function(y) {
    return(y=log(y))
  }

  x <- newfunc(x)
  retvec <- c(mean=mean(x),sd=sd(x))

  # One return statement if any
  return(list(xvec=x,vec=retvec))
}


mean(sample(rn,replace=T))


n <- 10000
somevec <- vector()
length(somevec) <- n
for (ii in 1:n) {
  somevec[ii] <- mean(sample(rn,replace=T))
}
plot(density(somevec))

#

n <- 1000000
somevec <- vector()
length(somevec) <- n
for (ii in 1:n) {
  somevec[ii] <- mean(sample(rn,replace=T))
}
plot(density(somevec))

# Plot as we go along


n <- 10000
somevec <- vector()

for (ii in 1:n) {
  somevec[ii] <- mean(sample(rn,replace=T))

  if ((length(somevec) > 50) & (length(somevec) %% 75 == 0)) {
   plot(density(somevec))
   Sys.sleep(1.5)
  }
}

#

n <- 1000000
somevec <- vector()
length(somevec) <- n
for (ii in 1:n) {
  somevec[ii] <- mean(sample(rn,replace=T))
}
plot(density(somevec))

# time these two

boot1 <- function(nt=1000) {
  n <- nt
  somevec <- vector()
  length(somevec) <- nt
  for (ii in 1:n) {
    somevec[ii] <- mean(sample(rn,replace=T))
  }
  plot(density(somevec))

}


boot2 <- function(nt=1000) {
  n <- nt
  somevec <- vector()
  for (ii in 1:n) {
    somevec[ii] <- mean(sample(rn,replace=T))
  }
  plot(density(somevec))

}

# apply

set.seed(1)  # Makes the call to rnorm generate the same numbers every time
( mymat <- matrix(round(rnorm(16,10),2),4,4) )

mean(mymat[,1])


mean(mymat[,2])


mean(mymat[,3])


mean(mymat[,4])

( mymatcolmean <- c(mean(mymat[,1]),mean(mymat[,2]),mean(mymat[,3]),mean(mymat[,4])) )

# Let's write a loop

retvec <- vector()
for (ii in 1:ncol(mymat)) {
  retvec[ii] = mean(mymat[,ii])
}
retvec

# We could even put this into a function for later use in case we need it.

myloop <- function(somemat) {
  retvec <- vector()
  length(retvec) <- ncol(somemat)
  for (ii in 1:ncol(somemat)) {
    retvec[ii] <- mean(somemat[,ii])
  }
  return(retvec)
}

myloop(mymat)


# This will now work for any matrix but of course it is
# specific to the columns of the matrix

set.seed(1)

newmat <- matrix(rnorm(100),10,10)

myloop(newmat)

# Create a function

myloop <- function(somemat,rc=2) {
  retvec <- vector()
  length(retvec) <- ncol(somemat)

  # Check to see if we are taking the mean of the columns or rows
  # 1 indicates rows, 2 indicates columns

  if (rc == 2) {
    for (ii in 1:ncol(somemat)) {
      retvec[ii] <- mean(somemat[,ii])
    }
  } else {
    for (ii in 1:nrow(somemat)) {
      retvec[ii] <- mean(somemat[ii,])
    }
  }
  return(retvec)
}

# Okay let's make sure it works.

myloop(mymat,2)   # This is correct

myloop(mymat,1)

# Easier way follows

apply(mymat, 2, mean)


apply(mymat, 1, mean)


apply(mymat,2,class)  # What class do the columns belong to ?

apply(mymat,2,sum)    # Get the sum of all the columns

apply(mymat,1,range)  # Get the range of all rows

apply(mymat,2,fivenum) # Get the fivenum summary for each column

# Some activities on matrices are so common that R actually has dedicated functions for them that are very efficient and fast:

rowMeans(mymat)     # Equivalent to apply(mymat,1,mean)

colMeans(mymat)     # Equivalent to apply(mymat,2,mean)

rowSums(mymat)      # Equivalent to apply(mymat,1,sum)

colSums(mymat)      # Equivalent to apply(mymat,2,sum)


##

apply(mymat, 2, mean)

mean(mymat[,1],trim=0.5)

apply(mymat, 2, mean(trim=0.5))

apply(mymat, 2, mean(x,trim=0.5))

apply(mymat,2,mean,trim=0.5)

apply(mymat,2,mean,trim=0.5, na.rm=T)

myspecmean <- function(x) {
  return(mean(x,trim=0.5,na.rm=T))
}
### write our own functions

mymat[,1]/sum(mymat[,1])


mymat[,2]/sum(mymat[,2])

mymat[,3]/sum(mymat[,3])

mymat[,4]/sum(mymat[,4])


myfunc <- function(x) {
  return(x/sum(x))
}

# Check it out to make sure it works

myfunc(mymat[,1])


all.equal(myfunc(mymat[,1]), mymat[,1]/sum(mymat[,1]))

apply(mymat, 2, myfunc)

apply(mymat, 2, function(x) x/sum(x))

url <- "http://steviep42.bitbucket.org/data/su_et_al_2002.txt"
mydata <- read.csv(url,header=T)

# B = brain, BF = fetal brain, L = liver, LF = fetal liver

str(mydata)

apply(mydata,2,function(x) c(mu=mean(x),sd=sd(x)))


## Coins


(flips <- sample(c("H","T"),1))

(flips <- sample(c("H","T"),3))

(flips <- sample(c("H","T"),3,T))

cointab <- flips <- sample(c("H","T"),10000,T)

table(cointab)

chisq.test(table(cointab),p=c(.5,.5))

table(cointab)

cointab <- sample(coins,1000000,T,prob=c(.70,.30))

table(cointab)

chisq.test(table(cointab),p=c(.5,.5))


mcoins <- function(times=1000,probs=c(.5,.5)) {
  cointab <- sample(c("H","T"),times,T,prob=probs)
  return(table(cointab))
}


mcoins <- function(times=1000,probs=c(.5,.5)) {
  cointab <- sample(c("H","T"),times,T,prob=probs)
  return(table(cointab))
}

# Do the above experiment 10 times

replicate(10,mcoins())


# Create a proportion table out of each column

prop.table(replicate(10,mcoins()),2)

#


apply((replicate(10,mcoins())),2,chisq.test,p=c(.5,.5))


apply((replicate(100,mcoins(times=10))),2,function(x)
                                  round(chisq.test(x,p=c(.5,.5))$p.value,2))


# Is this any better than ?

mcoins <- function(rep=10,times=1000,probs=c(.5,.5)) {

  for (ii in 1:rep) {
      cointab <- replicate(rep,sample(c("H","T"),times,T))
  }
  return(table(cointab))
}

# Gratuitous experiments with apply, tapply, sapply

tapply(mtcars$mpg,mtcars$cyl,mean)

split(mtcars,mtcars$cyl) -> myl

lapply(myl, function(x) mean(x$mpg))

unlist(lapply(myl, function(x) mean(x$mpg)))

sapply(myl, function(x) mean(x$mpg))

newmat <- matrix(rnorm(100),10,10)

apply(newmat,2,range)

apply(apply(newmat,2,range),2,sum)

