
# http://steviep42.bitbucket.org/YOUTUBE.DIR/chi_crimes.csv

# This gives us a scatterplot 

plot(mtcars$wt,mtcars$mpg) 

# We create a table and plot somehow knows how to deal with. How ?

mytable <- table(mtcars$am,mtcars$cyl)
plot(mytable)

# We create a Linear Modeling object and plot knows how to 
# plot it. How ? 

mylm <- lm(mpg~wt, data=mtcars)
plot(mylm)


plot(mtcars) 

# This will plot a histogram

myhisto <- hist(mtcars$mpg)
plot(myhisto)

# We create a pincipal components object 

myprcomp <- prcomp(~ Murder + Assault + Rape, 
                   data = USArrests, scale = TRUE)
plot(myprcomp)



mylm <- lm(mpg~wt,data=mtcars)
class(mylm)

mytable <- table(mtcars$am, mtcars$cyl)
class(mytable)

class(mtcars)


plot

  
methods(plot)


plot

methods(plot)

summary

grep("data.frame",methods(summary),value=TRUE)

summary(mtcars)

summary.data.frame(mtcars)

# Check to see if they are identical

identical(summary(mtcars), summary.data.frame(mtcars))

mylm <- lm(mpg~wt, data=mtcars)
class(mylm)

methods(plot)

grep("lm",methods(plot),value=T)

mylm <- lm(mpg~wt, data=mtcars)
class(mylm)


plot(mylm)    # Plot finds the right method based on the class of the object

class(23)

x <- matrix(1:16,4,4)
class(x)

class(mtcars)

John <- HomoSapien(name="John",age=33,height=72,weight=170,race="caucasian")

Mary <- HomeSapien(name="Mary",age=22,height=62,weight=132,race="american_indian")


aHomoSapien <- function(name,age,height,weight,race) {
  human <- list(name=name,age=age,height=height,weight=weight,race=race)
  class(human) <- "homosapien"
  return(human)
}

john <- aHomoSapien(name="John",age=33,height=72,weight=190,race="caucasian")
class(john)

john$name

john$age

john$race

john[1:2]   # John is actually a list so we can do this

$age

age <- c(33,35,37,39,41,43,45)
weight <- c(172,178,181,185,192,200,205)

aHomoSapien <- function(name,age,height,weight,race) {
  human <- list(name=name,info=data.frame(age=age,weight=weight),
                height=height,race=race)
  class(human) <- "homosapien"
  return(human)
}

john <- aHomoSapien(name="John",age=age,height=72,
                    weight=weight,race="caucasian")

class(john)

age <- c(33,35,37,39,41,43,45)
weight <- c(118,121,132,119,111,128,132)


mary <- aHomoSapien(name="Mary",age=age,height=62,
weight=weight,race="caucasian")

class(mary)

plot

summary

methods(plot)


plot.homosapien <- function(obj) {
  library(lattice)
  hold <- obj$info
  title <- paste("Weight vs. Age for",obj$name,sep=" ")
  xyplot(weight~age,data=hold,main=title,type=c("l","g"))
}

plot(john)


plot(mary)

summary.homosapien <- function(obj) {
str <- paste(obj$name,"had a mean weight of",
round(mean(obj$info$weight),2),"lbs over a period of",
max(obj$info$age)-min(obj$info$age),"years",sep=" ")
print(str)
}

summary(john)


summary(mary)

download.file("http://steviep42.bitbucket.org/YOUTUBE.DIR/chi_crimes.csv","chi_crimes.csv")
chi <- read.csv("chi_crimes.csv",header=T,sep=",",stringsAsFactors=FALSE)
chi <- chi[complete.cases(chi),]  # Get rid of incomplete cases
library(lubridate)
chi$Date <- parse_date_time(chi$Date,'%m/%d/%Y %I:%M:%S %p')

makecrime <- function(x,y,z) {
  crimes <- list(id=x, source=y, data=z)
  class(crimes) <- "crime"
  return(crimes)
}

chicrimes <- makecrime("Chicago,IL","https://data.cityofchicago.org/",chi)


print.crime <- function(object) {
  header <- paste("CITY,STATE:",object$id,"SOURCE:",object$source,sep=" ")
  cat(header,"\n\n")
  str(object$data,0) 
}

plot.crime <- function(object,...) {
  stopifnot(require(dplyr))
  stopifnot(suppressPackageStartupMessages(require(googleVis)))
  df <- object$data
  df$Date <- as.Date(df$Date,format="%m/%d/%Y") # We need only the day of year
  df %>% group_by(Date) %>% summarize(count=n()) -> dfout
  
  Cal <- gvisCalendar(dfout, datevar="Date", numvar="count",
  options=list(width=900,height=600,
  title="Daily Crime report", height=320,
  calendar="{yearLabel: { fontName: 'Times-Roman',
  fontSize: 32, color: '#1A8763', bold: true},
  cellSize: 13, 
  cellColor: { stroke: 'red', strokeOpacity: 0.2 },
  focusedCellColor: {stroke:'red'}}"))
  plot(Cal)
}

print(chicrimes)


plot(chicrimes)



mylm <- lm(mpg~wt, data=mtcars)
class(mylm)


plot(mylm)    # Plot finds the right method based on the class of the object

getRange <- function(obj,start,end,...) {
UseMethod("getRange")
}


getRange.homosapien <- function(obj,start,end,...) {
 holdf <- obj$info 
 retdf <- holdf[holdf$age >= start & holdf$age <= end,]
 return(retdf)
}

getRange(john,35,41)

                                                                                                              
setClass("homosapien", representation(name="character",
  info="data.frame",
  height="numeric",
  race="character") )

getSlots("homosapien")


age <- c(33,35,37,39,41,43,45)

weight <- c(172,178,181,185,192,200,205)

john <- new("homosapien",name="John",
info=data.frame(age=age,weight=weight),
race="caucasian")
slotNames(john)


john@name


john@info


setMethod(f = "show", signature = "homosapien",
definition = function(object) {
cat("Name: ",object@name,"\n")
print(object@info)
})



john

setMethod(f = "plot", signature = "homosapien", 
definition = function(x,y,...) {
main <- "Weight vs Age"
xlab <- "Age"
ylab <- "Weight in Lbs"
name <- x@name
hold <- x@info
x <- hold$age
y <- hold$weight
main <- paste(main,"for",name,sep=" ")
plot(x,y,main=main,type="l",xlab=xlab,ylab=ylab)
grid()
})


plot(john)



setGeneric("getRange",function(object,start,end) standardGeneric("getRange") )

setMethod("getRange","homosapien",
function(object,start,end) {
holdf <- object@info
retdf <- holdf[holdf$age >= start & holdf$age <= end,]
return(retdf)
})

getRange(john,35,41)

setGeneric("getRange",function(object,start,end) standardGeneric("getRange") )

setMethod("getRange","homosapien",
function(object,start,end) {
holdf <- object@info
retdf <- holdf[holdf$age >= start & holdf$age <= end,]
newhs <- new("homosapien",name="John",
info=retdf,race="caucasian")
return(newhs)
})

plot(getRange(john,35,41))


setClass("homosapien", representation(name="character",
info="data.frame",
height="numeric",
race="character") )

age <- c(33,35,37,39,41,43,45)
weight <- c(172,178,181,185,192,200,205)

john <- new("homosapien",name="John",
info=data.frame(age=age,weight=weight),
race="caucasian")

# That was okay but check this out:

john <- new("homosapien", name=23, 
info=data.frame(age=age,weight=weight),race="caucasian")

Error in validObject(.Object) :
invalid class ``homosapien'' object: 1: invalid object for slot ``name'' in class 
''homosapien'': got class ``numeric'', should be or extend class ''character''


setClass("homosapien", representation(name="character",
info="data.frame",
height="numeric",
race="character") )

setValidity("homosapien",
function(object) {
retval <- TRUE
if (!is.character(object@name)) {
print("Name should be character")
retval <- FALSE
}
if ( length(names(object@info)) != 2 ) {
print("Data frame is not valid")
retval <- FALSE
}
return(retval)
}
)
                                         
                                        