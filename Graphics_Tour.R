# Get restuarants.db from http://steviep42.bitbucket.org/YOUTUBE.DIR/restaurants.db
sqlite3 restuarants.db
# SQL

select count(score) as 'number_of_inspections' from inspections;

# How many scores were below 70 ?

select count(score) as score70 from inspections where score < 70;

# Okay well WHO has the LOWEST numeric score overall ? 

select name, score from businesses, inspections where 
businesses.business_id = inspections.business_id
order by score limit 15;

# How many violations exist for each restaurant ? 

select name, count(violationid) from businesses, violations
 where businesses.business_id = violations.business_id 
 group by name limit 15;

# Well that wasn't sorted. I want to know the place with the most violations

 select name, count(violationid) as volcnt from businesses, violations
 where businesses.business_id = violations.business_id
 group by name order by volcnt desc limit 15;
 

# Wait a minute. So was it one Starbuck's location that got 327 violations ? 
# Probably not. There are multiple Starbucks. How could we find out ? 
 
# The DISTINCT function in SQL will show us how many distinct business ids there are associated with any business containing ``STARBUCK'' in the name.
 
select count(distinct(business_id)) from businesses
 where name like '%STARBUCK%';
 
 
# So there are 71 Starbucks in the area. The 327 violations are spread over them. 
# But how many per each 
 
 select count(distinct(business_id)) from businesses
 where name like '%STARBUCK%';

 select name, businesses.business_id, count(violationid) from 
 businesses, violations where name like '%STARBUCK%' and 
 businesses.business_id = violations.business_id group by 
 businesses.business_id order by count(violationid) desc limit 10;
 
# Let's determine how many inspections took place between 
# March 27, 2014 and April 10, 2014 ? 

select count(*) from inspections where strftime(date) > 
   strftime('20140327') and strftime(date) < strftime('20140410');
 
 
# Frequently we have missing values in data. It might be blank or have something like "NA". We
# have to be on the lookout for this.
 
select avg(score) from inspections limit 5;

select avg(score) from inspections where score not like '%NA%';
 
select postal_code, avg(score) as mean from businesses, 
 inspections where businesses.business_id = inspections.business_id 
 and score not like '%NA%' group by postal_code order by mean 
 asc limit 5; 
 
 
# Check this out. If you have a n SQLite database such as restaurants.db 
# you can work with it using dplyr commands !
   
 library(dplyr)
 mydb <- src_sqlite(path="restaurants.db")
 mydb
 
 bus <- tbl(mydb,"businesses")
 ins <- tbl(mydb,"inspections")
 vio <- tbl(mydb,"violations")
 
 
# So we can join the inspections with the business ids to find out who 
# some of the worst restaurants are in terms of score


 head(ins)

 head(bus)
 
 # So we can join the inspections with the business ids to find out who some 
 # of the worst restaurants are in terms of score

inner_join(ins,bus) %>% arrange(score) %>% head(.,15) %>% 
  select(score,name,address)
 
# So we can join the violations and business tables to see the breakdown of violations across all Starbucks shops

inner_join(vio,bus) %>% filter(name=="STARBUCKS COFFEE") %>% 
  group_by(business_id) %>% 
  summarize(count=n(violationid)) %>% arrange(desc(count))


# Explanation of inner_join

df1 <- data.frame(id=c(10,20,30,40,50),name=c("Marge","Julio","Giselle","Lisa","Chuck"))
set.seed(123)
df2 <- data.frame(id=c(10,10,30,20,40,10,40,40,10),measure=c(round(rnorm(9,10),2)))

inner_join(df1,df2)

left_join(df1,df2)

### Graphics Section begins Here
 
 data(mtcars)
plot(mpg~wt, data=mtcars)

#  As the weight of the automobile goes up the MPG goes down. It looks to be linear
#  which means pehaps we can then do some regression. 

mylm <- lm(mpg~wt, data=mtcars)
abline(mylm)


str(mtcars)

# This next code segment will show us how many unique values there are in each
# column. mtcars is the most used dataset in education 

mtcars

sapply(mtcars, function(x) length(unique(x)))


# Setup some of the annotation strings for later use

title <- "Car Weight vs. MPG"
xlab <- "Wt in lbs/1,000"
ylab <- "MPG"

# Do the actual plot

plot(mtcars$wt,mtcars$mpg,pch=19, col="blue",main=title,xlab=xlab,ylab=ylab)

grid()  # Draw a grid


title <- "Car Weight vs. MPG"
xlab <- "Wt in lbs/1,000"
ylab <- "MPG"

# First we setup a blank plot - we do everythig EXCEPT put up the points

plot(mtcars$wt, mtcars$mpg, main=title,xlab=xlab,ylab=ylab, type="n")

# Find just the rows where mpg is >= the mean MPG for the whole data set
# Then use the points function to draw them with color blue

aboveavg <- mtcars[mtcars$mpg >= mean(mtcars$mpg),]
points(aboveavg$wt,aboveavg$mpg,col="blue",pch=19)

# Find the rows where mpg is < the mean MPG for the whole data set
# Use points to draw them with color red

belowavg <-  mtcars[mtcars$mpg < mean(mtcars$mpg),]
points(belowavg$wt,belowavg$mpg,col="red",pch=19)

# Draw a line that represents the average MPG
abline(h=mean(mtcars$mpg),lty=2)

# Now we draw a legend to identify what color matches which group

legend("topright",c("Above Avg MPG","Below Avg MPG"),pch=19,col=c("blue","red"))


# Another way to do this is to use the *ifelse* function to create a color label for each 
# row based on the value of mpg for that row

colvec <- ifelse(mtcars$mpg >= mean(mtcars$mpg),"blue","red")
colvec

plot(mtcars$wt, mtcars$mpg, col= colvec, pch=19)
grid()
abline(h=mean(mtcars$mpg),lty=2)


# We can create factors out of continuous quantities such as wt. Let's create 4 
# categories out of the car weight using the quantile function. This is easy. If we label the intervals carefully we can get them to correspond to numbers that are suitable for use with the *cex* option in the plot command which impacts the size of the point being plotted. A cex value of 1 does nothing to the point size. A cex value of less than 1 will shrink the point size. A cex value greater than 1 will increase the size. These are things you don't know until you see them in action.

# Chop the wts up based on where the fall into the buckets
# returned by the quantile function

# We then label them using numbers to be given to the cex parameter
# which controls the size of the points being plotted

sizes <- cut(mtcars$wt,breaks=quantile(mtcars$wt), include.lowest=TRUE,label=c(0.5,1.0,1.5,1.9))

sizes 

plot(mpg~wt, data=mtcars, cex=as.numeric(sizes))


# Chop the wts up based on where the fall into the buckets
# returned by the quantile function

# We then label them using numbers to be given to the cex parameter
# which controls the size of the points being plotted

library(RColorBrewer)

sizes <- cut(mtcars$wt,breaks=quantile(mtcars$wt),
include.lowest=TRUE,label=c(0.5,1.0,1.5,1.9))

# Pick some nice colors because I'm close to being color blind

mycols <- brewer.pal(4,"Set2")

# Okay let's do some tricks here to get the sizes to correspond
# to values 1,2,3,4 so we can index into the mycols vector

idxcolors <- round(as.numeric(sizes))

idxcolors

mycols[idxcolors]

plot(mpg~wt, data=mtcars, cex=as.numeric(sizes), 
     col=mycols[idxcolors],pch=19)

grid()


# Motivated by 
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# Data comes from National Morbidity and Mortality Air Pollution Study (NMMAPS)

nm <- read.csv("http://zevross.com/blog/wp-content/uploads/2014/08/chicago-nmmaps.csv", as.is=TRUE)

# We need to convert the date strings into actual Dates

nm$date <- as.Date(nm$date)

# We pull out the records after 12/31/96

nm <- nm[nm$date > as.Date("1996-12-31"),]

# All we care about is the year in XXXX format

nm$year <- substring(nm$date,1,4)

# So we plot it

plot(temp~date,data=nm,pch=18,cex=0.7,col="maroon")
grid()



# If we want to use different colors for each of the four seasons we have to do this 
# manually. That is, we: 1. have to pick the colors ourslves, setup a null plot
# 2. use the split command to partition the data frame into 4 groups for each of the four 
# seasons. 3. then loop through the four "splits" and use the *points* function to draw 
# the points for a given season while indexing into the color vector. 


library(RColorBrewer)

# Need to make some extra room for the legend

ylim <- c(min(nm$temp)-5,max(nm$temp)+5)

plot(temp~date,data=nm,pch=18,cex=0.7,type="n",ylim=ylim)
mycols <- brewer.pal(4,"Set2")

# Split the nm data frame on season labels

splits <- split(nm,nm$season)

str(splits,1)   # The str() function let's us peek at the structure

# Loop through the splits and use the points function to 
# plot the temperature per season 


for (ii in 1:length(mycols)) {
  points(splits[[ii]]$date,splits[[ii]]$temp,
         col=mycols[ii],pch=18,cex=0.9)
}
title(main="Temperature Across Seasons")
legend("topleft",names(splits),pch=18,col=mycols,horiz=TRUE,cex=0.9)
grid()

## That was a lot of work. Or was it ? 


# Each chart type has it's own function with it's own arguments that influence that particular 
# chart outcome. You have to dig into the help pages to figure out how to do things such as 
# putting text on the chart


r <- hist(nm$temp,col="aquamarine",breaks=12,main="Temperature Degrees F",xlab="Temperature")

text(r$mids, r$density, r$counts, adj = c(.5, -.5), col = "black")


# If we wanted to look at temperatures as "conditioned"" by season we need to use the *par* 
# function to carve out 4 panels. We try to plot such that the axes for all plots are the same 
# to enable better comparisons. 


par(mfrow=c(2,2))  # Get 2 rows by 2 columns

xlab <- "Temperature"
ylim <- c(0,140)
col <- "lightblue"

seasons <- unique(nm$season)
tempstr <- "Temperature in"
for (ii in 1:length(seasons)) {
 title  <- paste(tempstr,seasons[ii],sep=" ")
 hist(nm[nm$season==seasons[ii],]$temp, main=title,xlab=xlab,ylim=ylim,col=col)
}

par(mfrow=c(1,1)) # Reset plot window to 1 row by 1 column


## Other Base chart types

# Let's look at boxplots to see what is necessary to do some comparisons across seasons. 
# It turns out this isn't so bad at least in this case.


title <- "Boxplots Across Seasons"
boxplot(temp~season,data=nm,notch=TRUE,col="lightblue",main=title)


# What if we want to annotate the plot say with the median value ? Note that we don't really
# have to do this because the graph makes it fairly obvious what the median value is. 
# The "trick" is that **boxplot** returns information if we choose to capture it into a varable. 


box.out <- boxplot(temp~season,data=nm,notch=TRUE,col="lightblue",main=title) 

box.out

text(1:4,box.out$stats[4,]+4,box.out$stats[3,])


# So then we figure out how to rotate the plot after studying the help pages


title <- "Boxplots Across Seasons"
boxplot(temp~season,data=nm,notch=TRUE,col="lightblue",main=title,horizontal=TRUE)


# So lot's of times we want to do some plotting of a relationship, (say X/Y plot) such 
# as MPG vs wt (using mtcars as an example) though we want to see this relationship plotted 
# separately for each level of a factor such as cylinder group. Moreover, we want to have each 
# the points from each group (4,6,8) separated by automatic transmission and manual transmission. 
# We want to use a different color for each group. One way to do this is:


# Split the data frame based on cylinder values (4,6, or 8)
unique(mtcars$cyl)

mysplits <- split(mtcars,mtcars$cyl)

maxmpg <- max(mtcars$mpg)   # Find the max MPG value

# Now for each element (dataframe) in mysplits initialize a plot and
# then use the points function to put up the points just for that data frame
# element

for (ii in 1:length(mysplits)) {
  tmpdf  <- mysplits[[ii]]
 
# Separate the automatic and manual transmission records

  auto <- tmpdf[tmpdf$am == 0,]
  man <- tmpdf[tmpdf$am == 1,]

# Setup a blank plot and use the points function to plot the points
# using different colors for auto vs manual

  plot(tmpdf$wt, tmpdf$mpg,type="n",
  main=paste(names(mysplits[ii])," Cylinders"),
  ylim=c(0,maxmpg), xlab="wt",ylab="MPG")
  points(auto$wt,auto$mpg,col="blue",pch=19)
  points(man$wt,man$mpg,col="green",pch=19)
  grid()
  legend("topright", inset=0.05, c("manual","auto"),
         pch = 19, col=c("green","blue"))
}


# But something isn't quite right about this. If we run this from within R-Studio or R 
# Commander we only see the last of the three plots because each plot overwrites the 
# one before it. We want to see all three plots side by side to do some comparisons. We 
# have to use the **par** function to do this. Here is the same example as above yet we 
# just put a call to the par command up top.


par(mfrow=c(1,3))  # set the graphics device to be one row and three columns

# Split the data frame based on cylinder values (4,6, or 8)
unique(mtcars$cyl)

mysplits <- split(mtcars,mtcars$cyl)

maxmpg <- max(mtcars$mpg)   # Find the max MPG value

# Now for each element (dataframe) in mysplits initialize a plot and
# then use the points function to put up the points just for that data frame
# element

for (ii in 1:length(mysplits)) {
  tmpdf  <- mysplits[[ii]]
  
  # Separate the automatic and manual transmission records
  
  auto <- tmpdf[tmpdf$am == 0,]
  man <- tmpdf[tmpdf$am == 1,]
  
  # Setup a blank plot and use the points function to plot the points
  # using different colors for auto vs manual
  
  plot(tmpdf$wt, tmpdf$mpg,type="n",
       main=paste(names(mysplits[ii])," Cylinders"),
       ylim=c(0,maxmpg), xlab="wt",ylab="MPG")
  points(auto$wt,auto$mpg,col="blue",pch=19)
  points(man$wt,man$mpg,col="green",pch=19)
  grid()
  legend("topright", inset=0.05, c("manual","auto"),
         pch = 19, col=c("green","blue"))
}

par(mfrow=c(1,1))  # reset the graphics device to be one row and one column


# LAttice Package

# The lattice package does provide some relief from the tyranny of having to do all this 
# programming. It implements **grouping** and **conditioning/paneling** in an easy-to-use 
# way.  Lets put up our xyplot and have it create a panel for each level of the cylinder factor. 

library(lattice)

xyplot(mpg~wt|factor(cyl),data=mtcars, pch=19, cex=1.3,layout=c(3,1), 
type=c("p","g"),main="MPG vs Wt per Cylinder Group")



# Now let's emulate what we did above with Base graphics. That is let's have the points in each panel be colored according to whether it's corresponding transmission type is automatic or manual. That was a lot of work in Base graphics. With lattice we use the **groups** argument

library(lattice)

xyplot(mpg~wt|factor(cyl), data=mtcars, groups=factor(am), pch=19, 
       cex=1.3,layout=c(3,1),type=c("p","g"),main="MPG vs Wt per Cylinder Group")


# But how do we know which color corresponds to which transmission type ? Add a legend

library(lattice)

xyplot(mpg~wt|factor(cyl), data=mtcars, groups=factor(am), pch=19, 
       cex=1.3,layout=c(3,1),type=c("p","g"),main="MPG vs Wt per Cylinder Group",
       auto.key=TRUE)

# So this is great. Lattice takes care of the panelling and coloring for us so if we 
# are in exploration mode then don't worry about making the colors nicer or the legends 
# perfect. We are just trying to learn about the data. However, if you are in publication 
# mode you need to fix some things. 

# For example the legend, while understandable, doesn't have the same plot character 
# as do the points. Also, we have 0 and 1 instead is automatic and manual respectively. 
# We might also want to have the legend listed horizontally instead of vertically. 


library(lattice)

mtcars$am <- factor(mtcars$am,labels=c("Auto","Manual"))

xyplot(mpg~wt|factor(cyl), data=mtcars, groups=am, pch=19, 
       cex=1.3,layout=c(3,1),type=c("p","g"),main="MPG vs Wt per Cylinder Group",
       auto.key=list(columns=2),par.settings=list(superpose.symbol=list(pch=19)))

# Well okay this is nice though things are starting to get more involved. If we were doing 
# this in Base graphics we would be using separate function calls to do things. Within lattice 
# we try to do everything withing the call to the plot function itself. It's a different 
# paradigm though sometimes the amount of effort put forth in understanding what plot options 
# to use can be confusing. At a minimum you find yourself doing a lot of Googling and/or searching
# through the help pages for a given command.

# In this next example let's pick some different colors for the plot. Here we won't do the 
# conditioning but we will group by cylinder to see the points in different colors 
# corresponding to each level of cylinder (4,6, or 8)

library(lattice)

library(RColorBrewer)   # Not necessary but makes for nice colors

mycols <- brewer.pal(3,"Set2")

xyplot(mpg~wt,data=mtcars,groups=cyl,pch=19,cex=1.3,col=mycols,auto.key=TRUE)


# So now we have to fix the legend again to get the colors to match. Let's also put 
# the legend into the plot area itself.


xyplot(mpg~wt,data=mtcars,groups=cyl,pch=19,cex=1.3,col=mycols,
       auto.key=list(columns=3,corner=c(0.95,0.95),title="Cylinders"), 
       type=c("p","g"),
       par.settings=list(superpose.symbol=list(col=mycols,fill=mycols,pch=19)),
       main="MPG vs Wt")

# Lattice, like Base Graphics, work on the idea that you locate the function of interest 
# to do the plotting. Consequently there are multiple functions corresponding to a given 
# plot top. Thankfully, lattice did NOT name their functions to be the same as the Base 
# Graphics functions else there would have been a real problem. Here are some of the functions 
# as well as their Base graphics equivalent


## Formula interface

# Lattice also uses the concept of a formula when specifying the variables to be plotted. 
# This is useful since R has a number of functions that employ a formual interface:
  

mylm <- lm(mpg ~ wt + am, data=mtcars)    # Linear Regression

xtabs(~am + cyl,mtcars)  # Cross tabulation

aggregate(mpg ~ am + cyl, data=mtcars, mean)  # Aggregate

mtcars$am <- factor(mtcars$am,label=c("Auto","Manual"))

# Example of x~A

bwplot(mpg ~ am, data=mtcars)

# Example of ~x|A

bwplot(~mpg | am, data=mtcars,layout=c(1,2))

histogram(~mpg|am, data=mtcars)

# Example of ~x

histogram(~mpg, data=mtcars)

dotplot(~mpg, data=mtcars)

graph_type | description | example formulas
-----------|-------------|-----------------
  barchart | barchart | x ~ A or A ~ x
bwplot | boxplot | x~A or A~x
dotplot | dotplot | ~x|A
histogram | histogram | ~x
xyplot | scatterplot | y~x|A
stripplot | strip plots | A~x or x~A


# Let's look at the distribution of barley yields as conditioned by location

levels(barley$site)

str(barley)

histogram(~yield|site,data=barley)


## Compliance with dplyr ? 

# Does lattice graphics work with the chaining operators in dplyr ? In fact it does. 
# We just have to remember that the **data** argument in lattice programs needs to be 
# filled in with a period character which will denote the "incoming" data from the 
# chaining or pipe character %>% 
  

mtcars %>% xyplot(mpg~wt,data=.)
mtcars %>% bwplot(~mpg,data=.)
mtcars %>% histogram(~mpg|factor(cyl),data=.)
mtcars %>% group_by(cyl) %>% summarize(avg=mean(mpg)) %>% barchart(avg~cyl,data=.,horiz=F)


## ggplot2 

# Now that we've laid the ground work for R graphics it's time to investigate ggplot2 
# which represents an innovative approach to creating plots. 

### Why ggplot2 ? 

library(ggplot2)

mtcars %>% ggplot(aes(x=wt))   # Noting shows up

# We don't have to commit to a specific geometry or shape

mtcars %>% ggplot(aes(x=wt)) + geom_point(aes(y=mpg))

mtcars %>% ggplot(aes(x=wt)) + geom_histogram()

mtcars %>% ggplot(aes(x=wt)) + geom_histogram() + ggtitle("Histogram for Weight")

mtcars %>% ggplot(aes(x=wt)) + geom_point(aes(y=mpg,color=factor(cyl)))

mtcars %>% ggplot(aes(x=wt)) + geom_line(aes(y=mpg,color=factor(cyl)))

mtcars %>% ggplot(aes(x=wt)) + geom_line(aes(y=mpg,color=factor(cyl))) + 
  xlab("Weight in lbs/1,000") + ylab("Miles per Gallon") + 
  ggtitle("MPG vs Weight")

# note that if we don't use the dplyr form we do something like this:

ggplot(mtcars, aes(x=wt)) + geom_histogram() + ggtitle("Histogram for Weight")

library(ggplot2)

# We establish a relationship between the data and some basic aesthetic
# mappings - the x and y axes

myplot <- ggplot(mtcars, aes(x=wt,y=mpg))

# Now that we've estbalished an aestehtic mapping we can put up layers and try out different
# plots.

myplot + geom_point()    # Just basic points

myplot + geom_point(aes(color=factor(cyl)))   # Similar to lattice "groups"

myplot + geom_point(aes(shape=factor(cyl)))   # Get a different shape for cyl

myplot + geom_point(aes(color = cyl)) + scale_colour_gradient(low = "blue") + geom_smooth(method="lm")

myplot + geom_point( aes(color=factor(cyl), size=qsec))  # Map two new aesthetics

# Notice that we can change our basic ggplot mapping at any time

myplot <- ggplot(data=mtcars, aes(x=mpg))

myplot + geom_histogram(aes(fill=factor(cyl)),binwidth=4)

myplot + geom_dotplot() 

# Let's plot a density of the mpg variable

myplot + geom_density(fill="darkblue")

myplot + geom_density(aes(fill=factor(cyl)))

# Note that we can do groups explicitly

myplot <- ggplot(mtcars,aes(x=wt,y=mpg)) 
myplot  + geom_line(aes(group=factor(cyl),col=factor(cyl)))


# Let's look at the built in iris data for a change of pace. It's also important to know 
# how to do faceting which is the ggplot equivalent of conditioning in Lattice graphics. 
# This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters 
# of the variables sepal length and width and petal length and width, respectively, for 
# 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and 
# virginica.


str(iris)
head(iris)

# Let's illustrate faceting


iris_plot <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))
iris_plot + geom_point(aes(color=Species))

# Note also that we can override the x and y variables specified in the ggplot definition

iris_plot + geom_point(aes(x=Petal.Length,y=Petal.Width))

# Or we could break the comparion between Species into facets

iris_plot + geom_point() + facet_grid(.~Species)

iris_plot + geom_point() + facet_grid(Species~.)

# Of course faceting works indepenednetly of chart type

iris_plot <- ggplot(iris,aes(x=Sepal.Length))
iris_plot + geom_histogram(binwidth=0.25) 

iris_plot + geom_histogram(binwidth=0.25) + facet_grid(.~Species)

# Let's try some box plots

iris_plot <- ggplot(iris, aes(x=Species,y=Sepal.Length)) 

iris_plot + geom_boxplot()

iris_plot + geom_boxplot(aes(fill=Species)) # Not really necessary

iris_plot + geom_boxplot(aes(fill=Species)) + geom_jitter()



# When it comes to annotation and labelling there are a number of ways to do this. You can add labels in layers just as you would plot types and new aesthetics. When you add the legends, titles, axis labels, etc is up to you. Many put up the plot first and then add in the annotation later. 


iris_plot <- ggplot(iris,aes(x=Species,y=Sepal.Length)) 
iris_plot + geom_boxplot() + xlab("Species of Iris") + ylab("Sepal Length in Centimeters") + ggtitle("Species of Iris")

# This can be consolidate if you wish

iris_plot + geom_boxplot() + 
  labs(x="Species of Iris", y="Sepal Length in Cm", title="Species of Iris")



## Diamonds 

Let's look at some more involved data. Refer to the diamonds data frame that comes as part of the ggplot2 package. It's a dataset containing the prices and other attributes of almost 54,000 diamonds. There are 10 variables: 
  
  * price in US Dollars ($326 - $18,823)
* carat weight of the diamond (0.2 - 5.01)
* cut: quality of the cut (Fair, Good, Very Good, Premium, Ideal)
* colour: diamond colour, from J (worst) to D (best)
* clarity: a measurement of how clear the diamond is (I1 (worst), SI1, SI2, VS1, VS2, VVS1, VVS2, IF (best))
* x: length in mm (0–10.74)
* y: width in mm (0–58.9)
* z: depth in mm (0–31.8)
* depth. total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)
* table. width of top of diamond relative to widest point (43–95)




ggplot(diamonds,aes(x=cut,y=price,alpha=0.05)) + geom_boxplot() + coord_flip() + geom_jitter()



# We can use ggplot to plot clusters are returned by the K-Means clustering function in R. 
# The goal of this function is to partition the observations into a predetermined set of 
# clusters such that each observation belongs to one of the clusters. Obviously there will be 
# some misclassification but this helps us identify which observations might be in a group.


library(ggplot2)

df <- mtcars[,c(1,6)]
clus <- kmeans(df,3)
df$cluster <- factor(clus$cluster)
head(df)
centers <- as.data.frame(clus$centers)

iplot <- ggplot(data=df, aes(x=wt,y=mpg,color=cluster))

iplot + geom_point() + 
  geom_point(data=centers, aes(x=wt,y=mpg, color='Center',size=12)) +
  geom_point(data=centers, aes(x=wt,y=mpg,color='Center'),size=52,alpha=.1,show_guide=FALSE)



## Chicago Crime Data

# Let's look at some actual data. This is from the Chicago City Data Portal and relates to 
# crimes reported in the year 2013. You can go to the portal to download new data but for 
# purposes of this exercises just download the dataset from 
# http://steviep42.bitbucket.org/bios545r.orig/DATA.DIR/chi_crimes.csv


# You can download this offline if you wish 
url <- "http://steviep42.bitbucket.org/bios545r.orig/DATA.DIR/chi_crimes.csv"
download.file(url,"chi_crimes.csv")

library(ggplot2)
library(lubridate)   # Makes parting dates easier

# Make sure you refer to the correct donwload folder when reading this in

chi <- read.table("chi_crimes.csv",header=TRUE,sep=",")

chi <- chi[complete.cases(chi),]  # Filter out incomplete cases

# Let's create a factor that tells us if the reported crime was in the night or day

chi$ampm <- ifelse(grepl("PM",chi$Date),"PM","AM") 

# The following will show us how many unique values each column has. This gives us clues
# as to which variables/columns are factors 

sapply(chi, function(x) length(unique(x)))

# Now let's turn the character string dates into real dates

chi$Date <- parse_date_time(chi$Date,'%m/%d/%Y %I:%M:%S %p')
chi$month <- months(chi$Date)
chi$month <- factor(chi$month,
                    levels=c("January","February","March","April","May","June",
                             "July","August","September","October","November",
                             "December"),ordered=TRUE)

# Next we'll create a table that counts how many crimes per month were reported
# We'll turn it into a data frame since ggplot likes to work with data frame over
# tables

callstocops <- as.data.frame(table(chi$month))

# Let's plot the reported crimes per month as a bar chart

p <- ggplot(data=callstocops,aes(x=Var1,y=Freq)) 
p + geom_bar(stat="identity") + ggtitle("Chicago: Reported Crimes per Month 2013")

# Okay that was an interesting plot as it gives us an idea the occurrence of
# reported crime

# Now create a table that tells us how many Calls vs Arrests there were
# for a given month. This will look similar to the previous table except that
# we will use the Arrest variable as a "fill" 

callarrestsdf <- as.data.frame(table(chi$month,chi$Arrest))
names(callarrestsdf) <- c("month","Arrest","Count")

p <- ggplot(data=callarrestsdf,aes(x=month,y=Count,fill=Arrest))
p + geom_bar(stat="identity") + ggtitle("Chicago: Reported Crimes vs. Actual Arrests")



# Next up lets look at the counts for the most frequently committed types of crimes. 
# This will help us understand what our risks are.

categories <- rev(sort(table(chi$Primary.Type)))
catdf <- as.data.frame(categories)
catdf$crimes <- rownames(catdf)

# We need to reorder the dataframe by crime count from highest to lowest. In this
# case we plot the top 20 crime types.

catdf$crimes <- factor(catdf$crimes, levels=names(categories))

p2 <- ggplot(catdf[1:20,],aes(x=crimes,y=categories))
p2 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Let's see how many arrests there were for each category

catarrests <- as.data.frame(table(chi$Primary.Type,chi$Arrest))
names(catarrests) <- c("crime","arrest","count")
catarrests <- catarrests[order(-catarrests$count),]

catarrests$or <- factor(catarrests$crim)
p3 <- ggplot(catarrests,aes(x=crime,y=count,fill=arrest))
p3 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Well what about if we wanted to sort the bars ? This is actually easier in Base graphics 
# but we'll still do it with ggplot. We find something interesting here - that a vast majority 
# of the NARCOTICS crimes end in an arrest ! We also see that Interference with an Officer 
# tends to result in an arrest. Gambling and Prostitution also. 


library(reshape2)
myt <- table(chi$Primary.Type,chi$Arrest) # Count Arrests by Type of Crime Type
myt <- cbind(myt,rowSums(myt))           # Add the sum of (Arrests and Non Arrests) as a column

nydf <- as.data.frame(myt)                 # Create a data frame for ggplot
nydf <- nydf[order(nydf$V3,decreasing=T),] # Order it from highest call count to lowest

# Make a factor out of the crime column

nydf$crimes <- factor(rownames(nydf),levels=rownames(nydf)) 

# We melt down the data frame to make it easier to plot
# Get the top 20 reported crimes

newnydf <- melt(nydf[1:20,],id.vars=c("crimes","V3"))

names(newnydf)[3] <- "Arrest"

ggplot(newnydf,aes(x=crimes,y=value,fill=Arrest)) + geom_bar(stat="identity") + 
theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Here we will look at the crime type as it occurs (allegedly) in the morning or the night. 
# We might suspect that most crime happens at night but let's check it out to see if this is 
# really the case.


library(reshape2)
myt <- table(chi$Primary.Type,chi$ampm) # Count Night/Day by Type of Crime Type
myt <- cbind(myt,rowSums(myt))           

nydf <- as.data.frame(myt)                 # Create a data frame for ggplot
nydf <- nydf[order(nydf$V3,decreasing=T),] # Order it from highest call count to lowest

# Make a factor out of the crime column

nydf$crimes <- factor(rownames(nydf),levels=rownames(nydf)) 

# We melt down the data frame to make it easier to plot
# Get the top 15 reported crimes

newnydf <- melt(nydf[1:15,],id.vars=c("crimes","V3"))

names(newnydf)[3] <- "Arrest"

ggplot(newnydf,aes(x=crimes,y=value,fill=Arrest)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Let's check out crime that happens on the STREET. The Location.Description has a basic 
# description of where the crime occurred.


chi[grepl("STREET",chi$Location.Description),] -> street.crime

library(reshape2)
myt <- table(street.crime$Primary.Type,street.crime$ampm) # Count Arrests by Type of Crime Type
myt <- cbind(myt,rowSums(myt))           # Add the sum of (Arrests and Non Arrests) as a column

nydf <- as.data.frame(myt)                 # Create a data frame for ggplot
nydf <- nydf[order(nydf$V3,decreasing=T),] # Order it from highest call count to lowest

# Make a factor out of the crime column

nydf$crimes <- factor(rownames(nydf),levels=rownames(nydf)) 

# We melt down the data frame to make it easier to plot
# Get the top 15 reported crimes

newnydf <- melt(nydf[1:15,],id.vars=c("crimes","V3"))

names(newnydf)[3] <- "AM_PM"

ggplot(newnydf,aes(x=crimes,y=value,fill=AM_PM)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# What about the nacrotics offense ? 

narc <- chi[chi$Primary.Type=="NARCOTICS",]
narc2 <- as.data.frame(table(narc$Description))

# Reorder the data frame to show Descriptions with the highest number of offenses

narc2$Var1 <- reorder(narc2$Var1,-narc2$Freq)

ggplot(narc2,aes(x=Var1,y=Freq)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Uggh - too many. Let's just look at the top 20 Descriptions

expdf <- narc2[order(narc2$Freq,decreasing=TRUE),][1:15,]
ggplot(expdf,aes(x=Var1,y=Freq)) + geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# So most of the narcotics offenses are for Possesion of Cannabis under 30 grams
# Let's look at the records for possesion of cannabis for over 10 grams

narc[grep("CANNABIS MORE THAN 30GMS",narc$Description),] -> o30gms
nrow(o30gms)

# We have 1077 such incidents. Where did they take place

rev(sort(table(o30gms$Location.Description)))[1:5]

# Looks like most of these happened in the STREET, at a RESIDENCE, the SIDEWALK, APARTMENT

library(googleVis)
o30gms$LatLon <- paste(round(o30gms$Latitude,2),round(o30gms$Longitude,2),sep=":")
o30gms$Tip <- paste(o30gms$Block,"District:",o30gms$District,"Ward:",o30gms$Ward,"<BR>",sep=" ")

nrow(o30gms)

# Let's just isolate those cases that took place in the STREET

narcplot <- gvisMap(o30gms[o30gms$Location.Description=="STREET",],"LatLon","Tip")
plot(narcplot)

# Aside from Google Maps we can use something here called a Calendar Map. Think of it as a 
# heatmap for a calender year(s). Having problems visualizing that ? Check it out. Let's read 
# in the data set again since we've done a lot of transformation to it. 

# Now we have to figure out how many reported crimes there were on each day of the year

chi %>% group_by(Date) %>% summarize(count=n()) -> chiout

Cal <- gvisCalendar(chiout, 
                    datevar="Date", 
                    numvar="count",
                    options=list(width=900,height=600,
                                 title="Daily Crime report",
                                 height=320,
                                 calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 13,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
)
plot(Cal)


