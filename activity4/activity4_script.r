#jillian johns


#use built in iris dataset
#take a look at it 
#install.packages(c("ggplot2","dplyr"))
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships

#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

versicolor<-iris[iris$Species=="versicolor", ]
versicolor

x<- c("Sepal.Width","Petal.Length","Sepal.Length")
y<- c("Sepal.Width","Petal.Width","Petal.Length")

lm.out<- list()

for(i in 1:3){
  lm.out[[i]]<- lm(versicolor[,y[i]] ~ versicolor[,x[i]])
}


# always going to be y ~ x left of ~ is dependent, right is independent
#y is a function of (~) x
#lm.out<- lm(versicolor[ ,"Sepal.Width"] ~ versicolor[ ,"Sepal.Length"])
#summary(lm.out)
#lm.out<- lm(versicolor$Sepal.Width ~ versicolor$Sepal.Length) 
#does the same thing as above
#paste is helpful in gettting around assumptions that r wants to make about characters vs numbers

#####################################
##### Part 2: data in dplyr     #####
#####################################


#look at https://ggplot2.tidyverse.org/ for a cheat sheet
#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#iris left
#height right
iris2<-left_join(iris, height, by="Species")
iris2$Petal.Width/iris2$Height.cm

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot

ggplot(iris,aes(Sepal.Length, Sepal.Width)) + 
  geom_point() +
  theme_classic()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines

ggplot(iris,aes(Sepal.Length, Sepal.Width, color=Species)) + 
  geom_point(size=4) +
  theme_classic()

#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		