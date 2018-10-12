# Name:Jerfenson Cerda Mejia
# Date:10/11/18

# Run the below only if the library is not already installed.
# 
install.packages("dslabs")
library(dslabs)
library(dplyr)
library(tidyverse)

#Question 1.

##created a varible dat, it uses filter find  Measles and filters out Alaska and Hawaii
dat <- filter(us_contagious_diseases,  state != "Alaska",  state != "Hawaii", disease == "Measles")

## Calculation per100000rate
dat <- mutate(dat, per100000rate = ((count*100000)/population)* (weeks_reporting/52))


#Question 2.

##creates the variable cal and uses filter to find Measles in California only

cal <- filter(us_contagious_diseases, state == "California", disease == "Measles")

##I used the variable dat to create a new variable cal to only store California data. Then used the
##variable cal as data, geom_point to plots the values and geom_vline to create an vertical line in
ggplot(data = cal) + geom_point(mapping = aes(x = year, y = count)) + geom_vline(xintercept = 1965)


  
#Question 3.
'With the transformation of sqr it seems that is has similar variability than without the sqr because the range changes.'
dat_caliFocus <- filter(us_contagious_diseases, state == "California")


##Blocks all years besides 1950, 1960, 1970 for california 
dat_caliFocus$yearBlock[dat_caliFocus$year >=1950 & dat_caliFocus$year > 1960 ] <- "1950's"
dat_caliFocus$yearBlock[dat_caliFocus$year >=1960 & dat_caliFocus$year > 1970 ] <- "1960's"
dat_caliFocus$yearBlock[dat_caliFocus$year >=1970 & dat_caliFocus$year > 1980 ] <- "1970's"

## filters all the Na on the graphs. 
dat_caliFocus <- filter(dat_caliFocus, yearBlock != "NA")



## Creates bar graph without the sqrt tranformation
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

## Creates bar graph with sqrt transformation 
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))



#Question 4.

##Blocks all years besides 1950, 1960, 1970 all the states 
us_contagious_diseases$yearBlock[us_contagious_diseases$year >=1950 & us_contagious_diseases$year > 1960] <-"1950's"
us_contagious_diseases$yearBlock[us_contagious_diseases$year >=1960 & us_contagious_diseases$year > 1970 ] <- "1960's"
us_contagious_diseases$yearBlock[us_contagious_diseases$year >=1970 & us_contagious_diseases$year > 1980 ] <- "1970's"



## filters all the Na on the graphs and creates a variable us_copy that copies all the that from us_contagiuous diseases
us_copy<- filter(us_contagious_diseases, yearBlock != "NA")


## Creates bar graph without the sqrt tranformation
ggplot(data = us_copy) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))


## Creates bar graph with sqrt transformation 
ggplot(data = us_copy ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#The pattern did not hold for other states, not all of them had similar variability throught out the difference decades.



#Question 5.

## Creates bar graph for all the states with geom_tile
ggplot(data = us_copy) + geom_tile(mapping = aes(x = state, y = sqrt(count), color = count)) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#Question 6.
## I was not able to find public data that I was able to use and create a plot for it. But I was able to find some data 
#in the form of a graph that shows the cases of Autism over the year. The image will be on the reflection and report file.






