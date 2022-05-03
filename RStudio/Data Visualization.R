#Name : Katlego Mohale


library(ggplot2)
library(readr)

Peng <- read.table('penguins.txt',header=TRUE,sep='\t')
Infl <- read_csv('appendicitis_influenza.csv')

#Question 1

q1a <- ggplot(data=Peng,mapping=aes(x=Duration.min., y=Dive.Heart.Rate)) + 
  geom_point() 

q1b <- ggplot(data=Peng,mapping=aes(x=Duration.min., y=Dive.Heart.Rate, color=Bird)) + 
  geom_point(shape='*',size=5)

q1c <- ggplot(data=Peng,mapping=aes(x=Duration.min., y=Dive.Heart.Rate, color = Bird)) + 
  geom_point(shape='*', size=5) + labs(title='Heart rate vs Depth of penguin dives', y='Heart Rate', x='Depth in meters')

#ggplot object, include everything applicable to entire set of axis, not going to change 
#and global to the graph
#in plot add things specific to that graph/line/bar plot only


#Question 2

q2a <- ggplot(data=Peng,mapping=aes(x=Bird, y=Dive.Heart.Rate)) + 
  geom_bar(stat='summary')

q2b <- ggplot(data=Peng,mapping=aes(x=Bird, y=Dive.Heart.Rate)) + 
  geom_bar(stat='summary', colour='red', fill='Blue', alpha=0.2)

q2c <- ggplot(data=Peng,mapping=aes(x=Bird, y=Dive.Heart.Rate)) + 
  geom_bar(stat='summary', fill='Blue', colour='red', alpha=0.2) +
  labs(title='Spread of Dive heart rates by bird species')


#Question 3

q3a <- ggplot(data=Infl,mapping=aes(x=Year)) +
  geom_line(aes(y=Influenza), colour='Red')+
  geom_line(aes(y=Appendicitis), colour='Blue')

q3b <- ggplot(data=Infl,mapping=aes(x=Year)) +
  geom_area(aes(y=Influenza), colour='Red', fill='Red') +
  geom_area(aes(y=Appendicitis), colour='Blue', fill='Blue', alpha=0.6)


q3c <- ggplot(data=Infl,mapping=aes(x=Year)) +
  geom_area(aes(y=Appendicitis), colour='Blue', fill='Blue', alpha=0.6)+
  geom_area(aes(y=Influenza), colour='Red', fill='Red')+
  labs(title='Number of Influenza and Appendicitis case from 1970 to 2005', y='Number of cases' , x='Year')

