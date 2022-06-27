# Brandon Parmanand
# STA 5104 : Advanced Computer Processing of Statistical Data 
# 
# Clear workspace.
rm(list=ls(all=TRUE))

# Set Working Directory
wdpath <- ("~/GitHub/Parmanand STA5104 Project")
setwd(wdpath)

# Intall need libraries
library(tidyverse) 
library(ggplot2) 
library(GGally)


#Read in Data 
Movies.df <- read_csv("movies.csv")

# Do some initial data exploration such as simple observations for range of
# release years, dimensions, and column names
dim(Movies.df)
range(Movies.df$thtr_rel_year)
t(t(names(Movies.df)))

# Do some correlation outlook using scatter plot matrix
ggpairs(Movies.df[, c('audience_score', 'imdb_rating', 'critics_score',
                      'runtime')])

# Look at correlation between MPAA rating and Audience score
ggplot(Movies.df, aes(x = mpaa_rating, y = audience_score)) + 
  geom_boxplot()


#End Project Proposal Code
###############################################################################


# Additional Code not used

# #plot(Movies.df[, c('audience_score', 'imdb_rating', 'critics_score',
# 'runtime')])
# ggpairs(Movies.df[, c('audience_score', 'imdb_rating', 'critics_score',
# 'runtime')])
# ggplot(Movies.df, aes(x = mpaa_rating, y = audience_score)) + 
#   geom_boxplot()
# ggplot(Movies.df, aes(x = critics_score, y = audience_score))+
#   geom_jitter()
