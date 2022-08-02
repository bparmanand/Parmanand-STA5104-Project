# Brandon Parmanand
# STA 5104 : Advanced Computer Processing of Statistical Data 
 
# Clear workspace.
rm(list=ls(all=TRUE))

# Set Working Directory
wdpath <- ("~/GitHub/Parmanand STA5104 Project")
setwd(wdpath)

# Intall need libraries
library(tidyverse) 
library(ggplot2) 
library(GGally)
library(neuralnet)
library(caret)
library(nnet)
library(rtools)
library(rpart)
library(PreProcess)
library(AppliedPredictiveModeling)


#Read in Data 
Movies.df <- read.csv("movies.csv", stringsAsFactors = T)

# Do some initial data exploration such as simple observations for range of
# release years, dimensions, and column names
str(Movies.df)
dim(Movies.df)
range(Movies.df$thtr_rel_year)
t(t(names(Movies.df)))

head(Movies.df)
summary(Movies.df)
#Histrogram for Release Year

hist(Movies.df$thtr_rel_year, col = "maroon", xlab = "Release Year", 
     main = "Histogram of Movie Theater Release Year")

hist(Movies.df$thtr_rel_month, col = "maroon", xlab = "Release Month", 
     main = "Histogram of Movie Theater Release Month")

# Create Column in df for whether movie was released in November or December
# which is the season for awards
Movies.df <- Movies.df %>% 
  mutate(award_season = as.factor(ifelse(thtr_rel_month %in% c( '11', '12'),
                                         'yes', 'no')))

# Create Column in df for whether movie was released in summer
Movies.df <- Movies.df %>% 
  mutate(summer_season = as.factor(ifelse(thtr_rel_month %in% c('6', '7'), 
                                          'yes', 'no')))

# Audience Score in Summer Season
ggplot(data = Movies.df, aes(x = summer_season, y = audience_score, 
                             fill = summer_season)) + 
  geom_boxplot() + ggtitle('Summer Releases Audience Score') + 
  xlab('Summer Season') + 
  ylab('Audience Score')

# Audience Score against Award Season

ggplot(data = Movies.df, aes(x = award_season, y = audience_score, 
                             fill = award_season)) + 
  geom_boxplot() + ggtitle('Award Season Audience Score') + 
  xlab('Award Season') + 
  ylab('Audience Score')


# Critics Score in Summer Season
ggplot(data = Movies.df, aes(x = summer_season, y = critics_score, 
                             fill = summer_season)) + 
  geom_boxplot() + ggtitle('Summer Releases Critics Score') + 
  xlab('Summer Season') + 
  ylab('Critics Score')

# Critics Score against Award Season

ggplot(data = Movies.df, aes(x = award_season, y = critics_score, 
                             fill = award_season)) + 
  geom_boxplot() + ggtitle('Award Season Critics Score') + 
  xlab('Award Season') + 
  ylab('Critics Score')

# Do some correlation outlook using scatter plot matrix
ggpairs(Movies.df[, c('audience_score', 'imdb_rating', 'critics_score',
                      'runtime')])

# Look at correlation between MPAA rating and Audience score
ggplot(Movies.df, aes(x = mpaa_rating, y = audience_score, fill = mpaa_rating))
+ geom_boxplot()+ 
  xlab("MPAA Rating") + ylab("Audience Score") +
  ggtitle("Audience Score And MPAA Rating") + theme(legend.position="none")


# Best Pic Nom and AUdience Score
ggplot(Movies.df, aes(x = best_pic_nom, y = audience_score, fill=best_pic_nom)) 
+geom_boxplot() + 
  xlab("Best Picture Oscar Nomination") + ylab("Audience Score") +
  ggtitle("Audience Score And Movie Best Picture Oscar")



Movies.df$award_season <- as.numeric(Movies.df$award_season)
Movies.df$summer_season <- as.numeric(Movies.df$summer_season)
Movies.df$genre <- as.numeric(Movies.df$genre)
Movies.df$best_actor_win <- ifelse(Movies.df$best_actor_win == 'yes', 1, 0)


# pcs <- prcomp(na.omit(data.frame(Movies.df$audience_score, 
# Movies.df$critics_score,
#                          Movies.df$award_season, Movies.df$summer_season), 
#               scale. = T))

pcs <- prcomp(na.omit(Movies.df[,c("audience_score", "critics_score", "runtime",
                                   "summer_season","award_season",
                                   "best_actor_win",
                                   "genre")])) 


summary(pcs)



install.packages("factoextra")
library("factoextra") # for PCA Graphs

#Scree Plot
fviz_eig(pcs)

# With Normalization

pcs <- prcomp(na.omit(Movies.df[,c("audience_score", "critics_score", "runtime",
                                   "summer_season","award_season", 
                                   "best_actor_win",
                                   "genre")]), scale. = T) 


summary(pcs)
#Scree Plot
fviz_eig(pcs)

#Individual Graphs
fviz_pca_ind(pcs,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Variables PCA
fviz_pca_var(pcs,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
########################
#Neural Network
str(Movies.df)
Movies.df$title_type <- as.numeric(Movies.df$title_type)
Movies.df$best_actress_win <- ifelse(Movies.df$best_actress_win == 'yes', 1, 0)
Movies.df$critics_rating <- as.numeric(Movies.df$critics_rating)
Movies.df$audience_rating <- as.numeric(Movies.df$audience_rating)
Movies.df$best_director_win <- ifelse(Movies.df$best_director_win == 'yes'
                                      , 1, 0)
Movies.df$mpaa_rating <- as.numeric(Movies.df$mpaa_rating)


Movies.new.df <- data.frame(audience_score = Movies.df$audience_score,
                        title_type = Movies.df$title_type,
                        genre = Movies.df$genre,
                        runtime = Movies.df$runtime,
                        mpaa_rating = Movies.df$mpaa_rating,
                        thtr_rel_year = Movies.df$thtr_rel_year,
                        thtr_rel_month = Movies.df$thtr_rel_month,
                        thtr_rel_day = Movies.df$thtr_rel_day,
                        imdb_rating = Movies.df$imdb_rating,
                        imdb_num_votes = Movies.df$imdb_num_votes,
                        critics_rating = Movies.df$critics_rating,
                        critics_score = Movies.df$critics_score,
                        audience_rating = Movies.df$audience_rating,
                        best_actor_win = Movies.df$best_actor_win,
                        best_actress_win = Movies.df$best_actress_win,
                        award_season = Movies.df$award_season,
                        summer_season = Movies.df$summer_season)
str(Movies.new.df)


# partition
set.seed(1)
train.index <- sample(row.names(Movies.new.df), 0.6*dim(Movies.new.df)[1])
valid.index <- setdiff(row.names(Movies.new.df), train.index)
train.df <- Movies.new.df[train.index, ]
valid.df <- Movies.new.df[valid.index, ]


# normalize
norm.values <- preProcess(train.df, method="range")

train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

train.norm.df[complete.cases(train.norm.df), ]

NN = neuralnet(audience_score ~ title_type + genre + mpaa_rating +
                 thtr_rel_year + thtr_rel_month + thtr_rel_day + imdb_rating +
                 imdb_num_votes + critics_rating + critics_score + 
                 audience_rating 
               + best_actor_win + best_actress_win + award_season
               + summer_season
                 , data = train.norm.df, hidden = 5)

plot(NN, rep ="best")



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
