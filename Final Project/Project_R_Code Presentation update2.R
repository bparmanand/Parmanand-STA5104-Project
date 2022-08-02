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

options(
  digits = 2
)

RawMovies.df <- read.csv("movies.csv", stringsAsFactors = T)
RawMovies.df <- na.omit(RawMovies.df)

selected.var <- c(3, 4, 5, 6, 8, 9, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24)
movies <- sample(c(1:619), 619)
Movies.df <- RawMovies.df[movies, selected.var]

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
ggplot(Movies.df, aes(x = mpaa_rating, y = audience_score, fill = mpaa_rating)) +
  geom_boxplot() + xlab("MPAA Rating") + ylab("Audience Score") +
  ggtitle("Audience Score And MPAA Rating") + theme(legend.position="none")


# Best Pic Nom and AUdience Score
ggplot(Movies.df, aes(x = best_pic_nom, y = audience_score, fill=best_pic_nom)) +
  geom_boxplot() + xlab("Best Picture Oscar Nomination") +
  ylab("Audience Score") + ggtitle("Audience Score And Movie Best Picture Oscar")

summary(Movies.df$award_season)


# Clean/normalize data for neural network

Movies.df$award_season <- ifelse(Movies.df$award_season == 'yes',1,0)
Movies.df$summer_season <- ifelse(Movies.df$summer_season == 'yes',1,0)
Movies.df$best_actor_win <- ifelse(Movies.df$best_actor_win == 'yes', 1, 0)
Movies.df$best_actress_win <- ifelse(Movies.df$best_actress_win == 'yes', 1, 0)
Movies.df$best_dir_win <- ifelse(Movies.df$best_dir_win == 'yes', 1, 0)
Movies.df$best_pic_nom <- ifelse(Movies.df$best_pic_nom == 'yes', 1, 0)
Movies.df$audience_rating <- ifelse(Movies.df$audience_rating== 'Spilled', 1, 0)

library(fastDummies)
results <- dummy_cols(Movies.df, select_columns = 'title_type')
results1 <- dummy_cols(results, select_columns = 'genre')
results2 <- dummy_cols(results1, select_columns = 'critics_rating')
results3 <- dummy_cols(results2, select_columns = 'mpaa_rating')

Movies.new.df = subset(results3, select = -c(1,2,4,9))
str(Movies.new.df)

options(
  digits = 2
)

dat <- sapply(Movies.new.df, as.numeric )
dat <- as.data.frame(dat)
# maxs <- apply(dat, 2, max) 
# mins <- apply(dat, 2, min)
# norm.values.1 <- as.data.frame(scale(dat, center = mins, scale = maxs - mins))
norm.values <- preProcess(dat, method="range")
norm.df <- predict(norm.values, dat)
norm.df <- as.data.frame(norm.df)
#install.packages("janitor")
library(janitor)
norm.df <- clean_names(norm.df)
norm.df
# partition
set.seed(1)
train.index <- sample(row.names(norm.df), 0.6*dim(norm.df)[1])
valid.index <- setdiff(row.names(norm.df), train.index)
train.df <- norm.df[train.index, ]
valid.df <- norm.df[valid.index, ]


NN_train = neuralnet(audience_score ~ runtime + thtr_rel_year + thtr_rel_month +
                 imdb_rating + imdb_num_votes + critics_score + audience_rating +
                 best_pic_nom + best_actor_win + best_actress_win + best_dir_win +
                 award_season + summer_season + title_type_documentary +
                 title_type_feature_film + title_type_tv_movie + genre_action_adventure +
                 genre_animation + genre_art_house_international + genre_comedy +
                 genre_documentary + genre_drama + genre_horror + genre_musical_performing_arts +
                 genre_mystery_suspense + genre_other + genre_science_fiction_fantasy +
                 critics_rating_certified_fresh + critics_rating_fresh + critics_rating_rotten +
                 mpaa_rating_g + mpaa_rating_nc_17 + mpaa_rating_pg + mpaa_rating_pg_13 +
                 mpaa_rating_r + mpaa_rating_unrated
                 , data = train.df, hidden = c(5, 5))

plot(NN_train, rep ="best")

NN_valid = neuralnet(audience_score ~ runtime + thtr_rel_year + thtr_rel_month +
                       imdb_rating + imdb_num_votes + critics_score + audience_rating +
                       best_pic_nom + best_actor_win + best_actress_win + best_dir_win +
                       award_season + summer_season + title_type_documentary +
                       title_type_feature_film + title_type_tv_movie + genre_action_adventure +
                       genre_animation + genre_art_house_international + genre_comedy +
                       genre_documentary + genre_drama + genre_horror + genre_musical_performing_arts +
                       genre_mystery_suspense + genre_other + genre_science_fiction_fantasy +
                       critics_rating_certified_fresh + critics_rating_fresh + critics_rating_rotten +
                       mpaa_rating_g + mpaa_rating_nc_17 + mpaa_rating_pg + mpaa_rating_pg_13 +
                       mpaa_rating_r + mpaa_rating_unrated
                     , data = valid.df, hidden = c(5, 5))

plot(NN_valid, rep ="best")

NN_train$weights
prediction(NN_train)



x<- neuralnet::compute(NN_train, train.df)
x<- x$net.result
plot(train.df$audience_score, x*100, col='blue', pch=16, main = "Audience Score: Training Data Prediction to Actual",xlab = "Actual Audience Score",
     ylab = "Predicted Score")

z<- neuralnet::compute(NN_valid, valid.df)
z<- z$net.result
plot(valid.df$audience_score, z*100, col='blue', pch=16, main = "Audience Score: Valid Data Prediction to Actual",xlab = "Actual Audience Score",
     ylab = "Predicted Score")


## Linear Regression Model

Movies.df <- RawMovies.df[movies, selected.var]
# partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(row.names(Movies.df), 0.6*dim(Movies.df)[1])
valid.index <- setdiff(row.names(Movies.df), train.index)
train.df <- Movies.df[train.index, ]
valid.df <- Movies.df[valid.index, ]


# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
Movies.lm <- lm(audience_score ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(Movies.lm)


#### Table 6.6
# use step() to run stepwise regression.
Movies.lm.step <- step(Movies.lm, direction = "backward")
summary(Movies.lm.step)  # Which variables were dropped?
Movies.lm.step.pred <- predict(Movies.lm.step, valid.df)
accuracy(Movies.lm.step.pred, valid.df$audience_score)

options(scipen=999, digits = 0)
some.residuals <- valid.df$audience_score[1:20] - Movies.lm.step.pred[1:20]


pp <- data.frame("Predicted" = Movies.lm.step.pred[1:20], "Actual" = valid.df$audience_score[1:20],
                 "Residual" = some.residuals)

#### Table 6.7
# create model with no predictors
Movies.lm.null <- lm(audience_score~1, data = train.df)
# use step() to run forward regression.
Movies.lm.step <- step(Movies.lm.null, scope=list(lower=Movies.lm.null, upper=Movies.lm), direction = "forward")
summary(Movies.lm.step)  # Which variables were added?
Movies.lm.step.pred <- predict(Movies.lm.step, valid.df)
accuracy(Movies.lm.step.pred, valid.df$audience_score)

options(scipen=999, digits = 0)
some.residuals <- valid.df$audience_score[1:20] - Movies.lm.step.pred[1:20]

qq <- data.frame("Predicted" = Movies.lm.step.pred[1:20], "Actual" = valid.df$audience_score[1:20],
                 "Residual" = some.residuals)

#### Table 6.8
# use step() to run stepwise regression.
Movies.lm.step <- step(Movies.lm, direction = "both")
summary(Movies.lm.step)  # Which variables were dropped/added?
Movies.lm.step.pred <- predict(Movies.lm.step, valid.df)
accuracy(Movies.lm.step.pred, valid.df$audience_score)

rr <- data.frame("Predicted_LM" = Movies.lm.step.pred[1:20], "Actual" = valid.df$audience_score[1:20],
                 "Residual" = some.residuals)


Valid_LM_NN <- data.frame("Predicted_LM" = Movies.lm.step.pred[1:20], "Predicted_NN" = z[1:20]*100, "Actual" = valid.df$audience_score[1:20])
# help <- data.frame("Predicted" = x[1:371]*100,"Actual" = train.df$audience_score[1:371])
# Predicted_NN = NN_valid$net.result[1:20]

library(forecast)
# Movies.lm.step.pred <- predict(Movies.lm, valid.df)
all.residuals <- valid.df$audience_score - Movies.lm.step.pred
length(all.residuals[which(all.residuals > -7 & all.residuals < 7)])/248
hist(all.residuals, breaks = 20, xlab = "Residuals", main = "Histogram of Residuals", xlim = c(-30,30) )


Valid_LM_NN <- data.frame("Predicted_LM" = Movies.lm.step.pred[1:20], "Predicted_NN" = z[1:20]*100, "Actual" = valid.df$audience_score[1:20])

Valid_LM_NN
install.packages("MLmetrics")
library(MLmetrics)
data(cars)
reg <- lm(log(dist) ~ log(speed), data = cars)
RMSPE(y_pred = z*100, y_true = valid.df$audience_score)
#End Project Proposal Code

###############################################################################

