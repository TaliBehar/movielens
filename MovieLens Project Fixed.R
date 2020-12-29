# MovieLens Project # 

## Data Preperation

## 1. installing required packeges

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(colorspace)) install.packages("colorspace", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(scales)
library(gridExtra)
library(knitr)
library(DescTools) 
library(colorspace)
library(cowplot)
library(formattable)

## 2. Loading Movielens Dataset

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

### 3.Create Edx set (training set) and Validation Sets (final hold-out test set)

#### Use a shortcut to load the data from local computer
load("D:/OneDrive/Documents/Tali/Tali data science studies/movielens_dataset/movielens_dataset_fixed.RData")
####

#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                 col.names = c("userId", "movieId", "rating", "timestamp"))

#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
#colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                          title = as.character(title),
#                                         genres = as.character(genres))

#movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
#set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
#test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
#edx <- movielens[-test_index,]
#temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
#validation <- temp %>% 
#  semi_join(edx, by = "movieId") %>%
#  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
#removed <- anti_join(temp, validation)
#edx <- rbind(edx, removed)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)

## 3. Get A Glimpse Of Edx Data 
# each row provides a given rating to one movie by specific user
glimpse(edx)

## 4. sanitizing the data - timestamp column

# edx data set contains a timestamp column.This date-time is a point on the timeline, stored as the number
# of seconds since 1970-01-01 00:00:00 UTC. 
# convert timestamp into rate date, add rate year column, extract the release year from the movie title, 
# delete release year from movie title

#edx_year_sanitized <- edx %>% 
#  mutate(rate_year = year(as_datetime(timestamp)),
#          rate_date = date(as_datetime(timestamp)),
#          release_year =as.numeric(str_extract(title, "(?<=\\()(\\d{4})(?=\\))")),
#          title= str_remove(as.character(title),"(\\(\\d{4}\\))")) %>% 
#  select(-timestamp)

# Get a glimpse of the sanitized data  
glimpse(edx_year_sanitized)

## 5. Edx Dataset Overview
# edx_year_sanitized' contains about 70k different users that provided ratings 
# and about 11k different rated movies.

## summarized edx_year_sanitized data
edx_year_sanitized %>% 
  summarize("Number of users" = n_distinct(userId),
            "Number of movies" = n_distinct(movieId), 
            "Number of ratings (M)" = nrow(edx)/1000000,
            "Number of 'missing' ratings (M)"=
              ((n_distinct(userId)*n_distinct(movieId))-nrow(edx))/1000000) %>%
  knitr::kable()

# The following matrix contain random sample of 120 movies and 120 users. 

# figure 1 # 
users <- sample(unique(edx_year_sanitized$userId), 120)
edx_year_sanitized %>% 
  filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 120)) %>% 
  as.matrix() %>% 
  t(.) %>%
  image(1:120, 1:120,. , xlab="Movies", ylab="Users")
abline(h=0:120+0.5, v=0:120+0.5, col = "whitesmoke")
title("User / Movie Rating Combination")

## Evaluate the movie - Rating Score
# The average rate score is 3.512 stars while both median and 
# mode rate score is 4 stars

# rating score summarize - mean, median, mode
edx_year_sanitized %>% 
  summarize(mean=mean(rating),
            median=median(rating),
            mode=Mode(rating))%>% 
  knitr::kable()

# graph rating score distribution percentage, distinguish between whole and half star
# figure 2 # 
data.frame(edx_year_sanitized$rating, 
             stars=ifelse(floor(edx_year_sanitized$rating)==edx_year_sanitized$rating,"whole_star","half_star"))%>% 
group_by(edx_year_sanitized.rating, stars) %>% 
summarize(count=n()) %>% 
ggplot(aes(x=edx_year_sanitized.rating,y=(count/sum(count)),fill = stars))+
geom_bar(stat='identity') +
scale_x_continuous(breaks=seq(0.5, 5, by= 0.5)) +
scale_y_continuous(labels=percent)+
scale_fill_manual(values = c("half_star"="lightskyblue", "whole_star"="blue")) +
geom_vline(xintercept=mean(edx_year_sanitized$rating) ,color="black", linetype="dashed", size=0.5)+
geom_vline(xintercept=median(edx_year_sanitized$rating) ,color="red", linetype="dashed", size=0.5)+
labs(x="Stars Rating", y="Ratings Percentage") +
ggtitle("Stars Ratings Percentage")

# Top 15 blockbuster movies
# figure 3 # 
edx_year_sanitized %>% 
  group_by(title) %>%
  summarize(count_k=n()/1000, avg=mean(rating)) %>% # count in thousands
  top_n(15,count_k)%>%
  ggplot(aes(avg,count_k,lable=title)) +
  geom_point()+
  geom_text(aes(label=title), size=3,  hjust=0,vjust=0)+
  xlim(3,5)+
  ggtitle("Top 15 blockbuster movies")+
  labs(x="Rating Score", y="Total Number Of Ratings(Thousands)")

### Analysis And Modeling

## A. Create additional partition of training and test set

set.seed(755, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(755)`

# randomly splitting edx data set into 80% training set and 20% testing set 
test_index <- createDataPartition(y = edx_year_sanitized$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_edx <- edx_year_sanitized %>% slice(-test_index)
temp <- edx_year_sanitized %>% slice(test_index)

# making sure that the test set includes users and movies that appear in the training set.  
test_edx <- temp %>% 
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")
removed <- anti_join(temp,test_edx)
train_edx <- rbind(train_edx, removed)
rm(temp, removed)

## B. RMSE function
# RMSE function computes the rmse for vectors of ratings and their corresponding predictors
RMSE <- function(true_ratings, predicted_ratings){
        sqrt(mean((true_ratings - predicted_ratings)^2))
} 

## C. Naive Model   
# mu_hat is the average rating of all movies across all users
# predict the same ratings for all movies regardless of user 
mu_hat <- mean(train_edx$rating)
mu_hat

# if we predict all unknown ratings with mu we obtain the following RMSE 
naive_rmse <- RMSE(test_edx$rating, mu_hat)
naive_mse <- MSE(test_edx$rating, mu_hat)

#creating results table with naive approach
naive_model_results <- tibble(method = "Average only",MSE=naive_mse, RMSE = naive_rmse)
naive_model_results %>% knitr::kable()
# The RMSE we got is 1.06, which means our typical error is larger than one star, which is not good enough! 
# To remind ourselves, the goal is to aspire for RMSE < 0.8649 


## D. userId 
  
# 1. User Activity

# Each of the 70K different usesrs rated in average 103 different movies. 
# The most frequent value of ratings is only 17 movies, but there are 3% of 
# users who rated more than 500 movies! 

# Users Ratings summarize - mean, median, mode, min, max 
train_edx %>%  
  group_by(userId) %>% 
  summarize(count=n()) %>% 
  summarize(mean=round(mean(count)), 
            median=median(count), 
            mode=Mode(count,na.rm=FALSE), 
            min=min(count), max=max(count)) %>%
  knitr::kable()

# graph number of rating dist. by number of users 
# figure 4 # 
user_hist <- 
  train_edx %>% 
  group_by(userId) %>%
  summarize(count=n()) %>% 
  ggplot(aes(count)) +
  geom_histogram(bins = 30, color = "gray20")+ 
  geom_vline(aes(xintercept=median(count),color="median"), 
                 linetype="dashed", size=0.5)+
  geom_vline(aes(xintercept=Mode(count, na.rm = FALSE),color="Mode"), 
                 linetype="dashed", size=0.5)+
  geom_vline(aes(xintercept=mean(count),color="mean"), 
                 linetype="dashed", size=0.5)+
  scale_color_manual(name = "Statistics", 
                     values = c(median = "blue", mean = "red",Mode="green"))+
  scale_x_log10()+
  ggtitle("User Distribution")+ 
  labs(x="Number of Ratings Count", y="Number of Users") 

# graph number of ratings per user, see extreme observation  
# figure 5 # 
train_edx %>% 
  group_by(userId) %>%
  summarize(count=n()) %>% 
  ggplot(aes(userId,count)) +
  geom_point(alpha=0.1)+
  geom_hline(aes(yintercept=mean(count)),color="red", 
                 linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=Mode(count, na.rm = FALSE)),color="blue",
                linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=median(count)),color="green", 
                 linetype="dashed", size=0.5)+
  ggtitle("Total Number Of Ratings Per user")+ 
  labs(x="userId - 69,878 Unique Users", y="Total Ratings Per User")

# Some users love every movie they watch and simply rate most of them 5 stars, 
# but the left tail inidicate users that are very critic and rate lot of 1-2 stars. 
# plot user rating dist.
# figure 6 # 
train_edx %>% 
  group_by(userId) %>% 
  summarise(rating_score=mean(rating)) %>% 
  ggplot(aes(rating_score))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(train_edx$rating)),color="red",
             linetype="dashed", size=0.5)+
  ggtitle("Rating score dist. by number of users")+ 
  labs(x="Rating score", y="number of users")

## 2. Model 1 - Modeling User Effect

# previous analysis showed that users are rating different from each other. 
# also, some are very active while others rarely active.  

# average rating score
mu <- mean(train_edx$rating)

# b_u - average rating by user u regardless of movie
# Fit the model
fit_user_ave <- 
  train_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

# Predict the rarings - how much our prediction improves once using y=mu+b_u
predicted_ratings <- 
  test_edx %>% 
  left_join(fit_user_ave, by='userId') %>% 
  mutate(predicted=mu+b_u) %>%
  pull(predicted)

# plot the user spesific effect - these estimates very substantially
# figure 7 # 
user_b_u <- 
  fit_user_ave %>%
  ggplot(aes(b_u))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("b_u distribution")+
  labs(x="b_u", y="number of users")

# plot the user predicted rating
# figure 8 # 
user_predicted_ratings <-
  test_edx %>% 
  left_join(fit_user_ave, by='userId') %>% 
  mutate(predicted=mu+b_u)%>% 
  group_by(userId) %>% 
  summarise(predicted=mean(predicted)) %>% 
  ggplot(aes(predicted))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("Rating score dist.")+ 
  labs(x="Rating score", y="number of users")

# display 2 plots together
grid.arrange(user_predicted_ratings,user_b_u, ncol=2)

## Model 1 results
# model RMSE
model_1_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
# model MSE
model_1_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_1_results <- tibble(method = "User Effect",
                          MSE=model_1_mse, RMSE = model_1_rmse)
model_1_results %>% knitr::kable()
# we obtain RMSE = 0.9791, only 8% improvement from the naive model

# The model mean squared errors
# figure 9 #
test_edx %>% 
  left_join(fit_user_ave, by='userId') %>% 
  select(userId,rating,b_u, title) %>% 
  mutate(predicted=b_u+mu,
         se=((rating-predicted)^2)) %>%
  group_by(userId) %>% 
  summarise(mse=mean(se)) %>% 
  ggplot(aes(mse))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(mse)),color="red", linetype="dashed", size=0.5)+
  ggtitle("User Effect model squared errors")+ 
  labs(x="Mean squared errors", y="number of users")

# model 1 squared error summarize
test_edx %>% 
  left_join(fit_user_ave, by='userId') %>% 
  select(userId,rating,b_u) %>% 
  mutate(predicted=b_u+mu,
         squared_errors=(rating-predicted)^2) %>% 
  select(squared_errors) %>% 
  summary()

## 3. Model 1.1 - Modeling Reg. User Effect
  
# Regularization
# lambda is a tuning parameter. we will use cross-validation to choose it. 

# we'll create additional partition of the training for cross validation, 
# pick the lambda and evaluate the performance on the previos train+test.

set.seed(2020, sample.kind="Rounding") 
# if using R 3.5 or earlier, use `set.seed(2020)`

# randomly splitting train set into 90% training set and 10% testing set 
test_index_cv <- 
  createDataPartition(y = train_edx$rating, 
                      times = 1, p = 0.1, list = FALSE)

train_edx_cv <- train_edx %>% slice(-test_index_cv)
temp <- train_edx %>% slice(test_index_cv)

# making sure that the test set includes users and movies that appear in the training set.

test_edx_cv <- 
  temp %>% 
  semi_join(train_edx_cv, by = "movieId") %>%
  semi_join(train_edx_cv, by = "userId")

removed <- anti_join(temp, test_edx_cv)
train_edx_cv <- rbind(train_edx_cv, removed)
rm(temp, removed)

# choosing penalty term (lambda) for user effect
equation_mu <- mean(train_edx_cv$rating)

equation_sum_u <- 
  train_edx_cv %>%
  group_by(userId) %>%
  summarize(n_i=n(), 
            s=sum(rating-equation_mu))

lambdas <- seq(0,7,0.05)

user_rmses <- 
  sapply(lambdas,function(lambda){
    reg_predicted_ratings <- 
      test_edx_cv %>%
      left_join(equation_sum_u, by="userId") %>%
      mutate(reg_b_u=(s/(n_i+lambda)),
             predicted=(equation_mu+reg_b_u)) %>%
      pull(predicted)
    
    return(RMSE(true_ratings=test_edx_cv$rating,
                predicted_ratings=reg_predicted_ratings))
  })

qplot(lambdas,user_rmses)

penalty_term <- lambdas[which.min(user_rmses)]
penalty_lambda_rmse <- c(lambda=penalty_term,
                         cv_rmse=user_rmses[lambda=penalty_term])
penalty_lambda_rmse

# Apply lambda on edx train and test set

fit_reg_user_ave <- 
  train_edx %>% 
  group_by(userId) %>% 
  summarize(n_i=n(), 
            reg_b_u=(sum(rating - mu)/(n_i+penalty_term)))

# Penalized prediction
reg_predicted_ratings <- 
  test_edx %>% 
  left_join(fit_reg_user_ave, by='userId') %>%
  mutate(predicted=mu+reg_b_u) %>%
  pull(predicted)

# Penalized model results 
model_1_1_rmse <- RMSE(true_ratings=test_edx$rating,
                       predicted_ratings=reg_predicted_ratings)
model_1_1_mse <- MSE(test_edx$rating,reg_predicted_ratings)

# add the results to the table 
model_1_1_results <- tibble(method = "Reg. User Effect",
                            MSE=model_1_1_mse, RMSE = model_1_1_rmse)
model_1_1_results %>% knitr::kable()

# Model 1.1 squared error summarize; 
test_edx %>% 
  left_join(fit_reg_user_ave, by='userId') %>% 
  select(userId,rating,reg_b_u) %>% 
  mutate(predicted=reg_b_u+mu,
         reg_squared_errors=(rating-predicted)^2) %>% 
  select(reg_squared_errors) %>% 
  summary()

# we obtain RMSE= 0.9785479, the penalized estimates does not provide much improvement to the RMSE. 
# moving forward by testing the movie specific effect on the predicrion

## E. movieId 
  
# 1. Movie Rating 

# Movie rating summarize - mean, median, mode, min, max  
train_edx %>% 
  group_by(movieId) %>%
  summarize(count=n()) %>% 
  summarize(mean=round(mean(count)), 
            median=median(count), 
            mode=Mode(count,na.rm=FALSE), 
            min=min(count), max=max(count)) %>%
  knitr::kable()

# In average, each one of the 10.6k different movies get rated 674 times.

# graph number of rating dist. by number of movies 
# figure 10 # 
train_edx %>% 
  group_by(movieId) %>%
  summarize(count=n()) %>%
  ggplot(aes(count)) +
  geom_histogram(bins = 30, color = "gray20")+ 
  geom_vline(aes(xintercept=median(count),color="median"), 
                 linetype="dashed", size=0.5)+
  geom_vline(aes(xintercept=Mode(count, na.rm = FALSE),color="Mode"),
                 linetype="dashed", size=0.5)+
  geom_vline(aes(xintercept=mean(count),color="mean"), 
                 linetype="dashed", size=0.5)+
  scale_color_manual(name = "Statistics", 
                     values = c(median = "blue", mean = "red",Mode="green"))+
  scale_x_log10()+
  ggtitle("movieId Distribution")+ 
  labs(x="Number Of Ratings Count", y="Number of Movies") 

#graph number of ratings per movie, see extreme observation
# figure 11 #
train_edx %>% 
  group_by(title) %>%
  summarize(count=n()) %>%
  ggplot(aes(title,count)) +
  geom_point(alpha=0.1)+
  geom_hline(aes(yintercept=mean(count)),color="red", 
                 linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=Mode(count, na.rm = FALSE)),
                 color="green", linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=median(count)),color="blue", 
                 linetype="dashed", size=0.5)+
  ggtitle("Total N. Of Ratings Per Movie")+ 
  labs(x="10,677 Unique Movies", y="Total Ratings Per Movie") + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# movie rating dist quantiles
# 50% of the ratints are between 2.8 to 3.6 stars. very few gets prefect 5 srars
# Respectively very few got the shady 1 star
train_edx %>% 
  group_by(movieId) %>% 
  summarise(rating_score=mean(rating)) %>% summary(rating_score)

# plot movie rating dist.
# figure 12 # 
train_edx %>% 
  group_by(movieId) %>% 
  summarise(rating_score=mean(rating)) %>% 
  ggplot(aes(rating_score))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("Rating score dist. by number of movies")+ 
  labs(x="Rating score", y="number of movies")


## 2. Model 2 - Modeling Movie Effect

# We know from experience that some movies are just generally rated higher than others. This
# intuition, that different movies are rated differently, is confirmed by data

# b_i - average rating for movie i regardless of user
# Fit the model
fit_movie_ave <- 
  train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Predict the rarings - how much our prediction improves once using y=mu+b_i
predicted_ratings <- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  mutate(predicted = mu + b_i) %>% 
  pull(predicted)

# plot the movie spesific effect - these estimates very substantially
# figure 13 # 
movie_b_i <-
  fit_movie_ave %>%
  ggplot(aes(b_i))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("b_i distribution")+
  labs(x="b_i", y="number of movies")

# plot the movie predicted rating
# figure 14 # 
movie_predicted_ratings <-
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>% 
  mutate(predicted=mu+b_i)%>% 
  group_by(movieId) %>% 
  summarise(predicted=mean(predicted)) %>% 
  ggplot(aes(predicted))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("Rating score dist. by n. of movies")+ 
  labs(x="predicted Rating score", y="number of movies")

# plot 2 graphs together
grid.arrange(movie_b_i ,movie_predicted_ratings, ncol=2)

##  Model 2 results
# model RMSE
model_2_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
# model MSE
model_2_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_2_results <- tibble(method = "Movie Effect",
                          MSE=model_2_mse, RMSE = model_2_rmse)
model_2_results %>% knitr::kable()

# We obtain RMSE=0.9439868, improvement of 11% from the naice mosel RMSE

# The model mean squared errors
# figure 15 #
test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>% 
  select(movieId,rating,b_i, title) %>% 
  mutate(predicted=b_i+mu,
         se=((rating-predicted)^2)) %>%
  group_by(movieId) %>% 
  summarise(mse=mean(se)) %>% 
  ggplot(aes(mse))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(mse)),color="red", linetype="dashed", size=0.5)+
  ggtitle("User Effect model squared errors")+ 
  labs(x="Mean squared errors", y="number of movies")

# The model mean squared errors summary; 
test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>% 
  select(movieId,rating,b_i) %>% 
  mutate(predicted=b_i+mu,
         squared_errors=(rating-predicted)^2) %>% 
  select(squared_errors) %>% 
  summary()

## 3. Model 2.1 - Modeling Reg. Movie Effect

# Regularization - Choosing penalty term (lambda) for movie effect

equation_mu <- mean(train_edx_cv$rating)

equation_sum_m <- 
  train_edx_cv %>%
  group_by(movieId) %>%
  summarize(n_i=n(), 
            s=sum(rating-equation_mu))

lambdas <- seq(0,6,0.05)

movie_rmses <- 
  sapply(lambdas,function(lambda){
    reg_predicted_ratings <- 
      test_edx_cv %>%
      left_join(equation_sum_m, by="movieId") %>%
      mutate(reg_b_i=(s/(n_i+lambda)),
             predicted=(equation_mu+reg_b_i)) %>%
      pull(predicted)
    return(RMSE(true_ratings=test_edx_cv$rating,
                predicted_ratings=reg_predicted_ratings))
  })

qplot(lambdas,movie_rmses)

penalty_term <- lambdas[which.min(movie_rmses)]

penalty_lambda_rmse <- c(lambda=penalty_term,
                         cv_rmse=movie_rmses[lambda=penalty_term])
penalty_lambda_rmse

# Apply lambda on edx train and test set
fit_reg_movie_ave <- 
  train_edx %>% 
  group_by(movieId) %>% 
  summarize(n_i=n(), 
            reg_b_i=(sum(rating - mu)/(n_i+penalty_term)))

#  Penalized prediction
reg_predicted_ratings <- 
  test_edx %>% 
  left_join(fit_reg_movie_ave, by='movieId') %>%
  mutate(predicted=mu+reg_b_i) %>%
  pull(predicted)

# Penalize model results
model_2_1_rmse <- RMSE(true_ratings=test_edx$rating,
                       predicted_ratings=reg_predicted_ratings)

model_2_1_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_2_1_results <- tibble(method = "Reg. Movie Effect",
                            MSE=model_2_1_mse, RMSE = model_2_1_rmse)
model_2_1_results %>% knitr::kable()

# The penalized model obtain RMSE= 0.9439218. 
# there is no improvement with the penalty term. 

# reg model se summary
test_edx %>% 
  left_join(fit_reg_movie_ave, by='movieId') %>% 
  select(movieId,rating,reg_b_i) %>% 
  mutate(predicted=reg_b_i+mu,
         reg_squared_errors=(rating-predicted)^2) %>% 
  select(reg_squared_errors) %>% 
  summary()

# comparing between the estimates
# To see how the estimates shrink, we'll plot the regularized estimates 
# vs. least squre estimates 
# figure 16 #
data_frame(original = fit_movie_ave$b_i, 
           regularlized = fit_reg_movie_ave$reg_b_i, 
           n = fit_reg_movie_ave$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) 

## F. movieId and userId
# 1. Model 3 - Modeling Movie + User effect

# b_ui - average rating for movie i with user specific effect
# Fit the model
fit_user_movie_ave <- 
  train_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_ui = mean(rating - mu - b_i))

# Predict the rarings - how much our prediction improves once using y=mu+bi+b_ui
predicted_ratings <- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  left_join(fit_user_movie_ave, by='userId') %>%
  mutate(predicted = mu+b_i+b_ui) %>%
  pull(predicted)

# Model 3 results
model_3_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
# model MSE
model_3_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_3_results <- tibble(method = "Movie + User Effect",
                          MSE=model_3_mse, RMSE = model_3_rmse)
model_3_results %>% knitr::kable()

# The model RMSE= 0.8666408, 18.2% improvement from the naive model. 

# The model mean squared errors 
# figure 17 #
test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  left_join(fit_user_movie_ave, by="userId") %>%
  select(movieId,userId,rating,b_i,b_ui,title) %>% 
  mutate(predicted=mu+b_i+b_ui,
         se=((rating-predicted)^2)) %>%
  group_by(movieId) %>% 
  summarise(mse=mean(se)) %>% 
  ggplot(aes(mse))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(mse)),color="red", linetype="dashed", size=0.5)+
  ggtitle("Movie + User Effect model squared errors")+ 
  labs(x="Mean squared errors", y="number of movies")

# The model squared errors summary 
test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  left_join(fit_user_movie_ave, by="userId") %>%
  select(movieId,userId,rating,b_i,b_ui) %>% 
  mutate(predicted=mu+b_i+b_ui,
         squared_errors=(rating-predicted)^2) %>% 
  select(squared_errors) %>%
  summary()

## 2. Model 3.1 - Modeling Reg. Movie + User effect
  
# Choosing penalty term (lambda) for movie + user effect

lambdas <- seq(0,10,0.25)

equation_mu <- mean(train_edx_cv$rating)

movie_user_rmses <- 
  sapply(lambdas,function(lambda){
    fit_reg_movie_ave <- 
      train_edx_cv %>% 
      group_by(movieId) %>%
      summarize(n_i=n(),
                s= sum(rating - equation_mu),
                reg_b_i=(s/(n_i+lambda)))
    
    fit_reg_user_movie_ave <- 
      train_edx_cv %>%
      left_join(fit_reg_movie_ave, by='movieId') %>%
      group_by(userId) %>%
      summarize(n_i=n(), 
                s= sum(rating -reg_b_i -equation_mu), 
                reg_b_ui=(s/(n_i+lambda)))
    
    reg_predicted_ratings <- 
      test_edx_cv %>% 
      left_join(fit_reg_movie_ave, by='movieId') %>%   
      left_join(fit_reg_user_movie_ave, by='userId') %>%
      mutate(predicted = equation_mu+reg_b_i+reg_b_ui) %>%
      pull(predicted)
    
    return(RMSE(true_ratings=test_edx_cv$rating,
                predicted_ratings=reg_predicted_ratings))
    
  })

qplot(lambdas,movie_user_rmses)

penalty_term <- lambdas[which.min(movie_user_rmses)]
penalty_lambda_rmse <- c(lambde=penalty_term,
                         cv_rmse=movie_user_rmses[lambda=penalty_term])
penalty_lambda_rmse

# apply lambda on edx train and test set
fit_reg_movie_ave <- 
  train_edx %>%
  group_by(movieId) %>%
  summarize(n_i=n(), 
            s= sum(rating - mu),
            reg_b_i=(s/(n_i+penalty_term)))

fit_reg_user_movie_ave <- 
  train_edx %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  group_by(userId) %>% 
  summarize(n_i=n(),
            s= sum(rating -reg_b_i -mu), 
            reg_b_ui=(s/(n_i+penalty_term)))

# Penalized prediction
reg_predicted_ratings <- 
  test_edx %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui) %>%
  pull(predicted)

# Penalized model results
model_3_1_rmse <- RMSE(true_ratings=test_edx$rating,
                       predicted_ratings=reg_predicted_ratings)

model_3_1_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_3_1_results <- tibble(method = "Reg. Movie + User Effect",
                            MSE=model_3_1_mse, RMSE = model_3_1_rmse)
model_3_1_results %>% knitr::kable()

# The penalized model 3.1 obtain RMSE = 0.8659649

# reg squared errors summary
test_edx %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui,
         reg_squared_errors=(rating-predicted)^2) %>% 
  select(reg_squared_errors) %>% 
  summary()


# comparing between the estimates
# To see how the estimates shrink, we'll plot the regularized estimates vs. least squre estimates 
# figure 18 #
data_frame(original = fit_user_movie_ave$b_ui, 
           regularlized = fit_reg_user_movie_ave$reg_b_ui,
           n=fit_reg_user_movie_ave$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) 

## G. Age of the movie at rating
  
# 1. How old was the movie during rating**

# Age of a movie at rating summary - mean, median, mode, min, max  
# The average age of the movies is 12 years. 
train_edx %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  summarize(mean=mean(age_at_rating), 
            median=median(age_at_rating), 
            mode=Mode(age_at_rating,na.rm=FALSE), 
            min=min(age_at_rating), max=max(age_at_rating)) %>% 
  knitr::kable() 

# figure 19 # 
# graph - scoring by age; rate year vs. release year, fill= average rating score
scoring_by_age <- 
  train_edx %>% 
  group_by(title,rate_year) %>% 
  summarize(count=n(), 
            Ave_rating_score=mean(rating),
            rate_year=rate_year[1], 
            release_year=release_year[1]) %>%
  ggplot(aes(x=release_year,y=rate_year,fill=Ave_rating_score)) + 
  geom_tile() + 
  coord_fixed(expand = FALSE)+
  scale_fill_continuous_sequential(palette = "Blues")+
  ggtitle(" Movie Rating score - Release Year Vs. Rate Year")+ 
  labs(x="Movie Release Year", y="Movie Rate Year") 

# figure 20#
# graph - average scoring by age
ave_scoring_by_age <- 
  train_edx %>% 
  group_by(title,rate_year) %>% 
  summarize(count=n(), 
            Ave_rating_score=mean(rating),
            rate_year=rate_year[1], 
            release_year=release_year[1]) %>% 
  group_by(release_year) %>% #mutate(ave=mean(Ave_rating_score)) %>%
  ggplot(aes(x=release_year,y=as.character('mean'),fill=Ave_rating_score)) + 
  geom_tile() + 
  coord_fixed(expand = FALSE)+
  scale_fill_continuous_sequential(palette = "Blues")+
  ggtitle("mean Movie Rating score")+ 
  labs(x="Movie Release Year", y="mean") 

# plot 2 graphs - place multiple grobs on a page
plot_grid(ave_scoring_by_age, scoring_by_age, ncol=1, align="v") # TO DO- share legends 

# The next table show example of movies with more than total of 1000 ratings, 
# that released in 1984.
age_ex._1984 <- 
  train_edx %>% 
  group_by(title, rate_year) %>%
  summarize(count=n(),Ave_rating_score = round(mean(rating),3),
            rate_year=rate_year[1],
            release_year=release_year[1]) %>% 
  filter(release_year=="1984" & count>1000) %>% 
  group_by(rate_year) %>%
  mutate(ave = round(mean(Ave_rating_score),3)) %>% arrange(rate_year)
# display first 10 in a table
age_ex._1984[1:10,] %>% 
  knitr::kable() 

# figure 21 #
# graph average Ratings distribution by the age of the movie
# movies with 5000 ratings and up 
train_edx %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  group_by(age_at_rating) %>%
  summarize(k_count=n()/1000, ave=mean(rating)) %>% filter(k_count>5) %>%
  ggplot(aes(age_at_rating,ave)) +
  geom_line(aes(color = k_count))+
  scale_color_gradient2(low = 'white', mid ='blue' , high = 'red')+
  geom_hline(aes(yintercept=mean(train_edx$rating)),color="blue", linetype="dashed", size=0.5)+
  ggtitle("average Ratings distribution by the age of the movie")+ 
  labs(x="Age of a movie at rating", y="Average rating score")

# some avidence for age effect (the points=movies)
# figure 22 # 
train_edx %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  group_by(title) %>%
  summarize(count = n(), 
            ave_age_at_rating=mean(age_at_rating),
            ave_rating = mean(rating)) %>%
  ggplot(aes(ave_age_at_rating, ave_rating)) +
  geom_point() + 
  geom_smooth()+
  ggtitle("")+ 
  labs(x="ave age at rating", y="Average Rating Score") 

## Classical Hollywood cinema
# There are 1.3k Classical Hollywood cinema movies in our data with average 
# age of 54 years old and average rating of 3.9 stars, 0.4 stara more than the general averge.

# summarize of old movies and their average rating
train_edx %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0 & release_year %in% "1915":"1960") %>%
  summarize(n_movies=n_distinct(movieId),
            ave_rating=mean(rating),
            ave_age=mean(age_at_rating),
            min_age=min(age_at_rating), 
            max_age=max(age_at_rating)) %>%
  knitr::kable()

# figure 23 #
# Classical Hollywood cinema 36-93 years old movies and their average score as shown
train_edx %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0 & release_year %in% "1915":"1960") %>%
  group_by(age_at_rating) %>%
  summarize(count=n(), 
            ave=mean(rating), 
            rating=rating[1]) %>% 
  ggplot(aes(age_at_rating,ave)) +
  geom_point(aes(color = count))+
  geom_hline(aes(yintercept=3.899027),color="blue", linetype="dashed", size=0.5)+
  scale_color_gradient2(low = 'white', mid ='blue' , high = 'red')+
  ggtitle("Classical Hollywood cinema movies 1915-1960")+ 
  labs(x="Age of a movie at rating", y="average rating score") 

## 3. Model 4 - Reg. User and Age of the movie at rating Effect**

# The model will group the age of the movie at rating regardles it title since
# each movie rated several times over the years. 

# Choosing penalty term (lambda) for User and Age effect
 
lambdas <- seq(0,10,0.25)

equation_mu <- mean(train_edx_cv$rating)

user_age_rmses <- 
  sapply(lambdas,function(lambda){
    fit_reg_user_ave <- 
      train_edx_cv %>% 
      mutate(age_at_rating= abs(rate_year-release_year)) %>%
      filter(age_at_rating>=0)%>%
      group_by(userId) %>%
      summarize(n_i=n(),
                s= sum(rating - equation_mu),
                reg_b_u=(s/(n_i+lambda)))
    
    fit_reg_user_age_ave <- 
      train_edx_cv %>%
      mutate(age_at_rating= abs(rate_year-release_year)) %>%
      filter(age_at_rating>=0)%>%
      left_join(fit_reg_user_ave, by='userId') %>%
      group_by(age_at_rating) %>%
      summarize(n_i=n(), 
                s= sum(rating -reg_b_u -equation_mu), 
                reg_b_ua=(s/(n_i+lambda)))
    
    reg_predicted_ratings <- 
      test_edx_cv %>% 
      mutate(age_at_rating= abs(rate_year-release_year)) %>%
      filter(age_at_rating>=0)%>%
      left_join(fit_reg_user_ave, by='userId') %>%   
      left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
      mutate(predicted = equation_mu+reg_b_u+reg_b_ua) %>%
      pull(predicted)
    
    return(RMSE(true_ratings=test_edx_cv$rating,
                predicted_ratings=reg_predicted_ratings))
    
  })

qplot(lambdas,movie_user_rmses)

penalty_term <- lambdas[which.min(user_age_rmses)]
penalty_lambda_rmse <- c(lambda=penalty_term,
                         cv_rmse=user_age_rmses[lambda=penalty_term])
penalty_lambda_rmse

# Apply lambda on edx train and test set
fit_reg_user_ave <- 
  train_edx %>%
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  group_by(userId) %>%
  summarize(n_i=n(), 
            s= sum(rating - mu),
            reg_b_u=(s/(n_i+penalty_term)))

fit_reg_user_age_ave <- 
  train_edx %>%
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  left_join(fit_reg_user_ave, by='userId') %>%
  group_by(age_at_rating) %>% 
  summarize(n_i=n(),
            s= sum(rating -reg_b_u -mu), 
            reg_b_ua=(s/(n_i+penalty_term)))
# Penalized predicted ratings
reg_predicted_ratings <- 
  test_edx %>%
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  left_join(fit_reg_user_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  mutate(predicted = mu+reg_b_u+reg_b_ua) %>%
  pull(predicted)

# Penalized model 4 results
model_4_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=reg_predicted_ratings)

model_4_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_4_results <- tibble(method = "Reg. User + Age of the movie at rating Effect",
                            MSE=model_4_mse, RMSE = model_4_rmse)
model_4_results %>% knitr::kable()


# The penalized model obtain RMSE 0.9712367, improvement of only 8.3 than 
# the naive model and only 1% better than the user effect model. 

#The residual summary as shown; 
test_edx %>%
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  left_join(fit_reg_user_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  mutate(predicted = mu+reg_b_u+reg_b_ua,
         reg_squared_errors=(rating-predicted)^2) %>% 
  select(reg_squared_errors) %>% 
  summary()

# The mean squared residual distribution
# figure 24 #
test_edx %>%
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  left_join(fit_reg_user_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  mutate(predicted = mu+reg_b_u+reg_b_ua,
         se=((rating-predicted)^2)) %>%
  group_by(userId) %>% 
  summarise(mse=mean(se)) %>% 
  ggplot(aes(mse))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(mse)),color="red", linetype="dashed", size=0.5)+
  ggtitle("User + Age at rating model MSE dist.")+ 
  labs(x="Mean squared errors", y="number of users")
 
## H. Time Frame Ranges 
  
# movie release streched over 93 years, while they first started to be rated after 80 years 
tibble(Year = c("Release", "rate"),
       First = c(min(train_edx$release_year),min(train_edx$rate_year)), 
       Last = c(max(train_edx$release_year),max(train_edx$rate_year)),
       "Range in years" = Last-First) %>% 
  knitr::kable()

# 1. Release year

# The number of ratings for each movie against the year the movie came out
# (Each point represent different movie)

# graph number of rating per movie over time
# figure 25 #
train_edx %>% 
  group_by(movieId) %>% 
  summarize(count = n(), 
            year = as.character(first(release_year))) %>% 
  ggplot(aes(year, count))+
  geom_boxplot(aes(group = cut_width(year, 0.2)), outlier.alpha = 0.1)+
  geom_hline(aes(yintercept=mean(count)),color="red", linetype="dashed", size=0.5)+ 
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.7)))+
  ggtitle("number of rating for each movie per release year")+ 
  labs(x="Movie Release Year", y="number of ratings per movie") 

# Newer released movies gets more rated, but the amount of the ratings affects 
# the average score and lower it.
# figure 26 # 
# graph average rating score for each release_year   
train_edx %>% 
  group_by(release_year) %>%
  summarize(k_count=n()/1000,      # count in thousands
            rating_score = mean(rating)) %>%
  ggplot(aes(release_year, rating_score)) +
  geom_point(aes(color=k_count)) + 
  scale_color_gradient2(high = 'black', mid ='gray' ,low = 'white' )+
  geom_smooth()+
  geom_hline(aes(yintercept=mean(edx_year_sanitized$rating)),
             color="red", linetype="dashed", size=0.5)+
  ggtitle("Average Rating Per year")+ 
  labs(x="Release Year", y="Average Rating Score") 

# figure 27 # 
# post-1993 movies ratings per year and their average ratings
# As the rating per year increases, the average rate score increases
train_edx %>% 
  filter(release_year >= 1993) %>%
  group_by(movieId) %>%
  summarize(count = n(), years = 2018 - first(release_year),
            title = title[1],
            ave_rating = mean(rating)) %>%
  mutate(rating_per_year = count/years) %>%
  ggplot(aes(rating_per_year, ave_rating)) +
  geom_point() +
  geom_smooth()+
  ggtitle("post-1993 movies ratings per year and their average ratings")+ 
  labs(x="Rating Per Year", y="Average Rating Score") 

# Here are the top 10 movies with the most ratings per year, 
# along with their average ratings (represent the upper right part of the previous plot)
train_edx %>% 
  filter(release_year >= 1993) %>%
  group_by(movieId) %>%
  summarize(title = title[1], 
            Total_ratings_count = n(), 
            years = 2018 - first(release_year),
            ave_rating = round(mean(rating),2)) %>%
  mutate(rating_per_year =round( Total_ratings_count/years,2)) %>%
  top_n(10, rating_per_year) %>% select(-movieId) %>%
  arrange(desc(rating_per_year, years)) %>%
  knitr::kable()

### Rate year

# The numbers of the movies being rated generaly decreases every year.
# figure 28 # 
# graph Number of movies per each rate year  
rating_count_per_rate_year <- 
  train_edx %>% 
  group_by(movieId) %>%
  summarize(count = n(), 
            rate_year = as.character(first(rate_year))) %>%
  qplot(rate_year, count, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(aes(yintercept=mean(count)),color="red", linetype="dashed", size=0.5)+
  ggtitle("number of movies Per rate year")+ 
  labs(x="Rate Year", y="Movies count") 

# figure 29 # 
# graph average rating for each week 
score_by_week <- 
  train_edx %>% 
  mutate(week_of_rate = round_date(rate_date, unit = "week")) %>%
  group_by(week_of_rate) %>%
  summarize(rating_score = mean(rating)) %>%
  ggplot(aes(week_of_rate, rating_score)) +
  geom_point() +
  geom_smooth()+
  geom_hline(aes(yintercept=mean(rating_score)),color="red", linetype="dashed", size=0.5)+
  ggtitle("Average Rating Per Week")+ 
  labs(x="Week of rate", y="Average Rating Score") 

# plot 2 graphs - place multiple grobs on a page
grid.arrange(rating_count_per_rate_year,score_by_week, ncol=2)

## 5. Model 5 - Reg. Movie and User Effect + Time effect
# fit the model; 
fit_time_ave <-  
  train_edx %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  group_by(week) %>%
  summarize(d_ui=mean(rating-mu-reg_b_i-reg_b_ui))

# predict the ratings; 
predicted_ratings <- 
  test_edx %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui) %>%
  pull(predicted)

# the model results
model_5_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)

model_5_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_5_results <- tibble(method = "Reg. Movie + User Effect + Time effect",
                          MSE=model_5_mse, RMSE = model_5_rmse)
model_5_results %>% knitr::kable()


#The residual summary as shown
test_edx %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui,
         reg_squared_errors=(rating-predicted)^2) %>% 
  select(reg_squared_errors) %>% 
  summary()

# The model MSE distribution;  
# figure 30 #
test_edx %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui,
         se=((rating-predicted)^2)) %>%
  group_by(movieId) %>% 
  summarise(mse=mean(se)) %>% 
  ggplot(aes(mse))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(mse)),color="red", linetype="dashed", size=0.5)+
  ggtitle("User+Movie+Time effect model MSE dist.")+ 
  labs(x="Mean squared errors", y="number of movies")


## I. Genres
  
# 1. Genres Overview 

# example of the diffrent geners
train_edx[20:23,] %>% select (title,genres)

# We'll separat the edx train and test sets rows for unique genres.
# Movie with more than one genre will split into several rows, each row represent a unique genre.
train_edx_genres <- train_edx %>%  separate_rows(genres, sep="\\|")
test_edx_genres <- test_edx %>%  separate_rows(genres, sep="\\|")

# figure 31 # 
# The Distribution of the geners by their total ratings and total average score as follow
train_edx_genres %>% 
  group_by(genres) %>% 
  filter(genres!="(no genres listed)") %>%
  summarize(count = n(),ave_rating=mean(rating)) %>% 
  arrange(desc(count)) %>%
  mutate(percentage=100*count/sum(count)) %>% 
  ggplot(aes(ave_rating,percentage))+
  geom_point()+
  geom_text(aes(label=genres), size=3,  hjust=0,vjust=0)+
  ggtitle("Distribution of movies for each gener")+ 
  labs(x="average rating score", y="percentage of the rating count")

# filter genres with ave score over 3.4 stars and top 20 percent of total ratings
train_edx_genres %>% 
  group_by(genres) %>% 
  filter(genres!="(no genres listed)") %>%
  summarize(count_m = round(n()/1000000,2), #number of ratings in millions
            ave_rating=round(mean(rating),2),
            year=release_year[1]) %>% 
  mutate(percentage=round(100*count_m/sum(count_m)),2) %>% 
  filter(ave_rating>=3.4 & count_m>=quantile(count_m, 0.80)) %>% 
  arrange(desc(percentage)) %>%   
  select(genres,count_m,ave_rating, percentage) %>% 
  knitr::kable()

# We can see that those 4 genres has changed over the years, 
# but in general their ratings respectivly increases until the mid 90's 
# and their average rate score decreases constantly. 

# graph geners over the years
# figure 32 #
genres_rating_over_years <- 
  train_edx_genres %>% 
  group_by(genres,release_year) %>% 
  summarize(count_k=n()/1000,     #number of ratings in thousand
            year=release_year[1]) %>%
  filter(genres %in% c("Drama", "Comedy", "Action","Thriller"),
         release_year>="1960") %>%
  ggplot(aes(x =year, y = count_k)) +
  geom_point(aes(color=genres))+
  geom_smooth()

# score over the years
# figure 33 #
genres_score_over_years <- 
  train_edx_genres %>% 
  group_by(genres,release_year) %>% 
  summarize(ave=mean(rating),
            year=release_year[1]) %>%
  filter(genres %in% c("Drama", "Comedy", "Action", "Thriller"),
         release_year>="1960") %>%
  ggplot(aes(x =year, y = ave)) +
  geom_point(aes(color=genres)) + 
  geom_smooth()

# plot 2 graphs - place multiple grobs on a page
plot_grid(genres_rating_over_years,genres_score_over_years, ncol=1, align="v")

## 2. Model 6 - Reg. Movie + User Effect + Time Effect + Genres Effect 

# Fit the model
fit_genres_ave <-  
  train_edx_genres %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-reg_b_i-reg_b_ui-d_ui))

# Predict the ratings
predicted_ratings <- 
  test_edx_genres %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genres_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g) %>%
  pull(predicted)

# The model 6 results
model_6_rmse <- RMSE(true_ratings=test_edx_genres$rating,
                     predicted_ratings=predicted_ratings)

model_6_mse <- MSE(test_edx_genres$rating,predicted_ratings)

#add the results to the table 
model_6_results <- 
  tibble(method = "Reg. Movie + User Effect + Time Effect + Genres Effect",
         MSE=model_6_mse, RMSE = model_6_rmse)
model_6_results %>% knitr::kable()


# The model mean squared error distribution
# figure 34 #
test_edx_genres %>% mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genres_ave, by='genres') %>% 
  mutate((predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g),
         se=((rating-predicted)^2)) %>% 
  group_by(movieId) %>% 
  summarise(mse=mean(se)) %>% 
  ggplot(aes(mse))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mean(mse)),color="red", linetype="dashed", size=0.5)+
  ggtitle("Model 6 MSE dist.")+ 
  labs(x="Mean squared errors", y="number of movies")

#####################
# test on validation#
#####################

### Models results table
rbind(naive_model_results,
      model_1_results, 
      model_1_1_results,
      model_2_results, 
      model_2_1_results,
      model_3_results, 
      model_3_1_results,
      model_4_results, 
      model_5_results,
      model_6_results) %>%
  arrange(MSE) %>% knitr::kable()


# 1. Data Preparation 
test_validation <- 
  validation %>%  
  mutate (rate_date = date(as_datetime(timestamp)),
          release_year =as.numeric(str_extract(title,"(?<=\\()(\\d{4})(?=\\))")),
          title= str_remove(as.character(title), "(\\(\\d{4}\\))")) %>% 
  select(-timestamp) %>%
  separate_rows(genres, sep="\\|")

##2. Final Model
  
#Fit the model
fit_genre_ave <-  
  train_edx_genres %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-reg_b_i-reg_b_ui-d_ui))

# Predict the ratings 
predicted_ratings <- 
  test_validation %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genres_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g) %>%
  pull(predicted)

 # Final model results
model_final_rmse <- RMSE(true_ratings=test_validation$rating,
                         predicted_ratings=predicted_ratings)
model_final_mse <- MSE(test_validation$rating,predicted_ratings)

#add the results to the table 
model_final_results <- 
  tibble(method = "final",
         MSE=model_final_mse, RMSE = model_final_rmse)
model_final_results %>% knitr::kable()

 
### Models results table
rbind(naive_model_results,
      model_1_results, 
      model_1_1_results,
      model_2_results, 
      model_2_1_results,
      model_3_results, 
      model_3_1_results,
      model_4_results, 
      model_5_results,
      model_6_results, 
      model_final_results) %>%
  arrange(MSE) %>% knitr::kable()



