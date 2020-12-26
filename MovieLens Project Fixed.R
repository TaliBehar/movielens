# MovieLens Project # 

## Data Preperation

### 1. installing required packeges

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

### 2. Loading Movielens Dataset

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

#### Get A Glimpse Of Edx Data 
# each row provides a given rating to one movie by specific user
glimpse(edx)

### 4. sanitizing the data - timestamp column
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

### 5. Edx Dataset Overview
# edx data set contains _ different users and _ different movies.
# Hoever, we only gets about 9 million rated movies insted of 746 million. that's because each user 
# rated selected movies by his personal preference. By building well fitted model, we could fill-in 
# these "holes" ....

### summarized counted data 
edx_year_sanitized %>% 
  summarize("Number of users" = n_distinct(userId),
            "Number of movies" = n_distinct(movieId), 
            "Number of ratings" = nrow(edx)) %>%
  comma(,digits = 0)

#the following matrix contain random sample of 120 movies and 120 users TO DO 
# figure 1 # 
users <- sample(unique(edx_year_sanitized$userId), 120)
edx_year_sanitized %>% 
  filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 120)) %>% 
  as.matrix() %>% 
  t(.) %>%
  image(1:120, 1:120,. , xlab="Movies", ylab="Users")
abline(h=0:120+0.5, v=0:120+0.5, col = "whitesmoke")
title("User / Movie Rating Combination")

### Rating Score
#The users average rate score is _ stars while Both median and Mode rate score is the 4 stars (distribution is negatively skewed)
#About _ milion ratings, which is _ percent of the total ratings, are _ stars and only _ percent of the ratings is 3 stars and less.
#In general, whole star ratings are more common than half star ratings, and are 79.5 percent of the total ratings

# rating score - mean, median, mode
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

# Quantity and quality are connected. 50 percent of the ratings are 4 stars and up.

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

### 6. create additional partition of training and test set 

set.seed(167, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(167)`

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

### 7.RMSE function computes the rmse for vectors of ratings and their corresponding predictors
RMSE <- function(true_ratings, predicted_ratings){
        sqrt(mean((true_ratings - predicted_ratings)^2))
} 

####  A. naive model  
# mu_hat is the average rating of all movies across all users
# predict the same ratings for all movies regardless of user 
mu_hat <- mean(train_edx$rating)
mu_hat

# if we predict all unknown ratings with mu we obtain the following RMSE 
naive_rmse <- RMSE(test_edx$rating, mu_hat)
naive_mse <- MSE(test_edx$rating, mu_hat)

# we got a number larger than 1, which means our typical error is larger than one star=not good! 
# the goal is to aspire for RMSE as low as 0.857 

#creating results table with naive approach
naive_model_results <- tibble(method = "Average only",MSE=naive_mse, RMSE = naive_rmse)

### 8. userId 

#### User Activity

#Each of the 69.8K different usesrs rated in average _ different movies.
#the most frequent value of ratings is only _ movies, but there are _ percent of users 
#who rated more than 500 movies! 
#The distribution is positively skewed (describe Weibull or Gamma dist??), 
#as the mean is larger than the mode and median. 
#some users are more active than others at rating movies

# Users Ratings - mean, median, mode, min, max 
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
                     values = c(median = "skyblue1", mean = "pink1",Mode="blue"))+
  scale_x_log10()+
  ggtitle("User Distribution")+ 
  labs(x="Number of Ratings Count", y="Number of Users") 

# graph number of ratings per user, see extreme observation  
# figure 5 # 
rate_p_u<- 
  train_edx %>% 
  group_by(userId) %>%
  summarize(count=n()) %>% 
  ggplot(aes(userId,count)) +
  geom_point(alpha=0.1)+
  geom_hline(aes(yintercept=mean(count)),color="pink1", linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=Mode(count, na.rm = FALSE)),color="blue", linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=median(count)),color="skyblue1", linetype="dashed", size=0.5)+
  ggtitle("Total Number Of Ratings Per user")+ 
  labs(x="userId - 69,878 Unique Users", y="Total Ratings Per User")

# plot 2 graphs - place multiple grobs on a page 
grid.arrange(rate_p_u, user_hist, ncol=2)

####  B. first model - modeling user effect

# previous analysis showed that users are rating different from each other. 
# also, some are very active while others rarely active.  
# Rating score dist. by number of users
mu <- mean(train_edx$rating)

# plot user rating dist.
# figure 6 # 
train_edx %>% 
  group_by(userId) %>% 
  summarise(rating_score=mean(rating)) %>% 
  ggplot(aes(rating_score))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("Rating score dist. by number of users")+ 
  labs(x="Rating score", y="number of users")

# b_u - average rating by user u regardless of movie
fit_user_ave <- 
  train_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

#how much our prediction improves once using y=mu+b_u
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
  ggtitle("")+
  labs(x="b_u", y="number of users")

# plot the user predicred rating
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
  ggtitle("Rating score dist. by number of users")+ 
  labs(x="Rating score", y="number of users")

# display 2 plots together
grid.arrange(user_predicted_ratings,user_b_u, ncol=2)

#####
# model RMSE
model_1_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
# model MSE
model_1_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_1_results <- tibble(method = "User Effect",MSE=model_1_mse, RMSE = model_1_rmse)

# plot the mse
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

#### Regularization #

# choosing penalty terms #
# create additional partition of training and test set for cross validation

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
penalty_lambda_rmse <- c(penalty_term,user_rmses[lambda=penalty_term])

# apply lambda on edx train+test

fit_reg_user_ave <- 
  train_edx %>% 
  group_by(userId) %>% 
  summarize(n_i=n(), 
            reg_b_u=(sum(rating - mu)/(n_i+penalty_term)))

#how much our prediction improves once using y=mu+bi
reg_predicted_ratings <- 
  test_edx %>% 
  left_join(fit_reg_user_ave, by='userId') %>%
  mutate(predicted=mu+reg_b_u) %>%
  pull(predicted)

model_1_1_rmse <- RMSE(true_ratings=test_edx$rating,
                       predicted_ratings=reg_predicted_ratings)
model_1_1_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_1_1_results <- tibble(method = "Reg. User Effect",
                            MSE=model_1_1_mse, RMSE = model_1_1_rmse)
model_1_1_results


### 9. movieId

#### Movie Rating

#In average, each one of the 10.6k different movies get rated _ times.
#the movies most frequently gets only _ ratings, but there are exeptional like "Pulp Fiction" 
#that got rated _ more than average with _ users ratings.
#the distribution is positively skewed, like the user activity dist. (describe Weibull or Gamma dist??) as the mean is larger than the mode and median.

# Movie ratings- mean, median, mode, min, max  
train_edx %>% 
  group_by(movieId) %>%
  summarize(count=n()) %>% 
  summarize(mean=round(mean(count)), 
            median=median(count), 
            mode=Mode(count,na.rm=FALSE), 
            min=min(count), max=max(count)) %>%
  knitr::kable()


# graph number of rating dist. by number of movies 
# figure 10 # 
movie_hist <- 
  train_edx %>% 
  group_by(movieId) %>%
  summarize(count=n()) %>%
  ggplot(aes(count)) +
  geom_histogram(bins = 30, color = "gray20")+ 
  geom_vline(aes(xintercept=median(count),color="median"), linetype="dashed", size=0.5)+
  geom_vline(aes(xintercept=Mode(count, na.rm = FALSE),color="Mode"), linetype="dashed", size=0.5)+
  geom_vline(aes(xintercept=mean(count),color="mean"), linetype="dashed", size=0.5)+
  scale_color_manual(name = "Statistics", values = c(median = "skyblue1", mean = "pink1",Mode="blue"))+
  scale_x_log10()+
  ggtitle("movieId Distribution")+ 
  labs(x="Number Of Ratings Count", y="Number of Movies") 

#graph number of ratings per movie, see extreme observation
# figure 11 #
rate_p_m<- 
  train_edx %>% 
  group_by(title) %>%
  summarize(count=n()) %>%
  ggplot(aes(title,count)) +
  geom_point(alpha=0.1)+
  geom_hline(aes(yintercept=mean(count)),color="pink1", linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=Mode(count, na.rm = FALSE)),color="blue", linetype="dashed", size=0.5)+
  geom_hline(aes(yintercept=median(count)),color="skyblue1", linetype="dashed", size=0.5)+
  ggtitle("Total Number Of Ratings Per Movie")+ 
  labs(x="10,677 Unique Movies", y="Total Ratings Per Movie") + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# plot 2 graphs - place multiple grobs on a page
grid.arrange(rate_p_m, movie_hist, ncol=2)

####  c. second model - modeling movie effect

# We know from experience that some movies are just generally rated higher than others. This
# intuition, that different movies are rated differently, is confirmed by data


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

# b_i - average rating for movie i regardless of user
fit_movie_ave <- 
  train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#how much our prediction improves once using y=mu+bi
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
  ggtitle("")+
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
  ggtitle("Rating score dist. by number of movies")+ 
  labs(x="predicted Rating score", y="number of movies")

# plot 2 graphs together
grid.arrange(movie_b_i ,movie_predicted_ratings, ncol=2)

# model RMSE
model_2_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
# model MSE
model_2_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_2_results <- tibble(method = "Movie Effect",
                          MSE=model_2_mse, RMSE = model_2_rmse)
model_2_results

