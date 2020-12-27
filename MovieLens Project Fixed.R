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
            "Number of ratings (M)" = nrow(edx)/1000000,
            "Number of 'missing' ratings (M)"=
              ((n_distinct(userId)*n_distinct(movieId))-nrow(edx))/1000000) %>%
  knitr::kable()

#the following matrix contain random sample of 120 movies and 120 users TO DO 
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
naive_model_results

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

summary(train_edx$rating)[c("1st Qu.","3rd Qu.")]

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
model_1_results

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
penalty_lambda_rmse

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

# plot the mse
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

#### Regularization #
# choosing penalty term (lambda) for movie effect
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

penalty_lambda_rmse <- c(penalty_term,movie_rmses[lambda=penalty_term])
penalty_lambda_rmse

# apply lambda on edx train+test

fit_reg_movie_ave <- 
  train_edx %>% 
  group_by(movieId) %>% 
  summarize(n_i=n(), 
            reg_b_i=(sum(rating - mu)/(n_i+penalty_term)))

#how much our prediction improves once using y=mu+bi
reg_predicted_ratings <- 
  test_edx %>% 
  left_join(fit_reg_movie_ave, by='movieId') %>%
  mutate(predicted=mu+reg_b_i) %>%
  pull(predicted)

model_2_1_rmse <- RMSE(true_ratings=test_edx$rating,
                       predicted_ratings=reg_predicted_ratings)

model_2_1_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_2_1_results <- tibble(method = "Reg. Movie Effect",
                            MSE=model_2_1_mse, RMSE = model_2_1_rmse)
model_2_1_results

#to see hoe the estimates shrink, plot the regularized estimates vs least squre estimates 
# figure 16 #
data_frame(original = fit_movie_ave$b_i, 
           regularlized = fit_reg_movie_ave$reg_b_i, 
           n = fit_reg_movie_ave$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) 

### 10. movieId + userId
####  D. third model - modeling movie + user effect

# b_ui - average rating for movie i with user specific effect
fit_user_movie_ave <- 
  train_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_ui = mean(rating - mu - b_i))

# how much our prediction improves once using y=mu+bi+b_ui
predicted_ratings <- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  left_join(fit_user_movie_ave, by='userId') %>%
  mutate(predicted = mu+b_i+b_ui) %>%
  pull(predicted)

# model RMSE
model_3_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
# model MSE
model_3_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_3_results <- tibble(method = "Movie + User Effect",
                          MSE=model_3_mse, RMSE = model_3_rmse)
model_3_results

# plot the mse
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

#### Regularization #

# choosing penalty terms #

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
penalty_lambda_rmse <- c(penalty_term,movie_user_rmses[lambda=penalty_term])
penalty_lambda_rmse

# apply lambda on edx train+test

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

predicted_ratings <- 
  test_edx %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui) %>%
  pull(predicted)

model_3_1_rmse <- RMSE(true_ratings=test_edx$rating,
                       predicted_ratings=predicted_ratings)

model_3_1_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_3_1_results <- tibble(method = "Reg. Movie + User Effect",
                            MSE=model_3_1_mse, RMSE = model_3_1_rmse)
model_3_1_results

#to see how the estimates shrink, plot the regularized estimates vs least squre estimates 
# figure 18 #
data_frame(original = fit_user_movie_ave$b_ui, 
           regularlized = fit_reg_user_movie_ave$reg_b_ui,
           n=fit_reg_user_movie_ave$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) 

### 11. Age of the movie at rating
#### how old was the movie during rating

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

# 1984 release year example (filter movies with less than 1000 ratings)
age_ex._1984 <- 
  train_edx %>% 
  group_by(title, rate_year) %>%
  summarize(count=n(),Ave_rating_score = mean(rating),
            rate_year=rate_year[1],
            release_year=release_year[1]) %>% 
  filter(release_year=="1984" & count>1000) %>% 
  group_by(rate_year) %>%
  mutate(ave = mean(Ave_rating_score)) %>% arrange(rate_year)
# display first 10 in a table
age_ex._1984[1:10,] %>% 
  knitr::kable() 

# figure  # 
# 1984 release year example
#age_ex._1984 %>% ggplot(aes(x=as.character(release_year),y=rate_year,fill=Ave_rating_score)) + 
#  geom_tile() + 
#  coord_fixed(expand = FALSE)+
#  scale_fill_continuous_sequential(palette = "Blues")+
#  ggtitle("1984 release year example")+ 
#  labs(x="Movie Release Year", y="Movie Rate Year") 

# age of movie at rating - mean, median, mode, min, max  
train_edx %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  summarize(mean=mean(age_at_rating), 
            median=median(age_at_rating), 
            mode=Mode(age_at_rating,na.rm=FALSE), 
            min=min(age_at_rating), max=max(age_at_rating)) %>% 
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

# Classical Hollywood cinema
# Classical Hollywood cinema is a term used in film criticism to describe both a narrative and visual 
# style of filmmaking which became characteristic of American cinema between the 1910s 
# (rapidly after World War I) and the 1960s
# (https://en.wikipedia.org/wiki/Classical_Hollywood_cinema)

# number of movies and their average rating
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
# graph old movies from years 36-93 yeard old
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

#### E. forth model Reg. User + Age of the movie at rating Effect

# the model will group the age of the movie at rating regardles it title
# each movie rated several times over the years. 

# choosing penalty terms #
 
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
penalty_lambda_rmse <- c(penalty_term,user_age_rmses[lambda=penalty_term])
penalty_lambda_rmse

# apply lambda on edx train+test

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

predicted_ratings <- 
  test_edx %>%
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)%>%
  left_join(fit_reg_user_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  mutate(predicted = mu+reg_b_u+reg_b_ua) %>%
  pull(predicted)

model_4_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)

model_4_mse <- MSE(test_edx$rating,reg_predicted_ratings)

#add the results to the table 
model_4_results <- tibble(method = "Reg. User + Age of the movie at rating Effect",
                            MSE=model_4_mse, RMSE = model_4_rmse)
model_4_results


### 12. Time Frame Ranges

# movie release streched over 93 years, while they first started to be rated after 80 years 

tibble(Year = c("Release", "rate"),
       First = c(min(train_edx$release_year),min(train_edx$rate_year)), 
       Last = c(max(train_edx$release_year),max(train_edx$rate_year)),
       "Range in years" = Last-First) %>% 
  knitr::kable()

### Release year

# number of ratings for each movie against the year the movie came out; 
# about _K movies that releasd in 1995 got the higest rating count
# movies that released between 1992-1999 shows above average rating count while starting in 1993 the number of the movies being rated decreases with year
# The more recent movie is, the less time users have had to rate it (change the sentance- took it from notebook)

# graph number of rating per movie over time
# figure 24 #
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

# figure 25 # 
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

# figure 26 # 
# post-1993 movies ratings per year and their average ratings
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

# The top 15 movies with the most ratings per year, along with their average ratings
# (represent the upper right part of the figure 7 graph)
train_edx %>% 
  filter(release_year >= 1993) %>%
  group_by(movieId) %>%
  summarize(title = title[1], Total_ratings_count = n(), years = 2018 - first(release_year),
            ave_rating = mean(rating)) %>%
  mutate(rating_per_year = Total_ratings_count/years) %>%
  top_n(10, rating_per_year) %>% select(-movieId) %>%
  arrange(desc(rating_per_year, years)) %>%
  knitr::kable()

### Rate year

# There is some evidence of a time effect on average rating.

# figure 27 # 
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

# figure 28 # 
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

#### F. Fifth model Reg. Movie + User Effect + Time effect 
 
fit_time_ave <-  
  train_edx %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  group_by(week) %>%
  summarize(d_ui=mean(rating-mu-reg_b_i-reg_b_ui))

predicted_ratings <- 
  test_edx %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui) %>%
  pull(predicted)

model_5_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)

model_5_mse <- MSE(test_edx$rating,predicted_ratings)

#add the results to the table 
model_5_results <- tibble(method = "Reg. Movie + User Effect + Time effect",
                          MSE=model_5_mse, RMSE = model_5_rmse)
model_5_results

### 13. Genres

# separting train + test edx rows for unique genres
train_edx_genres <- train_edx %>%  separate_rows(genres, sep="\\|")
test_edx_genres <- test_edx %>%  separate_rows(genres, sep="\\|")

# figure 29 # 
# Distribution of movie ratings for each gener
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
  summarize(count_m = n()/1000000, #number of ratings in millions
            ave_rating=mean(rating),
            year=release_year[1]) %>% 
  mutate(percentage=100*count_m/sum(count_m)) %>% 
  filter(ave_rating>=3.4 & count_m>=quantile(count_m, 0.80)) %>% 
  arrange(desc(percentage)) %>%   
  select(genres,count_m,ave_rating, percentage) %>% 
  knitr::kable()

# graph geners over the years
# figure 30 #
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
# figure 31 #
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

#### G. Sisth model - Reg. Movie + User Effect + Time Effect + Genres Effect 

fit_genres_ave <-  
  train_edx_genres %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-reg_b_i-reg_b_ui-d_ui))

predicted_ratings <- 
  test_edx_genres %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genres_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g) %>%
  pull(predicted)

model_6_rmse <- RMSE(true_ratings=test_edx_genres$rating,
                     predicted_ratings=predicted_ratings)

model_6_mse <- MSE(test_edx_genres$rating,predicted_ratings)

#add the results to the table 
model_6_results <- 
  tibble(method = "Reg. Movie + User Effect + Time Effect + Genres Effect",
         MSE=model_6_mse, RMSE = model_6_rmse)
model_6_results

# plot the mse
# figure 32 #
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
  ggtitle("7")+ 
  labs(x="Mean squared errors", y="number of movies")

#####################
# test on validation#
#####################

test_validation <- 
  validation %>%  
  mutate (rate_date = date(as_datetime(timestamp)),
          release_year =as.numeric(str_extract(title,"(?<=\\()(\\d{4})(?=\\))")),
          title= str_remove(as.character(title), "(\\(\\d{4}\\))")) %>% 
  select(-timestamp) %>%
  separate_rows(genres, sep="\\|")

fit_genre_ave <-  
  train_edx_genres %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-reg_b_i-reg_b_ui-d_ui))

predicted_ratings <- 
  test_validation %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genres_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g) %>%
  pull(predicted)

model_final_rmse <- RMSE(true_ratings=test_validation$rating,
                         predicted_ratings=predicted_ratings)
model_final_mse <- MSE(test_validation$rating,predicted_ratings)

#add the results to the table 
model_final_results <- 
  tibble(method = "final",
         MSE=model_final_mse, RMSE = model_final_rmse)
model_final_results

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
  arrange(MSE)


