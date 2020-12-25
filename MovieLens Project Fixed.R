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


