# MovieLens Project # 

# Create Train and Validation Sets

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################
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

### 2. Loading Movielens Dataset

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# TO DO - few words on the movielens data and project
# TO DO- define the project goal 

# edx data set - basic description 
# as first step, befor jumping into data cleaning, data exploration and visualisation, lets take a look 
# of our raw data. This would help us understand the data scope, as well as define the features/predictors that 
# will be used to predict the outcome - rate a specific movie. 

#### Get A Glimpse Of Edx Data 
# each row provides a given rating to one movie by specific user. 

load("D:/OneDrive/Documents/Tali/Tali data science studies/movielens_dataset/movielens_dataset.RData")
load("D:/OneDrive/Documents/Tali/Tali data science studies/movielens_dataset/movielens_dataset_sanitized.RData")
glimpse(edx, width = 60)

### 3. Edx Dataset Overview
# edx data set contains _ different users and _ different movies.
# Hoever, we only gets about 9 million rated movies insted of 746 million. that's because each user 
# rated selected movies by his personal preference. By building well fitted model, we could fill-in 
# these "holes" ....

# summarized counted data 
edx %>% summarize("Number of users" = n_distinct(userId),
                  "Number of movies" = n_distinct(movieId), 
                  "Actual rated movies"= nrow(edx),
                  "Possible rated movies"= n_distinct(userId)*n_distinct(movieId)) %>%
  knitr::kable()
#the following matrix contain random sample of 160 movies and 160 users TO DO 
# figure 1 # 
users <- sample(unique(edx$userId), 160)
edx %>% 
  filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 160)) %>% 
  as.matrix() %>% 
  t(.) %>%
  image(1:160, 1:160,. , xlab="Movies", ylab="Users")
  abline(h=0:160+0.5, v=0:160+0.5, col = "whitesmoke")
  title("User / Movie Rating Combination")

### 4. sanitizing the data - timestamp column
# edx data set contains a timestamp column.This date-time is a point on the timeline, stored as the number
# of seconds since 1970-01-01 00:00:00 UTC. 
# convert timestamp into rate date, add rate year column, extract the release year from the movie title, 
# delete release year from movie title
edx_year_sanitized <- edx %>% 
mutate (rate_year = year(as_datetime(timestamp)),
        rate_date = date(as_datetime(timestamp)),
        release_year =as.numeric(str_extract(title, "(?<=\\()(\\d{4})(?=\\))")),
        title= str_remove(as.character(title), "(\\(\\d{4}\\))")) %>% 
select(-timestamp)
# Get a glimpse of the sanitized data  
glimpse(edx_year_sanitized, width = 60) %>% knitr::kable()

#### userId ####
 
### userId 
#### User Activity
#Each of the 69.8K different usesrs rated in average 129 different movies.
#the most frequent value of ratings is only 20 movies, but there are 4.2 percent of users 
#who rated more than 500 movies! 
#The distribution is positively skewed (describe Weibull or Gamma dist??), 
#as the mean is larger than the mode and median. 
#some users are more active than others at rating movies

# Users Ratings - mean, median, mode, min, max  
edx %>% 
  group_by(userId) %>% 
  summarize(count=n()) %>% 
  summarize(mean=round(mean(count)), 
            median=median(count), 
            mode=Mode(count,na.rm=FALSE), 
            min=min(count), max=max(count)) %>%
  knitr::kable()

# figure 2 # 
# graph number of rating dist. by number of users 
user_hist <- 
edx %>% group_by(userId) %>%
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
rate_p_u<- 
edx %>% group_by(userId) %>%
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

# User Preference ; 
# 1. Age effect - does users tend to rate movie by its age? (hard to answer-old movies genere)
# 2. Genere effect - popularity among users (rating the movie by its genere title) 

### movieId
#### Movie Rating  
#In average, each one of the 10.6k different movies get rated 843 times.
#the movies most frequently gets only 4 ratings, but there are exeptional like "Pulp Fiction" 
#that got rated 37% more than average with 31k users ratings.
#the distribution is positively skewed, like the user activity dist. (describe Weibull or Gamma dist??) as the mean is larger than the mode and median.

# Movie ratings- mean, median, mode, min, max  
edx %>% 
  group_by(movieId) %>%
  summarize(count=n()) %>% 
  summarize(mean=round(mean(count)), 
            median=median(count), 
            mode=Mode(count,na.rm=FALSE), 
            min=min(count), max=max(count)) %>%
  knitr::kable()

# figure 3 # 
# graph number of rating dist. by number of movies 
movie_hist <- 
  edx %>% group_by(movieId) %>%
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
rate_p_m<- 
  edx %>% group_by(title) %>%
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

### Rating Score
#The users average rate score is 3.512 stars while Both median and Mode rate score is the 4 stars (distribution is negatively skewed)
#About 2.5 milion ratings, which is 29 percent of the total ratings, are 4 stars and only 41 percent of the ratings is 3 stars and less.
#In general, whole star ratings are more common than half star ratings, and are 79.5 percent of the total ratings

# rating score - mean, median, mode
edx %>% 
  summarize(mean=mean(rating),
            median=median(rating),
            mode=Mode(rating))%>% 
  knitr::kable()

# figure 4 # 
# graph rating score distribution percentage, distinguish between whole and half star
stars_rating_percentage <- 
  data.frame(edx$rating, 
             stars=ifelse(floor(edx$rating)==edx$rating,"whole_star","half_star"))%>% 
  group_by(edx.rating, stars) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=edx.rating,y=(count/sum(count)),fill = stars))+
  geom_bar(stat='identity') +
  scale_x_continuous(breaks=seq(0.5, 5, by= 0.5)) +
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = c("half_star"="lightskyblue", "whole_star"="blue")) +
  geom_vline(xintercept=mean(edx$rating) ,color="black", linetype="dashed", size=0.5)+
  geom_vline(xintercept=median(edx$rating) ,color="red", linetype="dashed", size=0.5)+
  labs(x="Stars Rating", y="Ratings Percentage") +
  ggtitle("Stars Ratings Percentage")
stars_rating_percentage
# Quantity and quality are connected. 50 percent of the ratings are 4 stars and up.

# figure 5 # 
# Top 15 blockbuster movies
edx_year_sanitized %>% 
  group_by(title) %>%
  summarize(count=n()/1000, avg=mean(rating)) %>% 
  top_n(15,count)%>%
  ggplot(aes(avg,count,lable=title)) +
  geom_point()+
  geom_text(aes(label=title), size=3,  hjust=0,vjust=0)+
  xlim(3,5)+
  ggtitle("Top 15 blockbuster movies")+
  labs(x="Rating Score", y="Total Number Of Ratings(Thousands)")

### Time Frame Ranges

# movie release streched over 93 years, while they first started to be rated after 80 years 
tibble(Year = c("release", "rate"),
       First = c(min(edx_year_sanitized$release_year),min(edx_year_sanitized$rate_year)), 
       Last = c(max(edx_year_sanitized$release_year),max(edx_year_sanitized$rate_year)),
       "Range in years" = Last-First) %>% 
knitr::kable()

# release year #

# number of ratings for each movie against the year the movie came out; 
# about _K movies that releasd in 1995 got the higest rating count
# movies that released between 1992-1999 shows above average rating count while starting in 1993 the number of the movies being rated decreases with year
# The more recent movie is, the less time users have had to rate it (change the sentance- took it from notebook)

# figure 6 # (year + rating)
edx_year_sanitized %>% 
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

# figure 7 # (year + score)
# graph average rating score for each release_year   
score_by_year <- 
  edx_year_sanitized %>% 
  group_by(release_year) %>%
  summarize(k_count=n()/1000,
            rating_score = mean(rating)) %>%
  ggplot(aes(release_year, rating_score)) +
  geom_point(aes(color=k_count)) + 
  scale_color_gradient2(high = 'black', mid ='gray' ,low = 'white' )+
  geom_smooth()+
  geom_hline(aes(yintercept=mean(edx_year_sanitized$rating)),
                 color="red", linetype="dashed", size=0.5)+
  ggtitle("Average Rating Per year")+ 
  labs(x="Release Year", y="Average Rating Score") 

# figure 7 leads to age effect

# figure 8 # (year + rating + score)
# post-1993 movies ratings per year and their average ratings
edx_year_sanitized %>% 
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
edx_year_sanitized %>% 
  filter(release_year >= 1993) %>%
  group_by(movieId) %>%
  summarize(title = title[1], Total_ratings_count = n(), years = 2018 - first(release_year),
            ave_rating = mean(rating)) %>%
  mutate(rating_per_year = Total_ratings_count/years) %>%
  top_n(15, rating_per_year) %>% select(-movieId) %>%
  arrange(desc(rating_per_year, years))

# rate year #
# There is some evidence of a time effect on average rating.
# figure 9 # 
# graph Number of movies per each rate year  
rating_count_per_rate_year <- 
  edx_year_sanitized %>% 
  group_by(movieId) %>%
  summarize(count = n(), 
            rate_year = as.character(first(rate_year))) %>%
  qplot(rate_year, count, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(aes(yintercept=mean(count)),color="red", linetype="dashed", size=0.5)+
  ggtitle("number of movies Per rate year")+ 
  labs(x="Rate Year", y="Movies count") 

# figure 10 # (week + score)
# graph average rating for each week 
score_by_week <- 
  edx_year_sanitized %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  group_by(week) %>%
  summarize(rating_score = mean(rating)) %>%
  ggplot(aes(week, rating_score)) +
  geom_point() +
  geom_smooth()+
  geom_hline(aes(yintercept=mean(rating_score)),color="red", linetype="dashed", size=0.5)+
  ggtitle("Average Rating Per Week")+ 
  labs(x="Week", y="Average Rating Score") 

# plot 2 graphs - place multiple grobs on a page
grid.arrange(rating_count_per_rate_year,score_by_week, ncol=2)

# age # 
## how old was the movie during rating

# figure 11 # 
# graph - scoring by age; rate year vs. release year, fill= average rating score
scoring_by_age <- 
  edx_year_sanitized %>% 
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
  
# graph - average scoring by age
ave_scoring_by_age <- 
  edx_year_sanitized %>% 
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
  
#share legends
#grid.arrange(idmean+theme(legend.position='hidden'), id+theme(legend.position='bottom'), nrow=2) 
#align plots
# plot 2 graphs - place multiple grobs on a page
plot_grid(ave_scoring_by_age, scoring_by_age, ncol=1, align="v")


# 1984 release year example (filter movies with less than 1000 ratings)
age_ex._1984 <- edx_year_sanitized %>% 
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

# figure 12 # 
# 1984 release year example
age_ex._1984 %>% ggplot(aes(x=as.character(release_year),y=rate_year,fill=Ave_rating_score)) + 
      geom_tile() + 
      coord_fixed(expand = FALSE)+
      scale_fill_continuous_sequential(palette = "Blues")+
      ggtitle("1984 release year example")+ 
      labs(x="Movie Release Year", y="Movie Rate Year") 

# creating column age of movie at rating
edx_year_age_sanitized <- 
  edx_year_sanitized %>% 
  mutate(age_at_rating= abs(rate_year-release_year)) %>%
  filter(age_at_rating>=0)

# age of movie at rating - mean, median, mode, min, max  
edx_year_age_sanitized %>% 
  summarize(mean=mean(age_at_rating), 
            median=median(age_at_rating), 
            mode=Mode(age_at_rating,na.rm=FALSE), 
            min=min(age_at_rating), max=max(age_at_rating)) %>% 
  knitr::kable() 

# figure 13 #
# graph Number of Ratings distribution by the age of the movie
# movies with 5000 ratings and up 
ratings_by_age<- 
  edx_year_age_sanitized %>% 
  group_by(age_at_rating) %>%
  summarize(k_count=n()/1000) %>% filter(k_count>5) %>%
  ggplot(aes(age_at_rating,k_count)) +
  geom_line(aes(color = k_count))+
  scale_color_gradient2(low = 'white', mid ='blue' , high = 'red')+
  geom_vline(aes(xintercept=mean(edx_year_age_sanitized$age_at_rating)),color="blue", linetype="dashed", size=0.5)+
  ggtitle("Number of Ratings distribution by the age of the movie")+ 
  labs(x="Age of a movie at rating", y="Total number of ratings(Thousands)") 

# figure 14 #
# graph average Ratings distribution by the age of the movie
# movies with 5000 ratings and up 
score_by_age<- 
  edx_year_age_sanitized %>% 
  group_by(age_at_rating) %>%
  summarize(k_count=n()/1000, ave=mean(rating)) %>% filter(k_count>5) %>%
  ggplot(aes(age_at_rating,ave)) +
  geom_line(aes(color = k_count))+
  scale_color_gradient2(low = 'white', mid ='blue' , high = 'red')+
  geom_hline(aes(yintercept=mean(edx_year_age_sanitized$rating)),color="blue", linetype="dashed", size=0.5)+
  ggtitle("average Ratings distribution by the age of the movie")+ 
  labs(x="Age of a movie at rating", y="Average rating score") 

# plot 2 graphs - place multiple grobs on a page
plot_grid(ratings_by_age, score_by_age, ncol=1)  

# figure 15 # # some avidence for age effect (the points=movies)
edx_year_age_sanitized %>% 
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
classical_age <- 
  edx_year_age_sanitized %>% 
  filter(release_year %in% "1915":"1960")
# number of movies and their average rating
classical_age %>% 
  summarize(n_movies=n_distinct(movieId), 
            ave_age=mean(rating),
            min_age=min(age_at_rating), 
            max_age=max(age_at_rating)) %>%
  knitr::kable()
# figure 16 #
# graph old movies from years 36-93 yeard old
classical_age %>% 
  group_by(age_at_rating) %>%
  summarize(count=n(), 
            ave=mean(rating), 
            rating=rating[1]) %>% 
  ggplot(aes(age_at_rating,ave)) +
  geom_point(aes(color = count))+
  geom_hline(aes(yintercept=mean(classical_age$rating)),color="blue", linetype="dashed", size=0.5)+
  scale_color_gradient2(low = 'white', mid ='blue' , high = 'red')+
  ggtitle("Classical Hollywood cinema movies 1915-1960")+ 
  labs(x="Age of a movie at rating", y="average rating score") 

# The apogee of the studio system may have been the year 1939 (from Wiki)
#classical_age_1939 <- edx_year_age_sanitized %>% filter(release_year =="1939")

# figure #
# graph the year of 1939 movies with 500 ratings and up
#classical_age_1939 %>% group_by(title) %>%
  summarize(count=n(), ave=mean(rating), ave_age_at_rating=mean(age_at_rating)) %>% 
  filter(count>500) %>%
  ggplot(aes(ave_age_at_rating,ave)) +
  geom_point(aes(color = count))+
  geom_hline(aes(yintercept=mean(classical_age_1939$rating)),color="blue", linetype="dashed", size=0.5)+
  scale_color_gradient2(low = 'white', mid ='blue' , high = 'red')+
  geom_text(aes(label=title), size=3,  hjust=0,vjust=0)+
  ggtitle("1939 movies")+ 
  labs(x="Average Age of a movie at rating", y="average rating score") 

### Genres ###  
#separting rows for unique genres
edx_genres <- 
  edx_year_sanitized %>% 
  separate_rows(genres, sep="\\|")

# figure 17 #
# Distribution of movie ratings for each gener
edx_genres %>% 
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
edx_genres %>% 
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

# figure 18 #
# graph geners over the years
genres_rating_over_years <- 
  edx_genres %>% 
  group_by(genres,release_year) %>% 
  summarize(count_k=n()/1000,     #number of ratings in thousand
            year=release_year[1]) %>%
  filter(genres %in% c("Drama", "Comedy", "Action","Thriller"),
         release_year>="1960") %>%
  ggplot(aes(x =year, y = count_k)) +
  geom_point(aes(color=genres))+
  geom_smooth()

genres_score_over_years <- 
  edx_genres %>% 
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

# figure 19 #
#error bar (from the homework)- evidance of genre effect
# edx_genres  %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))







# In depth exploration of the above plot will help me to answer the next
# Questions regarding the stars rating dist. ;
# Time effect - rating score influenced by the age of the movie
# Gener effect - 
#    1. Quantity - are different geners get rated more then others?
#    2. Quality - are different geners score higher then others?

# More people watch popular movies, but what makes them so popular? 
# The number of ratings per movie can be explained by multiple factors. 
# edx dataset allows us to explore the following ;  
# 1. Time effect - the older the movie is, the more time to get rated
# 2. Genere effect - from which gener the movie comes from, how popular the gener is
# (3. Quality - what is the average rate score of a movie )
# Combination of all 
  
###

# percentage of rated movies against the year the movie was rated; 
# (no trend?)

#assumption- as the movie is older, users had more time to rate it.  
#Although the MovieLence database was last updated on 9/2018, when calculating the age of the movie 
# my point of reference will be the year of 2009 when the last rate has occured.
# adding 2 columns of movie age and the rating period 
  
#######################################################################################  
# start working on RMSE #

# rename the train dataset for easyer writing
edx_sanitized <- edx_year_age_sanitized

# create additional partition of training and test set 
set.seed(755, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(755)`
# randomly splitting edx data set into 80% training set and 20% testing set 
test_index <- createDataPartition(y = edx_sanitized$rating, times = 1,
                                    p = 0.2, list = FALSE)
train_edx <- edx_sanitized %>% slice(-test_index)
temp <- edx_sanitized %>% slice(test_index)

# making sure that the test set includes users and movies that appear in the training set.  
test_edx <- temp %>% 
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")
removed <- anti_join(temp,test_edx)
train_edx <- rbind(train_edx, removed)
rm(temp, removed)

# RMSE function computes the rmse for vectors of ratings and their corresponding predictors
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
} 

###  naive model  
# mu_hat is the average rating of all movies across all users
# predict the same ratings for all movies regardless of user 
mu_hat <- mean(train_edx$rating)
mu_hat

# if we predict all unknown ratings with mu we obtain the following RMSE 
naive_rmse <- RMSE(test_edx$rating, mu_hat)
naive_rmse
# we got a number larger than 1, which means our typical error is larger than one star=not good! 
# the goal is to aspire for RMSE as low as 0.857 

#creating results table with naive approach
rmse_results <- tibble(method = "Average only", RMSE = naive_rmse)

## first model- modeling user effect

# previous analysis showed that users are rating different from each other. 
# also, some are very active while others rarely active.  
# Rating score dist. by number of users
mu <- mean(train_edx$rating)
mu

# b_u - average rating by user u regardless of movie

fit_user_ave <- train_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

user_rating_score <-
  train_edx %>% 
  group_by(userId) %>% 
  summarise(rating_score=mean(rating)) %>% 
  ggplot(aes(rating_score))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("Rating score dist. by number of users")+ 
  labs(x="Rating score", y="number of users")

#these estimates very substantially
user_b_u <- 
  fit_user_ave %>%
  ggplot(aes(b_u))+
  geom_histogram(bins=30, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("")+
  labs(x="b_u", y="number of users")

grid.arrange(user_rating_score,user_b_u, ncol=2)

#how much our prediction improves once using y=mu+b_u
predicted_ratings <- test_edx %>% 
                     left_join(fit_user_ave, by='userId') %>% 
                     mutate(predicted=mu+b_u) %>%
                     pull(predicted)

model_1_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)

#add the results to the table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="User Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

## modeling user effect - check + visualization ##
# check if the predicted_rating is true

check_predicted_u<- 
  test_edx %>% 
  left_join(fit_user_ave, by='userId') %>% 
  select(userId,rating,b_u, title) %>% 
  mutate(predicted=b_u+mu,
         residual=rating-predicted, 
         abs_res=abs(residual))

check_predicted_u %>% mutate(se=((rating-predicted)^2)) %>%summarize(mse=mean(se), rmse=sqrt(mse))

RMSE(check_predicted_u$predicted, test_edx$rating)


# list of our 10 biggest "mistakes"(the reason for small improve in RMSE)

check_predicted_u %>% 
  arrange(desc(abs_res)) %>% 
  select(title, abs_res)%>% 
  distinct() %>%
  slice(1:10) %>% 
  knitr::kable()

# graph subset from the data; user_ave ratings vs. predicted 

user_ave_vs_predicted<- 
  check_predicted_u[20000:50000,] %>% 
  group_by(userId) %>% 
  summarize(user_ave=mean(rating), 
            predicted=predicted[1])%>%
  ggplot(aes(x=userId,y=user_ave)) +
  geom_point( color="black")+
  geom_point(aes(y = predicted), shape = 1)+
  geom_hline(aes(yintercept=mean(train_edx$rating)),
             color="red", linetype="dashed", size=0.5)

# graph the residuals + rmse line (extream erorr in red)

user_residual <- 
  check_predicted_u %>% 
  group_by(userId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(residual=residual[1],
            mse=mean(se), 
            rmse=sqrt(mse)) %>%
  ggplot(aes(x=userId,y=residual,
             color=ifelse(residual>=(-3)& residual<=3,"blue", "red"))) +
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.979),color="blue", linetype="dashed", size=0.5)

# graph the se + mse line (extream erorr in red) 

user_se <-
  check_predicted_u %>% 
  group_by(userId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(se=se[1],mse=mean(se)) %>%
  ggplot(aes(x=userId,y=se,
             color=ifelse(se>=(10),"red", "blue"))) +
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.958715),color="blue", 
             linetype="dashed", size=0.5)

# 2 plots together
grid.arrange(user_residual,user_se, ncol=2)

# Regularization #

# choosing penalty terms #

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

#add the results to the table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Reg. User Effects Model",  
                                     RMSE = model_1_1_rmse ))
rmse_results %>% knitr::kable()

# no improvement!!!!! #



# second model - modeling movie effect

# We know from experience that some movies are just generally rated higher than others. This
# intuition, that different movies are rated differently, is confirmed by data

fit_movie_ave <- 
  train_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

aa<-
  train_edx %>% 
  group_by(movieId) %>% 
  summarise(rating_score=mean(rating)) %>% 
  ggplot(aes(rating_score))+
  geom_histogram(bins=10, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("Rating score dist. by number of movies")+ 
  labs(x="Rating score", y="number of movies")
bb<-
  fit_movie_ave %>%
  ggplot(aes(b_i))+
  geom_histogram(bins=10, color="black")+
  geom_vline(aes(xintercept=mu),color="red", linetype="dashed", size=0.5)+
  ggtitle("")+
  labs(x="b_i", y="number of movies")
#these estimates very substantially

# plot 2 graphs together
grid.arrange(aa,bb, ncol=2)

#how much our prediction improves once using y=mu+bi
predicted_ratings <- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  mutate(predicted = mu + b_i) %>% 
  pull(predicted)

model_2_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)
#add the results to the table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


## modeling movie effect - check + visualization ##
# check if the predicted_rating is true
check_predicted_m<- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>% 
  select(movieId,rating,b_i,title) %>% 
  mutate(predicted=b_i+mu,
         residual=rating-predicted, abs_res=abs(residual))

check_predicted_m %>% mutate(se=((rating-predicted)^2))%>%
  summarize(mse=mean(se), rmse=sqrt(mse))

RMSE(check_predicted_m$predicted, test_edx$rating)

#graph subset from the data; movie_ave ratings vs. predicted 
movie_ave_vs_predicted<- 
  check_predicted_m[50000:80000,] %>% 
  group_by(title) %>% 
  summarize(movie_ave=mean(rating), predicted=predicted[1])%>%
  ggplot(aes(x=title,y=movie_ave)) +
  geom_point( color="black")+
  geom_point(aes(y = predicted), shape = 1)+
  geom_hline(aes(yintercept=mean(train_edx$rating)),color="red",
             linetype="dashed", size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
movie_ave_vs_predicted

# graph the residuals + rmse line (extream erorr in red)
movie_residual <- 
  check_predicted_m %>% 
  group_by(movieId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(residual=residual[1],
            mse=mean(se), 
            rmse=sqrt(mse)) %>%
  ggplot(aes(x=as.character(movieId),y=residual,
             color=ifelse(residual>=(-3)& residual<=3,"blue", "red"))) +
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.9439868),color="blue", 
             linetype="dashed",size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# graph the se + mse line (extream erorr in red) 
movie_se <-
  check_predicted_m %>% 
  group_by(movieId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(se=se[1],
            mse=mean(se)) %>%
  ggplot(aes(x=as.character(movieId),y=se,
             color=ifelse(se>=(10),"red", "blue"))) +
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.8911112),color="blue", 
             linetype="dashed", size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

#plot together
grid.arrange(movie_residual,movie_se, ncol=2)

# list of our biggest "mistakes"(the reason for small improve in RMSE)
check_predicted_m %>%
  mutate(se=((rating-predicted)^2))%>%
  arrange(desc(se)) %>%
  select(title,se)%>% 
  distinct() %>%
  slice(1:10) %>%
  pull(title)

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
#add the results to the table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Reg. Movie Effects Model",  
                                     RMSE = model_2_1_rmse ))
rmse_results %>% knitr::kable()

#to see hoe the estimates shrink, plot the regularized estimates vs least squre estimates 
data_frame(original = fit_movie_ave$b_i, 
           regularlized = fit_reg_movie_ave$reg_b_i, 
           n = fit_reg_movie_ave$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5) 

check_predicted_m_reg<- 
  test_edx %>% 
  left_join(fit_reg_movie_ave, by='movieId') %>%    
  select(movieId,rating,reg_b_i,title) %>% 
  mutate(predicted=reg_b_i+mu, 
         residual=rating-predicted, 
         abs_res=abs(residual))
RMSE(check_predicted_m_reg$predicted, test_edx$rating)

## regularization motivation # 
# create a database that connects "movieId" to movie title
titles <- 
  edx_sanitized %>%
  select(title, movieId) %>%
  distinct()

#best and worst movies befor regularization
best_movies <- 
  fit_movie_ave %>%
  left_join(titles, by="movieId") %>%
  group_by(title)%>%
  summarize(n_ratings=n(),
            b_i=b_i[1])%>% 
  arrange(desc(b_i)) %>%
  slice(1:10)%>%
  select("Movie title"=title,n_ratings)
best_movies

worst_movies <- 
  fit_movie_ave %>%
  left_join(titles, by="movieId") %>%
  group_by(title)%>%
  summarize(n_ratings=n(),
            b_i=b_i[1])%>% 
  arrange(b_i) %>%
  slice(1:10)%>%
  select("Movie title"=title,n_ratings) 
worst_movies   
#best and worst movies after regularization
best_movies_reg <- 
  fit_reg_movie_ave %>%
  left_join(titles, by="movieId") %>%
  group_by(title)%>%
  summarize(n_ratings=n(),
            reg_b_i=reg_b_i[1])%>% 
  arrange(desc(reg_b_i)) %>%
  slice(1:10)%>%
  select("Movie title"=title,n_ratings)
best_movies_reg

worst_movies_reg <- 
  fit_reg_movie_ave%>%
  left_join(titles, by="movieId") %>%
  group_by(title)%>%
  summarize(n_ratings=n(),
            reg_b_i=reg_b_i[1])%>% 
  arrange(reg_b_i) %>%
  slice(1:10)%>%
  select("Movie title"=title,n_ratings) 
worst_movies_reg 
# no improvement!!!!! #


# third model - modeling movie + user effect

fit_user_movie_ave <- 
  train_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_ui = mean(rating - mu - b_i))

predicted_ratings <- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  left_join(fit_user_movie_ave, by='userId') %>%
  mutate(predicted = mu+b_i+b_ui) %>%
  pull(predicted)

model_3_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

## modeling movie + user effect - check + visualization ##
# check if the predicted_rating is true
check_predicted_m_u<- 
  test_edx %>% 
  left_join(fit_movie_ave, by='movieId') %>%
  left_join(fit_user_movie_ave, by="userId") %>%
  select(movieId,userId,rating,b_i,b_ui,title) %>% 
  mutate(predicted = mu+b_i+b_ui, 
         residual=rating-predicted, 
         abs_res=abs(residual))

check_predicted_m_u %>% mutate(se=((rating-predicted)^2))%>%
                        summarize(mse=mean(se), rmse=sqrt(mse))

RMSE(check_predicted_m_u$predicted, test_edx$rating)

# graph the residuals + rmse line (extream erorr in red)
user_movie_residual <- 
  check_predicted_m_u %>% 
  group_by(movieId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(residual=residual[1],
            mse=mean(se), 
            rmse=sqrt(mse)) %>%
  ggplot(aes(x=as.character(movieId),y=residual,
             color=ifelse(residual>=(-3)& residual<=3,"blue", "red")))+
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.8666408),color="blue", 
             linetype="dashed", size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# graph the se + mse line (extream erorr in red) 
user_movie_se <-
  check_predicted_m_u %>% 
  group_by(movieId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(se=se[1],mse=mean(se)) %>%
  ggplot(aes(x=as.character(movieId),y=se,
             color=ifelse(se>=(10),"red", "blue"))) +
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.7510663),color="blue", 
             linetype="dashed", size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# 2 plots together
grid.arrange(user_movie_residual,user_movie_se, ncol=2)

# list of our biggest "mistakes"(the reason for small improve in RMSE)
check_predicted_m_u %>%
  mutate(se=((rating-predicted)^2))%>%
  arrange(desc(se)) %>% select(title,se)%>% 
  distinct() %>%slice(1:10) %>%pull(title)


# reg user effect + movie effect 

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

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Reg. Movie + User Effects Model",  
                                     RMSE = model_3_1_rmse ))
rmse_results %>% knitr::kable()

# forth model - modeling regularized age_at_rating effect (each movie rated several times over the years. 
# the model will group the age of the movie at rating regardles it title)
# choosing penalty term (lambda) for age_at_rating effect

#user + age 

lambdas <- seq(0,10,0.25)

equation_mu <- mean(train_edx_cv$rating)

user_age_rmses <- 
  sapply(lambdas,function(lambda){
    fit_reg_user_ave <- 
      train_edx_cv %>% 
      group_by(userId) %>%
      summarize(n_i=n(),
                s= sum(rating - equation_mu),
                reg_b_u=(s/(n_i+lambda)))
    
    fit_reg_user_age_ave <- 
      train_edx_cv %>%
      left_join(fit_reg_user_ave, by='userId') %>%
      group_by(age_at_rating) %>%
      summarize(n_i=n(), 
                s= sum(rating -reg_b_u -equation_mu), 
                reg_b_ua=(s/(n_i+lambda)))
    
    reg_predicted_ratings <- 
      test_edx_cv %>% 
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
  group_by(userId) %>%
  summarize(n_i=n(), 
            s= sum(rating - mu),
            reg_b_u=(s/(n_i+penalty_term)))

fit_reg_user_age_ave <- 
  train_edx %>%
  left_join(fit_reg_user_ave, by='userId') %>%
  group_by(age_at_rating) %>% 
  summarize(n_i=n(),
            s= sum(rating -reg_b_u -mu), 
            reg_b_ua=(s/(n_i+penalty_term)))

predicted_ratings <- 
  test_edx %>%
  left_join(fit_reg_user_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  mutate(predicted = mu+reg_b_u+reg_b_ua) %>%
  pull(predicted)

model_4_rmse <- RMSE(true_ratings=test_edx$rating,
                     predicted_ratings=predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Reg. User+Age Effects Model",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()



# fifth model - add time effect to reg. movie + user 

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

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="add time effect to reg. movie + user ",  
                                     RMSE = model_5_rmse ))
rmse_results %>% knitr::kable()

#### age+user+movie 

# separate rows to genres
train_edx_genre <- train_edx %>%  separate_rows(genres, sep="\\|")
test_edx_genre <- test_edx %>%  separate_rows(genres, sep="\\|")

fit_genre_ave <-  
  train_edx_genre %>% 
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-reg_b_i-reg_b_ui-reg_b_ua))

predicted_ratings <- 
  test_edx_genre %>% 
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_reg_user_age_ave, by='age_at_rating') %>%
  left_join(fit_genre_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+reg_b_ua+b_g) %>%
  pull(predicted)


model_6_rmse <- RMSE(true_ratings=test_edx_genre$rating,
                     predicted_ratings=predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="model ",
                                     RMSE = model_6_rmse ))
rmse_results %>% knitr::kable()


# seventh model - add Genre effect to time+ reg. movie + user 


fit_genre_ave <-  
  train_edx_genre %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  group_by(genres) %>%
  summarize(b_g=mean(rating-mu-reg_b_i-reg_b_ui-d_ui))

predicted_ratings <- 
  test_edx_genre %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genre_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g) %>%
  pull(predicted)

model_7_rmse <- RMSE(true_ratings=test_edx_genre$rating,
                     predicted_ratings=predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="add Genre effect to time+ reg. movie + user ",
                                     RMSE = model_7_rmse ))
rmse_results %>% knitr::kable()

# model_7_rmse 0.8640791 perfect! 
# check model 7

check_predicted_model_seven <- 
  test_edx_genre %>% 
  mutate(week = round_date(rate_date, unit = "week")) %>%
  left_join(fit_reg_movie_ave, by='movieId') %>%
  left_join(fit_reg_user_movie_ave, by='userId') %>%
  left_join(fit_time_ave, by="week") %>%
  left_join(fit_genre_ave, by='genres') %>%
  select(movieId,rating,reg_b_i,reg_b_ui,d_ui,b_g,title,genres)%>%  
  mutate(predicted=mu+reg_b_i+reg_b_ui+d_ui+b_g,
         sum=reg_b_i+reg_b_ui+d_ui+b_g,
         residual=rating-predicted, 
         abs_res=abs(residual))

check_predicted_model_seven %>% 
  mutate(se=((rating-predicted)^2)) %>%
  summarize(mse=mean(se), rmse=sqrt(mse))

RMSE(check_predicted_model_seven$predicted, test_edx_genre$rating)


best_movies_seven <- 
  check_predicted_model_seven%>%
  group_by(title)%>%
  summarize(n_ratings=n(),
            sum=mean(sum), 
            ave_score=mean(rating), 
            ave_predict=mean(predicted),
            ave_res=mean(residual))%>% 
  arrange(desc(sum)) %>%
  slice(1:10)%>%
  select("Movie title"=title,n_ratings,ave_score,ave_predict,ave_res,sum)
best_movies_seven

worst_movies_seven <- 
  check_predicted_model_seven%>%
  group_by(title)%>%
  summarize(n_ratings=n(),
            sum=mean(sum), 
            ave_score=mean(rating), 
            ave_predict=mean(predicted),
            ave_res=mean(residual))%>% 
  arrange(sum) %>%
  slice(1:10)%>%
  select("Movie title"=title,n_ratings,ave_score,ave_predict,ave_res,sum)
worst_movies_seven

# graph model 7 residuals + se 

model_seven_residual <- 
  check_predicted_model_seven %>% 
  group_by(movieId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(residual=residual[1],
            mse=mean(se), 
            rmse=sqrt(mse)) %>%
  ggplot(aes(x=as.character(movieId),y=residual,
             color=ifelse(residual>=(-3)& residual<=3,"blue", "red")))+
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.864),color="blue", linetype="dashed", size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

# graph the se + mse line (extream erorr in red) 

model_seven_se <-
  check_predicted_model_seven %>% 
  group_by(movieId) %>% 
  mutate(se=((rating-predicted)^2))%>%
  summarize(se=se[1],
            mse=mean(se)) %>%
  ggplot(aes(x=as.character(movieId),y=se,
             color=ifelse(se>=(10),"red", "blue"))) +
  geom_point(alpha=0.1,show.legend = FALSE)+
  geom_hline(aes(yintercept=0.747),color="blue", linetype="dashed", size=0.5)+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

grid.arrange(model_seven_residual,model_seven_se, ncol=2)
 
# test on validation 

test_validation <- 
  validation %>%  
  mutate (rate_date = date(as_datetime(timestamp)),
          release_year =as.numeric(str_extract(title,"(?<=\\()(\\d{4})(?=\\))")),
          title= str_remove(as.character(title), "(\\(\\d{4}\\))")) %>% 
  select(-timestamp) %>%
  separate_rows(genres, sep="\\|")


fit_genre_ave <-  
  train_edx_genre %>% 
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
  left_join(fit_genre_ave, by='genres') %>% 
  mutate(predicted = mu+reg_b_i+reg_b_ui+d_ui+b_g) %>%
  pull(predicted)

model_final_rmse <- RMSE(true_ratings=test_validation$rating,
                         predicted_ratings=predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="validation ",  
                                     RMSE = model_final_rmse ))
rmse_results


#####################





  