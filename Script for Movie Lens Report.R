
##NB to readers, please be aware at points the code might be extremely stressful to your machine. 
#This was created on a high end PC but won't run a High spec Mac do please read comments before running the code.

#Stressful elements highlighted >*Stressful*<

#The code will kick up 8 warnings, these are just about coercing data and the sampling method for set seed. 
#Any other warnings are unexpected, please check all packages are installed correctly 

#Libraries to be loaded
library(tidyverse)
library(caret)
library(data.table)
library(recommenderlab)
library(ggthemes)
library(irlba)

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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
#Creating a data partition within the edx data set to create a test/train for model training
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in validation set are also in train set

test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into train set

removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(test_index, temp, removed)

head(edx)
## Investigating the ratings

#Creating a Histogram of the scores given for some context

band <- ifelse((edx$rating == 1 | edx$rating == 2 |edx$rating == 3 |
                  edx$rating == 4 | edx$rating == 5),
               "Whole",
               "Half")
rating_hist <- data.frame(edx$rating, Rating_Star = band)

rating_hist %>% ggplot(aes(edx.rating, fill = Rating_Star))+
  geom_histogram(binwidth = 0.25)+
  scale_x_continuous(breaks = seq(0,5,0.5))+
  scale_fill_manual(values = c("Whole" = "dark grey", "Half" = "light grey"))+
  labs(x = 'Stars', y = 'Number of ratings', caption = 'Data Source: MovieLens 10M - transformed to edx')+
  ggtitle('Histogram depiciting the frequency of a star Rating')
rm(band,rating_hist)
mean(edx$rating)

## Understanding the users

n_distinct(edx$userId)
#UserID Count of Ratings 
#Graphically ploting the density of users by number of ratings (personal preference for density over histogram)
#Quantiles used to add text and lines to the data
quantiles <- edx %>% 
  count(userId)%>%
  summarise(lower = quantile(n, probs = .05),
            mean = round(mean(n),0),
            upper = quantile(n, probs = .95))
edx %>% 
  count(userId)%>%
  ggplot(aes(n))+
  geom_density(fill = 'grey', colour = 'grey')+
  scale_x_log10()+
  ggtitle('Distribtuion of ratings per user')+
  geom_vline(data = quantiles, aes(xintercept = lower), linetype = 'dotted')+
  geom_vline(data = quantiles,aes(xintercept = mean), color = 'red', linetype = 'dotted')+
  geom_vline(data = quantiles,  aes(xintercept = upper), linetype = 'dotted')+
  geom_text(data = quantiles, aes(x = lower, y = 0.5, label = lower))+
  geom_text(data = quantiles, aes(x = lower, y = 0.4, label = 'Quantile 5%'))+
  geom_text(data = quantiles, aes(x = mean, y = 0.35, label = mean))+
  geom_text(data = quantiles, aes(x = mean, y = 0.25, label = 'Mean'))+
  geom_text(data = quantiles, aes(x = upper, y = 0.5, label = upper))+
  geom_text(data = quantiles, aes(x = upper, y = 0.4, label = 'Quantile 95%'))+
  labs(x = 'Number of ratings per userId', y = 'Density of Users',caption = 'Data Source: MovieLens 10M - transformed to edx' )
#Understanding the users with the highest number of ratings (seeking outliers)
maxcount <- edx %>% 
  count(userId)%>%
  arrange(desc(n))%>%
  head(10)

maxcount

## Understanding the movies data

n_distinct(edx$movieId)
#Same as with UserIDs, using Movie IDs as the value
quantiles <- edx %>% 
  count(movieId)%>%
  summarise(lower = quantile(n, probs = .05),
            mean = round(mean(n),0),
            upper = round(quantile(n, probs = .95),0),
            sd = sd(n),
            median(n))

edx %>% 
  count(movieId)%>%
  ggplot(aes(n))+
  geom_density(fill = 'grey',color = 'grey')+
  scale_x_log10()+
  ggtitle('Distribtuion of ratings per movie')+
  geom_vline(data = quantiles, aes(xintercept = lower), linetype = 'dotted')+
  geom_vline(data = quantiles,aes(xintercept = mean),color = 'red', linetype = 'dotted')+
  geom_vline(data = quantiles,  aes(xintercept = upper), linetype = 'dotted')+
  geom_text(data = quantiles, aes(x = lower, y = 0.2, label = lower))+
  geom_text(data = quantiles, aes(x = lower, y = 0.225, label = 'Quantile 5%'))+
  geom_text(data = quantiles, aes(x = mean, y = 0.175, label = mean))+
  geom_text(data = quantiles, aes(x = mean, y = 0.15, label = 'Mean'))+
  geom_text(data = quantiles, aes(x = upper, y = 0.2, label = upper))+
  geom_text(data = quantiles, aes(x = upper, y = 0.225, label = 'Quantile 95%'))+
  labs(x = 'Number of ratings per movieId', y = 'Density of Users',caption = 'Data Source: MovieLens 10M - transformed to edx' )
# Finding the top 10 most rated movies
maxcount <- edx %>% 
  count(movieId,title)%>%
  arrange(desc(n))%>%
  head(10)

maxcount
# Finding the movies with the lowest number of ratings 
mincount <- edx %>% 
  count(movieId,title)%>%
  arrange(desc(n))%>%
  tail(10)

mincount
#Finding the number of movies with 1 rating 
count(edx %>% 
        count(movieId,title)%>%
        filter(n == 1))

## Exploring the genres variable >*Stressful*< - not recommended to run on lower spec machines

#Making this a data frame for presentation 

genres <- unlist(strsplit(edx$genres,"\\|"))

#Making this a data frame for presentation

Genre_Data <- as.data.frame(sort(table(genres),decreasing=TRUE))

Genre_Data %>% ggplot(aes(reorder(genres,Freq), Freq))+
  geom_bar(stat = 'identity', fill = 'grey')+
  coord_flip()+
  labs(y = 'Frequency of rating', x = 'Film Genre',caption = 'Data source: MovieLens 10M transformed into edx')+
  geom_text(data = Genre_Data, aes(x = genres, y = Freq+185000, label = Freq), size = 3)+
  ggtitle('Bar chart showing the number of ratings by Genre')

#Due to the size of the dataset, the following graphics only depict the first 100,000 obersvations within the dataset for illustrative purposes. 

genres <- unlist(strsplit(edx$genres[1:100000],"\\|"))

#Creating a series of graphs of the data, please refer to PDF if on low spec machine

Genre_Data <- as.data.frame(sort(table(genres),decreasing=TRUE))
Genre_rating_max <- edx[1:100000,] %>% separate_rows(genres,sep = "\\|") %>%
  group_by(rating,genres) %>%
  summarise(Freqs = n())%>%
  right_join(Genre_Data)%>%
  mutate(Percentage_of_Ratings = Freqs/Freq)%>%
  group_by(genres)%>%
  summarise(Max = max(Percentage_of_Ratings))

TopGenre_rating_Data <- edx[1:100000,] %>% separate_rows(genres,sep = "\\|") %>%
  group_by(rating,genres) %>%
  summarise(Freqs = n())%>%
  right_join(Genre_Data)%>%
  mutate(Percentage_of_Ratings = Freqs/Freq)%>%
  right_join(Genre_rating_max)%>%
  mutate(Max = ifelse(Percentage_of_Ratings == Max, T,F)) %>%
  arrange(desc(Freq))%>%
  head(50)

TopGenre_rating_Data %>% ggplot(aes(rating,Percentage_of_Ratings, fill = Max))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = c('TRUE' = 'dark grey', 'FALSE' = 'light grey'))+
  scale_x_continuous(minor_breaks = c(0.5,1.5,2.5,3.5,4.5))+
  facet_grid(.~reorder(genres,-Freq))+
  labs(x='Stars rating given', 
       y='Percentage of all ratings for genre', 
       caption = 'Data source = MovieLens 10M transformed into edx')+
  ggtitle('Bar Graph depicting the percentage of ratings for the top 5 genres')

BottomGenre_rating_Data <- edx[1:100000,] %>% separate_rows(genres,sep = "\\|") %>%
  group_by(rating,genres) %>%
  summarise(Freqs = n())%>%
  right_join(Genre_Data)%>%
  mutate(Percentage_of_Ratings = Freqs/Freq)%>%
  right_join(Genre_rating_max)%>%
  mutate(Max = ifelse(Percentage_of_Ratings == Max, T,F)) %>%
  arrange(desc(Freq))%>%
  tail(49)

BottomGenre_rating_Data %>% ggplot(aes(rating,Percentage_of_Ratings, fill = Max))+
  geom_bar(stat = 'identity')+
  scale_fill_manual(values = c('TRUE' = 'dark grey', 'FALSE' = 'light grey'))+
  scale_x_continuous(minor_breaks = c(0.5,1.5,2.5,3.5,4.5))+
  facet_grid(.~reorder(genres,-Freq))+
  labs(x='Stars rating given', 
       y='Percentage of all ratings for genre', 
       caption = 'Data source = MovieLens 10M transformed into edx')+
  ggtitle('Bar Graph depicting the percentage of ratings for the bottom 5 genres')


##End of >*Stressful*< zone

## Transforming the data into a matrix 

## Creation of sparse matrix for visualisation of the data set size problem - limited to first 10,000 rows to improve running performance. Purely for illustrative purposes.

sparse_edx <- edx[1:10000,]
sparse_edx$userId <- as.numeric(as.factor(sparse_edx$userId))
sparse_edx$movieId <- as.numeric(as.factor(sparse_edx$movieId))

sparse_edx <- sparseMatrix(i = sparse_edx$userId, j= sparse_edx$movieId, x = sparse_edx$rating, 
                           dims = c(length(unique(sparse_edx$userId)),length(unique(sparse_edx$movieId))),
                           dimnames = list(paste0('user',1:length(unique(sparse_edx$userId))),
                                           paste0('movie',1:length(unique(sparse_edx$movieId)))))

#graphically displaying the sparse matrix

sparse_edx[1:25,] %>% as.vector %>% 
  tibble(value = ., row = rep(1:nrow(sparse_edx[1:25,]), times = ncol(sparse_edx[1:25,])),
         col = rep(1: ncol(sparse_edx[1:50,]), each = nrow(sparse_edx[1:25,]))) %>%
  ggplot(aes(x = row, y = col, fill = value)) + coord_flip()+
  geom_tile(size = 1) +
  scale_fill_gradient(low = 'white',high = 'grey')+
  labs(x = 'UserID', y = 'MovieID', caption = 'Data source: MovieLens 10M transformed into sparse matrix, first 25 users')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle('Graphical representation of the sparse matrix')

## Bringing it together

# Recommendation Models, Calculating the accuracy 

#Calculating RMSE 

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}

# Regression Models

# Building the basic model - using the mean from the data set

mu_hat <- mean(train$rating)
mu_hat

naive_rmse <- RMSE(test$rating, mu_hat)
naive_rmse

#creating a table to stroe the results from running models on the test/train data - code similar in mutiple places here
rmse_results <- data.frame(Method = "Just the average", RMSE = naive_rmse)

##Exploring the Movie Effect
# fit <- lm(rating ~ as.factor(movieId), data = movielens)
mu <- mean(train$rating) 
# Creating a data frame withe the movieID and the average rating for each movie
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
#Graphical display of this data
movie_avgs %>% ggplot(aes(b_i))+
  geom_histogram(binwidth = 0.5, fill = 'grey', color = 'dark grey')+
  labs(x = 'b_i (movie effect)',y= 'Frequency',caption = 'Data source: MovieLens 10M')+
  ggtitle('Histogram of the movie effect')
#model creation 
predicted_ratings <- mu + test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
#Output 
model_1_rmse <- RMSE(test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

##Exploring the User Effect
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "dark grey", fill = 'grey')+
  labs(x = 'b_u (user average rating)',y= 'Frequency',caption = 'Data source: MovieLens 10M for all users with >100 ratings')+
  ggtitle('Histogram of the user effect')

# lm(rating ~ as.factor(movieId) + as.factor(userId))
# Calculating the average user ratings to create a user effect b_u 
user_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#model creation 
predicted_ratings <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(test$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
model_2_rmse

## Regularization 
# fit <- lm(rating ~ as.factor(movieId) as.factor(userId), data = movielens)
#Getting a data frame with the number of distinct movie ratings
movie_titles <- train %>% 
  select(movieId, title) %>%
  distinct()
#displaying titles with the largest number 
train %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

train %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

# Nominally selected lambda to just test the regularization methodology - Just the movie effect
    lambda <- 3
    mu <- mean(train$rating)
    movie_reg_avgs <- train %>% 
      group_by(movieId) %>% 
      summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
    
    data_frame(original = movie_avgs$b_i, 
               regularlized = movie_reg_avgs$b_i, 
               n = movie_reg_avgs$n_i) %>%
      ggplot(aes(original, regularlized, size=sqrt(n))) + 
      geom_point(shape=1, alpha=0.5)+
      labs(x = 'Original b_i',y= 'Regularized b_i',caption = 'Data source: MovieLens 10M')+
      ggtitle('Plot of the change in b_i with regularization')+
      theme(legend.position = 'none')
 # showing the top and bottom ten movies based on predicted ratings - showing why lambda of 3 is not perfect
    train %>%
      dplyr::count(movieId) %>% 
      left_join(movie_reg_avgs) %>%
      left_join(movie_titles, by="movieId") %>%
      arrange(desc(b_i)) %>% 
      select(title, b_i, n) %>% 
      slice(1:10)
    
    train %>%
      dplyr::count(movieId) %>% 
      left_join(movie_reg_avgs) %>%
      left_join(movie_titles, by="movieId") %>%
      arrange(b_i) %>% 
      select(title, b_i, n) %>% 
      slice(1:10)
    
    predicted_ratings <- test %>% 
      left_join(movie_reg_avgs, by='movieId') %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    
    model_3_rmse <- RMSE(test$rating, predicted_ratings)
    rmse_results <- bind_rows(rmse_results,
                              data_frame(Method="Regularized Movie Effect Model",  
                                         RMSE = model_3_rmse ))
    model_3_rmse
# Regularization for both user and movie effect model - nominally selected lambda of 3 remains in place
    mu <- mean(train$rating)
    b_i <- train %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+lambda))
    b_u <- train %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
    predicted_ratings <- 
      test %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred 
    
    model_3_1_rmse <- RMSE(test$rating,predicted_ratings)
    rmse_results <- bind_rows(rmse_results,
                              data_frame(Method="Regularized Movie & User Effect Model Lambda =3",  
                                         RMSE = model_3_1_rmse ))
    model_3_1_rmse     
# Cross tuning the lambda parameter using the test and train data set 
    lambdas <- seq(0, 10, 0.25)
    rmses <- sapply(lambdas, function(l){
      mu <- mean(train$rating)
      b_i <- train %>% 
        group_by(movieId) %>%
        summarize(b_i = sum(rating - mu)/(n()+l))
      b_u <- train %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_i - mu)/(n()+l))
      predicted_ratings <- 
        test %>% 
        left_join(b_i, by = "movieId") %>%
        left_join(b_u, by = "userId") %>%
        mutate(pred = mu + b_i + b_u) %>%
        .$pred
      return(RMSE(test$rating,predicted_ratings))
    })
# plotting the lambdas vs the RMSEs    
    qplot(lambdas, rmses)  
#Selecting the tuned parameter     
    lambda <- lambdas[which.min(rmses)]
    lambda 
#Re-running the model with the tuned parameter     
          mu <- mean(train$rating)
          b_i <- train %>% 
            group_by(movieId) %>%
            summarize(b_i = sum(rating - mu)/(n()+lambda))
          b_u <- train %>% 
            left_join(b_i, by="movieId") %>%
            group_by(userId) %>%
            summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
          predicted_ratings <- 
            test %>% 
            left_join(b_i, by = "movieId") %>%
            left_join(b_u, by = "userId") %>%
            mutate(pred = mu + b_i + b_u) %>%
            .$pred 
          
          model_3_2_rmse <- RMSE(test$rating,predicted_ratings)
          rmse_results <- bind_rows(rmse_results,
                                    data_frame(Method="Regularized Movie & User Effect Model ~ Lambda",  
                                               RMSE = model_3_2_rmse ))
          model_3_2_rmse  
          
          rmse_results
# Based on the regularize movie and user effect model + tuned lambda the model will be run on the edx and validation set          
         
           mu <- mean(edx$rating)
          movie_reg_avgs <- edx %>% 
            group_by(movieId) %>% 
            summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
          movie_titles <- edx %>% 
            select(movieId, title) %>%
            distinct()
   # showing that the new lambda figure fixes the issues caused by a nominal lambda of 3 (niave assumptions)       
          edx %>%
            dplyr::count(movieId) %>% 
            left_join(movie_reg_avgs) %>%
            left_join(movie_titles, by="movieId") %>%
            arrange(desc(b_i)) %>% 
            select(title, b_i, n) %>% 
            slice(1:10)
 # final model run          
          mu <- mean(edx$rating)
          b_i <- edx %>% 
            group_by(movieId) %>%
            summarize(b_i = sum(rating - mu)/(n()+lambda))
          b_u <- edx %>% 
            left_join(b_i, by="movieId") %>%
            group_by(userId) %>%
            summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
          predicted_ratings <- 
            validation %>% 
            left_join(b_i, by = "movieId") %>%
            left_join(b_u, by = "userId") %>%
            mutate(pred = mu + b_i + b_u) %>%
            .$pred 
  # output of final model run        
          final_model_reg_rmse <- RMSE(validation$rating, predicted_ratings)
          final_rmse_results <-                          data_frame(Method="Regularized Movie & User Effect Model with cross validated Lambda",  
                                                                    RMSE =  final_model_reg_rmse )
          final_rmse_results
# final results         
          final_model_reg_rmse
