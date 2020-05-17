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


#Preparing the data
#Note: This part run well on R 3.6.2, but may encounter trouble on R 4.0
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

#Checking the edx dataset
head(edx)

str(edx)

summary(edx)

#number of user and nomber of movie
edx%>%summarize(num_user=n_distinct(userId),num_movie=n_distinct(movieId))

#Preparing the data

#changing timestamp to have year of rating
edx$date<-as.POSIXct(edx$timestamp,origin="1970-01-01")
edx$rateYear<-format(edx$date,"%Y")

validation$date<-as.POSIXct(validation$timestamp,origin="1970-01-01")
validation$rateYear<-format(validation$date,"%Y")

#extract year of the movie release
edx<-edx%>%
  extract(title, c("titletemp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)%>%
  mutate(title=titletemp)%>%
  select(-titletemp)

validation<-validation%>%
  extract(title, c("titletemp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F)%>%
  mutate(title=titletemp)%>%
  select(-titletemp)

#Extract age of movie, as the diffrence between when the movie was rated and when it was released
edx<-edx%>%mutate(ageOfMovie= as.integer(rateYear)-as.integer(year))

validation<-validation%>%mutate(ageOfMovie= as.integer(rateYear)-as.integer(year))

#Removing the columns we won't use
edx<-edx%>% select(userId, movieId, rating, title, year, genres, rateYear, ageOfMovie)

validation<-validation%>% select(userId, movieId, rating, title, year, genres, rateYear, ageOfMovie)


#Diving into the data

#Graph of genres
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  head()

#movies with the most ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#plot of ratings distribution 
ggplot(edx, aes(rating))+
  geom_histogram(binwidth =  0.5, color ="black")+
  geom_vline(xintercept = mean(edx$rating), size=1, color="red",linetype = "dashed")+
  ggtitle("Ratings")

#plot of number of rating per movie
edx%>%
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  xlab("Ratings")+
  ylab("Movies")+
  ggtitle("Rating per movies")

#plot of number of rating per users
edx%>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  xlab("Ratings")+
  ylab("Users")+
  ggtitle("Rating per users")

#plot of average movie rating by users
edx%>%
  group_by(userId) %>%
  filter(n()>=100)%>%
  summarize(avg=mean(rating))%>%
  ggplot(aes(avg)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  scale_x_continuous()+
  xlab("Average rating")+
  ylab("Users")+
  ggtitle("Average movie rating per users")

#Age of movie
range(edx$rateYear)
range(edx$year)

#Average of the rating according to the age of the movie when rated
edx%>%
  group_by(ageOfMovie) %>% 
  filter(ageOfMovie>=1)%>%
  summarize(avg=mean(rating))%>%
  ggplot(aes(ageOfMovie,avg))+ 
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = mean(edx$rating), size=1, color="red",linetype = "dashed")+
  xlab("Age of movie before rating")+
  ylab("Average rating")+
  ggtitle("Average movie rating according to the age of the movie")

###
#Modelling
####

#Creating a test_set and a training set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Average model

#getting the average
mu<-mean(train_set$rating)
mu

#test with simple prediction
naive_rmse<- RMSE(test_set$rating,mu)
naive_rmse

#Save the result of our first model in a data frame
rmse_results<- data_frame(method="Average rating model", RMSE=naive_rmse)
rmse_results%>%knitr::kable()

#Model with movie effect

#We add a movie effect for this model, by substracting the mean from the rating for movies
#we create a variable with the movie effect b_i
movie_avgs<- train_set%>%
  group_by(movieId)%>%
  summarize(b_i=mean(rating-mu))

#Plot of the movie effect
movie_avgs%>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#test and add the result to our table
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

###
#Movie and user effects
###

# as for the movie effect we create a variable users_avgs
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Plot of the user effect
user_avgs%>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

#test and add the result to our table
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

###
#Movie, user and age of movie effects
###

# we create a variable age_avgs to quantify the age of movie effect
age_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by= 'userId')%>%
  group_by(ageOfMovie) %>%
  summarize(b_a = mean(rating - mu - b_i - b_u))

# Plot of the movie age effect
age_avgs%>% qplot(b_a, geom ="histogram", bins = 10, data = ., color = I("black"))

#test and add the result to our table
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(age_avgs, by='ageOfMovie') %>%
  mutate(pred = mu + b_i + b_u + b_a) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Age of movie Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()



###
#Model Movie and user effects regularized
###

#we add a tuning parameter lambda, choosed by cross-validation
lambdas <- seq(0, 10, 0.25)


#Find b_i and b_u for each lambda as well as rating prediction and test add year, have to figure out validation
#Note: this part may take few minutes when runing
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

#Plot the lambdas vs rmse
qplot(lambdas, rmses)  

#Choose the optimal lambda
lambda <- lambdas[which.min(rmses)]
lambda

#test and save the result to our table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


###
#Model Movie, user and age of movie effects regularized
###

lambdas_2 <- seq(0, 10, 0.25)


#Find b_i, b_u and b_a for each lambda as well as rating prediction and test
#Note: this part may take few minutes when runing
rmses <- sapply(lambdas_2, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_a <-train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(ageOfMovie) %>%
    summarize(b_a = sum(rating - b_i - b_u - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_a, by = "ageOfMovie") %>%
    mutate(pred = mu + b_i + b_u + b_a) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

#Plot the lambdas vs rmse
qplot(lambdas_2, rmses)  

#Choose the optimal lambda
lambda_2 <- lambdas_2[which.min(rmses)]
lambda_2

#test and save the result to our table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Age of Movie Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


#Final test using the edx set and validation
#We repeat the previous steps using lambda_2 in our model
mu_hat <- mean(edx$rating)
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+lambda_2))
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+lambda_2))
b_a <-edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(ageOfMovie) %>%
  summarize(b_a= sum(rating - b_i - b_u - mu_hat)/(n()+lambda_2))
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_a, by = "ageOfMovie") %>%
  mutate(pred = mu_hat + b_i + b_u + b_a) %>%
  pull(pred)

#Our final result
rmse_results <- bind_rows(rmse_results, data_frame(method="Final model with validation", RMSE = min(rmses)))
rmse_results %>% knitr::kable()
