# Code provided as course material in the edx plataform
#################################
# Create edx set, validation set#
#################################

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
set.seed(1) # set.seed(1, sample.kind="Rounding")
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

# Code created by the author, based on Irizarry (2019)
#################################
# Generate predictions and RMSE #
#################################

# Set the parameter to the optimized value
lambda = 5

# Calculate the regularized movie effect
m_i = edx %>%
  select(rating, movieId) %>%
  group_by(movieId) %>%
  summarize(m_i = sum(rating - mean(edx$rating))/(n() + lambda))

# Join movie effect to the data frame to facilitate the next steps
edx = edx %>%
  left_join(m_i, by = 'movieId')

# Calculate the regularized user effect
u_i = edx %>%
  select(rating, userId, m_i) %>%
  group_by(userId) %>%
  summarize(u_i = sum(rating - mean(edx$rating) - m_i)/(n() + lambda))

# Join user effect to the data frame to facilitate the next step
edx = edx %>%
  left_join(u_i, by = 'userId')

# Calculate the genres effect
g_i = edx %>%
  select(rating, genres, m_i, u_i) %>%
  group_by(genres) %>%
  summarize(g_i = sum(rating - mean(edx$rating) - m_i - u_i)/(n() + lambda))

# Calculate the predictions
y_hat = validation %>% 
  left_join(m_i, by = 'movieId') %>%
  left_join(u_i, by = 'userId') %>%
  left_join(g_i, by = 'genres') %>%
  mutate(y_hat = mean(edx$rating) + m_i + u_i + g_i) %>% .$y_hat

# Define RMSE functions
RMSE = function(true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Run the RMSE function with the prediction values and the validating set
RMSE(validation$rating, y_hat)