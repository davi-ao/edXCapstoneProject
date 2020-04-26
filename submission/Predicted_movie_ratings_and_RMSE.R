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

m_i = edx %>%
  group_by(movieId) %>%
  summarize(m_i = sum(rating - mean(edx$rating))/(n() + 5))

# Update data set with movie effect
edx = edx %>%
  left_join(m_i, by = "movieId")

# Update validation set with movie effect
validation = validation %>%
  left_join(m_i, by = "movieId")

# Remove unecessary data from memory
rm(m_i)

# Calculate user effect with the whole data set
u_j = edx %>%
  group_by(userId) %>%
  summarize(u_j = sum(rating - mean(edx$rating) - m_i)/(n() + 5))

# Update data set with user effect
edx = edx %>%
  left_join(u_j, by = "userId")

# Update validation set with user effect
validation = validation %>%
  left_join(u_j, by = "userId")

# Remove unecessary data from memory
rm(u_j)

# Calculate genres effect with the whole data set
g_k = edx %>%
  group_by(genres) %>%
  summarize(g_k = sum(rating - mean(edx$rating) - m_i - u_j)/(n() + 5))

# Update data set with genres effect
edx = edx %>%
  left_join(g_k, by = "genres")

# Update validation set with genres effect
validation = validation %>%
  left_join(g_k, by = "genres")

# Remove unecessary data from memory
rm(g_k)

# Calculate predictions
y_hat = validation %>% 
  mutate(y_hat = mean(edx$rating) + m_i + u_j + g_k) %>% .$y_hat

# Define RMSE functions
RMSE = function(true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Run the RMSE function with the prediction values and the validating set
RMSE(validation$rating, y_hat)