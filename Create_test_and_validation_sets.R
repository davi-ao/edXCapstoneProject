library(tidyverse)
library(caret)

dl = tempfile()

download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings = fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                col.names = c("userId", "movieId", "rating", "timestamp"))

movies = str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) = c("movieId", "title", "genres")
movies = as.data.frame(movies) %>%
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         genres = as.character(genres))

movielens = left_join(ratings, movies, by = "movieId")

set.seed(1)

test_index = createDataPartition(y = movielens$rating, times = 1, p = .1, list = F)
edx = movielens[-test_index, ]
temp = movielens[test_index, ]

validation = temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed = anti_join(temp, validation)
edx = rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

write_csv(edx, 'edx.csv')
write_csv(validation, 'validation.csv')
