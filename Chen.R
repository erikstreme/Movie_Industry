# Data Visualization - EChen & CDelaney 2/23/2020
# Complete Chart Assignment
# Start Code
# ---------------------------------------------------------------------------------------
# Load packages ggplot, scales, dplyr, extrafont
library(ggplot2)
library(scales)
library(dplyr)

# load 4 movie data files, all sourced from imdb.com
# this file contains unique movie title ids, actual title names, year produced, genre
main.movie.file <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/title.basics.year.genre.csv")
# this file contains unique movie title ids, average ratings, and number of votes
movies.ratings <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/title.ratings.csv")
# this file contains unique actor ids and their associated role in a film
movies.staractor <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/title.principals.csv")
#  this file contains the actual names of the invididual actors
actors.names <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/name.basics.csv")

# look at the column names of main file and movie ratings file
str(main.movie.file)
str(movies.ratings)

# merge main movie file and movie ratings file by $tconst values ($tconst is the unique identifier for movie title)
movies <- merge(x = main.movie.file, y = movies.ratings, by = "tconst", all = TRUE)
dim(movies)
summary(movies)
# take out movies with no ratings
movies <- movies[which(!is.na(movies$averageRating) == TRUE),]
dim(movies)

# look at the column names of star actors file and actor names file
str(movies.staractor)
str(actors.names)

# merge star actors file and actor names file by $nconst values ($nconst is the unique id for individual actors)
actors.info <- merge(x = actors.names, y = movies.staractor, by = "nconst", all = TRUE)
dim(actors.info)
summary(actors.info)

# make new dataset with only relevant columns in movie names file, eliminate movies prior to 1970
movies <- movies[c(1:3, 6, 9:1)] 
movies$startYear <- as.numeric(as.character(movies$startYear)) # convert the stateYear variable to a numeric type
str(movies) # look to see that conversion was successful
movies.sub <- subset(movies, startYear >= 1970)
dim(movies.sub)

# make another new dataset with only relevant columns in actor info file, keep only relevant columns, people born after 1930
actors.info <- actors.info[c(1:4, 7, 9, 11)]
actors.info$birthYear <- as.numeric(as.character(actors.info$birthYear))
actors.info.sub <- subset(actors.info, birthYear >= 1930 & (category == "actor" | category == "actress"))
dim(actors.info.sub) # check to see if it worked

# merge sub-data sets
movies.by.actors <- merge(x = actors.info.sub, y = movies.sub, by = "tconst", all = TRUE )
movies.by.actors <- movies.by.actors[which(!is.na(movies.by.actors$primaryTitle) == TRUE),]
movies.by.actors <- movies.by.actors[which(!is.na(movies.by.actors$nconst) == TRUE),]
dim(movies.by.actors) # check to see if it worked

# filter out only Robert De Niro's filmography
robert.deniro.only <- filter(.data = movies.by.actors, primaryName == "Robert De Niro")
str(robert.deniro.only) #check to see if it's accurate

# single out the 1st listed genre category
robert.deniro.only$Genre <- gsub("^(.*?),.*", "\\1", robert.deniro.only$genres)
str(robert.deniro.only) # check to see if it worked

# subset filmography to take out genres where De Niro has not consistently acted in throughout the 30-year time period
deniro.sub <- subset(robert.deniro.only, Genre != "Action" & Genre !="Adventure" & Genre != "Biography" & Genre != "Horror")

# plot graph, points and trace those points
deniro.chart <- ggplot(deniro.sub, aes(x = startYear, y = averageRating, color = Genre)) + geom_point() +
                  geom_path() +
                  labs(title = "IMDB User Ratings of Robert De Niro's Films 1970 - 2000", 
                       subtitle = "Ratings of De Niro's diverse portfolio of films in a 30-year period are scattered across genres, \n suggesting star power is not a major influence on ratings", 
                       x = "Year", y = "Average User Rating", caption = "Source: IMDB") +
# add annotations to highest rated and lowest rated film
                    annotate("text", x = 1977, y = 8.9, label = "The Godfather: Part II", size = 2) + 
                  annotate("text", x = 1995, y = 5.8, label = "Night and the City", size = 2) +
# add themes to graph to make it aesthetically easy to look at, edit the gridlines & legend                  
                  theme(panel.background = element_rect(fill = "white"),
                        panel.grid.major.y = element_line(color = "grey"),
                        panel.grid.major.x = element_blank(),
                        legend.box.background = element_rect(color = "black", fill = NA, size = 1),
                        legend.title = element_blank()) +
# add titles, stylize typeface
                  theme(axis.line.x = element_line(color = "black", size = 0.5),
                      axis.line.y = element_line(color = "black", size = 0.5),
                      axis.title.x = element_text(face = "bold"),
                      axis.title.y = element_text(face = "bold"),
                      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                      plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
                      plot.caption = element_text(hjust = 0))
              
deniro.chart






