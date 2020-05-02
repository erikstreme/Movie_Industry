# Data Visualization - EChen & CDelaney 3/27/2020
# Policy Brief
# Start Code
# ---------------------------------------------------------------------------------------
# Load packages ggplot, scales, dplyr, tidyr
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

# load 4 movie data files, all sourced from imdb.com
# this file contains unique movie title ids, actual title names, year produced, genre
main.movie.file <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/title.basics.year.genre.csv")
# this file contains unique movie title ids, average ratings, and number of votes
movies.ratings <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/title.ratings.csv")
# this file contains unique actor ids and their associated role in a film
movies.staractor <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/title.principals.csv")
#  this file contains the actual names of the invididual actors
actors.names <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/name.basics.csv")

# look at the structure
str(main.movie.file)
# make year variable a number
main.movie.file$nyear <- as.numeric(as.character(main.movie.file$startYear))
# filter for movies after 1970
movie.titles <- main.movie.file[which(main.movie.file$titleType == "movie" & main.movie.file$nyear >= 1970),]
# discard large movie file
rm(main.movie.file)
# keep only relevant columns
movie.titles <- movie.titles[c(1:4, 6, 9:10)]

# Repeat process for actors file
str(actors.names)

actors.names$birthYear <- as.numeric(as.character(actors.names$birthYear))
# only save info for actors born after 1940
actors.list <- actors.names[which(actors.names$birthYear >= 1940),]
# discard large actors file
rm(actors.names)

# Perform merge with movies with their ratings
movies <- merge(x = movie.titles, y = movies.ratings, by = "tconst", all = TRUE)
dim(movies)

# clean up and remove NAs in titles and ratings
movies <- movies[which(!is.na(movies$primaryTitle) == TRUE),]
movies <- movies[which(!is.na(movies$averageRating) == TRUE),]

# Perform merge for actors and titles they are in
actors <- merge(x = actors.list, y = movies.staractor, by = "nconst", all = TRUE)
# remove NAs
actors <- actors[which(!is.na(actors$primaryName) == TRUE),]
# subset for only actors and actresses
actors <- subset(actors, (category == "actor" | category == "actress"))
# discard large unmerged files
rm(movies.staractor)
rm(movies.ratings)

# merge actors with movie file with titles
actors.plus.movies <- merge(x = actors, y = movies, by = "tconst", all = TRUE)
# remove NAs
actors.plus.movies <- actors.plus.movies[which(!is.na(actors.plus.movies$nconst) == TRUE),]
actors.plus.movies <- actors.plus.movies[which(!is.na(actors.plus.movies$primaryTitle) == TRUE),]

actors.plus.movies <- group_by(.data = actors.plus.movies, primaryName)
#remove unused large data files
rm(actors)
rm(actors.list)
rm(movie.titles)
#------------------------------------------------------------------------------------
# Load TMDB 5000 data from Kaggle.com
meta.data2 <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/tmdb-movie-metadata/tmdb_5000_movies.csv")
# look at the structure
str(meta.data2)
# make sure our date is actually a date
meta.data2$release_date <- as.Date(meta.data2$release_date)
# extract year and convert to numeric format
meta.data2$release_year <- as.numeric(format(meta.data2$release_date, "%Y"))
# remove NA budgets and years
meta.data2 <- meta.data2[which(!is.na(meta.data2$budget & meta.data2$revenue & meta.data2$release_year) == TRUE),]
# make a factor variable that separates highly rated films from the ones with low scores
# defining a low score as being less than 7/10
meta.data2$average_score <- ifelse(meta.data2$vote_average >= 7, 1, 0)
# set data up for bar graph data
meta.data2 <- group_by(.data = meta.data2, release_year, average_score)
# create profit variable
meta.data2$profit <- meta.data2$revenue-meta.data2$budget
# create another factor that separates highly rated films with high budgets from all other films
meta.data2$budget.score <- ifelse(meta.data2$average_score == 1 & meta.data2$budget >= 100000000, 1, 0)
# create dataset for bar graph 1 that separates highly rated films from poorly rated films
# also create averages for each year on budget, profit, and user votes (not interested in revenue here)
bar_data <- summarize(.data = meta.data2, no_movies = n(), mean_budget = mean(budget), mean_profit = mean(profit), mean_users = mean(vote_count))
# take out budgets that are 0s
bar_data <- bar_data[which(bar_data$mean_budget != 0),]
# make our user score factor variable a factor to use as our fill for our bar graph
bar_data$average_score.f <- as.factor(bar_data$average_score)
# prep large data set for the creation of 2nd smaller dataset
meta.data2 <- group_by(.data = meta.data2, release_year, average_score, budget.score)
# create 2nd dataset for bar graph 2 - films are separated by user rating & budget
# also create averages for each year on budget, profit, and user votes (also not interested in revenue here)
bar_data2 <- summarize(.data = meta.data2, no_movies = n(), mean_budget = mean(budget), mean_profit = mean(profit), mean_users = mean(vote_count))
# remove 0s
bar_data2 <- bar_data2[which(bar_data2$mean_budget != 0),]
# make our factor a factor to use as fill for bar graph
bar_data2$budget.score.f <- as.factor(bar_data2$budget.score)
# prep large dataset for 3rd smaller data set that doesn't have distinct factor separation
meta.data2 <- group_by(.data = meta.data2, release_year)
# make 3rd data set for line and stacked line graphs
# also create averages for each year on budget, revenue, profit, and user votes
submeta2 <- summarize(.data = meta.data2, no_movies = n(), mean_budget = mean(budget), mean_revenue = mean(revenue), mean_profit = mean(profit), mean_users = mean(vote_count))
# remove 0s
submeta2 <- submeta2[which(submeta2$mean_budget != 0),]
# make our factor for highly rated films a factor
submeta2$average_score.f <- as.factor(submeta2$average_score)
# make sure everything looks right
str(submeta2)

##### Graphs ######
# create function for basic line graphs (trend 3, 4, 6, and 7)
line_funk <- function(dataset, var_x, var_y, the_color, var_title){
    my_line <- ggplot() + geom_line(data = dataset, mapping = aes_string(x = var_x, y = var_y, color = the_color), size = 1) +
    #geom_point(data = dataset, mapping = aes_string(x = var_x, y = var_y), size = 2) +
    scale_x_continuous(limits = c(1980, 2020), breaks = c(seq(1980, 2020, 5))) +
    labs(x = "", y = "", title = var_title, caption = "source: TMBD 5000 Movies (Kaggle.com)") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray"),
          legend.position = "right",
          legend.title = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 10),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#2b8cbe"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
          plot.caption = element_text(hjust = 0, size = 7.5))
  print(my_line)
}


# Graph 1 - Bar Graph - look at the number of films released over 30 years separated by high/low ratings
trend1 <- ggplot() + geom_col(data = bar_data, mapping = aes(x = release_year, y = no_movies, fill = average_score.f),
                              position = position_dodge()) +
                      scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980,2020,5)) +
                      labs(title = "A 30-Year Distribution of Films 1980-2016: \n Increase of Quantity, Not Quality", 
                           subtitle = "As the number of films released increased over time, \n the number of low-rated films outpaced those of 'better' quality",
                           x = "Year of Release",  y = "Number of Films", caption = "source: TMBD 5000 Movies (Kaggle.com)") +
                      # aesthetics
                      theme(panel.background = element_blank(),
                            axis.line = element_line(color = "gray"),
                            plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
                            legend.title = element_blank(),
                            plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
                            plot.caption = element_text(hjust = 0, size = 7.5)) +
                      # change legend labels
                      scale_fill_manual(labels = c("User Rating < 7/10", "User Rating > 7/10"), values = c("#f8766d", "#00bfc4"))
trend1

# Graph 2 - Bar Graph - look at the number of films released over 30 years that had high budgeted and received high ratings
trend2 <- ggplot() + geom_col(data = bar_data2, mapping = aes(x = release_year, y = no_movies, fill = budget.score.f),
                              position = position_dodge()) +
                      scale_x_continuous(limits = c(1980, 2020), breaks = seq(1980,2020,5)) +
                      labs(title = "A 30-Year Distribution of Films 1980-2016: \n Big Budgets Don't Help with Reviews",
                           subtitle = "Surprisingly, a rise in film budgets over time \ndid not lead to a corresponding rise of film quality & user satisfaction",
                           x = "Year of Release",  y = "Number of Films", caption = "source: TMBD 5000 Movies (Kaggle.com)") +
                      theme(panel.background = element_blank(),
                          axis.line = element_line(color = "gray"),
                          plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
                          legend.title = element_blank(),
                          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
                          plot.caption = element_text(hjust = 0, size = 7.5)) +
                      scale_fill_manual(labels = c("All other films", "Rating > 7/10 & \n $100M+ Budget"), values = c("#f8766d", "#00bfc4"))
trend2

# Graph 3 - Line Graph - plot the industry budget trend
trend3 <- line_funk(submeta2, "release_year", "mean_budget", NULL, "Film Budgets Steadily Increase over 1980 - 2016") +
                      scale_y_continuous(label = comma, limits = c(0, 50000000), breaks = c(seq(0, 50000000, 10000000))) +
                      labs(x = "Year of Release", y = "Unadjusted Avg. Budget Per Film ($)",
                            subtitle = "The steady increase of film budgets over time corresponds to \n the exponential growth of surrounding technology") +
                      annotate("text", x = 1990, y = 39000000, label = "1990s: \n Dot-Com Bubble:Increase in Independent Studios & CGI", size = 2) +
                      annotate("text", x = 2008, y = 39000000, label = "2008: \n Financial Recession", size = 2) +
                      annotate("text", x = 2000, y = 48000000, label = "2000s:\n End of Disney Renaissance Era \n Rise of Computer Animation", size = 2)
trend3

# Graph 4 - Line Graph - plot the industry revenue trend
trend4 <- line_funk(submeta2, "release_year", "mean_revenue", NULL, "30-Year Trend 2018-2016 Shows a Rocky trend of Film Revenues \n That Coincide with Financial Eras") +
                      scale_y_continuous(label = comma, limits = c(0, 140000000), breaks = c(seq(0, 140000000, 20000000))) +
                      labs(x = "Year of Release", y = "Unadjusted Avg.Revenue Per Film ($)",
                            subtitle = "Average industry revenues didn't ever reclaim peak performances of the 1990s until the recent decade") +
                      annotate("text", x = 2008, y = 138000000, label = "2008: \n Financial Crisis", size = 2) +
                      annotate("text", x = 2000, y = 35000000, label = "2000: \n End of Dot-Com Bubble", size = 2) +
                      annotate("text", x = 1993, y = 139000000, label = "1990s: \n Dot-Com Tech Bubble", size = 2)

trend4

# create a long dataset for stacked line graph to graph budget, revenue, profit on the same plot
long_data <- pivot_longer(data = submeta2, cols = c("mean_budget", "mean_revenue", "mean_profit"),
                                          names_to = "capital_type",
                                          values_to = "industry_average")
# make sure everything is okay
head(long_data)

# Graph 5 - Stacked Line - plot the average industry budgets, revenues, and profits
trend5 <- ggplot() + geom_area(data = long_data, mapping = aes(x = release_year, y = industry_average,
                                group = factor(capital_type),
                                fill = factor(capital_type)),
                                position = "stack") +
              scale_x_continuous(limits = c(1980, 2020), breaks = c(seq(1980, 2020, 5))) +
              scale_y_continuous(label = comma, limits = c(0, 300000000), breaks = c(seq(0, 300000000, 50000000))) +
              theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.grid.major.y = element_line(color = "gray"),
                legend.position = "bottom",
                legend.title = element_blank(),
                axis.line.x = element_line(color = "black"),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text = element_text(size = 10),
                plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#2b8cbe"),
                plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
                plot.caption = element_text(hjust = 0, size = 7.5)) +
              labs(title = "The Film Industry is Constantly Taking in Losses",
                   subtitle = "With advancements in technology inside and outside of the industry \n there has been an increasing trend of the industry taking on net losses",
                   x = "Year of Release",  y = "Unadjusted Amount ($)", caption = "source: TMBD 5000 Movies (Kaggle.com)") +
              scale_fill_manual(labels = c("revenue", "profit", "budget"), values = c("#f8766d", "#7cae00", "#045a8d")) +
                # add annotations for different decades
                annotate("text", x = 1990, y = 290000000, label = "1990s: \nRise of CGI", size = 2) +
                annotate("text", x = 2000, y = 210000000, label = "2000s: \nFantasy Franchises", size = 2) +
                annotate("text", x = 2010, y = 290000000, label = "2010s: \nReboots & Netflix", size = 2)
trend5

# Graph 6 - Line Graph - plot the industry profit trends separated by high/low rated films
trend6 <- line_funk(bar_data, "release_year", "mean_profit", "average_score.f", "30-Year Trend 1980-2016 Shows \nHigher Film Profits Coincide with Higher Ratings") +
            scale_y_continuous(label = comma, limits = c(0, 300000000), breaks = c(seq(0, 300000000, 50000000))) +
            labs(x = "Year of Release", y = "Unadjusted Avg. Profit Per Film ($)",
                 subtitle = "Although overall profits in poorly reviewed films seem more steady") +
            scale_color_manual(labels = c("User Rating < 7/10", "User Rating > 7/10"), values = c("#f8766d", "#00bfc4"))
trend6

# Graph 7 - Line Graph - look at the # of votes and the overall profits separated by high/low ratings
trend7 <- line_funk(bar_data, "release_year", "mean_users", "average_score.f", "Higher Rated Films Coincide with Larger User Vote Counts") +
            scale_y_continuous(label = comma, limits = c(0, 3500), breaks = c(seq(0, 3500, 500))) +
            labs(x = "Year of Release", y = "Avg. Number of User Votes Per Film",
                 subtitle = "The trend of an increasing overall number of users per year \n can be explained by the increasing presence of internet use in the past 2 decades.") +
            scale_color_manual(labels = c("User Rating < 7/10", "User Rating > 7/10"), values = c("#f8766d", "#00bfc4")) +
            # add annotations
            annotate("text", x = 1990, y = 2900, label = "1990: \n IMDB is born", size = 2)
            annotate("text", x = 2005, y = 2900, label = "2008: \n FB and social media rapidly grows", size = 2)

trend7

# Save graphs
graph1 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend1.jpg")
graph2 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend2.jpg")
graph3 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend3.jpg")
graph4 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend4.jpg")
graph5 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend5.jpg")
graph6 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend6.jpg")
graph7 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/trend7.jpg")

ggsave(plot = trend1,
       file = graph1,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)

ggsave(plot = trend2,
       file = graph2,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)

ggsave(plot = trend3,
       file = graph3,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)

ggsave(plot = trend4,
       file = graph4,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)

ggsave(plot = trend5,
       file = graph5,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)

ggsave(plot = trend6,
       file = graph6,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)

ggsave(plot = trend7,
       file = graph7,
       dpi = 300,
       units = c("in"),
       width = 7,
       height = 3.5)
