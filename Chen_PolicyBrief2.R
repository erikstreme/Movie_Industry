# Data Visualization - EChen
# Policy Brief
# Start Code
# ---------------------------------------------------------------------------------------
# Load packages ggplot, scales, dplyr, tidyr
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

# Load TMDB 45000 data from Kaggle.com
# Load TMDB 5000 data from Kaggle.com
meta.data.small <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/tmdb-movie-metadata/tmdb_5000_movies.csv")
meta.data.large <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/the-movies-dataset/movies_metadata.csv")

# look at the structures
str(meta.data.large)
str(meta.data.small)

# turn budget in large dataset to int
meta.data.large$budget <- as.numeric(as.character(meta.data.large$budget))
str(meta.data.large)
# make sure our dates are actually dates
meta.data.small$release_date <- as.Date(meta.data.small$release_date)
meta.data.large$release_date <- as.Date(meta.data.large$release_date)

# extract year and convert to numeric format
meta.data.small$release_year <- as.numeric(format(meta.data.small$release_date, "%Y"))
meta.data.large$release_year <- as.numeric(format(meta.data.large$release_date, "%Y"))

meta.data.small <- meta.data.small[which(meta.data.small$budget != 0),]
meta.data.small <- meta.data.small[which(meta.data.small$revenue != 0),]
meta.data.large <- meta.data.large[which(meta.data.large$budget != 0),]
meta.data.large <- meta.data.large[which(meta.data.large$revenue != 0),]

# merge data sets
meta.data2 <- merge(x = meta.data.small, y = meta.data.large, by = c("original_title", "release_year", "budget", "revenue"), all = TRUE)
str(meta.data2)
# remove NA budgets and years
meta.data2 <- meta.data2[which(!is.na(meta.data2$budget & meta.data2$revenue & meta.data2$release_year) == TRUE),]
meta.data2 <- meta.data2[which(meta.data2$release_year >= 1970 & meta.data2$release_year < 2017),]

# replace NA vote counts and averages and make one column
meta.data2$vote_average <- ifelse(!is.na(meta.data2$vote_average.y), meta.data2$vote_average.y, meta.data2$vote_average.x)
meta.data2$vote_count <- ifelse(!is.na(meta.data2$vote_count.y), meta.data2$vote_count.y, meta.data2$vote_count.x)

# clean up all NAs in these columns
meta.data2 <- meta.data2[which(!is.na(meta.data2$vote_average)),]
meta.data2 <- meta.data2[which(!is.na(meta.data2$vote_count)),]

# load CPI Index
cpi <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/CPI_Index.csv")
str(cpi)

# merge datasets
meta.data2 <- merge(x = meta.data2, y = cpi, by = "release_year", all = TRUE)

# adjust budgets & revenues for inflation, expressed in 2016 dollars
meta.data2$real_budget <- meta.data2$budget*(240.008/meta.data2$cpi_value)
meta.data2$real_revenue <- meta.data2$revenue*(240.008/meta.data2$cpi_value)

# make a subset of only relevant columns
sub.meta <- meta.data2[, c("original_title", "release_year", "real_budget", "real_revenue", "genres.y", "vote_average", "vote_count", "imdb_id")]
# make a factor variable that separates highly rated films from the ones with low scores
# defining a low score as being less than 7/10
sub.meta$average_score <- ifelse(sub.meta$vote_average >= 7, 1,0)
                                  
# create profit variable
sub.meta$profit <- sub.meta$real_revenue-sub.meta$real_budget

# export as a csv
write.csv(sub.meta, "/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/SubMeta.csv")
# set data up for line graph data
submeta2 <- group_by(.data = sub.meta, release_year, average_score)
# create another factor that separates highly rated films with high budgets from all other films
sub.meta$budget.score <- ifelse(sub.meta$average_score == 1 & sub.meta$real_budget >= 50*10^6, 1, 0)
# create dataset for bar graph 1 that separates highly rated films from poorly rated films
# also create averages for each year on budget, profit, and user votes (not interested in revenue here)
overview_data <- summarize(.data = submeta2, no_movies = n(), mean_budget = mean(real_budget), mean_profit = mean(profit), mean_users = mean(vote_count))
# take out budgets that are 0s
overview_data <- overview_data[which(overview_data$mean_budget != 0),]
# make our user score factor variable a factor to use as our color for our line graph
overview_data$average_score.f <- as.factor(overview_data$average_score)
# prep large data set for the creation of 2nd smaller dataset
submeta2 <- group_by(.data = sub.meta, release_year, budget.score)
# create 2nd dataset for bar graph 2 - films are separated by user rating & budget
# also create averages for each year on budget, profit, and user votes (also not interested in revenue here)
overview_data2 <- summarize(.data = submeta2, no_movies = n(), mean_budget = mean(real_budget), mean_profit = mean(profit), mean_users = mean(vote_count))
# remove 0s
overview_data2 <- overview_data2[which(overview_data2$mean_budget != 0),]
# make our factor a factor to use as color for line graph
overview_data2$budget.score.f <- as.factor(overview_data2$budget.score)
# prep large dataset for 3rd smaller data set that doesn't have distinct factor separation
submeta2 <- group_by(.data = sub.meta, release_year)
# make 3rd data set for line and stacked line graphs
# also create averages for each year on budget, revenue, profit, and user votes
overview_data3 <- summarize(.data = submeta2, no_movies = n(), mean_budget = mean(real_budget), mean_revenue = mean(real_revenue), mean_profit = mean(profit), mean_users = mean(vote_count))
# remove 0s
overview_data3 <- overview_data3[which(overview_data3$mean_budget != 0),]
# make our factor for highly rated films a factor
overview_data3$average_score.f <- as.factor(overview_data3$average_score)
# make sure everything looks right
str(overview_data3)
# --------------------------------------------------------------------------------------
##### Graphs ######
# --------------------------------------------------------------------------------------
# create function for basic line graphs (trend 3, 4, 6, and 7)
line_funk <- function(dataset, var_x, var_y, the_color, var_title){
  my_line <- ggplot() + geom_line(data = dataset, mapping = aes_string(x = var_x, y = var_y, color = the_color), size = 1) +
    #geom_point(data = dataset, mapping = aes_string(x = var_x, y = var_y), size = 2) +
    scale_x_continuous(limits = c(1980, 2016), breaks = c(seq(1980, 2020, 5))) +
    labs(x = "", y = "", title = var_title, caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com)") +
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
          plot.caption = element_text(hjust = 0, size = 7.5),
          plot.margin = unit(c(5.5,5.5,5.5,5.5), "points"))
  print(my_line)
}

line_funk2 <- function(dataset, var_x, var_y, the_color, var_title){
  my_line <- ggplot() + geom_line(data = dataset, mapping = aes_string(x = var_x, y = var_y, color = the_color), size = 1) +
    #geom_point(data = dataset, mapping = aes_string(x = var_x, y = var_y), size = 2) +
    scale_x_continuous(limits = c(1980, 2016), breaks = c(seq(1980, 2020, 5))) +
    labs(x = "", y = "", title = var_title, caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com) \n*Adjusted to 2016 Dollars") +
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
          plot.caption = element_text(hjust = 0, size = 7.5),
          plot.margin = unit(c(5.5,5.5,5.5,5.5), "points"))
  print(my_line)
}
# create a function that will shorten our y-axis labels for values over 100,000
unitLabels <- function(num){
  labels <- ifelse(num < 1000, num,
                   ifelse(num < 10^6, paste0(round(num/10^3), "K"),
                          ifelse(num < 10^9, paste0(round(num/10^6), "M"),
                                 ifelse(num < 10^12, paste0(round(num/10^9), "B")))))
  return(labels)
}

# Graph 1 - Bar Graph - look at the number of films released over 30 years separated by high/low ratings

trend1 <- line_funk(overview_data, "release_year", "no_movies", "average_score.f",
                   "A 30-Year Distribution of Films 1980-2016: \n Increase of Quantity, Not Quality") +
            scale_y_continuous(limits = c(0,250), breaks = c(seq(0,250,50))) +
            labs(subtitle = "As the number of films released increased over time, \n the number of low-rated films outpaced those of 'better' quality",
                x = "Year of Release",  y = "Number of Films") +
          # aesthetics
            theme(panel.background = element_blank(),
                  axis.line = element_line(color = "gray"),
                  plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
                  legend.title = element_blank(),
                  plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
                  plot.caption = element_text(hjust = 0, size = 7.5)) +
          # change legend labels
          scale_color_manual(labels = c("User Rating < 7/10", "User Rating > 7/10"), values = c("#f8766d", "#00bfc4"))
trend1

# Graph 2 - Bar Graph - look at the number of films released over 30 years that had high budgeted and received high ratings

trend2 <- line_funk(overview_data2, "release_year", "no_movies", "budget.score.f", 
                    "A 30-Year Distribution of Films 1980-2016: \n Big Budgets Don't Help with Reviews") +
            scale_y_continuous(limits = c(0,250), breaks = c(seq(0,250,50))) +
            labs(subtitle = "Surprisingly, a rise in film budgets (especially in the 1990s) over time \ndid not lead to a corresponding rise of film quality & user satisfaction \n",
                  x = "Year of Release",  y = "Number of Films") +
            theme(panel.background = element_blank(),
                  axis.line = element_line(color = "gray"),
                  plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
                  legend.title = element_blank(),
                  plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7.5),
                  plot.caption = element_text(hjust = 0, size = 7.5)) +
                  scale_color_manual(labels = c("All other films", "Rating > 7/10 & \n $50M+ Budget"), values = c("#f8766d", "#00bfc4"))
trend2

# Graph 3 - Line Graph - plot the industry budget trend

trend3 <- line_funk2(overview_data3, "release_year", "mean_budget", NULL, "Film Budgets Steadily Increase From 1980 Until Financial Crisis") +
  scale_y_continuous(labels = unitLabels, limits = c(0, 60000000)) +
  labs(x = "Year of Release", y = "Avg. Budget Per Film ($)*",
       subtitle = "The steady increase of film budgets over time corresponds to \n the exponential growth of surrounding technology \nup until the years leading up to the 2008 Recession") +
  annotate(geom = "segment", x = 1990, y = 0, xend = 1990, yend = 10000000, color = "#7cae00") +
  annotate(geom = "segment", x = 1990, y = 19000000, xend = 1990, yend = 60000000, color = "#7cae00") +
  annotate(geom = "segment", x = 2000, y = 0, xend = 2000, yend = 20000000, color = "#74a9cf") +
  annotate(geom = "segment", x = 2000, y = 34000000, xend = 2000, yend = 60000000, color = "#74a9cf") +
  annotate(geom = "segment", x = 2008, y = 0, xend = 2008, yend = 45000000, color = "#f8766d") +
  annotate(geom = "segment", x = 2008, y = 56000000, xend = 2008, yend = 60000000, color = "#f8766d") +
  annotate("text", x = 1990, y = 14000000, label = "1990s: \n Dot-Com Bubble", size = 3.5) +
  annotate("text", x = 2008, y = 50000000, label = "2008: \n Financial Recession", size = 3.5) +
  annotate("text", x = 2000, y = 27000000, label = "2000s:\n End: Disney Renaissance \n Rise: Computer Animation", size = 3)

trend3

# Graph 4 - Line Graph - plot the industry revenue trend
trend4 <- line_funk2(overview_data3, "release_year", "mean_revenue", NULL, "30-Year Trend 2018-2016 Shows a Rocky/Stagnant trend of Film Revenues \n That Coincide with Financial Eras \n") +
  scale_y_continuous(label = unitLabels, limits = c(0, 250000000)) +
  labs(x = "Year of Release", y = "Avg.Revenue Per Film ($)*",
       subtitle = "Average industry revenues didn't ever reclaim peak performances of the 1980s & 1990s") +
  annotate(geom = "segment", x = 1990, y = 0, xend = 1990, yend = 160000000, color = "#7cae00") +
  annotate(geom = "segment", x = 1990, y = 215000000, xend = 1990, yend = 250000000, color = "#7cae00") +
  annotate(geom = "segment", x = 2000, y = 0, xend = 2000, yend = 47000000, color = "#74a9cf") +
  annotate(geom = "segment", x = 2000, y = 85000000, xend = 2000, yend = 250000000, color = "#74a9cf") +
  annotate(geom = "segment", x = 2008, y = 0, xend = 2008, yend = 180000000, color = "#f8766d") +
  annotate(geom = "segment", x = 2008, y = 225000000, xend = 2008, yend = 250000000, color = "#f8766d") +
  annotate("text", x = 2008, y = 205000000, label = "2008: \n Financial Crisis", size = 3.5) +
  annotate("text", x = 2000, y = 65000000, label = "2000: \n End Bubble", size = 3.5) +
  annotate("text", x = 1990, y = 190000000, label = "1990s: \n Dot-Com \nTech Bubble", size = 3.5)

trend4

# create a long dataset for stacked line graph to graph budget, revenue, profit on the same plot
long_data <- pivot_longer(data = overview_data3, cols = c("mean_budget", "mean_revenue", "mean_profit"),
                          names_to = "capital_type",
                          values_to = "industry_average")
# make sure everything is okay
head(long_data)

write.csv(long_data, "/Users/erikchen/Desktop/Data Viz Programs/Tutorial 10/Data Files/film.industry_long.csv")


# Graph 5 - Stacked Line - plot the average industry budgets, revenues, and profits
trend5 <- ggplot() + geom_area(data = long_data, mapping = aes(x = release_year, y = industry_average,
                                                               group = factor(capital_type),
                                                               fill = factor(capital_type)),
                               position = "stack") +
  scale_x_continuous(limits = c(1980, 2020), breaks = c(seq(1980, 2016, 5))) +
  scale_y_continuous(label = unitLabels, limits = c(0, 500000000)) +
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
  labs(title = "The Film Industry Profit Margins Have Looked Stagnant Through the Recent Decade",
       subtitle = "Despite the advancements in technology and explosion of mega-franchises, \n the film industry's profits have seen little to no growth since the 1990s\n",
       x = "Year of Release",  y = "Amount ($)*", caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com) \n*Adjusted to 2016 Dollars") +
  scale_fill_manual(labels = c("revenue", "profit", "budget"), values = c("#f8766d", "#7cae00", "#045a8d")) +
  # add annotations for different decades
  annotate(geom = "segment", x = 1990, y = 0, xend = 1990, yend = 420000000, color = "gray") +
  annotate(geom = "segment", x = 2000, y = 0, xend = 2000, yend = 410000000, color = "gray") +
  annotate(geom = "segment", x = 2010, y = 0, xend = 2010, yend = 350000000, color = "gray") +
  annotate("text", x = 1990, y = 490000000, label = "1990s: \nRise of CGI", size = 3) +
  annotate("text", x = 2000, y = 460000000, label = "2000s: \nFantasy Franchises", size = 3) +
  annotate("text", x = 2010, y = 390000000, label = "2010s: \nReboots & Netflix", size = 3)

trend5

# Graph 6 - Line Graph - plot the industry profit trends separated by high/low rated films
trend6 <- line_funk2(overview_data, "release_year", "mean_profit", "average_score.f", "30-Year Trend 1980-2016 Shows \nHigher Film Profits Coincide with Higher Ratings") +
                scale_y_continuous(label = unitLabels, limits = c(0, 400000000)) +
                labs(x = "Year of Release", y = "Avg. Profit Per Film ($)*",
                      subtitle = "Although overall profits in poorly reviewed films seem more steady, \n suggesting even highly rated films may not perform well") +
                scale_color_manual(labels = c("User Rating < 7/10", "User Rating > 7/10"), values = c("#f8766d", "#00bfc4"))
trend6

# Graph 7 - Line Graph - look at the # of votes and the overall profits separated by high/low ratings
trend7 <- line_funk(overview_data, "release_year", "mean_users", "average_score.f", "Higher Rated Films Coincide with Larger User Vote Counts \n") +
  scale_y_continuous(label = comma, limits = c(0, 3500), breaks = c(seq(0, 3500, 500))) +
  labs(x = "Year of Release", y = "Avg. Number of User Votes Per Film",
       subtitle = "The trend of an increasing overall number of users per year \n can be explained by the increasing presence of internet use in the past 2 decades.") +
  scale_color_manual(labels = c("User Rating < 7/10", "User Rating > 7/10"), values = c("#f8766d", "#00bfc4")) +
  # add annotations
  annotate(geom = "segment", x = 1990, y = 0, xend = 1990, yend = 2500, color = "black") +
  annotate(geom = "segment", x = 1990, y = 3200, xend = 1990, yend = 3500, color = "black") +
  annotate(geom = "segment", x = 2008, y = 0, xend = 2008, yend = 2200, color = "black") +
  annotate(geom = "segment", x = 2008, y = 3000, xend = 2008, yend = 3500, color = "black") +
  annotate("text", x = 1990, y = 2900, label = "1990: \n IMDB is born", size = 3) +
  annotate("text", x = 2008, y = 2600, label = "2008: \n Social Media \n rapidly grows", size = 3)

trend7

# Save graphs
graph1 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend1.jpg")
graph2 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend2.jpg")
graph3 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend3.jpg")
graph4 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend4.jpg")
graph5 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend5.jpg")
graph6 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend6.jpg")
graph7 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 2/trend7.jpg")

ggsave(plot = trend1,
       file = graph1,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = trend2,
       file = graph2,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = trend3,
       file = graph3,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = trend4,
       file = graph4,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = trend5,
       file = graph5,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = trend6,
       file = graph6,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = trend7,
       file = graph7,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)
