# Data Visualization - EChen 3/27/2020
# Policy Brief
# Start Code
# ---------------------------------------------------------------------------------------
# Load packages ggplot, scales, dplyr, tidyr
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(data.table)

rm(list = ls())
# Load SubMeta dataset and IMDB data sets
myData <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/SubMeta.csv")

# this file contains submeta data on the director and actor levels
movies.meta <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/movie_metadata 2.csv")
movies.meta <- movies.meta[,c("movie_title", "director_name", "actor_1_name", "actor_2_name", "actor_3_name")]

movies.meta <- movies.meta %>% rename(original_title = movie_title)


info.merge <- merge(x = myData, y = movies.meta, by = "original_title", all = TRUE)

info.merge <- info.merge[which(!is.na(info.merge$actor_1_name) == TRUE),]
info.merge <- info.merge[which(!is.na(info.merge$release_year) == TRUE),]
info.merge <- info.merge[which(!is.na(info.merge$genres.y) == TRUE),]

info.merge <- info.merge[!duplicated(info.merge$imdb_id),]

info.merge$profit_margin <- info.merge$profit/info.merge$real_revenue
info.merge$ln_budget <- log10(info.merge$real_budget)
info.merge$ln_revenue <- log10(info.merge$real_revenue)

# End of Merge - Start subsetting datasets
# ---------------------------------------------------------------------------------------------
# subset by datasets w/o famous actors and actresses

target <- c("Meryl Streep", "Hugh Jackman", "Will Smith", "Scarlett Johansson", "Dwayne Johnson",
              "Natalie Portman", "Denzel Washington", "Robert Downey Jr.", "Leonardo DiCaprio", "Kate Winslet",
                "Charlize Theron", "Angelina Jolie Pitt")

target2 <- c("Martin Scorsese",  "Quentin Tarantino", "Ridley Scott", "Christopher Nolan", "Steven Spielberg", "Clint Eastwood")
no_big_names <- info.merge[!(info.merge$actor_1_name %in% target|info.merge$actor_2_name %in% target|info.merge$actor_3_name %in% target | info.merge$director_name %in% target2),]

# subset data by actors, directors

# Meryl Streep
meryl.streep <- filter(info.merge, actor_1_name == "Meryl Streep" | actor_2_name == "Meryl Streep" | actor_3_name == "Meryl Streep")
meryl.streep <- meryl.streep[-c(10),]
meryl.streep$presence <- "Meryl Streep"

# Hugh Jackman
hugh.jackman <- filter(info.merge, actor_1_name == "Hugh Jackman" | actor_2_name == "Hugh Jackman" | actor_3_name == "Hugh Jackman")
hugh.jackman$presence <- "Hugh Jackman"

# Will Smith
will.smith <- filter(info.merge, actor_1_name == "Will Smith" | actor_2_name == "Will Smith" | actor_3_name == "Will Smith")
will.smith$presence <- "Will Smith"

# Scarlett Johansson
scarlett.jo <- filter(info.merge, actor_1_name == "Scarlett Johansson" | actor_2_name == "Scarlett Johansson" | actor_3_name == "Scarlett Johansson")
scarlett.jo <- scarlett.jo[-c(12),]
scarlett.jo$presence <- "Scarlett Johansson"

# Dwayne Johnson
dwayne.jo <- filter(info.merge, actor_1_name == "Dwayne Johnson" | actor_2_name == "Dwayne Johnson" | actor_3_name == "Dwayne Johnson")
dwayne.jo <- dwayne.jo[-c(9),]
dwayne.jo$presence <- "Dwayne Johnson"

# Denzel Washington
denz.wash <- filter(info.merge, actor_1_name == "Denzel Washington" | actor_2_name == "Denzel Washington" | actor_3_name == "Denzel Washington")
denz.wash$presence <- "Denzel Washington"

# Robert Downey Jr.
robert.down <- filter(info.merge, actor_1_name == "Robert Downey Jr." | actor_2_name == "Robert Downey Jr." | actor_3_name == "Robert Downey Jr.")
robert.down <- robert.down[-c(15),]
robert.down$presence <- "Robert Downey Jr."

# Leonardo DiCaprio
leo.cap <- filter(info.merge, actor_1_name == "Leonardo DiCaprio" | actor_2_name == "Leonardo DiCaprio" | actor_3_name == "Leonardo DiCaprio")
leo.cap <- leo.cap[-c(15),]
leo.cap$presence <- "Leonardo DiCaprio"

# Kate Winslet
kate.wins <- filter(info.merge, actor_1_name == "Kate Winslet" | actor_2_name == "Kate Winslet" | actor_3_name == "Kate Winslet")
kate.wins$presence <- "Kate Winslet"


# Charlize Theron
charlize.theron <- filter(info.merge, actor_1_name == "Charlize Theron" | actor_2_name == "Charlize Theron" | actor_3_name == "Charlize Theron")
charlize.theron$presence <- "Charlize Theron"

# Angelina Jolie Pitt
angelina.jo <- filter(info.merge, actor_1_name == "Angelina Jolie Pitt" | actor_2_name == "Angelina Jolie Pitt" | actor_2_name == "Angelina Jolie Pitt")
angelina.jo$presence <-"Angelina Jolie Pitt"

# Julia Roberts
julia.ro <- filter(info.merge, actor_1_name == "Julia Roberts" | actor_2_name == "Julia Roberts" | actor_2_name == "Julia Roberts")
julia.ro$presence <-"Julia Roberts"

# emma stone
emma.stone <- filter(info.merge, actor_1_name == "Emma Stone" | actor_2_name == "Emma Stone" | actor_2_name == "Emma Stone")
emma.stone$presence <-"Emma Stone"

# big directors
director.films <- filter(info.merge, director_name == "Steven Spielberg" | director_name == "Clint Eastwood" | director_name == "Martin Scorsese" | director_name == "Christopher Nolan" | director_name == "Quentin Tarantino" | director_name == "Tim Burton" | director_name == "Ridley Scott")

actors.films <- rbind(hugh.jackman, will.smith, dwayne.jo, denz.wash, robert.down, leo.cap, meryl.streep, scarlett.jo, charlize.theron, angelina.jo, julia.ro, emma.stone)
actresses.films <- rbind(meryl.streep, scarlett.jo, kate.wins, charlize.theron, angelina.jo, julia.ro)
all_stars <- rbind(actors.films, actresses.films)

# ----------------------------------------------------------------------------------------------------------------
# transform all data sets

actors.films <- group_by(.data = actors.films, presence)
actors_data2 <- summarise(.data = actors.films, no_movies = n(), avg_profit_marg = mean(profit_margin), avg_budget = mean(real_budget), avg_revenue = mean(real_revenue), ln_budget = log(avg_budget), ln_sales = log(avg_revenue))

actresses.films <- group_by(.data = actresses.films, presence)
actresses_data2 <- summarise(.data = actresses.films, no_movies = n(), avg_profit_marg = mean(profit_margin), avg_budget = mean(real_budget), avg_revenue = mean(real_revenue), ln_budget = log(avg_budget), ln_sales = log(avg_revenue))

actors.films <- group_by(.data = actors.films, release_year)
actors_data3 <- summarize(.data = actors.films, no_movies = n(), avg_budget = mean(real_budget), avg_revenue = mean(real_revenue), ln_budget = log(avg_budget), ln_sales = log(avg_revenue))
long_data <- pivot_longer(data = actors_data3, cols = c("ln_budget", "ln_sales"), names_to = "capital_type", values_to = "star.average")

actresses.films <- group_by(.data = actresses.films, release_year)
actresses_data3 <- summarize(.data = actresses.films, no_movies = n(), avg_budget = mean(real_budget), avg_revenue = mean(real_revenue), ln_budget = log(avg_budget), ln_sales = log(avg_revenue))
long_data2 <- pivot_longer(data = actresses_data3, cols = c("ln_budget", "ln_sales"), names_to = "capital_type", values_to = "star.average")

all_stars$marker <- as.factor(1)
no_big_names$presence <- "none"
no_big_names$marker <- as.factor(0)
director.films$presence <- director.films$director_name
director.films$marker <- as.factor(1)


combine_actors <- rbind(all_stars, no_big_names)
combine_actors <- group_by(.data = combine_actors, presence, marker)
high_level_actors <- summarize(.data = combine_actors, no_movies = n(), avg_profit_marg = mean(profit_margin), avg_budget = mean(real_budget), ln_budget = log(avg_budget))
write.csv(combine_actors, "/Users/erikchen/Desktop/Data Viz Programs/Tutorial 10/Data Files/combine_actors.csv")


combine_directors <- rbind(director.films, no_big_names)
combine_directors <- group_by(.data = combine_directors, presence, marker)
high_level_directors<- summarize(.data = combine_directors, no_movies = n(), avg_profit_marg = mean(profit_margin), avg_budget = mean(real_budget), ln_budget = log10(avg_budget))


# directors
director.films <- group_by(.data = director.films, director_name, release_year)

director_data2 <- summarize(.data = director.films, no_movies = n(), avg_profit_marg = mean(profit_margin), avg_budget = mean(real_budget), avg_revenue = mean(real_revenue), ln_budget = log(avg_budget), ln_sales = log(avg_revenue))
long_data3 <- pivot_longer(data = director_data2, cols = c("ln_budget", "avg_profit_marg"), names_to = "capital_type", values_to = "director.average")

director.films <- group_by(.data = director.films, director_name)
director_bardata <- summarize(.data = director.films, no_movies = n(), avg_profit_marg = mean(profit_margin))

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# GRAPHS

# FUNCTIONS
# ----------------------------------------------------------------------------------------------------------
# create a function that will shorten our y-axis labels for values over 100,000
unitLabels <- function(num){
  labels <- ifelse(num < 1000, num,
                   ifelse(num < 10^6, paste0(round(num/10^3), "K"),
                          ifelse(num < 10^9, paste0(round(num/10^6), "M"),
                                 ifelse(num < 10^12, paste0(round(num/10^9), "B")))))
  return(labels)
}

point_funk <- function(dataset, var_x, var_y, the_color, var_title){
  my_plot <- ggplot() + geom_point(data = dataset, mapping = aes_string(x = var_x, y = var_y, color = the_color), size = 1) +
    #geom_point(data = dataset, mapping = aes_string(x = var_x, y = var_y), size = 2) +
    geom_hline(yintercept = 0, size =1, color = "black", linetype = "longdash") +
    labs(x = "Film Budget*", y = "Profit Margin", title = var_title, caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com) \n*Adjusted to 2016 Dollars with Logarithmic Scale") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(color = "gray"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key= element_rect(fill = "white"),
          legend.box.background = element_rect(color = "black", linetype = "dashed"),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(vjust = -2),
          plot.title = element_text(face = "bold", hjust = 0, size = 12, color = "#2b8cbe"),
          plot.subtitle = element_text(hjust = , face = "bold", size = 7.5),
          plot.caption = element_text(hjust = 0, size = 7.5),
          plot.margin = unit(c(5.5,5.5,5.5,5.5), "points"))
  print(my_plot)
}
# --------------------------------------------------------------------------------------------------
# BAR GRAPHS

## ACTORS ##
# Profit Margins, Lollipops
actors.graph5 <- ggplot(data = actors_data2) + geom_point(mapping = aes(x = reorder(presence, avg_profit_marg), y = avg_profit_marg), color = "orange", size = 2) +
  geom_segment(mapping = aes(x = presence, xend = presence, y = 0, yend = avg_profit_marg), color = "orange", size = 1, linetype = "dashed") +
  scale_y_continuous(labels = percent, limits = c(0, 0.6), breaks = c(seq(0, 0.6, 0.1))) +
  labs(title = "Acclaimed Actors' Average Profit Margins Over the Span of Their Careers", 
       subtitle = "A glance at some of the industry's largest names show large, positive returns \n",
       x = "Actor",  y = "Average Profit Margin", caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com)") +
  
  coord_flip() +
  # aesthetics
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))

actors.graph5

actors.graph6 <- ggplot(data = actors_data2) + geom_point(mapping = aes(x = reorder(presence, avg_profit_marg), y = avg_profit_marg),
                                                          color = ifelse(actors_data2$avg_profit_marg > 0.4, "orange", "gray"), 
                                                          size = ifelse(actors_data2$avg_profit_marg > 0.4, 2,1)) +
  geom_segment(mapping = aes(x = presence, xend = presence, y = 0, yend = avg_profit_marg), 
               color = ifelse(actors_data2$avg_profit_marg > 0.4, "orange", "gray"),
               size = ifelse(actors_data2$avg_profit_marg > 0.4, 1.5, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.6), breaks = c(seq(0, 0.6, 0.1))) +
  geom_hline(yintercept = 0.4, color = "black", linetype = "dashed") +
  labs(title = "Acclaimed Actors' Average Profit Margins Over the Span of Their Careers", 
       subtitle = "The majority of these stars have shown profit margins of over 40% with their film presence\n",
       x = "Actor",  y = "Average Profit Margin", caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com)") +
  
  coord_flip() +
  # aesthetics
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))

actors.graph6

actors.graph7 <- ggplot(data = actors_data2) + geom_point(mapping = aes(x = reorder(presence, avg_profit_marg), y = avg_profit_marg),
                                                          color = ifelse(actors_data2$avg_profit_marg > 0.5, "orange", "gray"), 
                                                          size = ifelse(actors_data2$avg_profit_marg > 0.5, 2,1)) +
  geom_segment(mapping = aes(x = presence, xend = presence, y = 0, yend = avg_profit_marg), 
               color = ifelse(actors_data2$avg_profit_marg > 0.5, "orange", "gray"),
               size = ifelse(actors_data2$avg_profit_marg > 0.5, 1.5, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 0.6), breaks = c(seq(0, 0.6, 0.1))) +
  geom_hline(yintercept = 0.4, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  labs(title = "Acclaimed Actors' Average Profit Margins Over the Span of Their Careers", 
       subtitle = "And these top 4 names in films has yielded average profit margins above 50% \n",
       x = "Actor",  y = "Average Profit Margin", caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com)") +
  
  coord_flip() +
  # aesthetics
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))

actors.graph7

# -----------------------------------------------------------------------------------------------------
## DIRECTORS ##

directors.graph1 <- ggplot(data = director_bardata) + geom_point(mapping = aes(x = reorder(director_name, avg_profit_marg), y = avg_profit_marg), color = "orange") +
  geom_segment(mapping = aes(x = director_name, xend = director_name, y = 0, yend = avg_profit_marg), color = "orange", linetype = "dashed") +
  scale_y_continuous(labels = percent, limits = c(0, 1), breaks = c(seq(0,1,0.25 ))) +
  labs(title = "Acclaimed Directors' Average Profit Margins Over the Span of Their Careers", 
       subtitle = "Generally, films with a prominent director produces even larger returns than with just star presence\n",
       x = "Director",  y = "Average Profit Margin", caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com)") +
  coord_flip() +
  # aesthetics
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))

directors.graph1

directors.graph2 <- ggplot(data = director_bardata) + geom_point(mapping = aes(x = reorder(director_name, avg_profit_marg), y = avg_profit_marg), 
                                                                 color = ifelse(director_bardata$avg_profit_marg > 0.5, "orange", "gray"),
                                                                 size = ifelse(director_bardata$avg_profit_marg > 0.5, 2, 1)) +
  geom_segment(mapping = aes(x = director_name, xend = director_name, y = 0, yend = avg_profit_marg),
                color = ifelse(director_bardata$avg_profit_marg > 0.5, "orange", "gray"),
                size = ifelse(director_bardata$avg_profit_marg > 0.5, 1.5, 1))+
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.6, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = percent, limits = c(0, 1), breaks = c(seq(0,1,0.25 ))) +
  labs(title = "Acclaimed Directors' Average Profit Margins Over the Span of Their Careers", 
       subtitle = "The top three industry names have produced average profit margins well over 50% \n",
       x = "Director",  y = "Average Profit Margin", caption = "source: TMBD 5000 & 45,000 Movies (Kaggle.com)") +
  
  coord_flip() +
  # aesthetics
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))

directors.graph2

# ----------------------------------------------------------------------------------------------------
## COMPARISONS & ROI ##

compare_ROI_all <- point_funk(no_big_names, "real_budget", "profit_margin", "marker", "Comparisons Across Films: Impact of Budgets on Profit Margins") +
  labs(subtitle = ("At the industry level, there is no clear relationship between films' budgets and profit margins")) +
  scale_y_continuous(limits = c(-2, 1), labels = percent) +
  scale_x_continuous(trans = "log10", labels = unitLabels, limits = c(100000, 100000000)) +
  scale_color_manual(values = c("#f8766d")) +
  theme(legend.position = "none")

compare_ROI_all



compare1 <- point_funk(no_big_names, "real_budget", "profit_margin", "marker", "Comparisons Across Films") +
              geom_point(data = all_stars, mapping = aes(x = real_budget, y = profit_margin, color = marker)) +
                  scale_y_continuous(limits = c(-2, 1), labels = percent) +
                  scale_x_continuous(trans = "log10", labels = unitLabels, limits = c(100000, 100000000)) +
                  scale_color_manual(values = c("#f8766d", "#00bfc4"), labels = c("Movies without acclaimed actors/actresses", "Movies with acclaimed actors/actresses"))

compare1

compare1_pre <- point_funk(no_big_names, "real_budget", "profit_margin", "marker", "Comparisons Across Films: Impact of Star Presence on Budgets and Profit Margins") +
   labs(subtitle = ("Without including films with prominent stars or directors, the general theme suggests: larger budget = larger risk, but also larger returns")) +
  scale_y_continuous(limits = c(-2, 1), labels = percent) +
  scale_x_continuous(trans = "log10", labels = unitLabels, limits = c(100000, 100000000)) +
  scale_color_manual(values = c("#f8766d")) +
  theme(legend.position = "none")

compare1_pre

compare1_2 <- point_funk(no_big_names, "real_budget", "profit_margin", "marker", "Comparisons Across Films: Impact of Star Presence on Budgets and Profit Margins") +
  geom_point(data = all_stars, mapping = aes(x = real_budget, y = profit_margin, color = marker)) +
  labs(subtitle = ("By adding in prominent stars from our list, the risk is somewhat lowered and films show higher than average returns")) +
  scale_y_continuous(limits = c(-2, 1), labels = percent) +
  scale_x_continuous(trans = "log10", labels = unitLabels, limits = c(100000, 100000000)) +
  scale_color_manual(values = c("gray", "#00bfc4"), labels = c("Movies without acclaimed actors/actresses", "Movies with acclaimed actors/actresses" ))

compare1_2

      
compare2 <- point_funk(no_big_names, "real_budget", "profit_margin", "marker", "Comparisons Across Films: Impact of Star Directors on Budgets and Profit Margins") +
              geom_point(data = director.films, mapping = aes(x = real_budget, y = profit_margin, color = marker)) +
                  scale_y_continuous(limits = c(-2,1), labels = percent) +
                  scale_x_continuous(trans = "log10", labels = unitLabels, limits = c(100000, 100000000)) +
                  scale_color_manual(values = c("#f8766d", "#00bfc4"), labels = c("Movies without acclaimed directors", "Movies with acclaimed directors"))

compare2


compare2_2 <- point_funk(no_big_names, "real_budget", "profit_margin", "marker", "Comparisons Across Films: Impact of Star Directors on Budgets and Profit Margins") +
  geom_point(data = director.films, mapping = aes(x = real_budget, y = profit_margin, color = marker)) +
  labs(subtitle = ("By adding in prominent directors from our list, the risk is lowered even further, and we see even higher rates of profitability")) +
  scale_y_continuous(limits = c(-2,1), labels = percent) +
  scale_x_continuous(trans = "log10", labels = unitLabels, limits = c(100000, 100000000)) +
  scale_color_manual(values = c("gray", "#00bfc4"), labels = c("Movies without acclaimed directors", "Movies with acclaimed directors"))

compare2_2


graph1 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend1.jpg")
graph2 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend2.jpg")
graph3 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend3.jpg")
graph4 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend4.jpg")
graph5 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend5.jpg")
graph6 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend6.jpg")
graph7 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend7.jpg")
graph8 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend8.jpg")
graph9 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend9.jpg")


ggsave(plot = actors.graph5,
       file = graph1,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = actors.graph6,
       file = graph2,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = actors.graph7,
       file = graph3,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)


ggsave(plot = directors.graph1,
       file = graph4,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = directors.graph2,
       file = graph5,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = compare1_pre,
       file = graph6,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = compare1_2,
       file = graph7,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = compare2_2,
       file = graph8,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)


ggsave(plot = compare_ROI_all,
       file = graph9,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

# End Actors & Directors Analysis
# ------------------------------------------------------------------------------------------------------------------
# start State Incentives Code
# 

bothdb <- readRDS("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/bothdbs.rds")
#reduce sample to 1930-2017
bothdb2 <- filter(.data = bothdb, is.na(startYear) == FALSE)
bothdb$startYear <- as.numeric(as.character(bothdb$startYear))
bothdb <- filter(.data= bothdb, startYear <= 2017)
bothdb <- filter(.data= bothdb, startYear >= 1930)
str(bothdb)

#reduce sample to only films with user vote data
reducedboth <- filter(.data = bothdb, is.na(id) == FALSE)
reducedboth <- filter(.data = reducedboth, is.na(averageRating) == FALSE)
reducedboth <- filter(.data = reducedboth, is.na(numVotes) == FALSE)
reducedboth$numVotes <- as.numeric(as.character(reducedboth$numVotes))
str(reducedboth)

#only movies with budget information available
budgetsonly <- reducedboth
budgetsonly$budget <- as.numeric(as.character(budgetsonly$budget))
budgetsonly <- filter(.data = budgetsonly, budget >=1)
budgetsonly <- filter(.data = budgetsonly, revenue >=1)
budgetsonly$profit <- budgetsonly$revenue-budgetsonly$budget
budgetsonly$margin <- budgetsonly$profit/budgetsonly$budget*100
str(budgetsonly)

#reduce dataset to get rid of some outliers
margins2 <- filter(.data=budgetsonly, margin <= 1500)
budgetgrouped <- group_by(.data=margins2, budget)
budgetgrouped <- filter(.data = budgetgrouped, budget >= 250000)
budgetgrouped2 <- summarize(.data=budgetgrouped, mn.rating=mean(margin,na.rm=TRUE))


#number of movies by release year
yearreleased <- count(bothdb, startYear)
yearreleased

all.releases <- ggplot(data = yearreleased,
                       mapping = aes(x = startYear, y = n)) +
  geom_line() +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(colour="gray"),
        panel.grid.major.x = element_line(colour="gray"),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))+
  scale_x_continuous(breaks=c(1930,1940,1950,1960,1970,1980,1990,2000,2010)) +
  scale_y_continuous(labels = unitLabels) +
  labs(title = "Number of Films Released Each Year, 1930-2017",
       subtitle = "The global film industry has seen significant growth in the 21st century",
       x = "Year",
       y = "Number of Films Released",
       caption = "Source: IMDB")
all.releases


#scores compared to profit percentage
scoregrouped <- group_by(.data=margins2, averageRating)
scoregrouped2 <- summarize(.data=scoregrouped, mn.rating=mean(margin,na.rm=TRUE))

margin.score <- ggplot(data = scoregrouped2,
                       mapping = aes(x = averageRating, y = mn.rating)) +
  geom_point() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(colour="gray"),
        panel.grid.major.x = element_line(colour="gray"),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))+
  labs(title = "Average Profit Margin of Film by Average User Rating",
       subtitle = "The better-liked a film is, the higher its profit margin",
       x = "Average IMDB User Rating",
       y = "Film Profit Margin as Percentage of Film Budget (%)",
       caption = "Source: IMDB, Kaggle")
margin.score


#loading the data about different financial incentive types
incenType <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/StateIncentiveType.csv")
str(incenType)

#differentiate the cumulative column from the others
incenType$isMPI <- 0
incenType$isMPI <- ifelse(incenType$IncentiveType=="At Least One Type",1,incenType$isMPI)
incenType$isMPI <- as.factor(incenType$isMPI)

incentive.type <- ggplot(data = incenType,
                         mapping = aes(x = IncentiveType, y = TotStates)) +
  geom_col(aes(fill=isMPI)) +
  scale_fill_manual(values=c("#00bfc4", "orange")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(colour="gray"),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))+
  theme(axis.text.x = element_text(angle=45, hjust = 0.95))  +
  ylim(0,50)+
  scale_x_discrete(limits=c("Tax Credit","Cash Rebate","Grant","Sales Tax Exemption","Lodging Exemption","Fee-Free Locations","At Least One Type")) +
  labs(title = "Number of States Employing Different Film Production Incentives",
       subtitle = "43 states employ at least one type of incentive, while many use more than one",
       x = "",
       y = "Number of States",
       caption = "Source: Tax Foundation")
incentive.type


#now load data of minimum spending against incentives
incenAmount <- read.csv("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Data Files/StateIncentives.csv")

incentive.amount <- ggplot(data = incenAmount,
                           mapping = aes(x = SpendMin, y = IncentiveMin)) +
  geom_point() +
  scale_x_log10(labels = unitLabels, breaks=c(10000,30000,100000,300000,1000000,3000000,10000000)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "gray"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(colour="gray"),
        panel.grid.major.x = element_line(colour="gray"),
        plot.subtitle = element_text(hjust = 0, face = "bold", size = 7.5),
        plot.caption = element_text(hjust = 0, size = 7.5))+
  annotate(geom = "text", x=5600000, y=26.5, label="NC", color = "black", size=3, hjust=1) +
  annotate(geom = "text", x=56000, y=36.5, label="OK", color = "black", size=3, hjust=1) +
  annotate(geom = "text", x=1150000, y=21.5, label="CA", color = "black", size=3, hjust=1) +
  annotate(geom = "text", x=560000, y=31.5, label="GA", color = "black", size=3, hjust=1) +
  annotate(geom = "text", x=20000, y=31.5, label="NY", color = "black", size=3, hjust=1) +
  annotate(geom = "text", x=275000, y=6.5, label="TX", color = "black", size=3, hjust=1) +
  #annotate(geom = "text", x=56000, y=26.5, label="MA", color = "black", size=3, hjust=1) +
  #annotate(geom = "text", x=360000, y=16.5, label="MT", color = "black", size=3, hjust=1) +
  #annotate(geom = "text", x=220000, y=26.5, label="TN", color = "black", size=3, hjust=1) +
  #annotate(geom = "text", x=85000, y=16.5, label="ME", color = "black", size=3, hjust=1) +
  ylim(2,36.5)+
  labs(title = "Lowest Awardable Incentive Level by Production Budget Minimum",
       x = "Minimum Spending Required ($)",
       y = "Lowest Possible Tax Incentive (%)",
       caption = "Source: Hollywood Reporter\nSome points represent more than one state.")
incentive.amount


graph10 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend10.jpg")
graph11 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend11.jpg")
graph12 <- ("/Users/erikchen/Desktop/Data Viz Programs/Policy Brief/Round 3/trend12.jpg")

ggsave(plot = all.releases,
       file = graph10,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

ggsave(plot = incentive.amount,
       file = graph11,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)


ggsave(plot = incentive.type,
       file = graph12,
       dpi = 300,
       units = c("in"),
       width = 8,
       height = 4)

# End Code

