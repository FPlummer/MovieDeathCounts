#Project looking at how film death counts correlate to film data; genre, rating, release date.

#Dataset accessed via https://figshare.com/articles/dataset/On_screen_movie_kill_counts_for_hundreds_of_films/889719

install.packages("here")
install.packages("tidyverse")
install.packages("ggthemes")

library(here)
library(tidyverse)
library(ggthemes)

#----DIRECTORY-SETUP----
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#---------------------IMPORT DATA--------------------------------

filepath <- file.path("data", "filmdeathcounts.csv")
rawdata <- read.csv("data/filmdeathcounts.csv")

head(rawdata)


#---------------------TIDY DATA----------------------------------

#---TIDY RATINGS---

df_v1 <- rawdata %>% 
  mutate (MPAA_Rating = case_when(
    MPAA_Rating == "Approved" ~ "PG",
    MPAA_Rating == "GP" ~ "PG",
    MPAA_Rating == "M" ~ "PG",
    MPAA_Rating == "NR" ~ "Unrated",
    TRUE ~ MPAA_Rating
    )
  )
#---ORDINAL RATINGS---

df_v2 <- df_v1 %>% 
  mutate (rating_order = case_when(
        MPAA_Rating == "G" ~ "1",
        MPAA_Rating == "PG" ~ "2",
        MPAA_Rating == "PG-13" ~ "3",
        MPAA_Rating == "R" ~ "4",
        MPAA_Rating == "X" ~ "5",
        MPAA_Rating == "Unrated" ~ "0"
    )
  )
#---DEATHS PER MINUTE---

df_v2 <- df_v2 %>% 
  mutate(dpm = Body_Count/Length_Minutes)

#----TIDIED DATA------
df_tidy <- df_v2

#--LIST GENRES---
#\\ to escape - removes special characters from being used as special characters

genres <- unlist(strsplit(df_v2$Genre, split = "\\|"))
  num_unique_genres <- length(unique(genres))
  print(paste("There are",num_unique_genres, "unique genres in this dataset"))

print(sort(unique(genres)))

#---------DUMMY CODING BY INTERESTED GENRES---

#Action
df_v2$is_action <- grepl("Action", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_action = as.numeric(is_action))
action_data <- subset(df_v2, is_action == "1")

#Crime
df_v2$is_crime <- grepl("Crime", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_crime = as.numeric(is_crime))
crime_data <- subset(df_v2, is_crime == "1")

#Fantasy
df_v2$is_fantasy <- grepl("Fantasy", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_fantasy = as.numeric(is_fantasy))
fantasy_data <- subset(df_v2, is_fantasy == "1")

#Horror
df_v2$is_horror <- grepl("Horror", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_horror = as.numeric(is_horror))
horror_data <- subset(df_v2, is_horror == "1")

#Thriller
df_v2$is_thriller <- grepl("Thriller", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_thriller = as.numeric(is_thriller))
thriller_data <- subset(df_v2, is_thriller == "1")

#Sci-Fi
df_v2$is_scifi <- grepl("Sci-Fi", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_scifi = as.numeric(is_scifi))
scifi_data <- subset(df_v2, is_scifi == "1")

#War
df_v2$is_war <- grepl("War", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_war = as.numeric(is_war))
war_data <- subset(df_v2, is_war == "1")


#----------------CHECKPOINT-----------
df_v3 <- df_v2
write.csv (df_v3, "df_v3.csv")

#----------------------------------ANALYSIS------------------------------

#----------HIGHEST BODY COUNT MOVIES: BAR CHART---------

#Arrange in descending order and then select the top 10
df_HBCM <- arrange(df_v3, desc(Body_Count))
df_HBCM <- head(df_HBCM, 10)

# Make a Bar Chart from this data frame
      #Note reordered films by body count; displays graph in descending order
ggplot(df_HBCM, aes(x = reorder(Film, Body_Count), 
                    y = Body_Count, 
                    fill = Body_Count)) +
  geom_bar(stat = "identity") +
        scale_fill_gradient2(low= "white", 
                             mid = "orange", 
                             high = "red",
                             midpoint = 600) +
  labs( title = "The Top 10 Most Death-Filled Movies",
        subtitle = "The Most On-Screen Deaths Tallied",
        x = "Film", 
        y = "On-screen deaths"
       ) +
#Black Background  
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
#Text colour swap
          plot.title = element_text(colour = "white", size = "20"),
          plot.subtitle = element_text(colour = "red", family = "", size = "12"),
#White Axes
           axis.line = element_line(colour = "white"),
           axis.text = element_text(colour = "white", size = "12"),
           axis.title = element_text(colour = "white", size = "16")) +
#Flip x and y axes for easier reading
  coord_flip()

#-------------HIGHEST BODYCOUNT GENRES-----------
df_HBCG <- arrange(df_v3, desc(dpm))
df_HBCG <- head(df_HBCG, 10)
HBCG_plot <- ggplot(df_HBCG, aes(x = Genre, y = dpm, fill = MPAA_Rating)) +
  geom_col(position = "stack") +
  labs(title = "The top 10 highest on-screen death counts",
       subtitle = "Which are the deadliest movies from between 1949 and 2013?",
       x = "Film", 
       y = "On-screen deaths per minute"
  ) +
  coord_flip() +
    theme_light()
print (HBCG_plot)
#----BODYCOUNT BY MPAA_RATING----
ggplot(df_v2, aes(x = MPAA_Rating, y = Body_Count)) +
  geom_col()


#-----------------------------------IMDB/BODYCOUNT BY GENRE-----------------------

#----ACTION ALONE------
  ggplot(action_data, aes(x = IMDB_Rating, 
                        y = Body_Count,
                        colour = factor(MPAA_Rating))) +
  geom_point(size = 2) +
  labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Action Films",
       subtitle = "Are action movies with higher body-counts better critically received?",
       x = "IMDB Rating", 
       y = "On-screen deaths",
       colour = "US Age Rating") +
  scale_y_sqrt() +
  geom_smooth(method = "lm",
              color = "black", 
              linetype = "dashed",
              se = FALSE) +
  theme_light()

#----CRIME ALONE------
  ggplot(crime_data, aes(x = IMDB_Rating, 
                        y = Body_Count,
                        colour = factor(MPAA_Rating))) +
  geom_point(size = 2) +
  labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Crime Films",
       subtitle = "Are crime movies with higher body-counts better critically received?",
       x = "IMDB Rating", 
       y = "On-screen deaths",
       colour = "US Age Rating") +
  geom_smooth(method = "lm",
              color = "black", 
              linetype = "dashed",
              se = FALSE) +
  theme_light()

#----FANTASY ALONE------
  ggplot(fantasy_data, aes(x = IMDB_Rating, 
                          y = Body_Count,
                          colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Fantasy Films",
         subtitle = "Are fantasy movies with higher body-counts better critically received?",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
    scale_y_sqrt() +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_light()
  
#----HORROR ALONE------
  ggplot(horror_data, aes(x = IMDB_Rating, 
                        y = Body_Count,
                        colour = factor(MPAA_Rating))) +
  geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Horror Films",
         subtitle = "Are horror movies with higher body-counts better received?",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
  geom_smooth(method = "lm",
              color = "black", 
              linetype = "dashed",
              se = FALSE) +
  theme_light()
  
#----THRILLER ALONE------
  ggplot(thriller_data, aes(x = IMDB_Rating, 
                          y = Body_Count,
                          colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Thriller Films",
         subtitle = "Are thriller movies with higher body-counts better critically received?",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
    scale_y_sqrt() +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_light()

#----SCI-FI ALONE------
  ggplot(scifi_data, aes(x = IMDB_Rating, 
                         y = Body_Count,
                         colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Sci-Fi Films",
         subtitle = "Are Sci-Fi movies with higher body-counts better critically received?",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
    scale_y_sqrt() +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_light()
 
#----WAR ALONE------
  ggplot(war_data, aes(x = IMDB_Rating, 
                         y = Body_Count,
                         colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in War Films",
         subtitle = "Are War movies with higher body-counts better critically received?",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
    scale_y_sqrt() +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_light() 
  
#---------------------------------IMDB BY DEATHS PER MINUTE within GENRE-----------

#----ACTION ALONE------
 ggplot(action_data, aes(x = IMDB_Rating, 
                         y = dpm,
                         colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths per minute in Action Films",
         subtitle = "Are Action movies with frequent on screen deaths better critically received?",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_light()

#----FANTASY ALONE------
  ggplot(fantasy_data, aes(x = IMDB_Rating, 
                          y = dpm,
                          colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    facet_wrap(~action_plot)
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths per minute in Fantasy Films",
         subtitle = "Are Fantasy movies with frequent on screen deaths better critically received?",
         x = "IMDB Rating", 
         y = "Kills Per Minute",
         colour = "US Age Rating") +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_light()
  
        