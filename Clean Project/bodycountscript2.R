#Project looking at how film death counts correlate to film data; genre, rating, release date.

#Dataset accessed via https://figshare.com/articles/dataset/On_screen_movie_kill_counts_for_hundreds_of_films/889719

install.packages("here")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("gridExtra")

library(here)
library(tidyverse)
library(ggthemes)
library(gridExtra)

#----DIRECTORY-SETUP----
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)


#---------------------IMPORT DATA--------------------------------

filepath <- file.path("data", "filmdeathcounts.csv")
rawdata <- read.csv("data/filmdeathcounts.csv")

head(rawdata)


#---------------------TIDY DATA----------------------------------

#---TIDY RATINGS---
       #Convert outdated ratings to modern equivalent

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
     
#Adding an order to MPA ratings for graphical purposes
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

#Save data
    write.csv (df_v2, "df_v2.csv")
        
#--IDENTIFY AND LIST UNIQUE GENRES---

#\\ to escape - removes special characters from being used as special characters

genres <- unlist(strsplit(df_v2$Genre, split = "\\|"))
  num_unique_genres <- length(unique(genres))
  print(paste("There are",num_unique_genres, "unique genres in this dataset"))

print(sort(unique(genres)))


#---------DUMMY CODING BY INTERESTED GENRES---
#1. Identify key genre title within genres using grepl().

#2. Then generate new column tagging rows as being within that genre.

#3. Finally, create subset of only films in that genre

#-Action-
df_v2$is_action <- grepl("Action", df_v2$Genre)
df_v2 <- df_v2 %>%
  mutate(is_action = as.numeric(is_action))
action_data <- subset(df_v2, is_action == "1")

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
#Save data
  df_v3 <- df_v2
    write.csv (df_v3, "df_v3.csv")

#----------------------------------VISUALISATIONS------------------------------

#--------------HIGHEST BODY COUNT MOVIES----------------

#Arrange in descending order and then select the top 10
df_HBCM <- arrange(df_v3, desc(Body_Count))
df_HBCM <- head(df_HBCM, 15)

# Make a Bar Chart from this data frame
      #Note reordered films by body count; displays graph in descending order
top15_HBCM <- 
  ggplot(df_HBCM, aes(x = reorder(Film, Body_Count), 
                    y = Body_Count, 
                    fill = Body_Count)) +
  geom_bar(stat = "identity") +
          scale_fill_gradient2(low= "white", 
                             mid = "orange", 
                             high = "red",
                             midpoint = 600) +
  labs( title = "The Top 15 Most Death-Filled Movies (1949-2013)",
        subtitle = "The Most On-Screen Deaths Tallied",
        x = "Film", 
        y = "On-screen deaths",
        fill = "Number of deaths"
       ) +
#Black Background  
  theme(panel.background = element_rect(fill = "grey25"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
#Text colour swap
          plot.title = element_text(colour = "white", size = "20"),
          plot.subtitle = element_text(colour = "red3", size = "12"),
          legend.text = element_text(colour = "white"),
          legend.title = element_text(colour = "white"),
#White Axes
           axis.line = element_line(colour = "white"),
           axis.text = element_text(colour = "white", size = "12"),
           axis.title = element_text(colour = "white", size = "16")) +
#Flip x and y axes for easier reading
  coord_flip()

#Print and Save
print(top15_HBCM, width = 12, height = 6, dpi = 300)

ggsave("top15deathsmovies.png", top15_HBCM, width = 12, height = 6, dpi = 300)


#------------DATA PREP 2: HIGHEST BODYCOUNT GENRES------------------------------
    #Split genres by row to duplicate films that have more than one genre
        df_split <- separate_rows(df_v3, Genre, sep = "\\|")
            df_HBCG <- arrange(df_split, desc(Genre))

#----------Average On-Screen Deaths Per Genre----------
avg_HBCG <- df_HBCG %>% 
          group_by(Genre) %>%
          summarize(avg_body = mean(Body_Count))




#1. Plot Average Body Count Per Genre

ABCG_plot <- ggplot(avg_HBCG, aes(x = reorder(Genre, avg_body), 
                                  y = avg_body, 
                                  fill = "white")) +
  geom_col(position = "stack") +
  labs(title = "On-Screen Deaths By Film Genre",
       subtitle = "Which Movies Depict The Most Death?",
       x = "Genre", 
       y = "Average On-Screen Deaths"
  ) +
  theme(panel.background = element_rect(fill = "grey25"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
#Text colour swap
        plot.title = element_text(colour = "white", size = "20"),
        plot.subtitle = element_text(colour = "white", family = "", size = "12"),
#White Axes
        axis.line = element_line(colour = "white"),
        axis.text = element_text(colour = "red3", size = "12"),
        axis.title = element_text(colour = "white", size = "16"), 
#Remove Legend
        legend.position = "none") +
#Flip x&y
        coord_flip()

#Print and Save plot
  print (ABCG_plot)
    ggsave("DeadliestGenres.png", ABCG_plot, width = 8, height = 5, dpi = 300)

    
    
    
#2. Plot body count proportional to MPA rating by on screen deaths

HBCG_plot <- 
  ggplot(df_HBCG, aes(x = Genre, y = Body_Count, fill = MPAA_Rating)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_gdocs() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of MPA Movie Ratings by Genre",
       subtitle = "How Do MPA Ratings Differ Between Genres That Depict On-Screen Deaths?",
       x = "Genre", 
       y = "Percentage of MPA Ratings",
       fill = "MPA Rating"
  ) +
#Assign colours to MPA rating
   scale_fill_manual(values = c("#53bd73",
                               "#Fa6f00", 
                               "#dbcd4f",
                               "#db0909", 
                               "#18f3fa", 
                               "#C014c1"),
                    labels = c("G",
                               "PG",
                               "PG-13",
                               "R",
                               "X",
                               "Unrated")) +
  
#Aesthetics similar to previous graphs  
  theme(panel.background = element_rect(fill = "grey25"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
#Text colour swap 
        plot.title = element_text(colour = "white", size = "16"),
        plot.subtitle = element_text(colour = "red", family = "", size = "12"),
#White Axes
        axis.line = element_line(colour = "white"),
        axis.text = element_text(colour = "red", size = "10"),
        axis.title = element_text(colour = "white", size = "14"),
#Bottom Text Align
        axis.text.x = element_text(angle = 50, hjust = 1))
#Print and Save Plot
    print (HBCG_plot)
      ggsave("MPAAdistribution.png", HBCG_plot, width = 8, height = 5, dpi = 300)
      
      
      
      
      
#-----------------------------------IMDB/BODYCOUNT BY GENRE-----------------------
#EXPLORATORY GRAPH
      # To determine which genres have a reasonable sample size for scatter graph

  ggplot(df_split, aes(x = reorder(Genre, table(Genre)[Genre]))) +
      geom_bar() +
        labs(title = "Exploratory graph of genre frequency in this dataset",
             x = "Genre",
             y = "Frequency") +
        theme_fivethirtyeight() +
        coord_flip()
      
#----SCATTERPLOTS BY SINGLE GENRES-----
      
#ACTION ALONE
action_plot <- ggplot(action_data, aes(x = IMDB_Rating, 
                        y = Body_Count,
                        colour = factor(MPAA_Rating))) +
  geom_point(size = 2) +
  labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Action Films",
       x = "IMDB Rating", 
       y = "On-screen deaths",
       colour = "US Age Rating") +
  xlim(0,10)+
  ylim(0, 850) +
  geom_smooth(method = "lm",
              color = "black", 
              linetype = "dashed",
              se = FALSE) +
  theme_stata() +
  scale_colour_manual(values = c("#53bd73",
                                 "#Fa6f00", 
                                 "#dbcd4f",
                                 "#db0909", 
                                 "#18f3fa", 
                                 "#C014c1"),
                      labels = c("G",
                                 "PG",
                                 "PG-13",
                                 "R",
                                 "X",
                                 "Unrated"))

#FANTASY ALONE
fantasy_plot <- ggplot(fantasy_data, aes(x = IMDB_Rating, 
                          y = Body_Count,
                          colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Fantasy Films",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating")+
  xlim(0,10)+
  ylim(0, 850) +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_stata() +
  scale_colour_manual(values = c("#53bd73",
                                 "#Fa6f00", 
                                 "#dbcd4f",
                                 "#db0909", 
                                 "#18f3fa", 
                                 "#C014c1"),
                      labels = c("G",
                                 "PG",
                                 "PG-13",
                                 "R",
                                 "X",
                                 "Unrated"))
#HORROR ALONE
horror_plot <-  ggplot(horror_data, aes(x = IMDB_Rating, 
                        y = Body_Count,
                        colour = factor(MPAA_Rating))) +
  geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Horror Films",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
  xlim(0,10)+
  ylim(0, 850) +
  geom_smooth(method = "lm",
              color = "black", 
              linetype = "dashed",
              se = FALSE) +
  theme_stata() +
  scale_colour_manual(values = c("#53bd73",
                                 "#Fa6f00", 
                                 "#dbcd4f",
                                 "#db0909", 
                                 "#18f3fa", 
                                 "#C014c1"),
                      labels = c("G",
                                 "PG",
                                 "PG-13",
                                 "R",
                                 "X",
                                 "Unrated"))
  
#THRILLER ALONE
thriller_plot <- ggplot(thriller_data, aes(x = IMDB_Rating, 
                          y = Body_Count,
                          colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Thriller Films",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
  xlim(0,10)+
  ylim(0, 850) +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_stata() +
  scale_colour_manual(values = c("#53bd73",
                                 "#Fa6f00", 
                                 "#dbcd4f",
                                 "#db0909", 
                                 "#18f3fa", 
                                 "#C014c1"),
                      labels = c("G",
                                 "PG",
                                 "PG-13",
                                 "R",
                                 "X",
                                 "Unrated"))

#SCI-FI ALONE
scifi_plot <- ggplot(scifi_data, aes(x = IMDB_Rating, 
                         y = Body_Count,
                         colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in Sci-Fi Films",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
  xlim(0,10)+
  ylim(0, 850) +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
    theme_stata() +
  scale_colour_manual(values = c("#53bd73",
                                 "#Fa6f00", 
                                 "#dbcd4f",
                                 "#db0909", 
                                 "#18f3fa", 
                                 "#C014c1"),
                      labels = c("G",
                                 "PG",
                                 "PG-13",
                                 "R",
                                 "X",
                                 "Unrated"))
 
#WAR ALONE
war_plot <- ggplot(war_data, aes(x = IMDB_Rating, 
                         y = Body_Count,
                         colour = factor(MPAA_Rating))) +
    geom_point(size = 2) +
    labs(title = "Scatterplot of IMDB Rating by on-screen deaths in War Films",
         x = "IMDB Rating", 
         y = "On-screen deaths",
         colour = "US Age Rating") +
  xlim(0,10)+
  ylim(0, 850) +
    geom_smooth(method = "lm",
                color = "black", 
                linetype = "dashed",
                se = FALSE) +
  theme_stata() +
  scale_colour_manual(values = c("#53bd73",
                                 "#Fa6f00", 
                                 "#dbcd4f",
                                 "#db0909", 
                                 "#18f3fa", 
                                 "#C014c1"),
                      labels = c("G",
                                 "PG",
                                 "PG-13",
                                 "R",
                                 "X",
                                 "Unrated"))
  
# -------------------COMBINE WITHIN-GENRE VISUALISATIONS----------

combined_graph <- grid.arrange(action_plot, 
                               fantasy_plot, 
                               horror_plot, 
                               thriller_plot, 
                               scifi_plot,
                               war_plot,
                               ncol = 2)

#Print and save plot    
  ggsave("CombinedGenres.png", combined_graph, width = 14, height = 15, dpi = 300)
      