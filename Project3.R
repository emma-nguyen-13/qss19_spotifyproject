## Advanced Data Visualization (QSS 19) Spring 2023
## Project 
##
## Name: Emma Nguyen
## Date: May 25, 2023

library(tidyverse)
library(dplyr)
library(plotly)
library(lubridate)
library(rvest)
library(shiny)
library(tools)
library(ggthemes)
library(gganimate)
library(gifski)
library(spotifyr)
library(shinycssloaders)
library(showtext)
library(colorspace)
library(colorBlindness)
library(shinyWidgets)

## --------------------------- Project Idea 1

font_add("gotham", "~/RProjects/QSS 019/Project 3/Spotify-Font/GothamBook.ttf")
font_add("gotham bold", "~/RProjects/QSS 019/Project 3/Spotify-Font/Gotham-Black.otf")
showtext_auto()

# Setting up Spotify
Sys.setenv(SPOTIFY_CLIENT_ID = 'de10b2aa62d1464b97b67688fdeadc63')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '51239d2592dd4c35adcc250e3471eb8d')

access_token <- get_spotify_authorization_code(scope = c('user-top-read','playlist-read-private','user-read-recently-played'))

file <- file.choose()

myartists <- read.csv(file) %>% 
  mutate(endTime = as.Date(endTime),
         month = format(endTime, "%Y-%m")) %>% 
  group_by(month, endTime, artistName) %>% 
  summarize(msPlayed = sum(msPlayed))

#Add missing rows
add_missing <- function(df1, df2, month, date) {
  
  missing <- setdiff(df1$artistName, df2$artistName)
  
  new_rows <- tibble()
  
  for (artist in missing){
    new_row <- c(month,
                 date,
                 artist,
                 0) 
    new_rows <- rbind(new_rows, new_row)}
  
  colnames(new_rows) <- c("month", "endTime", "artistName", "msPlayed")
  
  return(new_rows)
  
}

#Create df of top 10 artists for the month

plotdf <- function(choice){
    
  # Get top 10 artists for the month
  df10 <- myartists %>% 
    filter(month == choice) %>% 
    group_by(artistName) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    slice(1:10)
  
  #Filter large df to just that month
  top10art <- myartists %>% 
    filter(month == choice) %>% 
    inner_join(df10) %>% 
    select(!count) %>% 
    arrange(endTime, desc(msPlayed))
  
  dates <- as.character(unique(top10art$endTime))
  
  for(item in dates){
    df_day <- top10art %>% filter(endTime == item)
    sdf <- add_missing(top10art, df_day, choice, item)
    sdf$endTime <- as.Date(sdf$endTime)
    sdf$msPlayed <- as.numeric(sdf$msPlayed)
    top10art <- rbind(top10art, sdf)
  }
  
  top10art <- top10art %>% 
    arrange(artistName, endTime) %>% 
    group_by(artistName) %>% 
    mutate(cumulative = cumsum(msPlayed)) %>% 
    ungroup() %>% 
    arrange(endTime, cumulative) %>% 
    group_by(endTime) %>% 
    mutate(rank = row_number())
    
  return(top10art)
  
}

graphy <- function(choice){
  
  df <- plotdf(choice)
  
  anim_art <- ggplot(data = df) +
    geom_rect(aes(xmin = 0,
                  xmax = cumulative,
                  ymin = rank + 0.45,
                  ymax = rank - 0.45,
                  fill = artistName)) + 
    geom_text(aes(y = rank,
                  x = 0,
                  vjust = 0.2, 
                  hjust = 1,
                  label = paste0(artistName, " ")),
              family = "gotham",
              color = "white",
              size = 6) +
    geom_text(aes(y = rank,
                  x = cumulative,
                  label = paste0(" ", cumulative), 
                  hjust = 0),
              color = "white",
              family = "gotham",
              size = 6) +
    scale_x_continuous(expand = expansion(mult = 0.3)) +
    labs(title = "User @enguyen513's Top 10 Artists On Spotify: {frame_time}",
         subtitle = "\n
         Select a month of interest from the dropdown menu. The plot shows how many milliseconds of my top 10 artists for the month
         I listened to over the span of the month.") +
    guides(fill = "none") +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.1, color="grey"),
          panel.grid.minor.x = element_line(linewidth = 0.1, color="grey"),
          plot.title=element_text(size=25, hjust=0, family = "gotham bold", colour="green", vjust=-1),
          plot.subtitle=element_text(size=15, hjust=0, family = "gotham", colour="green", vjust=-1),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black",
                                         color = "black"))
  
}

frames <- function(choice){
  df <- plotdf(choice)
  
  return(nrow(df))
}

ui <-  fluidPage(
  setBackgroundColor("black"),
  tags$style("#month {background-color:black; 
             border:border: 1px solid #FFFFFF; color:white;}"),
  selectInput("month", HTML("<br><span style='color: white'>Month</span>"),
              c("May 22" = "2022-05",
                "June 22" = "2022-06",
                "July 22" = "2022-07",
                "August 22" = "2022-08",
                "September 22" = "2022-09",
                "October 22" = "2022-10",
                "November 22" = "2022-11",
                "December 22" = "2022-12",
                "January 23" = "2023-01",
                "February 23" = "2023-02",
                "March 23" = "2023-03",
                "April 23" = "2023-04",
                "May 23" = "2023-05"),
              selectize = FALSE),
  withSpinner(imageOutput("plot"))
)

server <- function(input, output) {
  
  output$plot <- renderImage(
    {
      outfile <- tempfile(fileext='.gif')
    
      p <- graphy(input$month) +
        transition_time(endTime) +
        ease_aes('cubic-in-out') +
        view_follow(fixed_y = TRUE)
      
      nframes <- frames(input$month)
      fps <- nframes / 4
      
      animate(p, nframes = nframes, fps = fps, width = 1200, height = 800, 
              renderer = gifski_renderer(), start_pause = 10, end_pause = 10)
    
      anim_save("outfile.gif")
      
      list(src = "outfile.gif", contentType = 'image/gif')
    },
    deleteFile = TRUE)
  }

shinyApp(ui, server)



