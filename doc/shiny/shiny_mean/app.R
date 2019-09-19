library(tidyverse)
library(tidytext)
library(shiny)
library(ggplot2)

load('../../output/processed1_lyrics.RData')
lyrics_list <- c("All","Folk", "R&B", "Electronic", "Jazz", "Indie", "Country", "Rock", "Metal", "Pop", "Hip-Hop", "Other")
time_list <- c("All","1970s", "1980s", "1990s", "2000s", "2010s")

ui <- fluidPage(fluidRow(
  column(4,selectInput('select_genre1', 'Genre', lyrics_list, selected='All')),
  column(4,selectInput('select_year1', 'Year', time_list, selected='All'))
),
fluidRow(
  column(6,plotOutput("mean1",height = "200px"))
),
fluidRow(
  column(6,plotOutput("mean2",height = "200px"))
)
)

selcet1.df <- function(select_year,select_genre,df=dt_lyrics){
  if(select_year=="All"){
    Year = c(1970,2020)
  }
  else{
    start = as.integer(strsplit(select_year, "s"))[[1]]
    Year = c(start,start+10)
  }
  if(select_genre=="All"){
    dt = df[df$year>=Year[1]&df$year<Year[2],]
  }
  else{
    dt = df[df$year>=Year[1]&df$year<Year[2]&df$genre==select_genre,]
  }
  return(dt[dt$affin.count!=0,])
}

selcet2.df <- function(select_year,select_genre,df=dt_lyrics){
  if(select_year=="All"){
    Year = c(1970,2020)
  }
  else{
    start = as.integer(strsplit(select_year, "s"))[[1]]
    Year = c(start,start+10)
  }
  if(select_genre=="All"){
    dt = df[df$year>=Year[1]&df$year<Year[2],]
  }
  else{
    dt = df[df$year>=Year[1]&df$year<Year[2]&df$genre==select_genre,]
  }
  return(dt[dt$bing.count!=0,])
}


server <- function(input, output) {
  output$mean1 <- renderPlot({
    selcet1.df(input$select_year1,input$select_genre1) %>% ggplot(aes(x=affin.mean, group=genre, fill=genre)) + geom_density(alpha = 0.2) + xlim(-5,5)
  }, width = 600, height = 200)
  output$mean2 <- renderPlot({
    selcet2.df(input$select_year1,input$select_genre1) %>% ggplot(aes(x=bing.mean, group=genre, fill=genre)) + geom_density(alpha = 0.2) + xlim(0,1)
  }, width = 600, height = 200)
}


shinyApp(ui, server)