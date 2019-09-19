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
                  column(6,plotOutput("amount1",height = "200px"))
                ),
                fluidRow(
                  column(6,plotOutput("amount2",height = "200px"))
                ),
                fluidRow(
                  column(6,plotOutput("amount3",height = "200px"))
                )
)

selcet.df <- function(select_year,select_genre,df=dt_lyrics){
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
  return(dt)
}


server <- function(input, output) {
  output$amount1 <- renderPlot({
    selcet.df(input$select_year1,input$select_genre1) %>% ggplot(aes(x=affin.count, group=genre, fill=genre)) + geom_density(alpha = 0.2) + xlim(0,50)
  }, width = 600, height = 200)
  output$amount2 <- renderPlot({
    selcet.df(input$select_year1,input$select_genre1) %>% ggplot(aes(x=bing.count, group=genre, fill=genre)) + geom_density(alpha = 0.2) + xlim(0,50)
  }, width = 600, height = 200)
  output$amount3 <- renderPlot({
    selcet.df(input$select_year1,input$select_genre1) %>% ggplot(aes(x=nrc.count, group=genre, fill=genre)) + geom_density(alpha = 0.2) + xlim(0,200)
  }, width = 600, height = 200)
}

shinyApp(ui, server)