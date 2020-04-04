#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(spotifyr)
library(lubridate)
library(readxl)
library(janitor)
library(rvest)
library(infer)
library(magrittr)
library(hrbrthemes)
library(extrafont)
Sys.setenv(SPOTIFY_CLIENT_ID = '1ebacb60ef2044ccadab18602f7c0b8f')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6fb54464f8dc45c88387a4fadde775db')

access_token <- get_spotify_access_token()

uk_2000 <- get_playlist("7uQBCcnNuxOBv0jFbEcYjH")
my_tracks <- get_playlist_tracks("7uQBCcnNuxOBv0jFbEcYjH")
testing <- get_track_audio_features(my_tracks$track.id)

danceability_uk_2000_test <- testing %>%
    ggplot(aes(danceability)) +
    geom_density(fill = "#faac0f", colour = "#fcab08", alpha = 0.7) +
    ggtitle("Danceability Distribution in UK Charts 2000") +
    theme_ipsum(base_family = "Calibri")

valence_uk_2000_test <- testing %>%
    ggplot(aes(valence)) +
    geom_density(fill = "#69b3a2", colour = "#e9ecef", alpha = 0.7) +
    ggtitle("Valence Distribution in UK Charts 2000") +
    theme_ipsum(base_family = "Calibri") 

tempo_uk_2000_test <- testing %>%
    ggplot(aes(tempo)) +
    geom_density(fill = "#4287f5", colour = "#316fd4", alpha = 0.7) +
    ggtitle("Tempo Distribution in UK Charts 2000") +
    theme_ipsum(base_family = "Calibri")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Features of Songs in 2000 UK Charts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            varSelectInput("feature","Features",
                        testing %>%
                            select(danceability, valence, tempo)
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$myPlot <- renderPlot({
        if(input$feature == "danceability"){
            danceability_uk_2000_test
        }
        else if(input$feature == "valence"){
            valence_uk_2000_test
        }
        else if(input$feature == "tempo"){
            tempo_uk_2000_test
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
