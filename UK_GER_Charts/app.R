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
library(shinythemes)
library(gt)
library(gganimate)
library(transformr)

# Below I read in the entire charts discography

yearly_uk_artists <- read_rds("yearly_uk_artists.rds")
yearly_uk_songs <- read_rds("yearly_uk_songs.rds")
yearly_uk_genres <- read_rds("yearly_uk_genres.rds")
yearly_ger_artists <- read_rds("yearly_ger_artists.rds")
yearly_ger_songs <- read_rds("yearly_ger_songs.rds")
yearly_ger_genres <- read_rds("yearly_ger_genres.rds")

# Below I read the entire chart data
uk_charts <- readRDS("uk_charts.rds")
ger_charts <- readRDS("ger_charts.rds")

# Below I read the data that I will use for the wordcloud

ukcloud <- readRDS("ukcloud.rds")
gercloud <- readRDS("gercloud.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("simplex"),
    # Application title
    
    "A Decade in Dance: Comparing the 2000s UK and Germany Charts",
    
    # Sidebar with a slider input for number of bins 
    tabPanel(
        title = "UK Chart Analysis",
        h3("Looking at the UK Charts from 2000 - 2010"),
        tags$div("Below are the features and artists/songs that were most prevalent in the UK Charts during the 2000s."),
        tabsetPanel(
            tabPanel(
                h6("Audio Features Analysis"),
                br(),
                sidebarPanel(
                    selectInput("feature","Features",
                                c("Danceability", "Energy", "Tempo", "Valence")
                    ),
                    p("If you are confused with the what the features mean, here are the definitions from Spotify:"),
                    br(),    
                    p("Dancebility: Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                    br(),
                    p("Energy: Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                    br(),
                    p("Tempo: The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."),
                    br(),
                    p("Valence: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry")
                ),  
                # Show a plot of the generated distribution
                mainPanel(
                    imageOutput("UKCharts")
                )
            ),
            tabPanel(
                h6("Most Popular in the Charts"),
                br(),
                sidebarPanel(
                    selectInput("yearly_uk", "Choose What You Would Like to View:",
                                c("Artists", "Songs", "Genres"))
                ),
                mainPanel(
                    gt_output("UKAnalysis")
                )
            )
            )
        ),
    tabPanel(
        title = "Germany Chart Analysis",
        h3("Looking at the Germany Charts from 2000 - 2010"),
        tags$div("Below are the features and artists/songs that were most prevalent in the 2000s German Charts."),
        tabsetPanel(
            tabPanel(
                h6("Audio Feature Analysis"),
                br(),
                sidebarPanel(
                    selectInput("feature_ger","Features",
                                c("Danceability", "Energy", "Tempo", "Valence")
                    ),
                    p("If you are confused with the what the features mean, here are the definitions from Spotify:"),
                    br(),    
                    p("Dancebility: Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
                    br(),
                    p("Energy: Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
                    br(),
                    p("Tempo: The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."),
                    br(),
                    p("Valence: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry")
                ),
                mainPanel(
                    imageOutput("GERCharts")
                )
                
            ),
            tabPanel(
                h6("Most Popular in the Charts"),
                br(),
                sidebarPanel(
                    selectInput("yearly_ger", "Choose What You Would Like to View:",
                                c("Artists", "Songs", "Genres"))
                ),
                mainPanel(
                    gt_output("GERAnalysis")
                )
            )
        )
    ),
    tabPanel(
        "UK-GER Genre Comparison",
        p("Here you can look at the most popular genres in the UK and Germany in the 2000s. Do you notice any major changes in which genres dominated the charts in the 2000s?"),
        br(),
        sidebarPanel(
            selectInput("uk_ger_comp", "Choose the country you would like to view:",
                        c("United Kingdom", "Germany")),
            sliderInput("year_input", "Select a year by using the slider below:",
                        min = 2000, max = 2010, value = 2000)
        ),
        mainPanel(
            plotOutput("wordcloud")
        )
    ),
    tabPanel(
        "About",
        h3("Interlude"),
        p("It is no doubt that hip hop and pop dominates the US charts, most likely due to the culture that emerged in the late 90s and took off in the 2000s. Each country has a genre or genres that dominate their charts and are representative of its culture to some degree. For example, Germany was tied to mostly EDM until fairly recently. Although some aspect of EDM culture is still present when observing the top tracks, according to Statistica, Pop and Rock are the most popular genre. The UK also had a similar story, being tied to the emergence of Eurodance in the 90s which sparked an era of EDM music that continued to evolve; from UK Garage to Dubstep for example. However, unlike Germany, most of these popular genres were 'underground', being played at now-closed nightclubs that many ravers enjoyed attending. Similar to the US, pop and hip hop were the most popular genres in the UK starting in the late 90s."),
        br(),
        h3("The Current Project"),
        p("This project aims to compare two countries that had a seemingly strong EDM following in the 90s and early 2000s, the United Kingdom and Germany, and see if the chart rankings match the expected results (see above). If what was mentioned above is true, there should be significantly more EDM songs in the Germany charts from 2000-2010 compared to the UK. It will also be useful to see how the genres that dominated the charts have changed over the first decade of this millenia."),
        br(),
        br(),
        h3("About Me"),
        p("My name is Saul Soto, and I am currently a sophomore at Harvard University studying Psychology and Computer Science. For more information about my project, or if you're an EDM fan like me and want to exchange songs, hit me up at ssoto1@college.harvard.edu.")
    )
)
 
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$UKCharts <- renderImage({
        
        # Creating a temp file to save the output of the gganimate plot. I found
        # this solution on StackOverflow.
        
        if(input$feature == "Danceability"){
            list(src = "uk_danceability.gif",
                 contentType = 'image/gif')
        }
        else if(input$feature == "Valence"){
            list(src = "uk_valence.gif",
                 contentType = 'image/gif')
        }
        else if(input$feature == "Tempo"){
            list(src = "uk_tempo.gif",
                 contentType = 'image/gif')
        }
        else if(input$feature == "Energy") {
            list(src = "uk_energy.gif",
                 contentType = 'image/gif')
        }
    }, deleteFile = FALSE)
    
    output$GERCharts <- renderImage({
        
        # Create temporary file to store animated plot.
        if(input$feature_ger == "Danceability"){
            list(src = "ger_danceability.gif",
                 contentType = 'image/gif')
        }
        else if(input$feature_ger == "Valence") {
            list(src = "ger_valence.gif",
                 contentType = 'image/gif')
        }
        else if(input$feature_ger == "Tempo") {
            list(src = "ger_tempo.gif",
                 contentType = 'image/gif')
        }
        else if(input$feature_ger == "Energy"){
            list(src = "ger_energy.gif",
                 contentType = 'image/gif')
        }
    }, deleteFile = FALSE)
    
    output$UKAnalysis <- render_gt({
        if(input$yearly_uk == "Artists"){
            yearly_uk_artists
        }
        else if(input$yearly_uk == "Songs"){
            yearly_uk_songs
        }
        else if(input$yearly_uk == "Genres"){
            yearly_uk_genres
        }
    })
    
    output$GERAnalysis <- render_gt({
        if(input$yearly_ger == "Artists"){
            yearly_ger_artists
        }
        else if(input$yearly_ger == "Songs"){
            yearly_ger_songs
        }
        else if(input$yearly_ger == "Genres"){
            yearly_ger_genres
        }
    })
    
    output$wordcloud <- renderPlot({
        if(input$uk_ger_comp == "United Kingdom"){
           uk_wordcloud <- ukcloud %>%
               filter(year == input$year_input)
           
           wordcloud(words = uk_wordcloud$genre, freq = uk_wordcloud$Count, min.freq = 1, max.words=80, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale = c(3.5, 0.25))
        }
        else if(input$uk_ger_comp == "Germany"){
            ger_wordcloud <- gercloud %>%
                filter(year == input$year_input)
            
            wordcloud(words = ger_wordcloud$genre, freq = ger_wordcloud$Count, min.freq = 1, max.words=80, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale = c(3.5, 0.25))
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
