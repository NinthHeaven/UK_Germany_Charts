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

# Below I read in the entire charts discography. I decided to just read in the
# gt tables here and have the actual code in the rmd file because the shiny app
# loaded much slower when it had to run the code for the three if else
# conditions.

yearly_uk_artists <- read_rds("yearly_uk_artists.rds")
yearly_uk_songs <- read_rds("yearly_uk_songs.rds")
yearly_uk_genres <- read_rds("yearly_uk_genres.rds")
yearly_ger_artists <- read_rds("yearly_ger_artists.rds")
yearly_ger_songs <- read_rds("yearly_ger_songs.rds")
yearly_ger_genres <- read_rds("yearly_ger_genres.rds")

# Below I read the entire chart data. I initially was going to do all the data
# processing here but that caused major lag when loading the animated plots so
# for now the Shiny reads in the features data without doing much with it. I am
# keeping it here for whenever I want to return back to the project and figure
# out how to do more analysis with the audio features in the uk and germany
# datasets.

uk_charts <- readRDS("uk_charts.rds")
ger_charts <- readRDS("ger_charts.rds")

# Below I read the data that I will use for the wordcloud. I decided to add a
# wordcloud last minute because I realized I never actually compared the genres
# in the UK and Germany and found which one had the most EDM influence. This was
# used as a substitute for not being able to plot any interesting information
# about the audio features.

ukcloud <- readRDS("ukcloud.rds")
gercloud <- readRDS("gercloud.rds")

# I decided to apply the theme simplex because I really enjoyed the design and
# thought it matched the plots very well.

ui <- navbarPage(theme = shinytheme("simplex"),
    
    # Application title
    
    "A Decade in Dance: Comparing the 2000s UK and Germany Charts",
    
    # Here I have the UK Chart Analysis data, where users can select what
    # features they would like to see on the Shiny App and an animated plot
    # showing the distribution of that audio feature over time will appear.
    # Another tab is used to create a page where users can look at the top ten
    # songs, artists, and the top 20 genres that were most prominent in the UK
    # charts.
    
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
    
    # The same thing is done in this tab with the German charts. Users can pick
    # the feature they want to see and an animated plot will appear, and they
    # can also view the top artists, songs, and genres in the German charts.
    
    tabPanel(
        title = "German Chart Analysis",
        h3("Looking at the German Charts from 2000 - 2010"),
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
                p("Fun fact: The second most popular song on the chart is the only EDM song to appear in the decade song analysis."),
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
    
    # Here is where users can look at the most popular genres per year in the UK
    # or German charts. I added this very last minute because I never actually
    # talked about the genres that appeared in the charts per year. I felt
    # that including a word cloud would allow users, as well as myself, to see
    # the changing music culture in the UK and Germany.
    
    tabPanel(
        "UK-GER Genre Comparison",
        p("Here you can look at the most popular genres in the UK and Germany in the 2000s. Do you notice any major changes in which genres dominated the charts in the 2000s? Do you also see any new genres emerging from old genres towards the end of the 2000s?"),
        br(),
        sidebarPanel(
            selectInput("uk_ger_comp", "Choose the country you would like to view:",
                        c("United Kingdom", "Germany")),
            sliderInput("year_input", "Select a year by using the slider below:",
                        min = 2000, max = 2010, value = 2000)
        ),
        mainPanel(
            plotOutput("wordcloud")
        ),
        br(),
        h3("Genre Analysis"),
        p("When looking at the most popular genres in the charts, you can see that although dance pop remained a consistent popular genre, other genres started to become more popular in the charts towards the end of the 2000s."),
        p("In the UK, urban contemporary, girl groups, and different kids of pop music (europop, pop rap, etc.) were the most popular genres in the early 2000s. Although girl groups started to fade from the charts towards the end of the decade, hip hop, r&b, and rap started to rise up in popularity. Additionally, in the early 2000s, influences from the eurodance scene in the 90s is seen in the charts as bubblegum dance and europop were popular genres in the charts."),
        p("In Germany, EDM was extremely dominant in the charts up until the mid-2000s. Genres such as eurodance, bubblegum dance, (german) techno, and trance were most prominent in the charts alongside dance pop. Towards the end of the 2000s, more rock, rap, and urban contemporary music started to fill up the charts."),
        p("It is important to note that although dance pop is shown as the most prominent genre in both charts in the early 2000s, this does not mean that almost all the songs fell under the dance pop genre (even though many did!). When analyzing audio genres with Spotify, a song is often flagged under multiple genre depending on whether it has elements consistent with the genre or if the style is similar. Thus, a rock song might have some dance pop element and be flagged as dance pop and rock alongside other genres, even though it would be considered rock.")
        
    ),
    
    # The about page is here, where I talk about why I started the project, how
    # I gathered my data, and give information about myself.
    
    tabPanel(
        "About",
        h3("Interlude"),
        p("It is no doubt that hip hop and pop dominates the US charts, most likely due to the culture that emerged in the late 90s and took off in the 2000s. Each country has a genre or genres that dominate their charts and are representative of its culture to some degree. For example, Germany was tied to mostly EDM until fairly recently. Although some aspect of EDM culture is still present when observing the top tracks, according to Statistica, Pop and Rock are the most popular genre. The UK also had a similar story, being tied to the emergence of Eurodance in the 90s which sparked an era of EDM music that continued to evolve; from UK Garage to Dubstep for example. However, unlike Germany, most of these popular genres were 'underground', being played at now-closed nightclubs that many ravers enjoyed attending. Similar to the US, pop and hip hop were the most popular genres in the UK starting in the late 90s."),
        br(),
        h3("The Current Project"),
        p("This project aims to compare two countries that had a seemingly strong EDM following in the 90s and early 2000s, the United Kingdom and Germany, and see if the chart rankings match the expected results (see above). If what was mentioned above is true, there should be significantly more EDM songs in the Germany charts from 2000-2010 compared to the UK. It will also be useful to see how the genres that dominated the charts have changed over the first decade of this millenia."),
        br(),
        h3("How Was This Data Gathered?"),
        p("I used the official German charts website and a UK charts listing website (sources below) to get the charts information for the 2000s. For audio analysis and data gathering, I used Spotify along with the spotifyr package to read and analyze all my data. I should note that some information is missing on the website, mostly due to only being able to analyze music that is already available in the US Spotify market."),
        br(),
        h3("Sources"),
        p("The following information was gathered from these websites:"),
        tags$a(href="http://www.uk-charts.top-source.info/index.shtml", "UK Charts Top-Source"),
        br(),
        tags$a(href="https://www.offiziellecharts.de/", "Offizielle Deutsche Charts"),
        br(),
        h3("About Me"),
        p("My name is Saul Soto, and I am currently a sophomore at Harvard University studying Psychology and Computer Science. For more information about my project, or if you're an EDM fan like me and want to exchange songs, hit me up at ssoto1@college.harvard.edu.")
    )
)
 
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$UKCharts <- renderImage({
        
        # Originally, I had created a temp file here to store the animated plots
        # and display them based on the user's input, but that caused the
        # shinyapp to load extremely slow. Instead, I already saved and loaded
        # the animated plots as gifs, and have them displayed based on the
        # user's input. This ends up working much better and creates for a more
        # smooth experience for the user! I did have to add deleteFile = FALSE
        # at the end because I noticed that my gifs kept getting deleted every
        # time they were displayed on the page :(.
        
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
        
        # The same thing I did for the UK animated plots was done here.
        
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
        
        # Here I load the gt tables based on the user's input (same as the chunk
        # of code below)
        
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
        
        #I did not need to include a year condition like I had originally for
        #the features analysis because I realized that I could treat the year
        #input as a variable. Depending on which country the user selects, I
        #load the genre datasets for the respective country and filter the year
        #out by the user's input. Then, I create a wordcloud from the new
        #dataframe that was formed. Note: The wordcloud did not display all the
        #genres until I added the scale argument. Furthermore, wordcloud2 did
        #not work as intended, probably due to the dataset being grouped by
        #year.
        
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
