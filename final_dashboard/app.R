rm(list=ls())

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(readr)
library(ggthemes)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(bslib)
library(wordcloud)
library(rvest)
library(tidyverse)
library(tm)
library(SnowballC)
library(tidytext)
library(textstem)
library(wordcloud)
library(ggthemes)
library(quanteda)
library(tokenizers)
library(janitor)
library(plotly)
library(shinyjs)


df<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/raw.csv",check.names=FALSE)
df$genres<-lapply(df$genres.x, function(x) as.list(str_trim(strsplit(x, ",")[[1]])))

df$primary_genre<-unlist(lapply(df$genres, function(x) x[[1]][1]))

JScode <-
  "$(function() {
    setTimeout(function(){
      var vals = [0];
      var powStart = 0;
      var powStop = 10;
      for (i = powStart; i <= powStop; i++) {
        var val = Math.pow(10, i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
      }
      $('#revenue').data('ionRangeSlider').update({'values':vals})
    }, 5)})"

movie_df = read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/movie_df_final1.csv",check.names=FALSE)
script_df = read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/script_df_final1.csv",check.names=FALSE)
sentiment_line = read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/sentiment_line.csv",check.names=FALSE)
plotly_data = read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/movie.csv",check.names=FALSE)

movie_df$genre1 = gsub(",.*$", "", movie_df$genre)

get_frequency = function(df){
  df = df %>%
    select(doc_id, text)
  
  df_source = DataframeSource(df)
  corpus = VCorpus(df_source)
  tdm_cleaned = TermDocumentMatrix(corpus)
  td_cleaned = tidy(tdm_cleaned)
  td_cleaned = 
    td_cleaned %>%
    group_by(term) %>%
    summarise(count = sum(count)) %>%
    mutate(lemma = lemmatize_words(term)) %>%
    ungroup()
  
  return(td_cleaned)
}


contain_yes <- function(list1,list2) {
  result<-FALSE
  for(i in list2){
    if(i %in% list1){result=TRUE}
  }
  return(result)
}

tri<-data.frame(x=c(0,100,100), y=c(0,0,100), t=c('a', 'a', 'a'), r=c(1,2,3))
regression<-lm(tomatometer_rating~audience_rating, data = df)
movies<-unique(df$title)
alpha<-coef(regression)[1]
beta<-coef(regression)[2]
ticks<-seq(1925,2010,10)


ui<-navbarPage(span("Movie Facts",style="font-weight:bold;text-align:center;"),  #tags$style(HTML(".navbar .navbar-default .navbar-static-top {margin-bottom:0px;padding-bottom:0px;}")),
               tags$head(tags$style(HTML('* {font-family: "Tahoma"};'))),
               tags$head(tags$style(".option{color: white;}")),
               tags$head(tags$style(".selectize-input{height:200px;padding-top: 0px;}")),
              
               tabPanel("Critics VS. Audience",
                        #tags$style(HTML(".tabbable > .nav > li > a {margin:0px;padding:0px;}")),
               dashboardPage(
  #dashboardHeader(tags$li(class = "dropdown",tags$style(".main-header {max-width: 200px};")),
  dashboardHeader(title =span("Critics VS. Audience",style="font-size:20px; font-family: Century Gothic, fantasy;")),
  
  dashboardSidebar(
    collapsed = FALSE,
    chooseSliderSkin("Flat"),

    selectizeInput(
      inputId = "movie", 
      label =tags$span("You may search for a particular movie:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
      multiple = FALSE,
      choices = c("Search Bar" = "",movies),
      options = list(
        create = FALSE,
        placeholder = "Movie Title",
        maxItems = '1',
        onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
        onType = I("function (str) {if (str === \"\") {this.close();}}")
      )),
    hr(style="margin:0px;"),
    selectizeInput('genres', tags$span("You may manually select the genre(s):",
                                       style="font-weight:bold; padding:0px;font-size:13px; margin:0px;"), 
                   options = list(create = FALSE,
                     placeholder = "Genre(s)"),
                   choices = sort(unique(unlist(df$genres))),selected = TRUE,  multiple = TRUE),
    hr(),
    tags$h4("Movie Filters:",style = "padding-left:10px;padding-bottom:0px;padding-top:0px;font-size: 20px;font-family: Century Gothic, fantasy;"),
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    sliderInput("yearend",
                label =tags$span("Range of release year:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
                min = 1925,
                max = 2017,
                ticks=FALSE,
                value=c(1925,2017),
                sep=""),
    tags$script(HTML("$(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('yearend').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 10);})
      ")),

    tags$head(tags$script(HTML(JScode))),
    sliderInput("revenue",
                label =tags$span("Box office revenue (USDs):",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;padding-bottom:0px"),
                min = 0,
                max = 1e10,
                ticks=FALSE,
                value=c(0,1e10),
                sep=","),
    tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('time').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 50);})
      ")),

    br(),
    tags$h5("Check out each major genre:",style = "padding-left:10px;font-weight: bold;margin-bottom:0px;font-size:13px;"),
    tags$h5("(The genres are sorted in descending order of critic preference. Since one movie can be of multiple genres, these subsets overlap)",
            style = "padding-left:10px;padding-right:0px;margin-bottom:0px;padding-bottom=0px;font-size: 12px;"),
    
    radioButtons(inputId = "genre", label = "",
                choiceNames = list(tags$span(style = "color:gold", "Art House & International"),"Animation","Kids & Family","Drama",
                                   tags$span(style = "color:white","All"),"Mystery & Suspense","Comedy","Science Fiction & Fantasy","Horror","Action & Adventure","Romance"),
                choiceValues = c("Art House & International","Animation","Kids & Family","Drama",
                                 "All Movies","Mystery & Suspense","Comedy","Science Fiction & Fantasy","Horror","Action & Adventure","Romance"),
                selected = "All Movies"),
    tags$script("$(\"input:radio[name='genre'][value='Art House & International']\").parent().css('color', '#EDB6B2');"),
    hr(),
    helpText("Data from Rotten Tomatoes",
             style = "left: 0.8em; position: relative;")
  ),
  
  dashboardBody(
    useShinyjs(),
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tags$head(tags$style(HTML('.small-box .icon-large {right: 15px; top: 17px;}'))),
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height:120px;}"))),
      mainPanel(width = 12,  
      valueBoxOutput("audience",width=3),
      valueBoxOutput("meter",width=3),
      valueBoxOutput("correlation",width=3),
      valueBoxOutput("ratio",width=3)
    )),
    fluidRow(

      tabBox(id="box",
        width = 8,
        height = "45em",
        #title = tagList(shiny::icon("bar-chart-o")),
        tabPanel("I. Scattered Ratings",
                 plotlyOutput(outputId = "plot", height = "40em")),
        tabPanel("II. Genre-wise Distribution of Ratings",
                 div(img(src = "violin.png", height = 600, width = 600), style="text-align: center;")
                 ),
        tabPanel("III. 20 Most Divisive Films",
                 div(img(src = "gaps.png", height = 600, width = 400), style="text-align: center;")
        )


    ),
      
      box(width = 4,
          strong("Overview",style = "padding-left:3px;margin-bottom:0px;color:white;font-size:16px;"),
          tags$h4("Are film critics losing sync with audiences?",style = "padding-left:3px;margin-bottom:0px;color:white;font-size:16px;"),
          tags$h4("What kind of movies that are acclaimed by professional reviewers are yet not likely to be enjoyed by most moviegoers?",style = "color:white;padding-left:3px;margin-bottom:0px;font-size:16px;"),
          tags$h4("Visualizations answer such questions strikingly.",style = "padding-left:3px;margin-bottom:0px;color:white;font-size:16px")),

      box(
        width = 4,
        height = "40em",
        tags$style("div{line-height:15px;}"),
        tags$style("intro {padding-left:0px;font-size:14px;line-height:15px;margin-bottom:0px;}"),
        tags$style("gold {padding-left:0px;font-size:14px;line-height:15px;margin-bottom:0px;color:gold}"),
        HTML("<intro><b>Tab I. Scatter Ratings</b></intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>The bubble chart displays the distribution of Rotten Tomatoes' critic ratings (a.k.a. <gold>Tomatometers</gold>) and audience ratings for more than 2000 popular films. Box-office revenue is represented by bubble size, and the color of a bubble indicates the primary genre of the film.</intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>Filter those movies, and you may find the Tomatometers covary with the audience scores in different manners <gold>(the red line that fit the filtered data points the best should be detached from the grey one)</gold>.</intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>The <gold>average ratio of Tomatometer rating to audience rating is used as a proxy for critic preference</gold>, which is reported in the box at the upper right corner.</intro>"),
        hr(),
        HTML("<intro><b>Tab II. Genre-wise Distribution of Ratings</b></intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>With violin plots, this chart illustrates the probablity distributions of both audience and critic ratings for movies of different genres. On the X axis, the genres are arrayed in descending order of critic preference.</intro>"),
        hr(),
        HTML("<intro><b>Tab III. 20 Most Devisive Films</b></intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>This chart displays the 20 movies with the greatest difference between Rotten Tomatoes' critic and audience ratings in the dataset. </intro>")
        
        )

    )
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  

)),
#############################End of YF's UI#################################

#############################Tab 2#################################
tabPanel("Script Sentiment Analysis", #new visualization layout here
dashboardPage(
  
  ####tab one starts
  dashboardHeader(title =span("Script Sentiment Analysis",style="font-size:16px;font-family: Century Gothic, fantasy;")),
  dashboardSidebar(
    collapsed = FALSE,
    chooseSliderSkin("Flat"),
    tags$head(
      tags$style(HTML(".selectize-input {height:10px;padding-top: 0px;}"))
    ),
    
    sidebarMenu(
      menuItem("Wordcloud & Term Frequency", tabName = "single", icon = icon("film")),
      menuItem("Sentiment Arc Analysis", tabName = "double", icon = icon("th")),
      menuItem("Sentiment & Critic Preference", tabName = "triple", icon = icon("theater-masks"))
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabItems(
      
      # First tab content
      tabItem(tabName = "single",
              
              fluidRow(
                box(width = 6,
                    strong("Overview", style = " font-size: 25px;"),
                    br(),
                    br(),
                    # p("As human beings, we show great empathy to individuals who 
                    #   have undergone sorrows that are similar to ours and have strong feelings 
                    #   towards events that echo our own life experiences. Hence, there may be some systematic 
                    #   differences between professional and non-professional audiences in terms of previous 
                    #   experiences that further impact how they respond to certain emotions and how they perceive a given movie."),
                    p("Twists and turns, ups and downs, despair and exhilaration on the big screen can all be recorded and 
                        reflected in movie scripts. We scraped the scripts of a total of 46 best-rated movies 
                        on IMDB and analyze their sentiment and emotional valence with visualizations from multiple perspectives. compare their critic preference score based on different genres.",
                      style="line-height:16px;font-size:13px"),
                    p("Tab 1 aims to retrieve and display various facts of a selected film including the title, rating, box office, and visualizations about movie script: the 
                        sentiment score, a word cloud as well as a word frequency bar chart. Tab 2 allows for comparing the sentiment in pairs across their running times. Tab 3 utilizes 
                        a bubble chart to demonstrate the relationship between the overall sentiment scores 
                        and the critic preference ratio.",style="line-height:16px;font-size:13px")
              ),
     box(width = 6,
                                             strong("Tab 1 Wordcloud & Term Frequency", style = "font-size: 25px;"),
                                             br(),
                                             br(),
                                             p("This tab takes a peek at the scripts by drawing word clouds and word 
                      frequency bar charts. The most common words and terms are highlighted in the word cloud. The character names are not stripped from the original 
                        script. Therefore the most frequent terms are likely to be the names of the movie characters with the most screen presense.",style="line-height:16px;font-size:13px")
              )),
              
              fluidRow(
                valueBoxOutput("name", width = 5),
                valueBoxOutput("rating", width = 2),
                valueBoxOutput("sales", width = 3),
                valueBoxOutput("sentiment", width = 2)
              ),
              
              fluidRow(
                box(
                  width = 3,
                  height = "44.5em",
                  p("The drop-down menu contain 46 best-rated movies on IMDB"),
                  br(),
                  selectInput("name",
                              "Choose one Movie:",
                              choices = unique(movie_df$name)
                              
                  )
                ),
                
                tabBox(width = 9,
                       tabPanel(title = "I. Word Cloud",
                                plotOutput("wordcloud", height = "40em",
                                           width = "100%")
                       ),
                       
                       tabPanel("II. Word Frequency", 
                                plotOutput("wordcountplot", height = "40em",
                                           width = "100%")
                       )
                       
                ),
                
                
              )
              
      ),
      ###tab 2 starts
      tabItem(tabName = "double",
              
              fluidRow(
                box(width = 12,
                    strong("Tab 2 Sentiment Arc Analysis", style = "font-size: 25px; font-weight: bold;"),
                    br(),
                    br(),
                    p("This tab examines the positive or negative connocations in scripts with two sub-tabs in detail.",style="line-height:16px;font-size:13px"),
                    p("Compare Sentiments of Two Movies", style = "text-decoration: underline;line-height:16px;font-size:13px"),
                    p("The line chart displays how the sentiment in the scripts changes as the movie 
                        progresses to the end. The y-axis is the computed sentiment score, and the x-axis denotes 
                        the fraction of the script (from 0 to 1) we're looking at. You can interact with the plot by select two movies to compare 
                        from the drop-down menu and look at the dynamics and flucuations of sentiment
                        in the film speeches.",style="line-height:16px;font-size:13px"),
                    p("Sentiment Plot by Genre", style = "text-decoration: underline;line-height:16px;font-size:13px"),
                    p("This line chart illustrates and juxtaposed the sentiment scores across the running time for movies of one particular genre. The y-axis is the calculated sentiment score, and the 
                        x-axis is the fraction of the script (from 0 to 1). You can manually select any genre from the drop-down menu.",style="line-height:16px;font-size:13px")
                )
              ),
              
              fluidRow(
                tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                valueBoxOutput("moviename1", width = 6),
                valueBoxOutput("moviename2", width = 6)
              ),
              
              fluidRow(
                valueBoxOutput("sentiment1", width = 3),
                valueBoxOutput("rating1", width = 3),
                valueBoxOutput("sentiment2", width = 3),
                valueBoxOutput("rating2", width = 3)
              ),
              
              fluidRow(
                tags$head(
                  tags$style(HTML("

      .selectize-input {
        height: 40px;
        width: 200px;
        padding-top: 5px;
      }

    "))
                ),
                box(
                  title = tags$p("Filters", style = "font-size: 130%; font-weight: bold"),
                  width = 3,
                  height = "44.5em",
                  sliderTextInput(
                    inputId = "percentage",
                    label = "The fraction of script to look at (in percentiles):", 
                    choices = seq(0,100,5),
                    selected = c(0, 100),
                    post="%"
                  ),
                  hr(),
                  h4('I. Compare Sentiment of Two Movies',style="font-weight:bold;font-size:15px"),
                  br(),
                  selectInput("movie1",
                              "Choose the First Movie:",
                              choices = unique(movie_df$name)
                              
                  ),
                  
                  selectInput("movie2",
                              "Choose the Second Movie:",
                              choices = c(unique(movie_df$name)),
                              selected = "The Godfather"
                  ),
                  
                  
                  hr(),
                  h4('II. Sentiment Score by Genre',style="font-weight:bold;font-size:15px"),
                  br(),
                  
                  selectInput("genre1",
                              "Choose a genre:",
                              choices = c(unique(movie_df$genre1)),
                              selected = "Drama"
                  )
                  
                ),
                
                
                tabBox(width = 9,
                       tabPanel(title = "I. Compare Sentiment of Two Movies",
                                plotOutput("sentimentplot", height = "40em",
                                           width = "100%")
                       ),
                       
                       tabPanel(title = "II. Sentiment Plot by Genre", 
                                plotlyOutput("genre_a", height = "40em",
                                             width = "100%")
                       )
                       
                )
              )
      ),
      ###tab 3 starts
      tabItem(tabName = "triple",
              
              fluidRow(
                box(width = 12,
                    strong("Tab 3 Sentiment Score & Critic Preference", style = "font-size: 25px; font-weight: bold;"),
                    br(),
                    br(),
                    p("This tab aims to investigate how might movie scripts' emotions shape critics' 
                        and audiences' views on movies. The x-axis is the critic preference score using 
                        data from RottenTomatoes and the y-axis is the average sentiment score of each movie.",style="line-height:16px;font-size:13px"),
                    p("Based on our graph, we can see that there is no detectable linear relationship between 
                        sentiments and critic preference. Movies' genres also seem to be uncorrelated with the 
                        taste divergence between professionals and non-professionals.",style="line-height:16px;font-size:13px")
                )
              ),
              
              fluidRow(
                box(
                  title = tags$p("Sentiment Score vs. Critic Preference Ratio", style = "font-size: 130%; font-weight: bold"),
                  width = 12,
                  height = "50em",
                  plotlyOutput("plotly_plot")
                )
              )
              
      )
    ) #end of tabitems
      
    ) #end of dashboardBody
  ) #end of dashboardpage
), 

tabPanel("Read More",#new visualization layout here
         dashboardPage(
           dashboardHeader(title=span("Read More", style="font-size:20px;font-family: Century Gothic, fantasy;")),
           dashboardSidebar(
             collapsed = FALSE,
             chooseSliderSkin("Flat"),
             tags$head(
               tags$style(HTML(".selectize-input {height:10px;padding-top: 0px;}"))
             ),
             
             sidebarMenu(id="must",
               menuItem("Discussion & Conclusion", tabName = "conclusion", icon = icon("file-signature")),
               menuItem("About the Team", tabName = "author", icon = icon("user-secret"))
               
             )
           ),
           
           dashboardBody(
             shinyDashboardThemes(
               theme = "grey_dark"
             ),
             
             tabItems(
               # Conclusion tab content
               tabItem(tabName = "conclusion",
                       fluidRow(
                         box(width = 12,
                             strong("Conclusions and Discussions", style = "font-size: 25px; font-weight: bold;"),
                             br(),
                             br(),
                             p("We aim to disentangle and analyze the massive divergence of taste 
                        between film critics and common movie-goers. To begin with, we acquire and utilize a RottenTomatoes dataset 
                        containing approx. 2,000 most-reviewed movies, and 46 movie scripts scraped from the Internet. We also construct a key indicator—— 
                        the critic preference score——to measure how much a movie is better received by the critics than the audiences. This metric is the ratio of the audience rating to the critic rating (tomatometer).",style="line-height:18px;"),
                             p("Here are several key findings of our analysis:",style="line-height:18px;"),
                             strong("(1) Good movies are generally considered good, and bad movies are generally considered bad."),
                             p("The correlation between the critic scores and audience scores is significantly positive, which suggests that 
                        professional reviewers and average movie enjoyers genearlly coincide in their opinions towards a movie.",style="line-height:18px;"),
                             strong("(2) Art house and romantic films cause the greatest disaccords.",style="line-height:18px;"),
                             p("Arthouse & International, Animation, and Kids & Families are strongly preferred by critics, 
                        while Romance is more received by common audience. Plots, CGI, artistic style, and 
                        storyline tempo are ikely to  contribute to such divergences.",style="line-height:18px;"),
                             strong("(3) Sentiment doesn't matter."),
                             p("There is no discernable association of critic preference with positive or negative sentiment perceived in a movie's script.",style="line-height:18px;")
                         )
                       )
               ),
               tabItem(tabName="author","dafdsfasdfadsfasddfasd")
             )
           )
         )
)#end of tabPanel

)








server<-function(input, output,session) {
  
observe({
  if((input$box!="I. Scattered Ratings")&(!input$sidebarCollapsed)){
shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")}
  })
  
  observe({
    if((input$box=="I. Scattered Ratings")&(input$sidebarCollapsed)){
      shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")}
  })  
  
  
observe({
  if((length(input$genres)!=0)|(length(input$movie)!=0)){
    updateRadioButtons(session, "genre", selected = "All Movies")
  }
})

##################server of Jianing's Dashboard starts

observe({if(!input$must%in%c("conclusion","author")){
  updateTabItems(session, "must", selected = "conclusion")}
})

movie = reactive({
  df = movie_df %>%
    filter(name == input$name)
  return(df)
})

word_count = reactive({
  df = script_df %>%
    filter(name == input$name)
  result = get_frequency(df)
  return(result)
})

sentiment_df = reactive({
  sentiment_df = sentiment_df %>%
    filter(name == input$name) %>%
    select(c(8:17)) %>%
    t() %>%
    as.data.frame() %>%
    add_column(index = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
  
  return(sentiment_df)
})

big_value<-reactive({tags$p(input$name,style="font-size:35px;")})
middle_value<-reactive({tags$p(input$name,style="font-size:70%;line-height: 45px")})
small_value<-reactive({tags$p(input$name,style="font-size:60%;line-height: 45px;overflow:hidden")})
smallest_value <- reactive({tags$p(input$name,style="font-size:40%;line-height: 45px;overflow:hidden")})

output$name <- renderValueBox({
  valueBox(value = if(nchar(input$name)<=10){big_value()} 
           else if(nchar(input$name)<=20){middle_value()} 
           else if (nchar(input$name)<=35){small_value()}
           else {smallest_value()},
           subtitle = ("Movie Title"), 
           icon = icon("film"),
           color = "maroon", href = NULL)
})

output$rating <- renderValueBox({
  valueBox(value = tags$p(movie()$rating, style="font-size:80%;line-height: 45px"), 
           subtitle = tags$p("IMDB rating", style = "font-size: 90%;"), icon = icon("star") ,
           color = "light-blue", href = NULL)
})

output$sales <- renderValueBox({
  valueBox(value = tags$p(scales::dollar(round(as.numeric(gsub(",", "",movie()$gross)),2)),
                          style="font-size:70%;line-height: 45px;overflow:hidden"), 
           subtitle = ("Box Office"), icon = icon("dollar-sign"),
           color = "purple", href = NULL)
})

output$sentiment <- renderValueBox({
  valueBox(value = tags$p(round(movie()$sentiment,2), style="font-size:80%;line-height: 45px"), 
           subtitle = tags$p(HTML(paste0('Sentiment Score')), style = "font-size: 100%;"),
           icon = icon("smile"), color = "teal", href = NULL)
})



# Second Page reactive functions
big_value1<-reactive({tags$p(input$movie1,style="font-size:45px;")})
middle_value1<-reactive({tags$p(input$movie1,style="font-size:20px;line-height: 45px")})
small_value1<-reactive({tags$p(input$movie1,style="font-size:17px;line-height: 45px")})

output$moviename1 <- renderValueBox({
  valueBox(value = if(nchar(input$movie1)<=20){big_value1()} else if(nchar(input$movie1)<=40){middle_value1()} else{small_value1()}, 
           subtitle = ("Movie 1"), icon = icon("film"),
           color = "light-blue", href = NULL)
})

big_value2<-reactive({tags$p(input$movie2,style="font-size:45px;")})
middle_value2<-reactive({tags$p(input$movie2,style="font-size:20px;line-height: 45px")})
small_value2<-reactive({tags$p(input$movie2,style="font-size:17px;line-height: 45px")})  

output$moviename2 <- renderValueBox({
  valueBox(value = if(nchar(input$movie2)<=20){big_value2()} else if(nchar(input$movie2)<=40){middle_value2()} else{small_value2()}, 
           subtitle = ("Movie 2"), icon = icon("film"),
           color = "maroon", href = NULL)
})


movie1 = reactive({
  movie1 = movie_df %>% 
    filter(name == input$movie1)
  return(movie1)
})

movie2 = reactive({
  movie2 = movie_df %>% 
    filter(name == input$movie2)
  return(movie2)
})


output$sentiment1 <- renderValueBox({
  valueBox(value = round(movie1()$sentiment,2), 
           subtitle = ("Average Sentiment Score"), icon = icon("smile"),
           color = "light-blue", href = NULL)
})

output$rating1 <- renderValueBox({
  valueBox(value = movie1()$rating, 
           subtitle = ("IMDB Rating"), icon = icon("star"),
           color = "light-blue", href = NULL)
})

output$sentiment2 <- renderValueBox({
  valueBox(value = round(movie2()$sentiment,2),
           subtitle = ("Average Sentiment Score"), icon = icon("smile"),
           color = "maroon", href = NULL)
})

output$rating2 <- renderValueBox({
  valueBox(value = movie2()$rating,
           subtitle = ("IMDB Rating"), icon = icon("star"),
           color = "maroon", href = NULL)
})

output$wordcloud = renderPlot({
  set.seed(2020)
  par(bg="black") 
  wordcloud(word_count()$term, word_count()$count,
            max.words = 500, 
            random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(3, "Dark2"),
            scale=c(4,0.5))
})


output$wordcountplot = renderPlot({
  word_count() %>% slice_max(count, n = 15) %>%
    ggplot(., aes(x = reorder(term, count), y = count)) +
    geom_bar(stat='identity', width=0.8, alpha=0.6, fill = "slategray") +
    geom_text(aes(label=count),stat="identity",color="white", hjust=1.2, size=5.5) +
    labs(x = "Term", y = "Frequency",
         title = paste0("The Top 15 most frequent words in the movie \n", input$name)) +
    theme_minimal()+
    theme(axis.text=element_text(size=13, face = "bold", color = "white"),
          title = element_text(size = 15, face = "bold", color = "white"),
          axis.title=element_text(face = "bold", color = "white"),
          plot.subtitle = element_text(size=7, face = "bold", color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "white"),
          panel.background = element_rect(fill = "#343E48",
                                          colour = "#343E48",
                                          size = 0.5, linetype = "solid"),
          plot.background = element_rect(fill = "#343E48"),
          panel.border = element_blank()) +
    coord_flip()+
    scale_y_continuous(expand = c(0, 0))
})

output$sentimentplot = renderPlot({
  
  df1 = reactive({sentiment_line %>%
      filter(name == input$movie1) %>%
      select(-c(X, name)) %>%
      t() %>%
      as.data.frame() %>%
      add_column (name = input$movie1) %>%
      rename(sentiment = V1) %>%
      add_column(time = seq(0.05, 1, 0.05))})
  
  df2 = reactive({sentiment_line %>%
      filter(name == input$movie2) %>%
      select(-c(X, name)) %>%
      t() %>%
      as.data.frame() %>%
      add_column %>%
      add_column (name = input$movie2) %>%
      rename(sentiment = V1) %>%
      add_column(time = seq(0.05, 1, 0.05))})
  
  rbind(df1(), df2()) %>%
    ggplot(aes(x=time, y=sentiment, group=name, color=name)) +
    geom_line(size=2) +
    ylab("Sentiment Score") + 
    xlab("Fraction of the Script")+
    theme_minimal() +
    labs(color="Movie")+
    theme(legend.position="top",title = element_text(size = 15, face = "bold", color = "white"),
          axis.text=element_text(face = "bold", color = "white",size=12),
          axis.title=element_text(face = "bold", color = "white"),
          legend.text=element_text(face = "bold", color = "white",size=12),
          plot.subtitle = element_text(size=7, face = "bold", color = "white"),
          axis.line = element_line(colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#343E48",
                                          colour = "#343E48",
                                          size = 0.5, linetype = "solid"),
          plot.background = element_rect(fill = "#343E48")) +
    xlim(input$percentage[1]/100, input$percentage[2]/100)
})

output$genre_a = renderPlotly({
  df = movie_df %>%
    select(name, genre1) %>%
    left_join(sentiment_line, by = "name") %>%
    filter(genre1 == input$genre1) %>%
    select(-c(genre1, X)) %>%
    t() %>%
    as.data.frame() %>%
    row_to_names(row_number = 1) %>%
    add_column(time = seq(0.05, 1, 0.05)) %>%
    pivot_longer(cols = -time) %>%
    mutate(value = as.numeric(value))
  
  ggplotly(ggplot(data = df, aes(x=time, y=value, color=name)) +
             geom_line(size=0.6) +
             ggtitle("Sentiment Scores of Movies by Genre") +
             ylab("Sentiment Score") + 
             xlab("Fraction of the Script")+
             theme_minimal() +
             labs(color="Movie")+
             theme(title = element_text(size = 10, face = "bold", color = "white"),
                   axis.text=element_text(face = "bold", color = "white",size=8),
                   axis.title=element_text(face = "bold", color = "white"),
                   legend.text=element_text(face = "bold", color = "white",size=8),
                   plot.subtitle = element_text(size=5, face = "bold", color = "white"),
                   axis.line = element_line(colour = "white"),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill = "#343E48",
                                                   colour = "#343E48",
                                                   size = 0.5, linetype = "solid"),
                   plot.background = element_rect(fill = "#343E48")) +
             xlim(input$percentage[1]/100, input$percentage[2]/100))
})

output$plotly_plot = renderPlotly({
  plot_ly(plotly_data, x=~criticpreference, y=~sentiment, size=~gross, color=~Genre,
          type="scatter", sizes=c(50,800),
          markers=list(opacity=0.7),
          hoverinfo="text",
          hovertext=paste("Movie:",plotly_data$name, "<br> Sentiment:",plotly_data$sentiment,
                          "<br> Critic Preference:", plotly_data$criticpreference,
                          "<br> Box Office (M):", plotly_data$gross,
                          "<br> Genre:",plotly_data$Genre)) %>%
    layout(xaxis = list(title="Critic Preference (Critic rating/ Audience rating)", color = '#ffffff',showgrid=FALSE),
           yaxis = list(title="Sentiment Score", color = '#ffffff',showgrid=FALSE, zeroline=FALSE),
           legend = list(title="Genre", font = list(color = "white")),
           margin = 10,
           height = 600,
           paper_bgcolor='#343E48',
           plot_bgcolor='#343E48')
})

################Server of Jianing's Dashboard Ends


data= reactive({
    if(input$movie!=""){return(df[which(df$title==input$movie),])}
    if(length(input$genres)>0){return(df[which(unlist(lapply(df$genres, function(x) contain_yes(x,input$genres)))),])}
    a <- df[which((df$year>=input$yearend[1])&(df$year<=input$yearend[2])),]
    if(nrow(a)==0){return(a)}
    a <- a[which((a$revenue>=10^(input$revenue[1]-1))&(a$revenue<=10^(input$revenue[2]-1))),]

    if(input$genre!="All Movies"){a<-a[which(unlist(lapply(a$genres,function(x) input$genre%in%x))),]}
    return(a)
  })
  

output$audience <- renderValueBox({
  valueBox(
    value = tags$p(round(mean(data()$audience_rating,na.rm=TRUE),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
    subtitle=HTML('<p style="font-size:100%;font-family: Century Gothic, fantasy;line-height:16px">Average<br>Audience Rating</p>'),
    icon =tags$i(class = "fa fa-thumbs-o-up", style = "color:white;padding-right=10px;") ,
    width = 3,
    color = "maroon",
    href = NULL)
})

output$meter <- renderValueBox({
  valueBox(
    value = tags$p(round(mean(data()$tomatometer_rating,na.rm=TRUE),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
    subtitle=HTML('<p style="font-size:110%;font-family: Century Gothic, fantasy;line-height:16px">Average<br>Tomatometer</p>'),
    icon =tags$i(class = "fa fa-star-o", style = "color:white;padding-right=10px;") ,
    width = 3,
    color = "maroon",
    href = NULL)
})

output$correlation <- renderValueBox({
  valueBox(
    value = tags$p(round(cor(data()$tomatometer_rating,data()$audience_rating),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
    subtitle=HTML('<p style="font-size:120%;font-family: Century Gothic, fantasy;line-height:16px">Correlation<br>Coefficient</p>'),    
    icon =tags$i(class = "fa fa-line-chart", style = "color:white;padding-right=10px;") ,
    width = 3,
    color = "maroon",
    href = NULL)
})

love <- tags$i(class = "fa fa-heart-o", style = "color:gold;padding-right=10px;")
white <- tags$i(class = "fa fa-heart-o", style = "color:white;padding-right=10px;")
love_value<-reactive({tags$p(scales::percent((mean(data()$crt_prf,na.rm=TRUE))),style="font-size:38px;font-family: Century Gothic, fantasy;color:gold;")})
white_value<-reactive({tags$p(scales::percent((mean(data()$crt_prf,na.rm=TRUE))),style="font-size:38px;font-family: Century Gothic, fantasy;")})
output$ratio <- renderValueBox({
    valueBox(
      value = if (nrow(data())==0){"NaN"} else if(mean(data()$crt_prf,na.rm=TRUE)>1) {love_value()} else {white_value()},
      subtitle = HTML('<p style="font-size:110%;font-family: Century Gothic, fantasy; line-height:11px">Critic Preference<br>
                      <p style="font-size:80%;font-family: Century Gothic, fantasy;">Tomatometer / Audience Rating</p></p>'),
      icon =if (nrow(data())==0){white} else if(mean(data()$crt_prf,na.rm=TRUE)>1) {love} else {white},
      width = 3,
      color = "maroon")
  })
  

output$plot <- renderPlotly({
  if(nrow(data())==0){
    ggplotly(ggplot()+
               geom_polygon(data=tri,mapping=aes(x=x,y=y),color=NA,fill="grey",alpha=0.2)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               ggplot2::annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               theme(panel.border = element_rect(colour = "grey20", fill=NA, size=1),
                     plot.background = element_rect(fill = '#353d46', colour = 'red'),
                     text = element_text(size = 12, colour = "grey90",family = "Arial"),
                     axis.text.y = element_text(colour = "grey85",family = "Arial",size=10),
                     axis.text.x = element_text(colour = "grey85",family = "Arial",size=10))
             )%>%config(displayModeBar = F)
  }
  else if(nrow(data())==1){
    ggplotly(data()%>%ggplot()+
               geom_polygon(data=tri,mapping=aes(x=x,y=y),color=NA,fill="grey",alpha=0.2)+
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=primary_genre,text=paste("year:",year,"</br>",title)),alpha=0.4)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               ggplot2::annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               theme(legend.position="none",
                     panel.border = element_rect(colour = "grey20", fill=NA, size=1),
                     plot.background = element_rect(fill = '#353d46', colour = 'red'),
                     text = element_text(size = 12, colour = "grey90",family = "Arial"),
                     axis.text.y = element_text(colour = "grey85",family = "Arial",size=10),
                     axis.text.x = element_text(colour = "grey85",family = "Arial",size=10))
             )%>%config(displayModeBar = F)
    
  }
  else if(nrow(data())==nrow(df)){
    ggplotly(data()%>%ggplot()+
               geom_polygon(data=tri,mapping=aes(x=x,y=y),color=NA,fill="grey",alpha=0.2)+
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=primary_genre,text=paste("year:",year,"</br>",title)),alpha=0.4)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               ggplot2::annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               theme(legend.position="none",
                     panel.border = element_rect(colour = "grey20", fill=NA, size=1),
                     plot.background = element_rect(fill = '#353d46', colour = 'red'),
                     text = element_text(size = 12, colour = "grey90",family = "Arial"),
                     axis.text.y = element_text(colour = "grey85",family = "Arial",size=10),
                     axis.text.x = element_text(colour = "grey85",family = "Arial",size=10))
    )%>%config(displayModeBar = F)
  }
  else{
    ggplotly(data()%>%ggplot()+
               geom_polygon(data=tri,mapping=aes(x=x,y=y),color=NA,fill="grey",alpha=0.2)+
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=primary_genre,text=paste("year:",year,"</br>",title)),alpha=0.4)+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0))+
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               geom_line(aes(audience_rating,tomatometer_rating),
                           stat="smooth",formula = y~x,method = "lm",size=2, se =FALSE,color="darkred",alpha=0.7)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               ggplot2::annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5,color="grey20")+
               theme_minimal()+
               theme(legend.position="none",
                     panel.border = element_rect(colour = "grey20", fill=NA, size=1),
                     plot.background = element_rect(fill = '#353d46', colour = 'red'),
                     text = element_text(size = 12, colour = "grey90",family = "Arial"),
                     axis.text.y = element_text(colour = "grey85",family = "Arial",size=10),
                     axis.text.x = element_text(colour = "grey85",family = "Arial",size=10))
             )%>%config(displayModeBar = F)
  }

  })
  
}


#options(shiny.sanitize.errors = TRUE)
shinyApp(ui = ui, server = server)



