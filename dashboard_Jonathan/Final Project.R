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

movie_df = read.csv("movie_df_final1.csv")
script_df = read.csv("script_df_final1.csv")
sentiment_line = read.csv("sentiment_line.csv")
plotly_data = read.csv("movie.csv")

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


######UI start
ui<-dashboardPage(
    dashboardHeader(title =span("Script Sentiment Analysis",style="font-size:18px;")),
    dashboardSidebar(
      collapsed = FALSE,
      chooseSliderSkin("Flat"),
      tags$head(
        tags$style(HTML(".selectize-input {height:10px;padding-top: 0px;}"))
      ),
      
      sidebarMenu(
        menuItem("Wordcloud & Term Frequency", tabName = "single", icon = icon("film")),
        menuItem("Sentiment Arc Analysis", tabName = "double", icon = icon("th")),
        menuItem("Sentiment vs. Critic Preference", tabName = "triple", icon = icon("theater-masks"))
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
                  box(width = 12,
                      strong("Overview", style = "font-family: 'Arial'; font-size: 25px;"),
                      # p("As human beings, we show great empathy to individuals who 
                      #   have undergone sorrows that are similar to ours and have strong feelings 
                      #   towards events that echo our own life experiences. Hence, there may be some systematic 
                      #   differences between professional and non-professional audiences in terms of previous 
                      #   experiences that further impact how they respond to certain emotions and how they perceive a given movie."),
                       p("Twists and turns, ups and downs, despair and exhilaration on the big screen can all be recorded and 
                        reflected in movie scripts. We scraped the scripts of a total of 46 best-rated movies 
                        on IMDB and analyze their sentiment and emotional valence with visualizations from multiple perspectives. compare their critic preference score based on different genres."),
                      p("Tab 1 aims to retrieve and display various facts of a selected film including the title, rating, box office, and information about movie script: the 
                        sentiment score, a word cloud b as well as a word frequency bar chart. Tab 2 allows for comparing the sentiment in pairs across their running times. Tab 3 utilizes 
                        a bubble chart to demonstrate the relationship between the overall sentiment scores 
                        and the critic preference ratio.")
                  )
                ),
                fluidRow(                  box(width = 12,
                                               strong("Tab 1 Wordcloud & Term Frequency", style = "font-family: 'Arial'; font-size: 25px;"),
                                               br(),
                                               p("This tab takes a peek at the scripts by drawing word clouds and word 
                      frequency bar charts. The most common words and terms are highlighted in the word cloud. The character names are not stripped from the original 
                        script. Therefore the most frequent terms are likely to be the names of the movie characters with the most screen presense.",
                                                 style = "font-family: 'Arial';")
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
        
        tabItem(tabName = "double",
                
                fluidRow(
                  box(width = 12,
                      strong("Tab 2 Sentiment Arc Analysis", style = "font-size: 25px; font-weight: bold;"),
                      p("This tab examines in more detail the positive or negative connocations in scripts with two sub-tabs."),
                      p("Compare Sentiments of Two Movies", style = "text-decoration: underline;"),
                      p("The line chart displays how the sentiment in the scripts changes as the movie 
                        progresses to the end. The y-axis is the computed sentiment score, and the x-axis denotes 
                        the fraction of the script (from 0 to 1) we're looking at. You can interact with the plot by select two movies to compare 
                        from the drop-down menu and look at the dynamics and flucuations of sentiment
                        in the film speeches."),
                      p("Sentiment Plot by Genre", style = "text-decoration: underline;"),
                      p("This line chart illustrates and juxtaposed the sentiment scores across the running time for movies of one particular genre. The y-axis is the calculated sentiment score, and the 
                        x-axis is the fraction of the script (from 0 to 1). You can manually select any genre from the drop-down menu.")
                  )
                ),
                
                # fluidRow(
                #   tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                #   valueBoxOutput("moviename1", width = 6),
                #   valueBoxOutput("moviename2", width = 6)
                # ),
                
                fluidRow(
                  valueBoxOutput("sentiment1", width = 3),
                  valueBoxOutput("rating1", width = 3),
                  valueBoxOutput("sentiment2", width = 3),
                  valueBoxOutput("rating2", width = 3)
                ),
                
                fluidRow(
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
                                  plotlyOutput("genre", height = "40em",
                                             width = "100%")
                         )
                         
                  )
                )
        ),
        
        tabItem(tabName = "triple",
                
                fluidRow(
                  box(width = 12,
                      strong("Tab 3 Sentiment Score vs. Critic Preference Ratio", style = "font-size: 25px; font-weight: bold;"),
                      p("This tab aims to investigate how might movie scripts' emotions shape critics' 
                        and audiences' views on movies. The x-axis is the critic preference score using 
                        data from RottenTomatoes and the y-axis is the average sentiment score of each movie."),
                      p("Based on our graph, we can see that there is no detectable linear relationship between 
                        sentiments and critic preference. Movies' genres also seem to be uncorrelated with the 
                        taste divergence between professionals and non-professionals.")
                  )
                ),
                
                fluidRow(
                  box(
                    title = tags$p("Sentiment Score vs. Critic Preference Ratio", style = "font-size: 130%; font-weight: bold"),
                    width = 12,
                    height = "50em",
                    plotlyOutput("plotly_plot")
                  )
                ),
                
                fluidRow(
                  box(width = 12,
                      strong("Conclusions and Discussions", style = "font-size: 25px; font-weight: bold;"),
                      p("We aim to disentangle the commonly seen divergence between the taste differences 
                        between movie critics and ordinary audiences. We deploy a RottenTomatoes dataset 
                        containing approx. 2,000 popular movies from 1925 to 2017, a self-constructed 
                        movie script dataset scraped from the Internet. We also developed a key metric-- 
                        the critic preference score-- to measure the extent to which a given movie is 
                        loved by critics but not so much by audiences. This metric is calculated as the 
                        critic score divided by the audience score."),
                      p("Here are our major findings:"),
                      strong("(1) There is no doubt that good movies are good, and bad movies are bad."),
                      p("The correlation coefficients between critic scores and audience scores are 
                        always positive and remain high during the 92-year time span, which means 
                        professional and non-professionals constantly share the same opinion about a movie."),
                      strong("(2) Arthouse and romantic movies see the most disagreements."),
                      p("Arthouse & International, Animation, and Kids & Families are more preferred by critics, 
                        whistle Romance is more enjoyed by the audience. Plots, CGI, artistic style, and 
                        storyline tempo contribute most to their debates."),
                      strong("(3) Sentiments mined through scripts are not associated with the taste difference."),
                      p("There is no discernable correlation between the preference divergence between movie 
                        dialogues' texts and the rating distinction.")
                  )
                ),
        )
        
      )
    )
)



####end UI

server<-function(input, output,session) {
  
  # First Page reactive functions
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
             subtitle = tags$p(HTML(paste0('Sentiment Score')), style = "font-size: 80%;"),
             icon = icon("smile"), color = "teal", href = NULL)
  })
  
  
  
  # Second Page reactive functions
  big_value1<-reactive({tags$p(input$movie1,style="font-size:45px;")})
  middle_value1<-reactive({tags$p(input$movie1,style="font-size:20px;line-height: 45px")})
  small_value1<-reactive({tags$p(input$movie1,style="font-size:17px;line-height: 45px")})
  
  output$moviename1 <- renderValueBox({
    valueBox(value = if(nchar(input$movie1)<=20){big_value1()} else if(nchar(input$movie1)<=40){middle_value1()} else{small_value1()}, 
             subtitle = ("Title of the First Movie"), icon = icon("film"),
             color = "light-blue", href = NULL)
  })
  
  big_value2<-reactive({tags$p(input$movie2,style="font-size:45px;")})
  middle_value2<-reactive({tags$p(input$movie2,style="font-size:20px;line-height: 45px")})
  small_value2<-reactive({tags$p(input$movie2,style="font-size:17px;line-height: 45px")})  
  
  output$moviename2 <- renderValueBox({
    valueBox(value = if(nchar(input$movie2)<=20){big_value2()} else if(nchar(input$movie2)<=40){middle_value2()} else{small_value2()}, 
             subtitle = ("Title of the Second Movie"), icon = icon("film"),
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
      theme(title = element_text(size = 15, face = "bold", color = "white"),
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
  
  output$genre = renderPlotly({
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
    
    ggplotly(ggplot(df, aes(x=time, y=value, color=name)) +
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
             title = list(text = 'Critic Preference vs. Sentiment Score', font = list(color = "white")),
             legend = list(title="Genre", font = list(color = "white")),
             margin = 10,
             height = 600,
             paper_bgcolor='#343E48',
             plot_bgcolor='#343E48')
  })
  
  
}




shinyApp(ui = ui, server = server)
