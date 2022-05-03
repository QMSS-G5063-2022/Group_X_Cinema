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

movie_df = read.csv("movie_df_final1.csv")
script_df = read.csv("script_df_final1.csv")
sentiment_line = read.csv("sentiment_line.csv")

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
    dashboardHeader(title =span("Critics VS. Audience",style="font-size:20px;")),
    dashboardSidebar(
      collapsed = FALSE,
      chooseSliderSkin("Flat"),
      tags$head(
        tags$style(HTML(".selectize-input {height:10px;padding-top: 0px;}"))
      ),
      
      sidebarMenu(
        menuItem("Single Movie", tabName = "single", icon = icon("th")),
        menuItem("Comparing Movies", tabName = "double", icon = icon("th")),
        menuItem("Sentiment vs. Rating Difference", tabName = "triple", icon = icon("th"))
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
                  valueBoxOutput("name", width = 3),
                  valueBoxOutput("rating", width = 3),
                  valueBoxOutput("sales", width = 3),
                  valueBoxOutput("sentiment", width = 3)
                ),
                
                fluidRow(
                  box(
                    title = "Controls",
                    width = 3,
                    height = "44.5em",
                    
                    selectInput("name",
                                "Choose the Movie:",
                                choices = unique(movie_df$name)
                                
                    )
                    
                  ),
                  
                  tabBox(width = 6,
                    tabPanel(title = "Word Cloud",
                             plotOutput("wordcloud", height = "40em",
                                        width = "100%")
                    ),
                    
                    tabPanel("Word Frequency", 
                             plotOutput("wordcountplot", height = "40em",
                                        width = "100%")
                    )
                    
                  ),
                  
                  box(width = 3,
                      height = "44.5em",
                      tags$h4("There are some text input")
                  )
                )
   
        ),
        
        tabItem(tabName = "double",
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
                  box(
                    title = "Controls",
                    width = 3,
                    height = "44.5em",
                    
                    selectInput("movie1",
                                "Choose the First Movie:",
                                choices = unique(movie_df$name)
                      
                    ),
                    
                    selectInput("movie2",
                                "Choose the Second Movie:",
                                choices = c(unique(movie_df$name)),
                                            selected = "The Godfather"
                    ),
                    
                    sliderTextInput(
                      inputId = "percentage",
                      label = "Choose a percentile range:", 
                      choices = seq(0,1,0.05),
                      selected = c(0, 1.0)
                    ),

                    selectInput("genre1",
                                "Choose a genre:",
                                choices = c(unique(movie_df$genre1)),
                                selected = "Drama"
                    )
                    
                  ),
                  
                  
                  tabBox(width = 9,
                         tabPanel(title = "Sentiment Plot",
                                  plotOutput("sentimentplot", height = "40em",
                                             width = "100%")
                         ),
                         
                         tabPanel(title = "Genre", 
                                  plotOutput("genre", height = "40em",
                                             width = "100%")
                         )
                      
                  )
                ),
                
                tabItem(tabName = "triple",
                        fluidRow(
                          
                        )
                        
                )
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
  
  output$name <- renderValueBox({
    valueBox(value = movie()$name, subtitle = ("Name of the Movie"), icon = icon("film") ,
      color = "maroon", href = NULL)
  })
  
  output$rating <- renderValueBox({
    valueBox(value = movie()$rating, subtitle = ("IMDB rating"), icon = icon("star") ,
             color = "light-blue", href = NULL)
  })
  
  output$sales <- renderValueBox({
    valueBox(value = scales::dollar(round(as.numeric(gsub(",", "",movie()$gross)),2)), subtitle = ("Box Office"), icon = icon("dollar-sign"),
             color = "purple", href = NULL)
  })
  
  output$sentiment <- renderValueBox({
    valueBox(value = round(movie()$sentiment,2), subtitle = ("Sentiment Score"), icon = icon("smile"),
             color = "teal", href = NULL)
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
      labs(x = "Term", y = "Frequency",
           title = paste0("The Top 15 most frequent words in the movie ", input$name)) +
      theme_minimal()+
      theme(axis.text=element_text(size=8, face = "bold", color = "white"),
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
      ggtitle("Sentiment Score Visualization") +
      ylab("Sentiment Score") + 
      xlab("Percentage of the Script")+
      theme_minimal() +
      labs(color="Title")+
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
      xlim(input$percentage[1], input$percentage[2])
  })
  
  output$genre = renderPlot({
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
    
    ggplot(df, aes(x=time, y=value, group=name, color=name)) +
      geom_line(size=1) +
      ggtitle("Sentiment Score Visualization by Genre") +
      ylab("Sentiment Score") + 
      xlab("Percentage of the Script")+
      theme_minimal() +
      labs(color="Title")+
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
      xlim(input$percentage[1], input$percentage[2])
  })
  
  
}




shinyApp(ui = ui, server = server)
