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

df<-read.csv("/Users/lingyunfan/all_repos/cinema_project/raw.csv")
tri<-data.frame(x=c(0,100,100), y=c(0,0,100), t=c('a', 'a', 'a'), r=c(1,2,3))
regression<-lm(tomatometer_rating~audience_rating, data = df)
alpha<-coef(regression)[1]
beta<-coef(regression)[2]
ticks<-seq(1925,2010,10)


ui<-dashboardPage(
  #dashboardHeader(tags$li(class = "dropdown",tags$style(".main-header {max-width: 200px};")),
  dashboardHeader(title =span("Critics VS. Audience",style="font-size:20px; font-family: Century Gothic, fantasy;")),
  
  dashboardSidebar(
    
    collapsed = FALSE,
    chooseSliderSkin("Flat"),
    tags$h4("Movie Filters:",style = "padding-left:10px;padding-bottom:10px;padding-top:10px;font-size: 20px;font-family: Century Gothic, fantasy;"),
    hr(),
    tags$h5("Range of release year:",style = "padding-left:10px;font-weight: bold;margin-bottom:0px;"),
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    sliderInput("yearend",
                label = "",
                min = 1925,
                max = 2017,
                ticks=FALSE,
                value=c(1925,2017),
                sep=""),
    tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('yearend').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 10);})
      ")),
    br(),
    tags$h5("Range of run time (minutes):",style = "padding-left:10px;font-weight: bold;margin-bottom:0px;"),
    sliderInput("time",
                label = "",
                min = 0,
                max = 240,
                ticks=FALSE,
                value=c(0,240),
                sep=""),
    tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('time').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 50);})
      ")),
    br(),
    tags$h5("Check out each genre:",style = "padding-left:10px;font-weight: bold;margin-bottom:0px;"),
    tags$h5("(The genres are sorted in descending order of critic preference)",
            style = "padding-left:10px;padding-right:0px;margin-bottom:0px;font-size: 12px;"),
    
    radioButtons(inputId = "genre", label = "",
                choices = list("Animation" = "Animation","Drama" = "Drama","Horror"="Horror","Crime"="Crime", "Thriller"="Thriller" ,"All"="All Movies",
                               "Romance"="Romance","Science Fiction"="Science Fiction","Comedy" = "Comedy","Fantasy"="Fantasy","Action" = "Action"),
                selected = "All Movies"),
    hr(),
    helpText("Data from Rotten Tomatoes",
             style = "left: 0.8em; position: relative;")
  ),
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tags$head(tags$style(HTML('.small-box .icon-large {right: 15px;}'))),
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height:120px};"))),
      valueBoxOutput("audience"),
      valueBoxOutput("meter"),
      valueBoxOutput("ratio")
    ),
    fluidRow(
      tabBox(
        width = 8,
        height = "45em",
        title = tagList(shiny::icon("bar-chart-o")),
        tabPanel("Audience and Critic Ratings",
                 plotlyOutput(outputId = "plot", height = "40em")),
        tabPanel("20 Films the Critics And Audience Disagree on the Most",
                 div(img(src = "gaps.png", height = 600, width = 400), style="text-align: center;")
                 )
        ),
      
      box(width = 4,
          tags$h4("Are fim critics losing sync with audiences?",style = "padding-left:3px;margin-bottom:0px;font-family: Century Gothic, fantasy;"),
          tags$h4("What kind of movies that are acclaimed by professional reviewers are yet not likely to be enjoyed by most moviegoers?",style = "padding-left:3px;margin-bottom:0px;font-family: Century Gothic, fantasy;"),
          tags$h4("Visulizations would answer these questions strikingly.",style = "padding-left:3px;margin-bottom:0px;font-family: Century Gothic, fantasy;")),

      box(
        width = 4,
        height = "40em",
        tags$style("intro {padding-left:0px; font-size:15px;}"),
        HTML("<intro><b>Tab 1</b></intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro><b>The bubble chart</b> displays the distributions of Rotten Tomatoes' critic and audience ratings (the critic ratings are technically refers to as the <em>Tomatometers</em>) for more than 2000 popular films. Box-office revenue is represented by bubble size.</intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>Filter those movies, and you may find the Tomatometers covary with the audience scores in a different manner (<b>the red line that fit the filtered data points best should be detached from the grey one</b>).</intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>The <b>average ratio of Tomatometer rating to audience rating</b> is used as a proxy for critic preference, which is reported in the box at the upper right corner.</intro>"),
        hr(),
        HTML("<intro><b>Tab 2</b></intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>This chart displays the 20 movies with the greatest difference between Rotten Tomatoes' critic and audience ratings in the dataset. </intro>")
        
        )

    )
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  
)



server<-function(input, output,session) {
  

data= reactive({
    a <- df[which((df$year>=input$yearend[1])&(df$year<=input$yearend[2])),]
    a <- a[which((a$runtime.x>=input$time[1])&(a$runtime.x<=input$time[2])),]
    if(input$genre!="All Movies"){a<-a[which(a$genre1==input$genre),]}
    return(a)
  })
  
output$audience <- renderValueBox({
  valueBox(
    value = tags$p(round(mean(data()$audience_rating,na.rm=TRUE),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
    subtitle = tags$p("Average Audience Rating",style="font-size:120%;font-family: Century Gothic, fantasy;"),
    icon =tags$i(class = "fa fa-thumbs-o-up", style = "color:white;padding-right=10px;") ,
    width = 2,
    color = "maroon",
    href = NULL)
})

output$meter <- renderValueBox({
  valueBox(
    value = tags$p(round(mean(data()$tomatometer_rating,na.rm=TRUE),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
    subtitle = tags$p("Average Tomatometer Rating",style="font-size:110%;font-family: Century Gothic, fantasy;"),
    icon =tags$i(class = "fa fa-star-o", style = "color:white;padding-right=10px;") ,
    width = 2,
    color = "maroon",
    href = NULL)
})

output$ratio <- renderValueBox({
    valueBox(
      value = tags$p(round(mean(data()$crt_prf,na.rm=TRUE),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
      subtitle = tags$p("Preference from Critics",style="font-size:130%;font-family: Century Gothic, fantasy;"),
      icon =tags$i(class = "fa fa-heart-o", style = "color:white;padding-right=10px;") ,
      width = 2,
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
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
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
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=content_rating,text=title),alpha=0.4)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               theme(panel.border = element_rect(colour = "grey20", fill=NA, size=1),
                     plot.background = element_rect(fill = '#353d46', colour = 'red'),
                     text = element_text(size = 12, colour = "grey90",family = "Arial"),
                     axis.text.y = element_text(colour = "grey85",family = "Arial",size=10),
                     axis.text.x = element_text(colour = "grey85",family = "Arial",size=10))
             )%>%config(displayModeBar = F)
    
  }
  else if(input$genre=="All Movies"){
    ggplotly(data()%>%ggplot()+
               geom_polygon(data=tri,mapping=aes(x=x,y=y),color=NA,fill="grey",alpha=0.2)+
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=content_rating,text=title),alpha=0.4)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               theme(panel.border = element_rect(colour = "grey20", fill=NA, size=1),
                     plot.background = element_rect(fill = '#353d46', colour = 'red'),
                     text = element_text(size = 12, colour = "grey90",family = "Arial"),
                     axis.text.y = element_text(colour = "grey85",family = "Arial",size=10),
                     axis.text.x = element_text(colour = "grey85",family = "Arial",size=10))
    )%>%config(displayModeBar = F)
  }
  else{
    ggplotly(data()%>%ggplot()+
               geom_polygon(data=tri,mapping=aes(x=x,y=y),color=NA,fill="grey",alpha=0.2)+
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=content_rating,text=title),alpha=0.4)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               geom_line(aes(audience_rating,tomatometer_rating),
                           stat="smooth",formula = y~x,method = "lm",size=2, se =FALSE,color="darkred",alpha=0.7)+
               labs(size="",color="",x="Audience Rating",y="Tomatometer Rating")+
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
               theme(panel.border = element_rect(colour = "grey20", fill=NA, size=1),
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



