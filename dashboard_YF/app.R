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


ui<-navbarPage("  Movie Facts",  tags$style(HTML(".navbar .navbar-default .navbar-static-top {margin-bottom:0px;padding-bottom:0px;}")),


               tabPanel("Critics VS. Audience",
                        #tags$style(HTML(".tabbable > .nav > li > a {margin:0px;padding:0px;}")),
               dashboardPage(
  #dashboardHeader(tags$li(class = "dropdown",tags$style(".main-header {max-width: 200px};")),
  dashboardHeader(title =span("Critics VS. Audience",style="font-size:20px; font-family: Century Gothic, fantasy;")),
  
  dashboardSidebar(
    collapsed = FALSE,
    chooseSliderSkin("Flat"),
    tags$head(
      tags$style(HTML(".selectize-input {height:10px;padding-top: 0px;}"))
    ),
 
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
          tags$h4("Are film critics losing sync with audiences?",style = "padding-left:3px;margin-bottom:0px;font-family: Century Gothic, fantasy;color:gold;"),
          tags$h4("What kind of movies that are acclaimed by professional reviewers are yet not likely to be enjoyed by most moviegoers?",style = "color:gold;padding-left:3px;margin-bottom:0px;font-family: Century Gothic, fantasy;"),
          tags$h4("Visulizations answer such questions strikingly.",style = "padding-left:3px;margin-bottom:0px;font-family: Century Gothic, fantasy;color:gold;")),

      box(
        width = 4,
        height = "40em",
        tags$style("intro {padding-left:0px;font-size:14px;line-height:1;margin-bottom:0px;}"),
        HTML("<intro><b>Tab 1</b></intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro><b>The bubble chart</b> displays the distribution of Rotten Tomatoes' critic ratings (a.k.a. <b>Tomatometers</b>) and audience ratings for more than 2000 popular films. Box-office revenue is represented by bubble size, and the color of a bubble indicates the primary genre of the film.</intro>"),
        tags$br(),
        tags$br(),
        HTML("<intro>Filter those movies, and you may find the Tomatometers covary with the audience scores in different manners (<b>the red line that fit the filtered data points best should be detached from the grey one</b>).</intro>"),
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
),
#############################End of 凌云帆's UI#################################
tabPanel("2"),#new visualization layout here
tabPanel("3"),#new visualization layout here
tabPanel("4")#new visualization layout here
)



server<-function(input, output,session) {
  

observe({
  if((length(input$genres)!=0)|(length(input$movie)!=0)){
    updateRadioButtons(session, "genre", selected = "All Movies")
  }
})
  
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
    subtitle = tags$p("Average Audience Rating",style="font-size:110%;font-family: Century Gothic, fantasy;"),
    icon =tags$i(class = "fa fa-thumbs-o-up", style = "color:white;padding-right=10px;") ,
    width = 2,
    color = "maroon",
    href = NULL)
})

output$meter <- renderValueBox({
  valueBox(
    value = tags$p(round(mean(data()$tomatometer_rating,na.rm=TRUE),2),style="font-size:38px;font-family: Century Gothic, fantasy;"),
    subtitle = tags$p("Average Tomatometer Rating",style="font-size:100%;font-family: Century Gothic, fantasy;"),
    icon =tags$i(class = "fa fa-star-o", style = "color:white;padding-right=10px;") ,
    width = 2,
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
      subtitle = tags$p("Critic Preference",style="font-size:130%;font-family: Century Gothic, fantasy;"),
      icon =if (nrow(data())==0){white} else if(mean(data()$crt_prf,na.rm=TRUE)>1) {love} else {white},
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
               geom_point(aes(audience_rating,tomatometer_rating,size=revenue,color=primary_genre,text=paste("year:",year,"</br>",title)),alpha=0.4)+
               theme_minimal()+
               scale_x_continuous(breaks=seq(25,100,25),limits = c(0,100),expand = c(0,0)) +
               scale_y_continuous(limits = c(0,100),expand = c(0,0)) +
               geom_abline(intercept=alpha,slope=beta,color="grey47",alpha=0.6,size=2)+
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
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
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5)+
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
               annotate("text", x = 7, y = 5, label = "line of best fit\nfor all movies",size=2.5,color="grey20")+
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



