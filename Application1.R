#install.packages("shinydashboard")
#install.packages("leaflet")

# app.R ##
library(shiny)
library(shinydashboard)
library(leaflet)
library(readr)
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(memoise)
library(globals)
library(lubridate)

## setwd("C:/Users/Arpita/Desktop/MGM")
lat_long <- read.csv("WWW/lat_long.csv")
Comment_sample50 <- read_csv("Data/Comment_sample50.csv")
Sent_rating <- read_csv("Data/Sent_rating.csv")
Sent_rating$Year = year(dmy(Sent_rating$jsondat.Reviews.Date))
yearly_com <- read_csv("WWW/yearly_com.csv")



Sent_rating$sentiment_class =as.factor(Sent_rating$sentiment_class)
Sent_rating$Service = as.factor(Sent_rating$Service)
Sent_rating$Cleanliness = as.factor(Sent_rating$Cleanliness)
Sent_rating$Overall = as.factor(Sent_rating$Overall)
Sent_rating$Value = as.factor(Sent_rating$Value)
Sent_rating$Sleep.Quality = as.factor(Sent_rating$Sleep.Quality)
Sent_rating$Location = as.factor(Sent_rating$Location)
Sent_rating$Sleep.Quality = as.factor(Sent_rating$Sleep.Quality)
Sent_rating$Rooms = as.factor(Sent_rating$Rooms)
Sent_rating$Year = as.factor(Sent_rating$Year)
yearly_com$Year = as.factor(yearly_com$Year)
yearly_com$Hotel_Name = as.factor(yearly_com$Hotel_Name)

# Comment_sample50$``=NULL
ui <- dashboardPage(
  dashboardHeader(title = "MGM Resorts Internatioal"),
  
  dashboardSidebar(
    includeCSS("WWW/styles.css"),
    #includeScript("www/d3.v3.min.js"),
    #includeScript("www/d3-tip.js"),
    
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home", lib="font-awesome")),
      menuItem("Sentiment Analysis", tabName = "senti", icon = icon("th", lib="font-awesome")),
      menuItem("Rating Insights", tabName = "graph", icon = icon("th", lib="font-awesome")),
      menuItem("Comments Hub", tabName = "comments", icon = icon("share-alt", lib="font-awesome")),
      menuItem("Comments Topic Modeling", tabName = "topic", icon = icon("share-alt", lib="font-awesome")),
      menuItem("Property map", tabName = "map", icon = icon("map", lib="font-awesome")),
      menuItem("About", tabName = "about", icon = icon("info-circle", lib="font-awesome"))
    )
  ),
  
  dashboardBody(
      tabItems(
        tabItem(tabName = "welcome",
              column(12,img(filepath = "WWW/", src="combine_images.jpg",weidth ="100%")),
              hr(),hr(),hr(),
              box(width = 12,h4(includeText("WWW/Company_Details1.txt"))),
              box(width = 12,h4(includeText("WWW/Company_Details2.txt")))
              
      ),
      tabItem(tabName = "senti",
              h2(strong("Customer's feedback analysis - Text Mining"))
              ,box(width = 12,fluidRow(column(11,tabsetPanel(
                                       tabPanel("All Hotels", h2(''), h3(strong(textOutput("head11"))), plotlyOutput("yearplotly"),tags$li("Above line plot shows the increasing/decreasing number of review comments
provided by the users from June-2003 to May-2012."),tags$li("Year 2011 looks a very significant year in terms of MGM hotel popularity as customers were very vocal about their experience at MGM."),tags$li("The graph is plotted based on 35115 number of comments provided by TripAdvisor.")),
                                       tabPanel("Sentiment Analysis", h2('') , selectInput("selection11", "Select your choice of hotel:", choices = list("MGM Grand Hotel and Casino"=1,"Signature at MGM Grand"=2,"Skylofts at MGM Grand"=3,
                                                                                                                                                  "ARIA Resort & Casino"=4,"Bellagio Las Vegas"=5,"The Mirage Hotel & Casino" = 6))
                                                                        , h3(strong(textOutput("head1"))),plotlyOutput("sentiment"),tags$li("Above bar plot shows the customer's sentiment for each hotel."),tags$li("Customer's sentiment is measured by using text mining algorithms."),tags$li("By monitoring customer's sentiment, MGM can improve its hospitality service."))
                            ))
                    )
              )
      ),
      tabItem(tabName = "graph",
              h2(strong("How are MGM Las Vegas Properties performing ?")),
              box(width = 12,fluidRow(column(11,
                                box(width = NULL,
                                    fluidRow(
                                      column(width = 5,selectInput("selection1", "Select your choice of hotel:", choices = list("MGM Grand Hotel and Casino"=1,"Signature at MGM Grand"=2,"Skylofts at MGM Grand"=3,
                                                                                                          "ARIA Resort & Casino"=4,"Bellagio Las Vegas"=5,"The Mirage Hotel & Casino" = 6)))),
                                   
                                      htmlOutput("rating_category"), h3(strong(textOutput("head2"))),plotlyOutput("rating"),tags$li("Above visualization is from the User's rating (1 being lowest and 5 being highest rating) for each inividual hotel and individual hotel features."),tags$li("This part of analysis is very useful to understand the connection between Hotel's Overall popularity and different hotel features."),tags$li("In terms of Overall Rating, Bellagio Las vegas is the most popular hotel among the travellers.")
                                     
                                )
                          )
                        
                ))
       ),
      tabItem(tabName = "comments",
              h2(strong("User Comments provided by TripAdvisor")), 
              fluidRow(column(6,selectInput("selection", "Select your choice of hotel:", choices = list("MGM Grand Hotel and Casino"=1,"Signature at MGM Grand"=2,"Skylofts at MGM Grand"=3,
                                                                                                        "ARIA Resort & Casino"=4,"Bellagio Las Vegas"=5,"The Mirage Hotel & Casino" = 6))),
                       column(6,sliderInput("integer", "Number of words :",
                                   min=0, max=500, value=100))
                       ),
              
              fluidRow(plotOutput("img1")),
              
              fluidRow(column(12,DT::dataTableOutput("table")))
              
      ),
      tabItem(tabName = "topic",
              h2(strong("Topic Modeling using customer feedback.")),
              h3("Select your choice of Hotel and click the link provided below which will lead you to Topic Modeling interactive web page."),
              box(width =12,fluidRow(column(11,selectInput('website', 'Choose a Hotel: '
                                                           , list("MGM Grand Hotel and Casino" = "http://bl.ocks.org/ArpitaM01/raw/f181c73ff358eaebdddf24d813137d6f/#topic=2&lambda=0.14&term="
                                                                  , "Bellagio Las Vegas" = "http://bl.ocks.org/ArpitaM01/raw/f7bc5b33749fa7c3567f8f71c857b938/"
                                                                  , "Signature at MGM Grand" = "http://bl.ocks.org/ArpitaM01/raw/ffe4907302e4905b9c628e91faa6eb2c/#topic=0&lambda=1&term=")), hr(),hr(), htmlOutput("mySite")
                                      )
                            )
              )
      ),
      tabItem(tabName = "map",
              h2(strong("Las Vegas Property location")), 
              box(width=12,fluidRow(column(11,leafletOutput('map'),h4("List of MGM Las Vegas properties:"),tags$li("Bellagio Las Vegas")
                                           ,tags$li("MGM grand"),tags$li("Bellagio Las Vegas"),tags$li("Vdara Hotel and spa")
                                           ,tags$li("The Signature at MGM grand"),tags$li("Mandalay Bay"),tags$li("Delano Las Vegas")
                                           ,tags$li("Mirage Las Vegas"),tags$li("Monte Carlo Las Vegas Resort & Casino")
                                           ,tags$li("New york New york"),tags$li("Luxor Las Vegas"),tags$li("Excalibur Las Vegas")
                                           ,tags$li("Bellagio Las Vegas"),tags$li("Circus Circus"))
              ))
      ),
      tabItem(tabName = "about",
              h2(" R Shiny application for MGM Resorts International "), 
              h3("About data:"),
              h5("Trip Advisor dataset - http://sifaka.cs.uiuc.edu/~wang296/Data/index.html"),
              h3("Analysis:"),
              h5(tags$li("Sentiment analysis - What exactly a customer likes/dislikes in a specific hotel and suggest ways a hotel needs to improve to increase the bookings.")),
              h5(tags$li("Rating Insights - Compare the performance of different MGM properties.")),
              h5(tags$li("Comments Hub - Important key words as a Word cloud and User reviews.")),
              h5(tags$li("Comments Topic Modeling - Cluster different topics from User comments and visualize the results.")),
              h3("Contact Details:"),
              h5("Arpita Majumder"),
              h5("Email: arpita.majumder@uconn.edu"),
              h5("LinkedIn :https://www.linkedin.com/in/arpitam1")
      )
    )
  )
)










server <- function(input, output) { 
  output$mySite <- renderUI({
    tags$a(href = input$website, input$website)
    })
  
  
  output$head11 = renderText({paste("Yearly review comments by Hotel ")})
  output$yearplotly = renderPlotly({
   
    p12 = plot_ly(yearly_com, x = yearly_com$Year, y = yearly_com$Count, type = 'scatter', mode = 'lines',color = yearly_com$Hotel_Name)
    layout(p12, yaxis = list(title = "Number of Comments in Ten years"),xaxis = list(title = "Years"))
  })
  
  output$rating_category <- renderUI({ selectInput( "category", "Select the Rating category :", choices = list("Overall"= "Overall",
                                                                                                               "Service" = "Service",
                                                                                                               "Cleanliness" = "Cleanliness",
                                                                                                               "Value" = "Value", 
                                                                                                               "Sleep Quality"= "Sleep.Quality",
                                                                                                               "Rooms" = "Rooms" , 
                                                                                                               "Location" = "Location" ))})
  output$head2 = renderText({paste("Results based on User Ratings ")})
  
  output$rating = renderPlotly({
    S2 = Sent_rating[Sent_rating$number == input$selection1 , c(input$category)  ]
    # S2 = data.frame(S2)
    # S2 = S2[!is.na(S2)]
    p2 = ggplot(data = na.omit(S2), aes_string(input$category)) + geom_bar(aes_string(fill = input$category))
    a2 = ggplotly(p2)
    layout(a2, xaxis = list(title = "Number of Ratings"))
  })
  
  output$head1 = renderText({paste("Sentiment analysis based on User comments")})
  
  output$sentiment = renderPlotly({
    S1 = Sent_rating[Sent_rating$number == input$selection11,  ]
    p = ggplot(S1, aes(sentiment_class)) + geom_bar(aes(fill = sentiment_class))
    a =ggplotly(p)
    layout(a, xaxis = list(title = "Reviewer's sentiment"),yaxis = list(title = "Comments Count") )
  })
  
  
  

  output$img1 = renderPlot({
     subset_C = Comment_sample50[Comment_sample50$number == input$selection, ]

     xkcd.corpus <- Corpus(DataframeSource(data.frame(subset_C[, 2])))
     xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
     xkcd.corpus <- tm_map(xkcd.corpus, PlainTextDocument)
     xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
     tdm <- TermDocumentMatrix(xkcd.corpus)
     
     m <- as.matrix(tdm)
     v <- sort(rowSums(m),decreasing=TRUE)
     d <- data.frame(word = names(v),freq=v)
     pal <- brewer.pal(8,"Dark2")
     #png("WWW/wordcloud1.png", width=1500,height=500)
     wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=input$integer, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))


     
     })

    
    output$table = DT::renderDataTable(DT::datatable({
      subset_C = Comment_sample50[Comment_sample50$number == input$selection, -4 ]
      subset_C
    }))
    
  output$map <- renderLeaflet({ 
    map = leaflet(data = lat_long) %>%
      addTiles() %>%  
      addMarkers(~long, ~lat, popup= ~as.character(name))
      
    
    map 
    
    })
  
  }

shinyApp(ui, server)