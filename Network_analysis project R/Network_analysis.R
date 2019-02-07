library(shiny)
install.packages("networkD3")
library(networkD3)
install.packages("lubridate")
library(lubridate)
# map
install.packages("leaflet")
library(leaflet)
library(sp)


# project 1
df_friends <- read.csv("desktop/friends.csv", header=TRUE) 

# project 2
df_friends1 = read.csv("desktop/friends.csv", header=TRUE)

df_user_id = df_friends1[!duplicated(df_friends1[,c('follower_user_id')]),]

df_user_id  = df_user_id[,c('follower_user_id')]

df_address = read.csv('desktop/address.csv',header = TRUE)

### user_id has 116059 , address has 51 rows, 
df_address_all = df_address

for ( i in 1:2276){     
  df_address_all = rbind(df_address_all,df_address)
}

# df_address_all has 116127 rows
df_address_all = df_address_all[1:116059,]

# dealing with follower
df_user_address = cbind(df_user_id, df_address_all)

# rename the column in order to merge the tables
colnames(df_user_address)[1] <- "follower_user_id"

add_state_folower = merge(df_friends1,df_user_address, on= "follower_user_id")

colnames(add_state_folower)[4] <- "follower_State"
colnames(add_state_folower)[5] <- "follower_Latitude"
colnames(add_state_folower)[6] <- "follower_Longitude"
head(add_state_folower)

# dealing with followed
df_user_address1 = cbind(df_user_id, df_address_all)

# rename the column in order to merge the tables
colnames(df_user_address1)[1] <- "followed_user_id"
head(df_user_address1)

add_state_both = merge(add_state_folower,df_user_address1, on= "followed_user_id")

colnames(add_state_both)[7] <- "followed_State"
colnames(add_state_both)[8] <- "followed_Latitude"
colnames(add_state_both)[9] <- "followed_Longitude"
head(add_state_both)

# detecting missing values
sum(is.na(add_state_both))

# treating missing values
add_state_both = add_state_both[complete.cases(add_state_both),]

# shuffling since it is all the same followed_user_id in the first 50 rows
add_state_both = add_state_both[sample(nrow(add_state_both)),]





# Define UI for application that draws a SimpleNetwork
ui <- fluidPage(
  
  # Application title
  titlePanel("Network Analysis of Friendships"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Create Network Analysis with 
               information from 2007 to 2011"),
      
      selectInput("year", 
                  label = "Choose a Year to display the interactive graph for that year",
                  choices = c(2007,2008,2009,2010,2011 
                  ),
                  selected = 2008)
    ),
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Graph", br(),
                           textOutput("selected_year"),
                           tags$head(tags$style("#selected_year{color: #000099;
                                 font-size: 20px;}")),
                           br(),
                           simpleNetworkOutput("Graph"),
                           h3("Objective", style = "color:#0033cc"),
                           p("The objective of this" ,strong("interactive network graph"), "is to provide a visual illustration of ",strong("friendships"),
                             "between the users of the Scratch website.", 
                             "Friendship here refers to", strong("relationship between users and their followers."), 
                             "The information is available to be viewed by", strong("year.")),
                           br(),
                           h3("Methodology", style = "color:#0033cc"),
                           p("The interactive graph is based on",strong("friends table"), "of the Scratch website. The", strong("friends table"), "is 
                             identified by the", strong("unique key - follower_user_id"),"where every user can have one or more followers which is 
                             represented by", strong("followed_user_id."), "Due to huge number of records, I have based the graph on random 
                             top 50 records and the data is broken to be viewed by", strong("year.")),
                           br(),
                           h3("Observation", style = "color:#0033cc"),
                           p("We can see that in",strong("2008, 2009, 2010"), "and",strong("2011,"), " more connections and 
                           friendships were built than in",strong("2007."), "In other words,",strong("in the years after 2007,
                             the user network got wide spread."),
                             "This is also an indication of organic growth of the user base and shows increased activity."),
                           br(),
                           br()),
                  
                  tabPanel("Map", br(),
                           textOutput("selected_year1"), 
                           tags$head(tags$style("#selected_year1{color: #000099;
                                 font-size: 20px;}")),
                           em("The first version of the map", style = "color:#0033cc"),
                           br(),
                           br(),
                           leafletOutput("mymap"),
                           h3("Objective", style = "color:#0033cc"),
                           p("The objective of this" ,strong("interactive network map"), "is to provide a visual illustration of ",strong("friendships"), 
                             "between the users of the Scratch website across the US. Friendship here refers to", strong("relationship between users and their followers."), 
                             "The information is available to be viewed by", strong("year.")),
                           br(),
                           h3("Methodology", style = "color:#0033cc"),
                           p("The interactive map is based on",strong("friends table "), "of the Scratch website. The", strong("friends table"), "is 
                             identified by the", strong("unique key - follower_user_id"),"where every user can have one or more followers which is 
                             represented by", strong("followed_user_id."), "Due to huge number of records, I have based the graph on random 
                             top 50 records and the data is broken to be viewed by", strong("year.")),
                           p("Each", strong("marker represents an user ID"), "and", strong("each line represents the connection between the users."), 
                             "Moreover, each", strong("marker"), "includes the information -", strong("user ID."), "We specifically focus on the
                             friendships were built in the US."),
                           br(),
                           h3("Observation", style = "color:#0033cc"),
                           p("The interactive map shows most of the users are in the",strong( "east coast" ), "each year."),
                           p("Since we assigned the state to each user randomly,", strong("each marker in the map is actually duplicated by multiple users"), 
                             "so that it only shows one of the user IDs."),
                           p("However, the map shows the", strong("brightness of connection."), "If there are",
                             strong("more connections between two states,"), "their line could be", strong("darker"), "which in fact shows how different
                             users are distributed."),
                           br(),
                           br()),
                  
                  
                  tabPanel("Night Map",br(),
                           textOutput("selected_year2"), 
                           tags$head(tags$style("#selected_year2{color: #000099;
                                                font-size: 20px;}")),
                           em("The second version of the map", style = "color:#0033cc"),
                           br(),
                           br(),
                           leafletOutput("mymap1"),
                           br(),
                           h3("Objective", style = "color:#0033cc"),
                           p("The objective of this" ,strong("interactive netwrok night map"), "is to provide a visual illustration of ",strong("friendships"), 
                             "between the users of the Scratch website accross the US. Friendship here refers to", strong("relationship between users and their followers."), 
                             "The information is available to be viewed by", strong("year.")),
                           br(),
                           h3("Methodology", style = "color:#0033cc"),
                           p("The interactive map is based on",strong("friends table"), "of the Scratch website. The", strong("friends table"), "is 
                             identified by the", strong("unique key - follower_user_id"),"where every user can have one or more followers which is 
                             represented by", strong("followed_user_id."), "Due to huge number of records, I have based the graph on random 
                             top 50 records and the data is broken to be viewed by", strong("year.")),
                           p("Each", strong("marker represents an user ID"), "and", strong("each line represents the connection between the users."), 
                             "Moreover, each", strong("marker"), "includes the information -", strong("user ID."), "We specifically focus on the
                             friendships were built in the US."),
                           br(),
                           h3("Observation", style = "color:#0033cc"),
                           p("The interactive night map shows most of the users are in the",strong( "east coast" ), "each year."),
                           p("Since we assigned the state to each user randomly,", strong("each marker in the map is actually duplicated by multiple users"), 
                             "so that it only shows one of the user IDs."),
                           p("However, the map shows the", strong("brightness of connection."), "If there are",
                             strong("more connections between two states,"), "their line could be", strong("darker"), "which in fact shows how different
                             users are distributed. Furthermore, this interactive network night map clearly shows that", strong("most of the users/markers"), 
                             "are", strong("located in the big cities."), "For instance, when it has the yellow brightness in the map, 
                             it means it is the city lights."),
                           br(),
                           br())
        )
      )
    )
  )


# Define server logic required to draw a SimpleNetwork
server <- function(input, output) {
 
  output$selected_year <- renderText({ 
    paste("The network graph shows the new friendships created in",input$year)
  })
  
  output$selected_year1 <- renderText({ 
    paste("The network map shows the new friendships created in",input$year)
  })
  
  output$selected_year2 <- renderText({ 
    paste("The network night map shows the new friendships created in",input$year)
  })
  
  
  output$Graph <- renderSimpleNetwork({
    
    df_friends$years = year(df_friends$date_followed)
    df_tmp = df_friends[df_friends$years==input$year,]
    
    networkData = df_tmp[1:50,c('follower_user_id','followed_user_id')]
    
    simpleNetwork(networkData)
  })
  
  
  output$mymap <- renderLeaflet({
    
    # filter by year
    add_state_both$years = year(add_state_both$date_followed)
    add_state_both_year = add_state_both[add_state_both$years==input$year,]
    
    # top 50 records
    networkData = add_state_both_year[1:50,]
    
    networkData$follower_Longitude = as.numeric(networkData$follower_Longitude)
    networkData$follower_Latitude = as.numeric(networkData$follower_Latitude)
    
    networkData$followed_Longitude = as.numeric(networkData$followed_Longitude)
    networkData$followed_Latitude = as.numeric(networkData$followed_Latitude)
    
    networkData$follower_user_id = as.character(networkData$follower_user_id)
    networkData$followed_user_id = as.character(networkData$followed_user_id)
    
    networkData.sp = SpatialPointsDataFrame(networkData[, c(5,6)],networkData[, -c(5,6)])
    
    m=leaflet() %>% 
      addTiles()%>% 
      addMarkers(data = networkData, lng = ~follower_Longitude, lat= ~follower_Latitude,
                 popup = ~paste("<b>Follower_User ID:</b>", follower_user_id))%>%  
      addMarkers(data = networkData, lng = ~followed_Longitude, lat= ~followed_Latitude, 
                 popup = ~paste("<b>Followed_User ID:</b>", followed_user_id))
    
    for(i in 1:nrow(networkData)){
      m= addPolylines(m, lat = as.numeric(networkData[i, c(5, 8)]), 
                        lng = as.numeric(networkData[i, c(6, 9)]))
    }
    m
  })
  
  output$mymap1 <- renderLeaflet({
    # filter by year
    add_state_both$years = year(add_state_both$date_followed)
    add_state_both_year = add_state_both[add_state_both$years==input$year,]
    
    # top 50 records
    networkData = add_state_both_year[1:50,]
    
    networkData$follower_Longitude = as.numeric(networkData$follower_Longitude)
    networkData$follower_Latitude = as.numeric(networkData$follower_Latitude)
    
    networkData$followed_Longitude = as.numeric(networkData$followed_Longitude)
    networkData$followed_Latitude = as.numeric(networkData$followed_Latitude)
    
    networkData$follower_user_id = as.character(networkData$follower_user_id)
    networkData$followed_user_id = as.character(networkData$followed_user_id)
    
    networkData.sp = SpatialPointsDataFrame(networkData[, c(5,6)],networkData[, -c(5,6)])
    
    m=leaflet() %>% 
      addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>% 
      addMarkers(data = networkData, lng = ~follower_Longitude, lat= ~follower_Latitude, popup = ~paste("<b>Follower_User ID:</b>", follower_user_id))%>%  
      addMarkers(data = networkData, lng = ~followed_Longitude, lat= ~followed_Latitude, popup = ~paste("<b>Followed_User ID:</b>", followed_user_id))
    
    for(i in 1:nrow(networkData)){
      m= addPolylines(m, lat = as.numeric(networkData[i, c(5, 8)]), 
                      lng = as.numeric(networkData[i, c(6, 9)]))
    }
    m
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)







