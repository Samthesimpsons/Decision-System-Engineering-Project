#################################### GETTING STARTED #################################### 
source("setAWSPassword.R")
library(DBI)
library(shiny)
library(shinyjs)
library(jsonlite)
library(sortable)
library(tidyverse)
library(rsconnect)
library(htmlwidgets)
library(shinydashboard)
library(shinydashboardPlus)

#################################### INDIVIDUAL ACCOUNTABILITY #################################### 

# The get functions were created by John who is also in charge of creation of data base and its content.
# Sarah assisted with the collection of data which was intensive, produced the vector graphics images for the game and assisted in the UI layout.
# The remaining functions are done up by Min Shuen and Samuel. Sarah produced the decision tree for the game flow hence coding flow.
  

#################################### ATTRIBUTION  #################################### 

# All graphics are under the Free Media License Agreement of Canva and no attribution required in game.
# Code present has been written by us, no online repository or reference game was used. 


#################################### HELPER FUNCTIONS #################################### 

# I LOVE ESD FUNCTION
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student070",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student070",
    password = getOption("AWSPassword"))
  conn
}

# Get products for chosen skintype (SARAH, JOHN)
getProductList <- function(categoryname){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT * FROM ProductTable WHERE categoryname=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=categoryname)
  products <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  products
}

# Get description for selected product (SARAH, JOHN)
getProductDescription <- function(productname){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT productdescription FROM ProductTable WHERE productname=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=productname)
  products <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  products
}

# Get skintype for selected product (SARAH, JOHN)
getProductSkintype <- function(productname){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT skinname FROM ProductTable WHERE productname=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=productname)
  products <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  products
}

# Get description for skin type (SARAH, JOHN)
getSkinDescription <- function(skintype){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT skindescription FROM SkinTable WHERE skinname=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=skintype)
  skin <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  skin
}

# Get description for ordering tips (SARAH, JOHN)
getOrderTipsDescription <- function(categoryname,randomnumber){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT OrderTipsDesc FROM OrderTipsTable WHERE categoryname=?id1 AND RandomNumber=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=categoryname,id2=randomnumber)
  tips <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  tips
}


# Get category description for chosen category (SARAH, JOHN)
getCategoryDetails <- function(category){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT * FROM CategoryTable WHERE categoryname=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=category)
  categoryinfo <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  desc  <- categoryinfo$categorydescription
  desc
}

# Hit Budget or already purchased item Modal (SAMUEL)
HitbudgetModal <- function(){
  modalDialog(
    title = NULL,
    h3(strong("Oops"),align = "center"),
    h3("You have already selected an item from the same category"),
    h3("Please choose another category or un-select your current selection."),
    easyClose = TRUE,
    footer = NULL
  )
}

# Category in the cart is empty Modal (SAMUEL)
emptyModal <- function(){
  modalDialog(
    title = NULL,
    h3("No product of the category has been purchased yet."),
    easyClose = TRUE,
    footer = NULL
  )
}

# Get score for selected product (SARAH, JOHN)
getProductScore <- function(productname){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT facescore FROM ProductTable WHERE productname=?id1;"
  query<- sqlInterpolate(conn, querytemplate,id1=productname)
  score <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  score
}

# Get order score for applied category (SARAH, JOHN)
getOrderScore <- function(category, categoryorder){
  #open the connection
  conn <- getAWSConnection()
  querytemplate <- "SELECT orderscore FROM CategoryOrderScoreTable WHERE categoryname=?id1 AND categoryorder=?id2;"
  query<- sqlInterpolate(conn, querytemplate, id1=category, id2=categoryorder)
  #print(query)
  orderscore <- dbGetQuery(conn, query)
  
  #Close the connection
  dbDisconnect(conn)
  orderscore
}

# Function multiplier for bonus scoring (SAMUEL)
multiplier <- function(mylist,actuallist){
  samelist <- NULL
  results <- rbind(actuallist %in% mylist,actuallist)
  for(i in 1:6){
    if (results[1,i]=="TRUE"){
      samelist <- c(samelist,results[2,i])
    }
  }
  names(samelist) <- NULL
  samelist
  pattern <- str_detect(samelist,mylist)
  count = 0
  for (i in 1:length(pattern)){
    if (pattern[i] == "TRUE"){
      count = count + 1
    }
    scoring <- count/length(samelist)
  }
  scoring
}

# Create history dataframe (MINSHUEN)
prephistory <- function(){
  historydf <- data.frame(matrix(ncol = 9, nrow = 0))
  x <- c("moisturizer_order", "cleanser_order", "exfoliator_order", "serum_order", "toner_order", "sunscreen_order", 
         "score", "actioncount", "lastclicked")
  colnames(historydf) <- x
  historydf
}

# Finish game Modal (TO-DO: THRESHOLD VALUE FOR COMPLEMENT) (MINSHUEN)
finishModal <- function(listoforders, finalscore, historydf){
  ##print(listoforders)
  df <- historydf[nrow(historydf),] %>% select(-c(score, actioncount, lastclicked))
  categorynames <- colnames(df)
  df <- df %>% 
    pivot_longer(categorynames, "category", values_to="categoryorder")  %>% 
    filter(categoryorder != 0) %>% 
    mutate(product=listoforders, category=gsub('.{6}$', '', category)) %>%
    arrange(desc(-categoryorder))
  #print(df)
  
  modalDialog(
    title = NULL,
    h2(strong("Your Final Score and List!")),
    h3(strong("In order of usage, your products chosen are: ")),
    lapply(1:nrow(df), function(i) h4(paste0(i, ". ", str_to_title(df$category[[i]]), ": ", df$product[[i]]))),
    h3(strong(paste0("Final score: ", round(finalscore,0)," / 200"))),
    h3(" "),
    actionButton("answerbtn", h5(strong("Show correct order")), style="color: #fff; background-color: #AE0404; border-color: #AE0404"),
    actionButton("notdonebtn",h5(strong("No I'm not done yet"))),
    footer = actionButton("replaybtn", h5(strong("Play Again")), style="color: #fff; background-color: #FF6347; border-color: #FF6347")
  )
}

# Final answer of ordering Modal (MINSHUEN)
answerModal <- function(){
  modalDialog(
    h2(strong("The correct ordering of products are:")),
    h3("1. Cleanser"),
    h3("2. Exfoliator"),
    h3("3. Toner"),
    h3("4. Serum"),
    h3("5. Moisturizer"),
    h3("6. Sunscreen"),
    footer = tagList(modalButton(h5(strong("Return to game"))),
                           actionButton("replaybtn", h5(strong("Play Again")), style="color: #fff; background-color: #FF6347; border-color: #FF6347"))

  )
}

#################################### UI #################################### 

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = strong("Glow Up!")),
  dashboardSidebar(
    width = 0, collapsed = FALSE, sidebarMenu(id = "tabs",
    menuItem("Choose Your Avatar",tabName ="start"),
    menuItem("Shop", tabName = "shop"),
    menuItem("Apply Products",tabName ="interact")),
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"))
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "start",
              tags$img(src = "logo2.png",height = "75%",style = 'position: absolute;opacity: 0.5'),
              fluidRow(class="center",
                       column(width = 12,
                              h2(strong("Welcome! First, Customise Your Avatar.")),
                              h3(strong("1) Set Your Gender:")),
                              selectInput("selectGender", "", c("Male","Female")),
                              h3(strong("2) Set Your Skin Type:")),
                              strong(htmlOutput("skindesc")),
                              selectInput("selectSkin", "", c("Oily", "Dry", "Sensitive", "Normal","Combination")),
                              actionButton("createConfirm", h4(strong("Let's get started!")), style="color: #fff; background-color: #AE0404; border-color: #AE0404")))),
      tabItem(tabName = "shop",
              tags$img(src = "logo2.png",height = "75%",style = 'position: absolute;top: 50%;left: 50%;transform: translate(-50%, -50%);opacity: 0.5'),
              fluidRow(width =12,
                       align="center",
                       h2(strong("Secondly, click on each Category to know more and to shop.")),
                       br()),
              fluidRow(
                column(width=5, id="test",
                       h3(strong("Product Categories"), align = "center"),
                       actionButton(inputId="moisturizer",label = NULL,style = "width: 187.5px; height: 300px; background: url('moisturiser.svg');  background-size: cover; background-position: center;"),
                       actionButton(inputId="cleanser",label = NULL,style = "width: 187.5px; height: 300px; background: url('cleanser.svg');  background-size: cover; background-position: center;"),
                       actionButton(inputId="exfoliator",label = NULL,style = "width: 187.5px; height: 300px; background: url('exfoliator.svg');  background-size: cover; background-position: center;")
                       ),
                ),
              fluidRow(
                column(width=5,
                       actionButton(inputId="serum",label = NULL,style = "width: 187.5px; height: 300px; background: url('serum.svg');  background-size: cover; background-position: center;"),
                       actionButton(inputId="toner",label = NULL,style = "width: 187.5px; height: 300px; background: url('toner.svg');  background-size: cover; background-position: center;"),
                       actionButton(inputId="sunscreen",label = NULL,style = "width: 187.5px; height: 300px; background: url('sunscreen.svg');  background-size: cover; background-position: center;")
                       ),
                column(width = 7,
                       box(title = strong("Cart: Click to see each chosen product in your cart"),
                           width = 12,
                           uiOutput("cartbuttons"),
                           br(),
                           actionButton("editcart", h5(strong("Edit Cart")), style="color: #fff; background-color: #AE0404; border-color: #AE0404"))
                       )
                ),
              fluidRow(
                column(width = 12, 
                       h3(strong("Once done, let's go and apply the products on your avatar! Or you can go back to change your avatar.")),
                       actionButton("changeAvatar", h4(strong("Change my avatar")), style="color: #fff; background-color: #AE0404; border-color: #AE0404"),
                       actionButton("confirmproducts", h4(strong("Apply on Avatar")), style="color: #fff; background-color: #AE0404; border-color: #AE0404"),
                       h5(em("Note: Changing your avatar will reset the game!"))                       )
              ),
              fluidRow(
                column(width = 12,
                       align = "right",
                       h5(em("List of sources: https://docs.google.com/spreadsheets/d/1aZhuZNXFon_khsykVDj0UQ9cZV8TEStS1u_evASkq0o/edit?usp=sharing"))
                )
              )
              ),
      tabItem(tabName = "interact",
              tags$img(src = "logo2.png",height = "75%",style = 'position: absolute;top: 50%;left: 50%;transform: translate(-50%, -50%);opacity: 0.5'
              ),
              fluidRow(align="center",
                       h2(strong("Lastly, let us play with the order of applying your products!"))),
              fluidRow(column(width=12,
                              align="center",
                              h3(strong("Avatar")),
                              uiOutput("avatar"),
                              htmlOutput("facescore"))
                       ),
              fluidRow(useShinyjs(),
                       class="center",
                       column(width=6,
                              align="center",
                              selectInput("cat",h3(strong("Select your category choice for tips:")),
                                          choices = c("Moisturizer","Cleanser","Exfoliator","Serum","Toner","Sunscreen"),
                                          width = "75%"
                                          ),
                              uiOutput("active_side_1"),
                              flipBox(id = "flipbox1",
                                      width = 12,
                                      trigger = "hover",
                                      front = div(class = "text-center",h3(strong("Click for a random tip!")),
                                                  img(src = "tips.svg",height = "300px",width = "100%"),
                                                  tags$br()),
                                      back = div(class = "text-center",height = "300px",width = "100%",
                                        h3(" "),
                                        h3(strong("Here is a random tip on ordering!")),
                                        h3(" "),
                                        uiOutput("tips")))
                            ),
                       column(width=6,
                              align="center",
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              uiOutput("ordering"))),
              fluidRow(
                br(),
                column(width = 12, 
                       actionButton("changeShoppingList",h4(strong("Go Back to Shopping Page")),style="color: #fff; background-color: #AE0404; border-color: #AE0404"),
                       uiOutput("finish"))),
              fluidRow(
                column(width = 12,
                       align = "right",
                       h5(em("Sources: https://www.dermstore.com/blog/in-what-order-do-i-apply-my-skin-care-products-infographic;
                                    https://www.adorebeauty.com.au/skin-care/cleansers/guide/how-cleansers-work;
                                    https://theskincareedit.com/order-of-skincare-products")))
              )
              )
      )
    )
  )
    

#################################### SERVER #################################### 

server <- function(input, output, session) {
  vals <- reactiveValues(state_moisturizer=0,state_cleanser=0,state_exfoliator=0,state_serum=0,state_toner=0,state_sunscreen=0,
                         moisturizer="",cleanser="",exfoliator="",serum="",toner="",sunscreen="",
                         gender=NULL, skintype=NULL, historydf=prephistory(), listoforders=NULL,
                         occupied=0, score=0, lastclicked=NULL, actioncount=0, selectedcount=6,
                         moisturizer_order=0, cleanser_order=0,exfoliator_order=0,serum_order=0,toner_order=0,sunscreen_order=0,
                         randomnumber=0, bonusscore=80,previousbonusscore=0,correctlist=c("cleanser","exfoliator","toner","serum","moisturizer","sunscreen"),
                         playersorders=numeric(0))
  
  # Observe a change in gender (SAMUEL)
  observeEvent(input$selectGender,{
    vals$gender <- input$selectGender
    #print(vals$gender)
    
    output$avatar <- renderUI({
      if(vals$gender == "Male"){            
        tags$img(src = "male.svg",width= 160 , height = 200)
      }                                        
      else if(vals$gender == "Female"){
        tags$img(src = "female.svg",width= 160 , height = 200)
      }
    })
  })
  
  # Observe a change in skintype (SAMUEL)
  observeEvent(input$selectSkin,{
    vals$skintype <- input$selectSkin
  #print(vals$skintype)
  })

  
  # Observe confirm create avatar (SAMUEL)
  observeEvent(input$createConfirm, {
    #print("Avatar creation confirmed")
    
    vals$gender <- input$selectGender
    vals$skintype <- input$selectSkin
    
    #print(paste0("Attributes: ", vals$skintype))
    updateTabsetPanel(session, inputId="tabs", selected="shop")
  })
  
  # Observe button click to change to Avatar page (SAMUEL)
  observeEvent(input$changeAvatar, {
    #print("Returning to avatar creation")
    updateTabsetPanel(session, inputId="tabs", selected="start")
    
    vals$state_moisturizer<-0
    vals$state_cleanser<-0
    vals$state_exfoliator<-0
    vals$state_serum<-0
    vals$state_toner<-0
    vals$state_sunscreen<-0
    vals$moisturizer<-""
    vals$cleanser<-""
    vals$exfoliator<-""
    vals$serum<-""
    vals$toner<-""
    vals$sunscreen<-""
    vals$score<-0
    vals$selectedcount<-6
    
  })
  
  # Observe button click to change to Shopping page (SAMUEL)
  observeEvent(input$changeShoppingList, {
    #print("Returning to shopping page")
    updateTabsetPanel(session, inputId="tabs", selected="shop")
    vals$moisturizer_order=0
    vals$cleanser_order=0
    vals$exfoliator_order=0
    vals$serum_order=0
    vals$toner_order=0
    vals$sunscreen_order=0
    vals$score=0
    vals$actioncount <- 0
    vals$playersorders <- numeric(0)
    vals$previousbonusscore <- 0 
    
  })
  
  # Observe button click to change to facescore page (MINSHUEN)
  observeEvent(input$confirmproducts, {
    #print("Proceding to interact")
    updateTabsetPanel(session, inputId="tabs", selected="interact")
    
    listoforders <- c(vals$moisturizer, vals$cleanser, vals$exfoliator, vals$serum, vals$toner, vals$sunscreen)
    listoforders <- listoforders[listoforders != ""]
    vals$listoforders <- listoforders
    buttonlist <- c("moisturizerbtn", "cleanserbtn", "exfoliatorbtn", "serumbtn", "tonerbtn", "sunscreenbtn")
    vals$selectedcount <- length(listoforders)
    #print(paste0(vals$selectedcount, " product categories selected"))

    
    if (length(listoforders) == 0 ){
      output$ordering <- renderUI({
        box(width = 12,
            title = h3(strong("Click the products in the sequence of how you would apply them (Ordering gives you bonus scores!):")),
            "There is nothing inside your cart")
      })
    }else{
      listoforders2 <- c(vals$moisturizer, vals$cleanser, vals$exfoliator, vals$serum, vals$toner, vals$sunscreen)
      catnames <- c("Moisturizer", "Cleanser", "Exfoliator", "Serum", "Toner", "Sunscreen")
      output$ordering <- renderUI({
        box(width = 12,
            title = h3(strong("Apply the products (You get bonus points for correct ordering!)")),
            lapply(which(!(listoforders2 %in% c(""))), function(i) {
              actionButton(buttonlist[[i]], paste0(catnames[[i]], ": ", listoforders2[[i]]), width = "100%")
              }),
            h3(" "),
            actionButton("undobtn", h5(strong("Undo previous action")), style="color: #fff; background-color: #AE0404; border-color: #AE0404")
            
            )
      })
      }
    })
  
  # Dynamically update shopping cart (MINSHUEN)
  output$cartbuttons <- renderUI({
    listoforders2 <- c(vals$moisturizer, vals$cleanser, vals$exfoliator, vals$serum, vals$toner, vals$sunscreen)
    catnames <- c("Moisturizer", "Cleanser", "Exfoliator", "Serum", "Toner", "Sunscreen")
    
    lapply(which(!(listoforders2 %in% c(""))), function(i) {
      actionButton(paste0("product",i), paste0(catnames[[i]], ": ", listoforders2[[i]]), width = "100%")
      
      })
  })
  
  # Observe undo apply btn (MINSHUEN)
  observeEvent(input$undobtn, {
    if (vals$actioncount > 1) {
      #print("Undoing last action!")
      
      vals$playersorders <- vals$playersorders[-length(vals$playersorders)]
      vals$previousbonusscore <- vals$bonusscore*multiplier(vals$playersorders ,vals$correctlist)
      
      vals$lastclicked <- vals$historydf$lastclicked[nrow(vals$historydf)]
      
      vals$historydf <- vals$historydf[-nrow(vals$historydf),]
      
      vals$moisturizer_order <- vals$historydf$moisturizer_order[nrow(vals$historydf)]
      vals$cleanser_order <- vals$historydf$cleanser_order[nrow(vals$historydf)]
      vals$exfoliator_order <- vals$historydf$exfoliator_order[nrow(vals$historydf)]
      vals$serum_order <- vals$historydf$serum_order[nrow(vals$historydf)]
      vals$toner_order <- vals$historydf$toner_order[nrow(vals$historydf)]
      vals$sunscreen_order <- vals$historydf$sunscreen_order[nrow(vals$historydf)]
      
      vals$score <- vals$historydf$score[nrow(vals$historydf)]
      #print(vals$historydf)
      
      shinyjs::enable(vals$lastclicked)
      vals$actioncount = vals$actioncount - 1
      #print(paste0("actioncount: ", vals$actioncount))
    } else {
      vals$lastclicked <- vals$historydf$lastclicked[nrow(vals$historydf)]
      vals$historydf <- prephistory()
      vals$score <- 0
      vals$actioncount <- 0
      vals$moisturizer_order=0
      vals$cleanser_order=0
      vals$exfoliator_order=0
      vals$serum_order=0
      vals$toner_order=0
      vals$sunscreen_order=0
      
      vals$playersorders <- vals$playersorders[-length(vals$playersorders)]
      vals$previousbonusscore <- 0
      
      #print(vals$historydf)
      #print(paste0("actioncount: ", vals$actioncount))
      
      shinyjs::enable(vals$lastclicked)
      
      }
  })
  
  # Observe apply products (MINSHUEN)
  observeEvent(input$moisturizerbtn,{
    vals$moisturizer_order <- 1
    vals$moisturizer_order <- sum(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                                  vals$serum_order, vals$toner_order, vals$sunscreen_order)!=0)
    #print(paste0("moisturizer order: ", vals$moisturizer_order))
    productskintype <- getProductSkintype(vals$moisturizer)
    if (vals$skintype == productskintype) {
      productscore <- getProductScore(vals$moisturizer)
    } else {
      productscore <- 0.5*getProductScore(vals$moisturizer)
    }
    
    category <- "moisturizer"
    
    vals$playersorders <- c(vals$playersorders,category)
    #print(vals$playersorders)
    
    orderscore <- vals$bonusscore * multiplier(vals$playersorders ,vals$correctlist) 
    #print(paste0(category, " order score: ", orderscore))
    #print(paste0("moisturizer score: ", productscore))
    vals$score <- vals$score + productscore + orderscore - vals$previousbonusscore
    vals$previousbonusscore <- orderscore
    
    vals$lastclicked <- "moisturizerbtn"
    shinyjs::disable("moisturizerbtn")
    
    #Storing in history (MISHUEN)
    actioncount = max(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order))
    
    add_to_history <- c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order, 
                        vals$score, actioncount, vals$lastclicked)
    vals$historydf[actioncount,] <-  add_to_history
    vals$actioncount <- actioncount
    
    #print(vals$historydf)
    
 })
  
  observeEvent(input$cleanserbtn,{
    vals$cleanser_order <- 1
    vals$cleanser_order <- sum(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                                 vals$serum_order, vals$toner_order, vals$sunscreen_order)!=0)
    #print(paste0("cleanser order: ", vals$cleanser_order))
    
    category <- "cleanser"
    
    vals$playersorders <- c(vals$playersorders,category)
    orderscore <- vals$bonusscore*multiplier(vals$playersorders ,vals$correctlist)
    #print(paste0(category, " order score: ", orderscore))
    
    productskintype <- getProductSkintype(vals$cleanser)
    if (vals$skintype == productskintype) {
      productscore <- getProductScore(vals$cleanser)
    } else {
      productscore <- 0.5*getProductScore(vals$cleanser)
    }    
    
    #print(paste0("cleanser score: ", productscore))
    vals$score <- vals$score + productscore + orderscore - vals$previousbonusscore
    vals$previousbonusscore <- orderscore
    
    vals$lastclicked <- "cleanserbtn"
    shinyjs::disable("cleanserbtn")
    
    #storing in history
    actioncount = max(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order))
    add_to_history <- c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order, 
                        vals$score, actioncount, vals$lastclicked)
    vals$historydf[actioncount,] <-  add_to_history
    vals$actioncount <- actioncount
    
    #print(vals$historydf)
  })
  
  observeEvent(input$exfoliatorbtn,{
    vals$exfoliator_order <- 1
    vals$exfoliator_order <- sum(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                                   vals$serum_order, vals$toner_order, vals$sunscreen_order)!=0)
    #print(paste0("exfoliator order: ", vals$exfoliator_order))
    category <- "exfoliator"
    
    vals$playersorders <- c(vals$playersorders,category)
    orderscore <- vals$bonusscore*multiplier(vals$playersorders ,vals$correctlist)
    #print(paste0(category, " order score: ", orderscore))
    
    productskintype <- getProductSkintype(vals$exfoliator)
    if (vals$skintype == productskintype) {
      productscore <- getProductScore(vals$exfoliator)
    } else {
      productscore <- 0.5*getProductScore(vals$exfoliator)
    }    
    
    
    #print(paste0("exfoliator score: ", productscore))
    vals$score <- vals$score + productscore + orderscore - vals$previousbonusscore
    vals$previousbonusscore <- orderscore    
    
    vals$lastclicked <- "exfoliatorbtn"
    shinyjs::disable("exfoliatorbtn")
    
    #storing in history
    actioncount = max(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order))
    add_to_history <- c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order, 
                        vals$score, actioncount, vals$lastclicked)
    vals$historydf[actioncount,] <-  add_to_history
    vals$actioncount <- actioncount
    
    #print(vals$historydf)
  })
  
  observeEvent(input$serumbtn,{
    vals$serum_order <- 1
    vals$serum_order <- sum(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                              vals$serum_order, vals$toner_order, vals$sunscreen_order)!=0)
    #print(paste0("serum order: ", vals$serum_order))
    category <- "serum"
    
    vals$playersorders <- c(vals$playersorders,category)
    orderscore <- vals$bonusscore*multiplier(vals$playersorders ,vals$correctlist)
    #print(paste0(category, " order score: ", orderscore))
    
    productskintype <- getProductSkintype(vals$serum)
    if (vals$skintype == productskintype) {
      productscore <- getProductScore(vals$serum)
    } else {
      productscore <- 0.5*getProductScore(vals$serum)
    }    
    
    #print(paste0("serum score: ", productscore))
    vals$score <- vals$score + productscore + orderscore - vals$previousbonusscore
    vals$previousbonusscore <- orderscore    
    
    vals$lastclicked <- "serumbtn"
    shinyjs::disable("serumbtn")
    
    #storing in history
    actioncount = max(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order))
    add_to_history <- c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order, 
                        vals$score, actioncount, vals$lastclicked)
    vals$historydf[actioncount,] <-  add_to_history
    vals$actioncount <- actioncount
    
    #print(vals$historydf)
  })
  
  observeEvent(input$tonerbtn,{
    vals$toner_order <- 1
    vals$toner_order <- sum(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                              vals$serum_order, vals$toner_order, vals$sunscreen_order)!=0)
    #print(paste0("toner order: ", vals$toner_order))
    category <- "toner"
    
    vals$playersorders <- c(vals$playersorders,category)
    orderscore <- vals$bonusscore*multiplier(vals$playersorders ,vals$correctlist)
    #print(paste0(category, " order score: ", orderscore))
    
    productskintype <- getProductSkintype(vals$toner)
    if (vals$skintype == productskintype) {
      productscore <- getProductScore(vals$toner)
    } else {
      productscore <- 0.5*getProductScore(vals$toner)
    }    
    
    #print(paste0("toner score: ", productscore))
    vals$score <- vals$score + productscore + orderscore - vals$previousbonusscore
    vals$previousbonusscore <- orderscore    
    
    vals$lastclicked <- "tonerbtn"
    shinyjs::disable("tonerbtn")
    
    #storing in history
    actioncount = max(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order))
    add_to_history <- c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order, 
                        vals$score, actioncount, vals$lastclicked)
    vals$historydf[actioncount,] <-  add_to_history
    vals$actioncount <- actioncount
    
    #print(vals$historydf)
  })
  
  observeEvent(input$sunscreenbtn,{
    vals$sunscreen_order <- 1
    vals$sunscreen_order <- sum(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                                  vals$serum_order, vals$toner_order, vals$sunscreen_order)!=0)
    #print(paste0("sunscreen order: ", vals$sunscreen_order))
    
    category <- "sunscreen"
    vals$playersorders <- c(vals$playersorders,category)
    orderscore <- vals$bonusscore*multiplier(vals$playersorders ,vals$correctlist)
    #print(paste0(category, " order score: ", orderscore))

    productskintype <- getProductSkintype(vals$sunscreen)
    if (vals$skintype == productskintype) {
      productscore <- getProductScore(vals$sunscreen)
    } else {
      productscore <- 0.5*getProductScore(vals$sunscreen)
    }
    
    #print(paste0("sunscreen score: ", productscore))
    vals$score <- vals$score + productscore + orderscore - vals$previousbonusscore
    vals$previousbonusscore <- orderscore    
    
    vals$lastclicked <- "sunscreenbtn"
    shinyjs::disable("sunscreenbtn")
    
    #storing in history
    actioncount = max(c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order))
    add_to_history <- c(vals$moisturizer_order, vals$cleanser_order, vals$exfoliator_order,
                        vals$serum_order, vals$toner_order, vals$sunscreen_order, 
                        vals$score, actioncount, vals$lastclicked)
    vals$historydf[actioncount,] <-  add_to_history
    vals$actioncount <- actioncount
    
    #print(vals$historydf)
  })
  
  # Modal to edit cart (SAMUEL)
  editcartModal <- function(){
    itemlist <- c(vals$moisturizer, vals$cleanser, vals$exfoliator, vals$serum, vals$toner, vals$sunscreen)
    modalDialog(
      selectInput("cartlist", "Your current cart:", itemlist),
      footer = tagList(
        actionButton("refunditem",h5(strong("Remove from cart")), style="color: #fff; background-color: #AE0404; border-color: #AE0404"),
        modalButton(h5(strong("Cancel"))))
    )
  }
  
  # Observes when clicking on refund button (SAMUEL)
  observeEvent(input$refunditem,{
    itemlist <- c(vals$moisturizer, vals$cleanser, vals$exfoliator, vals$serum, vals$toner, vals$sunscreen)
      #print(itemlist)
      for (i in 1:length(itemlist)){
        if (input$cartlist == itemlist[i] && input$cartlist != ""){
          if (i == 1){
            vals$moisturizer <- ""
            vals$state_moisturizer <- 0
            
          }else if (i == 2){
            vals$cleanser <- ""
            vals$state_cleanser <- 0
            
          }else if (i == 3){
            vals$exfoliator <- ""
            vals$state_exfoliator <- 0
            
          }else if (i == 4){
            vals$serum <- ""
            vals$state_serum <- 0
            
          }else if (i == 5){
            vals$toner <- ""
            vals$state_toner <- 0
            
          }else{
            vals$sunscreen <- ""
            vals$state_sunscreen <- 0
          }
        }
    }
    removeModal()
  })
  
  # Modal to show description, ingredients of item in selectinput (SAMUEL)
  observeEvent(input$moreinfo, {
    moreinfoModal <- function(){
      modalDialog(
        h3(strong((input$productselection))),
        h4(getProductDescription(input$productselection)),
        easyClose = TRUE,
        footer = NULL
      )
    }
    showModal(moreinfoModal())
  })
  
  # Modal to show description, ingredients of chosen item in cart (SAMUEL)
  observeEvent(input$product1,{
    if (vals$moisturizer != ""){
      moreinfoModal2 <- function(){
        modalDialog(
          h2(vals$moisturizer),
          h3(getProductDescription(vals$moisturizer)),
          easyClose = TRUE,
          footer = NULL
        )}
        showModal(moreinfoModal2())
    }else {
      showModal(emptyModal())
    }
    })
  
  observeEvent(input$product2,{
    if (vals$cleanser != ""){
      moreinfoModal <- function(){
        modalDialog(
          h2(vals$cleanser),
          h3(getProductDescription(vals$cleanser)),
          easyClose = TRUE,
          footer = NULL
        )}
        showModal(moreinfoModal())
    }else {
      showModal(emptyModal())
    }
  })  
  
  observeEvent(input$product3,{
    if (vals$exfoliator != ""){
      moreinfoModal <- function(){
        modalDialog(
          h2(vals$exfoliator),
          h3(getProductDescription(vals$exfoliator)),
          easyClose = TRUE,
          footer = NULL
        )}
        showModal(moreinfoModal())
    }else {
      showModal(emptyModal())
    }
  })  
  
  observeEvent(input$product4,{
    if (vals$serum != ""){
      moreinfoModal <- function(){
        modalDialog(
          h2(vals$serum),
          h3(getProductDescription(vals$serum)),
          easyClose = TRUE,
          footer = NULL
        )}
        showModal(moreinfoModal())
    }else {
      showModal(emptyModal())
    }
  })  
  
  observeEvent(input$product5,{
    if (vals$toner != ""){
      moreinfoModal <- function(){
        modalDialog(
          h2(vals$toner),
          h3(getProductDescription(vals$toner)),
          easyClose = TRUE,
          footer = NULL
        )}
        showModal(moreinfoModal())
    }else {
      showModal(emptyModal())
    }
  })  
  
  observeEvent(input$product6,{
    if (vals$sunscreen != ""){
      moreinfoModal <- function(){
        modalDialog(
          h2(vals$sunscreen),
          h3(getProductDescription(vals$sunscreen)),
          easyClose = TRUE,
          footer = NULL
        )}
        showModal(moreinfoModal())
    }else {
      showModal(emptyModal())
    }
  }) 
  
  # Insert helper function for insertUI() (SAMUEL)
  insert <- function(category,skintype){
    insertUI(
      selector = "#test",
      where = "afterEnd",
      ui = column(width=7, id="toremove",
                  h3(strong("Description"),align = "center"),
                  h3(paste0("What is a ", category, "?")),
                  h4(getCategoryDetails(category)),
                  h4("Here is a list of products to choose from:"),
                  br(),
                  box(title = strong("Shopping: Select a product"),
                    width = 12,
                    selectInput("productselection", NULL , choices = getProductList(category)[,4]),
                    actionButton("moreinfo", h5(strong("View Product Description")), style="color: #fff; background-color: #AE0404; border-color: #AE0404"),
                    actionButton(paste0("purchase", category) ,h5(strong("Add to cart")), style="color: #fff; background-color: #AE0404; border-color: #AE0404")
                    #h5(em("Source:https://www.sephora.sg/categories/skincare"))
                  )
      )
    )
  }

  # Observes clicking on category image buttons (SAMUEL)
  observeEvent(input$moisturizer, {
    #print("Moisturizer selected")
    if (vals$state_moisturizer==0) {
      category <- "moisturizer"
      if (vals$occupied==0) {
        #print("inserting UI")
        insert(category,vals$skintype)
        vals$occupied <- 1
      } else if (vals$occupied==1) {
        #print("removing UI")
        removeUI(selector = "#toremove")          
        #print("inserting UI")
        insert(category,vals$skintype)
      } 
    } else {
      showModal(HitbudgetModal())
    }
  })  
  
  observeEvent(input$cleanser, {
    #print("Cleanser selected")
    if (vals$state_cleanser==0) {
      category <- "cleanser"
      if (vals$occupied==0) {
        #print("inserting UI")
        insert(category,vals$skintype)
        vals$occupied <- 1
      } else if (vals$occupied==1) {
        #print("removing UI")
        removeUI(selector = "#toremove")          
        #print("inserting UI")
        insert(category,vals$skintype)
      } 
    } else {
      showModal(HitbudgetModal())
    }
  })  
  
  observeEvent(input$exfoliator, {
    #print("Exfoliator selected")
    if (vals$state_exfoliator==0) {
      category <- "exfoliator"
      if (vals$occupied==0) {
        #print("inserting UI")
        insert(category,vals$skintype)
        vals$occupied <- 1
        
      } else if (vals$occupied==1) {
        #print("removing UI")
        removeUI(selector = "#toremove")          
        #print("inserting UI")
        insert(category,vals$skintype)
      } 
    } else {
      showModal(HitbudgetModal())
    }
  })
  
  observeEvent(input$serum, {
    #print("Serum selected")
    if (vals$state_serum==0) {
      category <- "serum"
      if (vals$occupied==0) {
        #print("inserting UI")
        insert(category,vals$skintype)
        vals$occupied <- 1
        
      } else if (vals$occupied==1) {
        #print("removing UI")
        removeUI(selector = "#toremove")          
        #print("inserting UI")
        insert(category,vals$skintype)
      } 
    } else {
      showModal(HitbudgetModal())
    }
  })
  
  observeEvent(input$toner, {
    #print("Toner selected")
    if (vals$state_toner==0) {
      category <- "toner"
      if (vals$occupied==0) {
        #print("inserting UI")
        insert(category,vals$skintype)
        vals$occupied <- 1
        
      } else if (vals$occupied==1) {
        #print("removing UI")
        removeUI(selector = "#toremove")          
        #print("inserting UI")
        insert(category,vals$skintype)
      } 
    } else {
      showModal(HitbudgetModal())
    }
  })
  
  observeEvent(input$sunscreen, {
    #print("Mask selected")
    if (vals$state_sunscreen==0) {
      category <- "sunscreen"
      if (vals$occupied==0) {
        #print("inserting UI")
        insert(category,vals$skintype)
        vals$occupied <- 1
        
      } else if (vals$occupied==1) {
        #print("removing UI")
        removeUI(selector = "#toremove")          
        #print("inserting UI")
        insert(category,vals$skintype)
      } 
    } else {
      showModal(HitbudgetModal())
    }
  })
  
  # Reacts to clicking Purchase  (SAMUEL)
  observeEvent(input$purchasemoisturizer,{
    if (vals$state_moisturizer == 0) {
      #print("purchased")
      vals$state_moisturizer <- 1
      vals$moisturizer <- input$productselection
    } else {
      showModal(HitbudgetModal())
    }

    #print(paste0(vals$moisturizer, " purchased for moisturizer category"))
    
  })
   
  observeEvent(input$purchasecleanser,{
    if (vals$state_cleanser == 0) {
      #print("purchased")
      vals$state_cleanser <- 1
      vals$cleanser <- input$productselection
    } else {
      showModal(HitbudgetModal())
    }

    #print(paste0(vals$cleanser, " purchased for cleanser category"))
  
  })
  
  observeEvent(input$purchaseexfoliator,{
    if (vals$state_exfoliator == 0) {
      #print("purchased")
      vals$state_exfoliator <- 1
      vals$exfoliator <- input$productselection
    } else {
      showModal(HitbudgetModal())
    }

    #print(paste0(vals$exfoliator, " purchased for exfoliator category"))
    
  })
  
  observeEvent(input$purchaseserum,{
    if (vals$state_serum == 0) {
      #print("purchased")
      vals$state_serum <- 1
      vals$serum <- input$productselection
    } else {
      showModal(HitbudgetModal())
    }    

    #print(paste0(vals$serum, " purchased for serum category"))
    
  })
  
  observeEvent(input$purchasetoner,{
    if (vals$state_toner == 0) {
      #print("purchased")
      vals$state_toner <- 1
      vals$toner <- input$productselection
    } else {
      showModal(HitbudgetModal())
    }

    #print(paste0(vals$toner, " purchased for toner category"))
    
  })
  
  observeEvent(input$purchasesunscreen,{
    if (vals$state_sunscreen == 0) {
      #print("purchased")
      vals$state_sunscreen <- 1
      vals$sunscreen <- input$productselection
    } else {
      showModal(HitbudgetModal())
    }

    #print(paste0(vals$sunscreen, " purchased for sunscreen category"))
    
  })
  
  # Edit cart (SAMUEL)
  observeEvent(input$editcart, {
    showModal(editcartModal())
  })
  
  # Renders out the reactive value of the facescore (SAMUEL)
  output$facescore <- renderUI({
    column(width = 4, offset=4,
    h3(strong(paste0("Product suitability score: ", round(vals$score - vals$previousbonusscore,0), "/120"))),
    h3(strong(paste0("Order bonus: ", round(vals$previousbonusscore, 0),"/80"))),
    h3(strong(paste0("Total face score: ", round(vals$score, 0),"/200"))),
    )
  })
  
  
  # Renders out the reactive value of the skin description in UI (SAMUEL)
  output$skindesc <- renderUI({
    numclicks <- input$selectSkin #ensure refresh after every skintype selection
    paste(getSkinDescription(vals$skintype))
  })
  
  # Continuously observe reactive values for end game state condition (MINSHUEN)
  observe({
    if(vals$actioncount == vals$selectedcount && length(vals$listoforders)>0) {
      showModal(finishModal(vals$listoforders, vals$score, vals$historydf))
      }
  })
  
  # Observe endgame replay btn to change to starting avatar page (MINSHUEN)
  observeEvent(input$replaybtn, {
    #print("Restarting game")    
    removeModal()
    updateTabsetPanel(session, inputId="tabs", selected="start")


    vals$state_moisturizer=0
    vals$state_cleanser=0
    vals$state_exfoliator=0
    vals$state_serum=0
    vals$state_toner=0
    vals$state_sunscreen=0
    vals$moisturizer=""
    vals$cleanser=""
    vals$exfoliator=""
    vals$serum=""
    vals$toner=""
    vals$sunscreen=""
    vals$historydf=prephistory()
    vals$listoforders=NULL
    vals$score=0
    vals$lastclicked=NULL
    vals$actioncount=0
    vals$selectedcount=6
    vals$moisturizer_order=0
    vals$cleanser_order=0
    vals$exfoliator_order=0
    vals$serum_order=0
    vals$toner_order=0
    vals$sunscreen_order=0
    vals$randomnumber=0
    vals$playersorders <- numeric(0)
    vals$previousbonusscore <- 0 
  })
  
  # Observe endgame notdone btn to continue ordering (MINSHUEN)
  observeEvent(input$notdonebtn, {
    removeModal()

    #print(vals$actioncount)
    #print(vals$selectedcount)
  })

  # Observe a change in random number id for getordertips each type flipbox is clicked (SAMUEL)
  observeEvent(input$flipbox1,{
    vals$randomnumber <- sample(seq(from = 1, to = 4, by = 1), 1, replace=TRUE)
    #print(vals$randomnumber)
  })

  # Renders out the tip in the UI of the flipped box (SAMUEL)
  output$tips <- renderUI({
    column(width = 10, offset = 1,
      h3(getOrderTipsDescription(input$cat,vals$randomnumber)))
  })
  
  # Renders out the final answer Modal (MINSHUEN)
  observeEvent(input$answerbtn, {
    showModal(answerModal())
  })
  
}
  
shinyApp(ui, server)
  

