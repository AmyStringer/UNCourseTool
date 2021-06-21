#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app was designed for use by organisations within the United Nations. 
# The app is based upon a framework developed to upskill people within the organisation 
# in the area of using remote sensing for agricultural monitoring.
# A course "database" was established containing names, link and contents contained within various online
# courses. It is this database which is inputted into this app and it is this document that the 
# search engine and course recommender components search over. 


library(shiny)
library(stringr)      # for all of the string search and count functions 
library(tidyverse)    # for almost everything else 
library(shinythemes)  # for the theme 
library(DT)           # for interactive data tables 

# read in the course info data frame 
dat <- read_csv("CourseSumDB.csv")

# theoretically, the first time using this app, this should just be an empty dataframe 
# with header names c(OverallScore,	Relevance,	SuitabilityLevel,	Clarity,	Delivery,	OtherFeedback)
coursefeedback <- read_csv("CourseFeedback.csv")

# these are the files containing the search functions 
source("SearchEngine.R")     # contains a function called searchEngine
source("RecommenderFun.R")   # contains a function called Recommender

# Define UI for application that draws a histogram
ui <- navbarPage(title = "United Nations Course Tool", 
                 theme = shinythemes::shinytheme("flatly"), 
                 # shinythemes::themeSelector(), # uncomment this line if you wish to look at other themes 
                 
                 #### WELCOME SCREEN ####                
                 tabPanel(title = "Welcome",
                          # use h1 for the main heading on any one tab - be consistent 
                          h1("Welcome to the Course Search and Evaluation Tool", style = "float:right"),
                          
                          fluidRow(
                            column(width = 4, 
                                   # got to add some logos, these should be contained in the www folder 
                                   img(src = "qut.png", height = 100, width = 262, style = "float:left"), 
                                   img(src = "ACEMS.png", height = 150, width = 260, style = "float:left"), 
                                   img(src = "FAO.png", height = 200, width = 200, style = "float:left")
                                   
                            ), 
                            column(width = 8, 
                                   
                                   code("Test Phase", style = "float:right"),
                                   
                                   # h3 is used for the level 2 headings 
                                   h3("The Search Engine", style = "foat:right"),
                                   # h5 is used for paragraph text in the main panel on any tab 
                                   h5("This tool allows you to search for courses based on a simple keyword search. Searches it is capable of include searches based on user profile, searches based on knowledge area, or simply searches based on key topics you may wish to learn about. ", style = "foat:right"),
                                   h5("It is important to note that mispelled words will not return any results, and it is best to keep phrases as short as possible in order to return the most results. "),
                                   h5("Below are a few example searches"), 
                                   # build an unordered list 
                                   tags$ul(
                                     # add list items to said list 
                                     tags$li("LDAS; data access;"), 
                                     tags$li("data scientist; regression;"), 
                                     tags$li("Remote sensing;"), 
                                     tags$li("Manager;")
                                   ),
                                   
                                   h3("The Personalised Self Learning System"),
                                   h5("Here you will have the opportunity to build your own learning profile from recommendations based off your personal profile. "),
                                   h5("Instructions for use are contained in the tools dedicated welcome page. "),
                                   
                                   h3("Course Evaluation Tool"), 
                                   h5("This tool will be used to collect feedback on the courses recommended. Feedback will be given on things such as applicability, ease of learning, content and so on, to allow for continued development of the course database. "), 
                                   tags$hr(), 
                                   h6("Original code developed by Amy Stringer, under the guidance of Distinguished Professor Kerrie Mengersen at the Queensland University of Technology, in collaboration with the Food and Agriculture Organisation of the United Nations. "), 
                                   h6("If you wish to contribute to our course list, or if you have any feedback on the usability of the application, please contact", tags$a(href = "mailto:aj.stringer@qut.edu.au", "Amy Stringer"))
                                   
                            )
                          ),
                          
                 ),
                 #### SEARCH ENGINE UI ####
                 tabPanel(title = "Search Engine",
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          fluidRow(
                            column(width = 4, 
                                   # need those logos 
                                   img(src = "qut.png", height = 100, width = 262), 
                                   img(src = "ACEMS.png", height = 150, width = 260), 
                                   img(src = "FAO.png", height = 200, width = 200)
                                   
                            ),
                            column(width = 4,
                                   h1("Keyword Course Search"),
                                   h5("Enter keywords or phrases, separated by semi-colons (;), in the field below to search for relevant courses. For a list of all available courses, simply don't type anything in to the search and click the search button. "),
                                   shiny::textInput("keywords", "Keywords:"),
                                   shiny::actionButton("search", "Search")
                            )
                          ),
                          
                          # dataTableOutput allows for more interaction with the table 
                          # it also improves the readability of the data within the app 
                          # it also allows for html code included in the dataset to be executed 
                          # for the purposes of creating, for example, hyperlinks. 
                          DT::dataTableOutput("searchResults"),
                          style = "height:900px; overflow-y: scroll;overflow-x: scroll;"
                 ),
                 #### LEARNING SYSTEM UI #### 
                 
                 tabPanel(title = "Personalised Self Learning System", 
                          # im not certain that the error hiding code needs to be in every tab. 
                          # but also included here is some css styling that allows for pop up notifications used in this tab
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }",
                                     "#shiny-notification-search_notif {position: fixed; top: 600px; left: 40px; width : 15em; opacity: 1;}", 
                                     "#shiny-notification-add_notif {position: fixed; top: 200px; right: 60px; width : 15em; opacity: 1;}"
                          ),
                          # img(src = "acems.png", height = 220, width = 260)
                          sidebarLayout(
                            sidebarPanel(
                              # need to build the questionnaire to develop the recommendations 
                              h5("Which of the below option best suits your role?"),
                              shiny::selectInput("user", "", 
                                                 choices = c(None = "", 
                                                             "Manager", 
                                                             "GIS Expert", 
                                                             "Data Scientist"), 
                                                 multiple = TRUE, 
                                                 selected = ""), 
                              h5("Which knowledge area are you looking to improve?"),
                              shiny::selectInput("core", "", 
                                                 choices = c(None = "", 
                                                             "Data Analysis", 
                                                             "GIS", 
                                                             "Software", 
                                                             "Data Management", 
                                                             "Remote Sensing", 
                                                             "Visualisation"), 
                                                 multiple = TRUE,
                                                 selected = ""), 
                              h5("How would you rate your current knowledge in this area?"),
                              shiny::selectInput("level", "", 
                                                 choices = c(None = 0, 
                                                             "Fundamental" = 1, 
                                                             "Introductory" = 2, 
                                                             "Intermediate" = 3, 
                                                             "Advanced" = 4), 
                                                 selectize = TRUE), 
                              h5("What level of knowledge are you aiming for? "),
                              shiny::selectInput("levelAim", "", 
                                                 choices = c(None = 0, 
                                                             "Fundamental" = 1, 
                                                             "Introductory" = 2, 
                                                             "Intermediate" = 3, 
                                                             "Advanced" = 4), 
                                                 selectize = TRUE), 
                              # we don't want the program to do anything until all fields have been answered 
                              # and the user hits search 
                              shiny::actionButton(inputId = "search2", "Search")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Welcome",
                                         h3("Welcome to the Personalised Learning System"), 
                                         
                                         fluidRow(
                                           # logo logo logo 
                                           img(src = "qut.png", height = 100, width = 262, style = "float:right"),
                                           h5("To the left you will see some options. This is where we build your personal profile. Steps for doing so are outline below.  "),
                                           # ordered list 
                                           tags$ol(
                                             # list item providing some instructions on how to use the tool
                                             tags$li("First you select what kind of user you are out of \"GIS Expert \", \"Manager\" or \"Data Scientist\". If you fit into more than one category, just", tags$b("select all that apply.")), 
                                             tags$li("Next you must identify which knowledge areas you are looking to learn about. You may be interested in more than one area, if this is the case, select all that apply. "), 
                                             tags$li("Next you need to assess your level of knowledge in your selected knowledge areas. You can choose between \"None\", \"Fundamental\", \"Introductory\", \"Intermediate\", or \"Advanced\". There are no courses above the level of Advanced. "), 
                                             tags$li("Next, using the same skill level scale, enter what level you would like to achieve by the end of this training. "),
                                             tags$li("Click Search")
                                           ),
                                           h5("Once you have filled out your personal profile and clicked search,", tags$b(tags$em("your results will appear under the Course Options Tab.")), "From here you will be able to hand pick courses to add to your personal profile. "), 
                                           style = "float:left"
                                         ),
                                         fluidRow(
                                           # add another logo
                                           img(src = "ACEMS.png", height = 150, width = 260, style = "float:right"), 
                                           img(src = "FAO.png", height = 200, width = 200, style = "float:right")
                                         ),
                                         
                                ),
                                tabPanel("Course Options", 
                                         
                                         fluidRow(
                                           h1("Course Options"),
                                           # more instructions 
                                           h5("Click on the courses you would like to add to your personal profile, and then hit the Add to Cart button below. "),
                                           # a button that allows us to add selected rows to a new table in another tab 
                                           shiny::actionButton("add", "Add to Cart"),
                                           h5("Once you have added all the courses you would like to try, head across to the Personal Profile Tab and review your cart. Should you wish to remove any courses, you can do so there. "),
                                           # datatableoutput for interactive table - this allows us to select rows and add them to another table 
                                           DT::dataTableOutput("recommendations"), 
                                           style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
                                         )
                                ), 
                                tabPanel("Personal Profile",
                                         fluidRow(
                                           h1("Personal Profile"),
                                           h5("To remove a course from your personal profile, select it and hit the Remove from Cart button below."),
                                           # we have two buttons this time, one to remove things from the current table 
                                           # and one to download your personal profile results as a .csv
                                           shiny::actionButton("remove", "Remove from Cart"),
                                           shiny::downloadButton("downloadData", "Download"),
                                           # as above, we want to be able to interact with this table 
                                           # but it also helps to have the multiple page view rather than one long table
                                           DT::dataTableOutput("profile"), 
                                           style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
                                         )
                                )
                              )
                            )
                            
                          )
                          
                 ), 
                 #### COUSE EVALUATION UI ####    
                 tabPanel(title = "Course Evaluation Form ", 
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          # this tab will have no outputs within the app, but the responses here
                          # will contribute to updating a csv file contained in the application folder
                          fluidRow(
                            column(width = 4, 
                                   # need those logos on every page 
                                   img(src = "qut.png", height = 100, width = 262, style = "float:left"), 
                                   img(src = "ACEMS.png", height = 150, width = 260, style = "float:left"), 
                                   img(src = "FAO.png", height = 200, width = 200, style = "float:left")
                                   
                            ),
                            column(width = 8, 
                                   # select input to select from the list of potential course names 
                                   shiny::selectInput("courseName", "Which course are you giving feedback on?", 
                                                      choices  = unique(dat$CourseName), 
                                                      selected = ""),
                                   # \U2606 is the code to print a star, so this is a 1-5 star rating system
                                   shiny::radioButtons("rat1", "How would you rate this course overall (5 stars = very good)?", 
                                                       choices = c("\U2606" = 1, "\U2606 \U2606" = 2, "\U2606 \U2606 \U2606" = 3, 
                                                                   "\U2606 \U2606 \U2606 \U2606" = 4, "\U2606 \U2606 \U2606 \U2606 \U2606" = 5), 
                                                       inline = TRUE), 
                                   shiny::radioButtons("role1", "Based on your role, how relevant did you find the content in this course?", 
                                                       choices = c("Irrelevant" = 1, "Minimally Relevant" = 2, 
                                                                   "Somewhat Relevant" = 3, "Relevant" = 4, "Highly Relevant" = 5),
                                                       inline = TRUE), 
                                   shiny::radioButtons("level1", "Based on your starting level, how did you find the pace of the course?", 
                                                       choices = c("Too slow" = 1, "Tolerable" = 2, "Just Right" = 3, "Manageable" = 4, "Too Fast" = 5), 
                                                       inline = TRUE), 
                                   shiny::radioButtons("clarity", "How would you rate the clarity of the content in this course? (1 star = not clear, 5 stars = very clear)", 
                                                       choices = c("Unclear" = 1, "Somewhat clear" = 2, "Unsure" = 3, "Mostly clear" = 4, "Clear" = 5), 
                                                       inline = TRUE), 
                                   shiny::radioButtons("delivery", "How would you rate the delivery of the course?", 
                                                       choices = c("\U2606" = 1, "\U2606 \U2606" = 2, "\U2606 \U2606 \U2606" = 3, 
                                                                   "\U2606 \U2606 \U2606 \U2606" = 4, "\U2606 \U2606 \U2606 \U2606 \U2606" = 5), 
                                                       inline = TRUE),
                                   # we want the feedback window to fill the space 
                                   tags$style(type = "text/css", "textarea {width:100%}"),
                                   tags$textarea(id = "feedback", rows = 5, placeholder = "Any Other Feedback? "), 
                                   # again, we don't want the app to do anything until all fields have been filled out 
                                   # so we link this to the submit button 
                                   shiny::actionButton("submit", "Submit Feedback")
                                   
                                   
                            )
                          )
                 )
)


#### SERVER ####
server <- function(input, output, session) {
  
  #### SEARCH ENGINE SERVER #### 
  # initialise the search results as a reactive empty object 
  results <- reactiveValues(data = NULL)
  
  # when the user hits the search button we want the app to observe this 
  # and then carry out some task in response. 
  # in particular, we want the app to use the searchEngine function contained in
  # the source we read in at the very beginning 
  observeEvent(input$search, {
    
    # cases shouldn't matter
    dat <- dat %>% mutate(
      Contents = str_to_lower(Contents),
      Level = str_to_lower(Level),
      User = str_to_lower(User)
    )
    
    # call to the search engine function contained in the sourced R file
    # results$data calls the reactiveValues object created above 
    results$data <- searchEngine(keywords = input$keywords, dat = dat)
    
    # we want the link in the link columns to present as hyperlinks in the datatable
    # to do this we can add some html code around the link text already contained in the column
    results$data <- results$data %>% 
      mutate(Link = paste0("<a href='", Link,"'>", Link,"</a>"))
    
    results$data
    
  })
  
  output$searchResults <- renderDataTable({
    
    # escape = FALSE means DT does not remove the html code we added above 
    DT::datatable(results$data, escape = FALSE)
    
  })
  
  #### Moving on to the recommendations outputs #### 
  # set up a reactive data object to store the recommendations in 
  recommendationsDT <- reactiveValues(data = NULL)
  
  observeEvent(input$search2, {
    # makes a call to the recommenderFun.R document that was sourced at the 
    # beginning of this file 
    # it outputs a dataframe 
    recommendationsDT$data <- Recommender(input, dat = dat)
    
    # just link above, we want those links to by hyperlinked, so we add in some html 
    recommendationsDT$data <- recommendationsDT$data %>% 
      mutate(Link = paste0("<a href='", Link,"'>", Link,"</a>"))
    
  })
  
  # this is actually pretty neat. Because the results of the search show up in a 
  # different tab. what we can do is direct the user to that tab through the use of popup
  # notifications using the shownotifications function 
  # we can use observeEvent to link this to the search button 
  observeEvent(input$search2, {
    
    shiny::showNotification("Go to the Course Options Tab", type = "message", id = "search_notif")
    
  })
  
  # this data frame is then outputted to the mainpanel using renderDataTable 
  output$recommendations <- renderDataTable({
    DT::datatable(recommendationsDT$data, escape = FALSE)
  }, 
  # selection = "multiple" allows us to select more than one data row at a time
  selection = "multiple")
  
  # here we create a reactive object for the personal profile selections 
  # we will intialise this as an empty dataframe, for the purposes on binding
  profileCourses <- reactiveValues(data = data.frame())
  
  # when a user hits the add button, we want a row to be added to the personal profile 
  # without overwriting the information that is already there
  # we also want the personal profile not to contain duplicates of things that have already 
  # been selected 
  observeEvent(input$add, {
    
    profileCourses$data <- bind_rows(profileCourses$data, recommendationsDT$data[input$recommendations_rows_selected, ]) 
    profileCourses$data <- profileCourses$data[!duplicated(profileCourses$data$CourseName), ]
    
  })
  
  # after we click the add button, the app adds things to another table which we can't see in the current tab
  # so we add another pop up message in a different spot, to direct the user 
  observeEvent(input$add, {
    shiny::showNotification("Go to the Personal Profile Tab to review your cart.", type = "message", id = "add_notif")
  })
  
  # users might make mistakes and wish to remove courses from their profile 
  # we can make this happen using the remove button 
  # it is important to note that this updates the profileCourses reactive object 
  observeEvent(input$remove, {
    profileCourses$data <- profileCourses$data[-input$profile_rows_selected, ]
  })
  
  # here we want to display the course profile 
  output$profile <- renderDataTable({
    DT::datatable(profileCourses$data, escape = FALSE)
  }, 
  selection = "multiple")
  
  # here is where we deal with the download button 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('ProfileData_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(profileCourses$data, con)
    }
  )
  #### Feedback tab #### 
  # we want to create a reactive values object to store the feedback 
  feedback <- reactiveValues(data = NULL)
  
  # once a user hits submit, we want to update that dataset 
  # at the very top we read in an empty (potentially) feedback csv file 
  # called CourseFeedback
  observeEvent(input$submit, {
    
    feedbackUser <- tibble(
      OverallScore = input$rat1, 
      Relevance = input$role1, 
      SuitabilityLevel = input$level1, 
      Clarity = input$clarity, 
      Delivery = input$delivery, 
      OtherFeedback = input$feedback
    )
    # bind the feedback from a single user to the feedback from all users so far 
    coursefeedback <- bind_rows(coursefeedback, feedbackUser)
    # and update the CourseFeedback csv file 
    write_csv(coursefeedback, "CourseFeedback.csv")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
