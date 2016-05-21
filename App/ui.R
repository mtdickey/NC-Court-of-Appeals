# UI

#Required library
library(shiny)
library(shinydashboard)

#initialize shiny interface
shinyUI(dashboardPage(skin = "black",
  
  #App Title
  dashboardHeader(title = "NC Court of Appeals"),
  dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "Introduction", icon = icon("info-circle")),
    menuItem("Time Until Decision", tabName = "length", icon = icon("clock-o")),
    menuItem("Disposition", tabName = "disposition", icon = icon("gavel")),
    menuItem("Dissent", tabName = "dissent", icon = icon("remove"))
  )),
    
  #Dashboard body content
  dashboardBody(
    tabItems(
      #First tab - predict length of time until decision made
      tabItem(tabName = "length",
        fluidRow(
            box( 
              h2("Case Details:"), 
              selectInput("oral", label="Scheduled for Oral Argument?", choices = levels(appeals$OralArgument),
                                                                                       selected = "No"),
              selectizeInput("judges", label="Judges Appointed:", choices = levels(appeals$Author), multiple=T),
              selectInput("type", label = "Type of Case:", choice = levels(appeals$Type)),
              br(),
              br(),
              h3(textOutput("PredictedLength"))
            ),
            box( 
                sliderInput("greaterThan", label = "What's the chance it will take longer than ___ days?", min = 1, max = 500, value = 100),
                h5(textOutput("LengthProb")),
                plotOutput("LengthPlot")
            )
        ) # close row
      ), # close length tab
    
    
      # Second tab content - Disposition prediction
      tabItem(tabName = "disposition",
              fluidRow(
                box(
                  h2("Case Details:"),
                  selectizeInput("judges2", label="Judges Appointed:", choices = levels(appeals$Author), multiple=T),
                  selectInput("county2", label = "County:", choices = sort(unique(appeals$County))),
                  selectInput("type2", label = "Type of Case:", choice = levels(appeals$Type)),
                  selectInput("oral2", label="Scheduled for Oral Argument?", choices = levels(appeals$OralArgument), 
                                                                                           selected = "No"),
                  br(),
                  h3("Predicted Disposition:"),
                  tableOutput("dispPredTable")
                ),
                box(
                  plotOutput("DispositionPlot")
                )
              )
      ), # close disposition tab
      
      # Second tab content - Disposition prediction
      tabItem(tabName = "dissent",
              fluidRow(
                box(
                  h2("Case Details:"),
                  selectizeInput("judges3", label="Judges Appointed:", choices = levels(appeals$Author), multiple=T),
                  selectInput("oral3", label="Scheduled for Oral Argument?", choices = c("No", "Yes", "Fast Track"), 
                              selected = "No"),
                  br(),
                  h3(textOutput("dissentPrediction"))
                ),
                box(
                  plotOutput("DissentPlot")
                )
              )      ), # close disposition tab
      
      tabItem(tabName = "Introduction",  
              
              fluidRow(
                
                box(
                  title = " ", solidHeader = T,
                  img(src="flag.png", height = 220, width = 300, align="left", position="absolute"),
                  h1("NC Court of Appeals - Case Prediction Dashboard"),
                  br(),
                  p("This dashboard is a tool for anyone interested in making predictions regarding a case in the NC Appellate Court System.  
                    The purpose of this is to be able to anticipate three key pieces of information: "),
                  tags$ul(
                    tags$li(tags$b("How long it will be before a decision will be made:"), " this ranges from 8 to 400 days in the past.", 
                            " Knowing information such as the type of case and whether an oral argument is involved can help narrow down 
                            an expected wait time."), 
                    tags$li(tags$b("What the disposition will be: "), "about 65% of appellate cases in 2015 were affirmed - however, 
                            this varies across judges and case type."),
                    tags$li(tags$b("Whether a judge will dissent: "), "while judges are usually in argreement, more recently judges
                            have been more likely to dissent a decision and therefore grant the automatic right for another appeal.")
                  ),
                  br(),
                  br(),
                  p("Explore the following tabs to get a better idea of what to expect for the case you are interested in."),
                  br(),
                  h4(strong("Disclaimer")),
                  p("This website is not endorsed by the North Carolina Administrative Office of the Courts or the North Carolina Judicial Branch."),
                  p("The information on this website is provided for educational purposes, and not for legal advice.  Although this website may make general predictions, it does so based upon objective,
                    publicly-available data from prior cases and cannot guarantee that the facts or circumstances of future cases will conform with historic trends.
                    By using this website you acknowledge that you and the website contributors have no attorney-client relationship or express or implied contract.  
                    The information contained in this website should not be used as a substitute for legal advice from a licensed lawyer.  The information contained on this website is not a substitute for legal research."),
                  br(),
                  h4(strong("Contact")),
                  p("The idea for this application was developed by ", 
                    span(a("Kenzie Rakes", href = "mailto:rakeskm@gmail.com"), style = "color:blue"),
                    ". Any feedback is appreciated."), 
                  
                  width=10)
                  )
              
      ) #close intro tab no comma.
      
  ### Final closes ###
    )# close all tabs
  )# close dashboard body
) # close dashboard page 
) # close shiny UI