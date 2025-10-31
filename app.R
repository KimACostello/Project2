
library(shiny)
library(bslib)

source("helpers.R")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Mobile Device Usage Dataset"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          
              h4("Let's subset the data!"),
              
              #Allows user to select operating system
              radioButtons(inputId = "opsystem",
                        "Select the device operating system:",
                        choiceNames = list("Android",
                                           "iOS",
                                           "Both"),
                        choiceValues = list(opsystem_vals[1],
                                             opsystem_vals[2],
                                             opsystem_vals)
            ),
            
            #Allows user to select gender
            radioButtons(inputId = "gender",
                         "Select gender of user:",
                         choiceNames = list("Male",
                                            "Female",
                                            "Both"),
                         choiceValues = list(gender_vals[1],
                                             gender_vals[2],
                                             gender_vals)), 
            
            #Allows user to select user behavior class
            checkboxGroupInput(inputId = "behavior_class",
                         "Select user usage behavior:",
                         choiceNames = list("Low Usage",
                                            "Low-Moderate Usage",
                                            "Moderate Usage",
                                            "Moderate-High Usage",
                                            "High Usage",
                                            "All Usage Levels"),
                         choiceValues = list(behavior_vals[1],
                                             behavior_vals[2],
                                             behavior_vals[3],
                                             behavior_vals[4],
                                             behavior_vals[5],
                                             behavior_vals)),
            
            br(),
            
            h4("Choose your numeric variables!"),
            
            #Allows user to select a numeric variable
            selectInput(inputId = "num_var1",
                        label = "Select first numeric variable:",
                        choices = numeric_vars),
            
            #Allows user to select the range of the first numeric variable
            sliderInput(inputId = "slider1", 
                        "Select range of values:",
                        min = 0,
                        max = 0, 
                        value = c(0, 0),
                        step = 1,
                        round = TRUE), 
            
            #Allows user to select another numeric variable
            selectInput(inputId = "num_var2",
                        label = "Select second numeric variable:",
                        choices = numeric_vars),
            
            #Allows user to select the range of the second numeric variable
            sliderInput(inputId = "slider2", 
                        "Select range of values:",
                        min = 0,
                        max = 0, 
                        value = c(0, 0),
                        step = 1,
                        round = TRUE)
            
            
            
            ),
        
        
        
        
          
        # Main Panel
        mainPanel(
           
        )
    
    )
)

#Read in data
mobile_usage <- read_csv("user_behavior_dataset.csv")
mobile_usage <- clean_names(mobile_usage, "snake")

# Define server logic 
server <- function(input, output, session) {
  
  
  observe({
  
# Updating Slider1 widget based on selection of first numeric variable    
  updateSliderInput(session, 
                    "slider1", 
                    max = max(mobile_usage[input$num_var1]),
                    min = min(mobile_usage[input$num_var1]), 
                    value = c(min(mobile_usage[input$num_var1]),
                              max(mobile_usage[input$num_var1]))
                    )
    
    updateSliderInput(session, 
                      "slider2", 
                      max = max(mobile_usage[input$num_var2]),
                      min = min(mobile_usage[input$num_var2]), 
                      value = c(min(mobile_usage[input$num_var2]),
                                max(mobile_usage[input$num_var2]))
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


