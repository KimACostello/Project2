
library(shiny)
library(bslib)
library(DT)

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
                                            "High Usage"),
                         choiceValues = list(behavior_vals[1],
                                             behavior_vals[2],
                                             behavior_vals[3],
                                             behavior_vals[4],
                                             behavior_vals[5]), 
                         selected = c("1", "2", "3", "4", "5")
                         ),
            
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
                        round = TRUE),
            
            actionButton(inputId = "subset_data", "Subset the Data!")
            
            ),
        
        

          
        # Main Panel
        mainPanel(
          tabsetPanel(
            
            tabPanel("About", "UPDATE CONTENT"),
            
            tabPanel("Data Download", 
                     dataTableOutput(outputId = "subsetted_data")), 
            
            tabPanel("Data Exploration", "UPDATE CONTENT")
          )
           
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

# Updating Slider2 widget based on selection of second numeric variable
    updateSliderInput(session, 
                      "slider2", 
                      max = max(mobile_usage[input$num_var2]),
                      min = min(mobile_usage[input$num_var2]), 
                      value = c(min(mobile_usage[input$num_var2]),
                                max(mobile_usage[input$num_var2]))
    )
  })
  
  # Create reactive values  
  subset_data <- reactiveValues(usage_data = NULL)
    
  observeEvent(input$subset_data, {
      
      if(input$opsystem == "Android"){
        os_sub <- opsystem_vals[1]
      } else if(input$opsystem == "iOS"){
        os_sub <- opsystem_vals[2]
      } else{
        os_sub <- opsystem_vals
      }
      
      
      if(input$gender == "Male"){
        gender_sub <- gender_vals[1]
      } else if(input$gender == "Female"){
        gender_sub <- gender_vals[2]
      } else{
        gender_sub <- gender_vals
      }
      

      filtered_data <- mobile_usage |>
        filter(operating_system == os_sub,
               gender == gender_sub,
               user_behavior_class %in% input$behavior_class)
        
      
      #Update the subset_data reactive value object
      subset_data$usage_data <- filtered_data |>
        clean_names(case = "title")
      
    })
  
  output$subsetted_data <- renderDataTable(subset_data$usage_data)
  
  
  
  }
  
  
  
  


# Run the application 
shinyApp(ui = ui, server = server)


