
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(see)
library(ggpointdensity)
library(viridis)

source("helpers.R")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Mobile Device Usage Dataset"),
    theme = bs_theme(preset = "yeti"),

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
                                             opsystem_vals),
                        ),
            
            #Allows user to select gender
            radioButtons(inputId = "gender",
                         "Select gender of user:",
                         choiceNames = list("Male",
                                            "Female",
                                            "Both"),
                         choiceValues = list(gender_vals[1],
                                             gender_vals[2],
                                             gender_vals),
                         ), 
            
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
            
            h4("Choose your numeric variables."),
            
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
                        choices = numeric_vars,
                        selected = numeric_vars[2]),
            
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
                     
                     # Output the data table based on selections in side panel
                     dataTableOutput(outputId = "subsetted_data"),
                     
                     # Allows user to download the data as a CSV file
                     downloadButton("download_data", "Download CSV")
                     ), 
            
            tabPanel("Data Exploration", 
                     tabsetPanel(
                       
                       tabPanel("Data Summaries", 
                                
                                br(),
                                
                      # Allows user to select Numeric or Categorical Summaries
                                selectInput("summaries",
                                            label = "Select type of summaries", 
                                            choices = c("Categorical", "Numeric"),
                                            selected = "Categorical"),
                                
            # If Categorical is chosen, this will ask for user to select which categorical variables
                                conditionalPanel(
                                  condition = "input.summaries == 'Categorical'",
                                  checkboxGroupInput(inputId = "categorical_vars",
                                                     "Select categorical variables
                                                     to summarize:",
                                                     choices = clean_cat_vars[-5]),
                                  
                                  # Output contingency table for selected cat vars
                                  uiOutput("contingency_table_output")
                                  ),
            # If Numeric is chosen, this will ask for user to select which numeric variables
                      conditionalPanel(
                        condition = "input.summaries == 'Numeric'",
                        checkboxGroupInput(inputId = "numeric_var_options",
                                           label = "Select numeric variables
                                                     to summarize:",
                                           choices = clean_num_vars),
                       
                        #Allows user to select a grouping for the numeric summaries 
                        selectInput(inputId = "group",
                                    label = "Select Group:",
                                    choices = clean_cat_vars[-5]),
                        
                        # Output numeric summaries for selected numeric vars
                        uiOutput("numeric_summaries_output")
                      )
                                  
                                ),
                                
                                
                       tabPanel("Barplot", 
                                
                                br(),
                                
                                #Allows user to select barplot options
                                layout_columns(
                                card(radioButtons(inputId = "bar_x",
                                            label = "Select variable for barplot:",
                                            choices = clean_cat_vars[-5]
                                            )),
                                
                                card(radioButtons(inputId = "bar_fill",
                                            label = "Select a subgroup:",
                                            choices = clean_cat_vars[-5],
                                            selected = "Operating System"
                                            )), 
                                
                                card(radioButtons(inputId = "bar_facet",
                                            label = "Select faceting group:", 
                                            choices = clean_cat_vars[-5],
                                            selected = "Gender"
                                            )),
                                row_heights = c(5,5,5)),
                                
                                actionButton("go_barplot", "Create barplot!"),
                                
                                plotOutput(outputId = "barplot")
                                ),
            
            tabPanel("Scatterplot", 
                     
                     br(),
                     
                     #Allows user to select scatterplot options
                     fluidRow(
                       column(4,selectInput(inputId = "scatter_x",
                                         label = "Select x variable:",
                                         choices = clean_num_vars[c(1,2,3)]
                       )),
                       
                       column(4,selectInput(inputId = "scatter_y",
                                         label = "Select y variable:",
                                         choices = clean_num_vars[c(4,5)],
                       )), 
                       
                       column(4,selectInput(inputId = "scatter_color",
                                         label = "Select a category for color coding:", 
                                         choices = clean_cat_vars[c(3,4)]
                       )),
                       ),
                     
                     actionButton("go_scatterplot", "Create scatterplot!"),
                     
                     plotOutput(outputId = "scatterplot"),
                     ),
            
            tabPanel("Boxplot", 
                     
                     br(),
                     
                     # Allows user to select box plot options
                     fluidRow(
                       column(4,selectInput(inputId = "box_x",
                                            label = "Select x variable:",
                                            choices = clean_cat_vars[c(1,2)],
                       )),
                       
                       column(4,selectInput(inputId = "box_y",
                                            label = "Select y variable:",
                                            choices = clean_num_vars,
                       )), 
                       
                       column(4,selectInput(inputId = "box_fill",
                                            label = "Select a subcategory:", 
                                            choices = clean_cat_vars[c(3,4)],
                       )),
                     ),
                     
                     actionButton("go_boxplot", "Create boxplot!"),
                     
                     plotOutput(outputId = "boxplot")
            ),
            
            tabPanel("Violin/Dot Plot",
                     
                     br(),
                     
                     # Allows user to select violin/dot plot options
                     fluidRow(
                       column(4,selectInput(inputId = "violin_x",
                                            label = "Select x variable:",
                                            choices = clean_cat_vars[c(1,2)],
                       )),
                       
                       column(4,selectInput(inputId = "violin_y",
                                            label = "Select y variable:",
                                            choices = clean_num_vars,
                       )), 
                       
                       column(4,selectInput(inputId = "violin_fill",
                                            label = "Select a subcategory:", 
                                            choices = clean_cat_vars[c(3,4)],
                       )),
                     ),
                     
                     actionButton("go_violinplot", "Create voilin/dot plot!"),
                     
                     plotOutput(outputId = "violin_plot")
            ),
            
            tabPanel("Combo Plot",
                     
                     br(),
                     
                     "This plot is a cross between a Scatterplot and a Density Plot.",
                     
                     br(),
                     br(),
                     
                     # Allows user to select combo plot options
                     fluidRow(
                       column(4,selectInput(inputId = "combo_x",
                                            label = "Select x variable:",
                                            choices = combo_num_vars[c(4,6)],
                       )),
                       
                       column(4,selectInput(inputId = "combo_y",
                                            label = "Select y variable:",
                                            choices = combo_num_vars[c(1,2,3,5)],
                       )), 
                       
                       column(4,selectInput(inputId = "combo_facet",
                                            label = "Select a faceting group:", 
                                            choices = clean_cat_vars[-5],
                       )),
                     ),
                     
                     actionButton("go_comboplot", "Create combo plot!"),
                     
                     plotOutput(outputId = "combo_plot")
            )
            
            
            
            
            
            )))
          )
           
        )
    
    )


#Read in data
mobile_usage <- read_csv("user_behavior_dataset.csv")
mobile_usage <- mobile_usage |>
  clean_names("snake") |>
  mutate(behavior_class = as.factor(user_behavior_class),
         device_model = as.factor(device_model),
         operating_system = as.factor(operating_system),
         user_id = as.character(user_id),
         user_behavior_class = as.factor(user_behavior_class))

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
  
  # Sub-setting the Data  
  observeEvent(input$subset_data, {
      
      if(input$opsystem == "Android"){
        os_sub <- opsystem_vals[1]
      } else if(input$opsystem == "iOS"){
        os_sub <- opsystem_vals[2]
      } else{
        os_sub <- c(opsystem_vals[1], opsystem_vals[2])
      }
      
      if(input$gender == "Male"){
        gender_sub <- gender_vals[1]
      } else if(input$gender == "Female"){
        gender_sub <- gender_vals[2]
      } else{
        gender_sub <- c(gender_vals[1], gender_vals[2])
      }
    
    num1_col <- mobile_usage[[input$num_var1]]
    num2_col <- mobile_usage[[input$num_var2]]
    
    filtered_data <- mobile_usage |>
        filter(operating_system %in% os_sub,
               gender %in% gender_sub,
               user_behavior_class %in% input$behavior_class,
               num1_col >= input$slider1[1],
               num1_col <= input$slider1[2],
               num2_col >= input$slider2[1],
               num2_col <= input$slider2[2]
               )
        
      
      #Update the subset_data reactive value object
      subset_data$usage_data <- filtered_data |>
        clean_names(case = "title")
      
    })
  
  output$subsetted_data <- renderDataTable(subset_data$usage_data)
  
  
  #Allows user to download the data table
  output$download_data <- downloadHandler(
    filename = function(){
      paste('mobile_usage_data-',Sys.Date(), '.csv', sep = '')
    },
    content = function(con){
      write.csv(subset_data$usage_data, con)
    }
  )
  

  # Render the contingency table dynamically
  output$contingency_table_output <- renderUI({
    req(input$categorical_vars) # Require at least one variable to be selected
  
    df <- subset_data$usage_data[,input$categorical_vars]
    
    if (length(input$categorical_vars) == 1) {
      renderTable({
        table(df[1])
      }, rownames = TRUE)
    } else if (length(input$categorical_vars) == 2) {
      renderTable({
        table(df[[1]], df[[2]])
      }, rownames = TRUE)
    } else if (length(input$categorical_vars) == 3) {
      renderTable({
        table(df[[1]], df[[2]], df[[3]])
      }, rownames = TRUE) 
    } else if (length(input$categorical_vars) == 4) {
      renderTable({
        table(df[[1]], df[[2]], df[[3]], df[[4]])
      })
    }
    })
  
  # Render the numeric summaries dynamically
  output$numeric_summaries_output <- renderUI({
    req(input$numeric_var_options) # Require at least one variable to be selected
    
    df <- subset_data$usage_data
    
    renderTable({
      df |>
        dplyr::group_by(df[[input$group]]) |>
        summarize(across(.cols = input$numeric_var_options,
                         .fns = list("mean" = mean, 
                                     "median" = median, 
                                     "sd" = sd),
                         .names = "{.fn}_{.col}"))
      
    })
   
  })
  # Render barplot
  observeEvent(input$go_barplot, {
    
    output$barplot <- renderPlot({
      
      x <- isolate(input$bar_x)
      fill <- isolate(input$bar_fill)
      facet <- isolate(input$bar_facet)
      
      
      ggplot(data = subset_data$usage_data) +
        geom_bar(aes(x =!!sym(x), fill = !!sym(fill)),
                 position = "dodge",
                 color = "black", 
                 alpha = 0.6) +
        facet_wrap(~ get(facet)) 
    })
  })
  
  
  # Render scatterplot
  
  observeEvent(input$go_scatterplot, {
    
    output$scatterplot <- renderPlot({
      
      x <- isolate(input$scatter_x)
      y <- isolate(input$scatter_y)
      color <- isolate(input$scatter_color)
      
      ggplot(data = subset_data$usage_data) +
        geom_point(aes(x = !!sym(x), 
                       y = !!sym(y), 
                       color = !!sym(color))) +
        scale_color_manual(values = c("#FFFF00",
                                      "#FF00FF")) +
        labs(dictionary = var_dictionary) +      # looks up variables in dictionary for display name
        theme_dark()
    })
  })

  
  # Render boxplot
  observeEvent(input$go_boxplot, {
    
    output$boxplot <- renderPlot({
    
    x <- isolate(input$box_x)
    y <- isolate(input$box_y)
    fill <- isolate(input$box_fill)
    
    ggplot(data = subset_data$usage_data) +
      geom_boxplot(aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill))) +
      scale_fill_manual(values = c("lightpink",
                                   "lightblue")) +
      labs(dictionary = var_dictionary)
    })
  })
  
  # Render violin/dot plot
  observeEvent(input$go_violinplot, {
    
    output$violin_plot <- renderPlot({

      x <- isolate(input$violin_x)
      y <- isolate(input$violin_y)
      fill <- isolate(input$violin_fill)
      
      ggplot(data = subset_data$usage_data) +
        geom_violindot(aes(x = !!sym(x),
                           y = !!sym(y),
                           fill = !!sym(fill)), 
                       fill_dots = "black") +
        labs(dictionary = var_dictionary) +
        theme_modern() +
        scale_fill_material_d()
    })
  })
  
  # Render the combo plot
  observeEvent(input$go_comboplot, {
    
    output$combo_plot <- renderPlot({
      
      x <- isolate(input$combo_x)
      y <- isolate(input$combo_y)
      facet <- isolate(input$combo_facet)
      
      ggplot(data = subset_data$usage_data) +
        geom_pointdensity(aes(x = !!sym(x), y = !!sym(y))) +
        scale_color_viridis() +
        labs(dictionary = var_dictionary) +
        facet_wrap(~ get(facet))
    })
  })
  
  
  
  
  
  
  
  }
  
  
  
  


# Run the application 
shinyApp(ui = ui, server = server)


