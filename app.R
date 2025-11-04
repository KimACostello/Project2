
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(tidyr)
library(janitor)
library(see)
library(ggpointdensity)
library(viridis)
library(GGally)
library(shinyalert)
library(ggplot2)
library(shinyjs)

source("helpers.R")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel(tags$b("Mobile Device Usage App")),
    theme = bs_theme(preset = "yeti"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(width = 3,
          
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
            
            # About Tab
            tabPanel("About",
                     
                     br(),
                     
                     h4(tags$u("About the Mobile Device Usage App")),
                     
                     br(),
                     
                     h5(tags$b("Purpose:")),
                     tags$p("     This Mobile Device Usage App allows you to subset and explore data about mobile device usage from 700 individual users. The dataset provides analysis of mobile usage patterns and user behavior classification. "),
                     
                     tags$p("     Users are categorized into 1 of 5 User Behavior Classes based on usage patterns:"),
                     
                     # List of User Behavior Classes
                     tags$menu(tags$li("1 = Low Usage"),
                               tags$li("2 = Low-Moderate Usage"),
                               tags$li("3 = Moderate Usage"),
                               tags$li("4 = Moderate-High Usage"),
                               tags$li("5 = High Usage"),),
                     
                     br(),
                     
                     tags$p("Other key features of this dataset include:",
                            
                            # List of variables 
                            tags$menu(tags$li("User ID: Unique identifier for each user."),
                                      tags$li("Device Model: Model of the user's smartphone."),
                                      tags$li("Operating System: The OS of the device (iOS or Android)."),
                                      tags$li("App Usage Time: Daily time spent on mobile applications, measured in minutes."),
                                      tags$li("Screen On Time: Average hours per day the screen is active."),
                                      tags$li("Battery Drain: Daily battery consumption in mAh."),
                                      tags$li("Number of Apps Installed: Total apps available on the device."),
                                      tags$li("Data Usage: Daily mobile data consumption in megabytes."),
                                      tags$li("Age: Age of the user."),
                                      tags$li("Gender: Gender of the user (Male or Female)."))),
                     
                     h5(tags$b("How to use the app:")),
                     
                     # ordered list
                     tags$ol(
                       tags$li("Subset the data by making selections on the sidepanel. To view the entire dataset, select 'Both' for device operating system and gender, and leave all other selections as is. Click 'Subset the Data!` once selections are made."),
                       tags$li("The Data Download tab will provide a data table based on the selections you made to subset the data. A CSV file of the data table can be downloaded by clicking on the 'Download CSV' button."),
                       tags$li("The Data Exploration tab lets you explore the dataset via numeric and categorical summaries and numerous types of plots. This tab is broken into many sub-tabs.",
                               
                               br(),
                               br(),
                               
                               # List the sub-tabs and what they do
                               tags$menu
                               (tags$li("Summaries: This tab allows you to choose between numeric or categorical summaries, and allows you to select the variables you would like to summarize."),
                                         tags$li("Barplot: This tab allows you to select a categorical variable and groups to generate a barplot."),
                                         tags$li("Scatterplot: This tab allows you to generate a scatterplot by selecting the x and y variables of your choice, and selecting a category to color code the points."), 
                                         tags$li("Boxplot: This tab allows you to generate a box plot by selecting the x and y variables of your choice, and selecting a subcategory."), 
                                         tags$li("Violin Plot: This tab allows you to generate a plot that is a combination of a violin plot and a dot plot. X and y variables, and subcategory selections can be made."),
                                         tags$li("Density Plot: This tab allows you to select a numeric variable and a group to generate a density plot."),
                                         tags$li("Combo Plot: This tab allows you to generate a plot that is a combination of a scatterplot and a density plot. You can select the x and y variables, and a faceting group. "), 
                                         tags$li("Correlation: This tab allows you to generate a correlation matrix by selecting at least two numeric variables.")
                     ))),
                     
                     h5(tags$b("Source:")),
                     
                     tags$p("The data is provided by Kaggle datasets expert, Vala Khorasani, and more information can be found here:"),
                     
                     # Provides link to the data source
                     tags$a(href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset", "Source Data Here"),
                     
                     br(),
                     br(),
                     
                     tags$small("This application was created by Kim Costello.")
                     
                     ),
            
            tabPanel("Data Download", 
                     
                     # Output the data table based on selections in side panel
                     dataTableOutput(outputId = "subsetted_data"),
                     
                     # Allows user to download the data as a CSV file
                     layout_columns(
                       card(downloadButton("download_data", "Download CSV")),
                       col_widths = c(-9, 3)
                     )
                     ), 
            
            tabPanel("Data Exploration", 
                     tabsetPanel(
                       
                       tabPanel("Summaries", 
                                
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
                                
                                useShinyjs(), #must be included in UI for shinyjs functions to work
                                
                                # Message will display until create plot button is clicked.
                                div(id = "barplot_message",
                                    p("Please select variable and groups, then click button 'Create barplot!' to generate plot.")),
                                
                                
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
                     
                     # Message will display until create plot button is clicked.
                     div(id = "scatterplot_message",
                         p("Please select variables and category, then click button 'Create scatterplot!' to generate plot.")),
                     
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
                     
                     # Message will display until create plot button is clicked.
                     div(id = "boxplot_message",
                         p("Please select variables and category, then click button 'Create boxplot!' to generate plot.")),
                     
                     plotOutput(outputId = "boxplot")
            ),
            
            tabPanel("Violin Plot",
                     
                     br(),
                     
                     "This plot is a combination of a Violin Plot and a Dot Plot.",
                     
                     br(),
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
                     
                     # Message will display until create plot button is clicked.
                     div(id = "violinplot_message",
                         p("Please select variables and subcategory, then click button 'Create violin/dot plot!' to generate plot.")),
                     
                     plotOutput(outputId = "violin_plot")
            ),
            
            tabPanel("Density Plot",
                     
                     br(),
                     
                     # Allows user to select density plot options
                     fluidRow(
                       column(6,selectInput(inputId = "density_x",
                                            label = "Select variable:",
                                            choices = combo_num_vars,
                       )),
                       
                       column(6,selectInput(inputId = "density_fill",
                                            label = "Select group:",
                                            choices = clean_cat_vars[-5],
                       ))
                     ),
                     
                     actionButton("go_densityplot", "Create density plot!"),
                     
                     # Message will display until create plot button is clicked.
                     div(id = "density_message",
                         p("Please select variable and group, then click button 'Create density plot!' to generate plot.")),
                     
                     plotOutput(outputId = "density_plot")
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
                     
                     # Message will display until create plot button is clicked.
                     div(id = "combo_message",
                         p("Please select variables and group, then click button 'Create combo plot!' to generate plot.")),
                     
                     plotOutput(outputId = "combo_plot")
            ),
            
            tabPanel("Correlation",
                     
                     br(),
                     
                     # Allows user to select numeric variables for correlation matrix
                     fluidRow(
                       column(3,checkboxGroupInput(inputId = "corr_vars",
                                                   "Select at least two variables:",
                                                   choices = combo_num_vars),
                              actionButton("go_corrplot", "Create correlation matrix!")
                       ),
                       
                       column(9,plotOutput(outputId = "corr_plot"),
                              
                              # Message will display until create matrix button is clicked.
                              div(id = "corr_message",
                                  p("Please select variables, then click button 'Create correlation matrix!' to generate plot.")),
                              )
                       )
                     )

            )))
          )
           
        )
    
    )


#Read in data and set type for variables
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
  
  output$subsetted_data <- renderDataTable({
    
    # Message to display until subset button is clicked.
    validate(
      need(!is.null(subset_data$usage_data), 
           "Please select your variables and click the 'Subset the Data!' button.")
    )
    
    subset_data$usage_data
    })
  
  
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
    
    hide("barplot_message")  # This will hide the message after the button is clicked
    
    output$barplot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )
      
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
    
    hide("scatterplot_message")  # This will hide the message after the button is clicked
    
    output$scatterplot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )
      
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
    
    hide("boxplot_message")  # This will hide the message after the button is clicked
    
    output$boxplot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )
    
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
    
    hide("violinplot_message")  # This will hide the message after the button is clicked
    
    output$violin_plot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )

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

  # Render the density plot
  observeEvent(input$go_densityplot, {
    
    hide("density_message")  # This will hide the message after the button is clicked
    
    output$density_plot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )
      
      x <- isolate(input$density_x)
      fill <- isolate(input$density_fill)
      
      ggplot(data = subset_data$usage_data) +
        geom_density(aes(x = !!sym(x), fill = !!sym(fill)), 
                     alpha = 0.3) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        labs(dictionary = var_dictionary)
    })
  })
  
    
  # Render the combo plot
  observeEvent(input$go_comboplot, {
    
    hide("combo_message")  # This will hide the message after the button is clicked
    
    output$combo_plot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )
      
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
  
  # Render the correlation matrix
  observeEvent(input$go_corrplot, {
    
    hide("corr_message")  # This will hide the message after the button is clicked
    
    output$corr_plot <- renderPlot({
      
      # Message if subset button is not clicked.
      validate(
        need(!is.null(subset_data$usage_data), 
             "Please subset the data using the side panel and click the 'Subset the Data!' button.")
      )
      
      x <- isolate(input$corr_vars)
      
      # Users will get an error message if they don't select two
      if (length(x) <2) shinyalert(text = "Please select at least two variables!")
      
      df_corr <- subset_data$usage_data[,x]
      
      ggcorr(df_corr,
             nbreaks = 6,
             label = TRUE,
             palette = "BuPu",
             size = 3,
             label_size = 6,
             hjust = 0.50,
             angle = -15,
             layout.exp = 1)
    })
  })
  
  }
  
  



# Run the application 
shinyApp(ui = ui, server = server)


