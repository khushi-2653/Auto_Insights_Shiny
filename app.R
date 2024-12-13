library(shiny)
library(shinythemes)
library(dplyr)
library(imager)
library(ggplot2)
library(tidyr)
library(GGally)
library(ggcorrplot)
load("Cars_Data.RData")
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  
  # Reactive value to track if the app has started
  appStarted <- reactiveVal(FALSE)
  
  # Landing Page UI
  output$mainUI <- renderUI({
    if (!appStarted()) {
      # Landing page with image and action button
      fluidPage(
        img(src = "https://png.pngtree.com/background/20230411/original/pngtree-sports-car-wild-light-effect-creative-background-picture-image_2387864.jpg", height = "100%", width = "100%"), # Adjust path and height
        actionButton("startApp", "Let's go", class = "btn-primary")
      )
    } else {
      # Main App UI with navbarPage
      navbarPage(
        theme = shinytheme("cyborg"),
        title = "Car Data Insights",
        
        # Home Page
        tabPanel("Home",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("priceRange", "Select Price Range:", 
                                 min = min(data$price_asnum, na.rm = TRUE), 
                                 max = max(data$price_asnum, na.rm = TRUE), 
                                 value = c(min(data$price_asnum, na.rm = TRUE), 
                                           max(data$price_asnum, na.rm = TRUE))),
                     
                     # CheckboxGroupInput for Origin Country
                     checkboxGroupInput("origin", "Select Origin Country:", 
                                        choices = unique(data$origin[!is.na(data$origin)]),
                                        selected = unique(data$origin[!is.na(data$origin)])),
                     
                     # CheckboxGroupInput for Car Type
                     checkboxGroupInput("type", "Select Car Type:", 
                                        choices = unique(data$car_type), 
                                        selected = unique(data$car_type)),
                     # CheckboxGroupInput for Brand
                     checkboxGroupInput("brands", "Manufactured/Tuned By:", 
                                        choices = unique(data$brand), 
                                        selected = unique(data$brand)),
                   ),
                   mainPanel(
                     h3("Data Summary"),
                     # Summary statistics and sample cars displayed here
                     htmlOutput("summaryTextHome"),
                     h3("Filtered Data"),
                     DT::dataTableOutput("filteredDataTable")
                   )
                 )
        ),
        #Individual Specifics
        tabPanel("Individual Specifics",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("car", "Select Car for Specifications:", choices = data$names)
                   ),
                   mainPanel(
                     h3("Specifications of a Particular Car"),
                     uiOutput("carImage")
                   )
                 )
        ),
        
        # Car Comparison Page
        tabPanel("Car Comparison",
                 sidebarLayout(
                   sidebarPanel(
                     # Widgets to select cars for comparison
                     selectInput("compareCars", "Select Cars for Comparison:", choices = data$names, multiple = TRUE),
                     checkboxGroupInput("attributes", "Attributes to Compare:",
                                        choices = c("Price" = "price_asnum", "Weight" = "weight", "Top Speed" = "top_speed", "Acceleration" = "max_acc"))
                   ),
                   mainPanel(
                     h3("Car Comparison"),
                     # Comparison charts or tables displayed here
                     plotOutput("comparisonPlot", width = "100%", height = "800px")
                   )
                 )
        ),
        
        # Visualization Insights Page
        tabPanel("Visualization Insights",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("xvar", "Select X-axis Variable:", choices = c("Price" = "price_asnum", "Weight" = "weight", "Top Speed" = "top_speed", "Acceleration" = "max_acc", "0 to 60 mph" = "zero_to_60", "0 to 100 kph" = "zero_to_100")),
                     selectInput("yvar", "Select Y-axis Variable:", choices = c("Price" = "price_asnum", "Weight" = "weight", "Top Speed" = "top_speed", "Acceleration" = "max_acc", "0 to 60 mph" = "zero_to_60", "0 to 100 kph" = "zero_to_100")),
                     selectInput("cat", "Categorize By:", choices = c("Brand" = "brand", "Car Type" = "car_type", "Country of Origin" = "origin")),
                     selectInput("plotType", "Select Plot Type:",
                                 choices = c("Scatter Plot", "Correlation Heatmap", "Box Plot", "Density Plot")),
                     conditionalPanel(
                       condition = "input.plotType == 'Box Plot'",
                       helpText("Note: For Box Plot, only the Y-axis variable and the categorization variable are relevant.")
                     ),
                     conditionalPanel(
                       condition = "input.plotType == 'Density Plot'",
                       helpText("Note: For Density Plot, only the X-axis variable and the categorization variable are relevant.")
                     )
                   ),
                   mainPanel(
                     plotOutput("insightPlot", width = "100%", height = "1000px")
                   )
                 )
        )
      )
    }
  })
  
  # Observe button click to start the app
  observeEvent(input$startApp, {
    appStarted(TRUE)
  })
  
  # HOME PAGE LOGIC
  filtered_data <- reactive({
    data %>%
      filter(price_asnum >= input$priceRange[1], 
             price_asnum <= input$priceRange[2],
             origin %in% input$origin,
             car_type %in% input$type,
             brand %in% input$brands)
  })
  
  output$summaryTextHome <- renderText({
    # Calculate statistics based on filtered data
    num_cars <- nrow(filtered_data())
    avg_price <- mean(filtered_data()$price_asnum, na.rm = TRUE)
    origin_count <- table(filtered_data()$origin)
    car_type_count <- table(filtered_data()$car_type)
    brand_count <- table(filtered_data()$brand)
    
    summary_text <- paste(
      "Number of cars:", num_cars, "<br>",
      "Average price (lower bound):", round(avg_price, 2), "€<br>",
      "Origin distribution:<br>",
      paste(names(origin_count), origin_count, sep = ": ", collapse = ", "), "<br>",
      "Car type distribution:<br>",
      paste(names(car_type_count), car_type_count, sep = ": ", collapse = ", "), "<br>",
      "Brand distribution:<br>",
      paste(names(brand_count), brand_count, sep = ": ", collapse = ", ")
    )
    
    # Use HTML() to render as HTML in the UI
    HTML(summary_text)
  })
  
  output$filteredDataTable <- DT::renderDataTable({
    filtered_data() %>% select(-img)
  })
  
  #INDIVIDUAL SPECIFICS
  output$carImage <- renderUI({
    selected_car <- data[data$names == input$car, ]
    #tags$img(src = selected_car$img, width = "400px", height = "auto")
    specs <- list(
      tags$img(src = selected_car$img, width = "600px", height = "500px"),
      tags$p(if (!is.null(selected_car$price_asnum)) paste("Price:", selected_car$price)),
      tags$p(if (!is.null(selected_car$weight)) paste("Weight:", selected_car$weight, " kg")),
      tags$p(if (!is.null(selected_car$top_speed)) paste("Top Speed:", selected_car$top_speed, " kph")),
      tags$p(if (!is.null(selected_car$max_acc)) paste("Acceleration:", selected_car$max_acc)),
      tags$p(if (!is.null(selected_car$car_type)) paste("Car Type:", selected_car$car_type)),
      tags$p(if (!is.null(selected_car$origin)) paste("Origin Country:", selected_car$origin)),
      tags$p(if (!is.null(selected_car$brand)) paste("Manufactured By:", selected_car$brand)),
      tags$p(if (!is.null(selected_car$zero_to_60)) paste("Time taken to go from 0 to 60 mph:", selected_car$zero_to_60, " s")),
      tags$p(if (!is.null(selected_car$zero_to_100)) paste("Time taken to go from 0 to 100 kph:", selected_car$zero_to_100, " s"))
    )
    do.call(tagList, specs)
  })
  
  # CAR COMPARISON PAGE LOGIC
  output$comparisonPlot <- renderPlot({
    if (length(input$compareCars) == 0 | length(input$attributes) == 0) {
      return(NULL)  # If nothing is selected, don't plot anything
    }
    
    # Filter the data for the selected cars
    selectedCars <- data[data$names %in% input$compareCars, ]
    
    # Reshape the data for plotting
    selectedCars_long <- selectedCars %>%
      select(names, all_of(input$attributes)) %>%
      pivot_longer(cols = -names, names_to = "Attribute", values_to = "Value")
    
    # Create the comparison plot
    ggplot(selectedCars_long, aes(x = names, y = Value, fill = Attribute)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Attribute, scales = "free_y", ncol = 2) +
      labs(title = "Car Comparison", y = "Value", x = "Car") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # VISUALIZATION INSIGHTS LOGIC
  output$insightPlot <- renderPlot({
    req(input$xvar, input$yvar)  # Ensure inputs are selected
    
    if (input$plotType == "Scatter Plot") {
      # Dynamic scatter plot with selected x and y variables
      ggplot(data, aes_string(x = input$xvar, y = input$yvar, color = input$cat)) +
        geom_point(size = 4) +
        labs(title = "Dynamic Scatter Plot", x = input$xvar, y = input$yvar) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(angle = 45, hjust = 1))
      
    } 
    else if (input$plotType == "Correlation Heatmap") {
      # Correlation heatmap of selected numeric variables
      data_numeric <- data %>% select_if(is.numeric)
      corr_matrix <- cor(data_numeric, use = "complete.obs")
      ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3) +
        labs(title = "Correlation Matrix Heatmap")
      
    } 
    else if (input$plotType == "Box Plot") {
      # Box plot for selected group variable (by car_type or origin)
      ggplot(data, aes_string(x = input$groupVar, y = input$yvar, fill = input$cat)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", input$yvar, "by", input$cat),
             x = input$groupVar, y = input$yvar) +
        theme_minimal()
      
    } else if (input$plotType == "Density Plot") {
      # Density plot of top speed or other numeric variable by selected fill variable
      ggplot(data, aes_string(x = input$xvar, fill = input$cat)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Density Plot of", input$xvar, "by", input$cat),
             x = input$xvar, y = "Density") +
        theme_minimal()
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
