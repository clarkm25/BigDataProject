# app.R
# Fall 2024
# This is the main script for the shiny app. This contains the code for our
# dashboard and controls the creation of the models and predictions.
# 
# Authors: Tyler, Max, Gavin

library(shiny)
library(bslib)
library(dplyr)
library(rpart)
library(rpart.plot)

about_layout <- layout_columns(
  card("About Information")
)

visuals_layout <- layout_columns(
  card("Add Graphs Here")
)

model1_layout <- page_fillable(
  layout_columns(
    card(card_header("Sliders")),
    layout_columns(
      card(card_header("Classification Tree")),
      card(card_header("Description")),
      col_widths = c(12,12),
      row_heights = c(9,3)
    ),
    col_widths = c(3,9)
  )
)

# Page for model that predicts a region
region <- page_fillable(
  layout_columns(
    card(
      card_header("Sliders"),
      layout_columns(
        card(
          # This contains the sliders for building the tree
          card_header("Tree Parameters"),
          numericInput("cp", "Complexity Parameter (cp):", value = 0.0001, min = 0, max = 1, step = 0.001),
          numericInput("max_depth", "Maximum Depth:", value = 5, min = 1, max = 30, step = 1),
          actionButton("build_tree", "Build Tree")
        ),
        card(
          # This contains the input options to make a prediction
          card_header("Prediction Input"),
          selectInput(
            "platform", "Platform:",
            choices = unique(game_data_clean$Platform),
            selected = unique(game_data_clean$Platform)[1]
          ),
          numericInput("na_sales", "NA Sales:", value = 0, min = 0),
          numericInput("eu_sales", "EU Sales:", value = 0, min = 0),
          numericInput("jp_sales", "JP Sales:", value = 0, min = 0),
          numericInput("other_sales", "Other Sales:", value = 0, min = 0),
          actionButton("predict", "Predict Region")
        ),
        col_widths = c(6, 6)
      ),
      card(
        # For outputting the prediction
        card_header("Prediction Result"),
        textOutput("prediction_result")
      )
    ),
    card(
      layout_columns(
        card(
          # For outputting the tree model
          card_header("Classification Tree"),
          plotOutput("tree_plot")
        ),
        card(
          # Shows the description of the model
          card_header("Description"),
          textOutput("description_text") # Add content here as needed
        ),
        col_widths = c(12) 
      )
    ),
    col_widths = c(3, 9)
  )
)

# Main UI page with tabs for the models, graphs, and description
ui <- page_fillable(
  navset_card_tab( 
    nav_panel("About", about_layout),
    nav_menu(
      "Visuals - Graphs",
      nav_panel("Global Sales Based on Genre", 
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src="global_sales_by_genre.png")
                )
              ),
      nav_panel("Global Sales Based on Console", 
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src="global_sales_by_console_1.png"),
                  tags$img(src="global_sales_by_console_2.png"),
                  tags$img(src="global_sales_by_console_3.png"),
                  tags$img(src="global_sales_by_console_4.png")
                )
              ),
      nav_panel("Global Sales Based on Publisher", 
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src="global_sales_by_publisher.png"),
                  tags$img(src="global_sales_by_publisher_top_10.png")
                )
              ),
      nav_panel("Global Sales Per Genre Based on Publisher", 
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src="global_sales_per_genre_by_publisher.png")
                )
              ),
      nav_panel("Global Sales Based on Year", 
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src = "global_sales_based_on_year.png", style = "width: 800px; height: auto;")
                )
              ),
      nav_panel("Sales Per Region Based on Genre",
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src="sales_per_region_based_on_genre_1.png"),
                  tags$img(src="sales_per_region_based_on_genre_2.png"),
                  tags$img(src="sales_per_region_based_on_genre_3.png"),
                  tags$img(src="sales_per_region_based_on_genre_4.png")
                )),
      nav_panel("Sales Per Region Based on Top 10 Platforms of Each Region", 
                div(
                  style = "display: flex; gap: 10px;",
                  tags$img(src="sales_per_region_based_on_top_10_platforms_of_each_region.png")
                )
              )
    ),
    nav_panel("Genre Based on Sales", model1_layout), 
    nav_panel("Region Based on Sales & Platform", region), 
    nav_menu( 
      "Sales Based on Platform and Genre", 
      nav_panel("NA Sales", "NA_Sales Regression Tree"),
      nav_panel("EU Sales", "EU_Sales Regression Tree"),
      nav_panel("JP Sales", "JP_Sales Regression Tree"),
      nav_panel("Other Sales", "Other_Sales Regression Tree")
    )
    
  ), 
  id = "tab" 
)


server <- function(input, output, session) {
  # Load the pre-processed data and tree for predicting region
  load("tree_model.RData")  # Load the saved model (assuming tree_model is the saved object)
  
  # Create the model for the region prediction and initialize as null
  tree_model <- reactiveVal(NULL)  
  
  # Builds the tree using the chosen cp and max depth
  observeEvent(input$build_tree, {
    tree <- build_tree(input$cp, input$max_depth)
    tree_model(tree)  # Update the reactive value with the new tree
  })
  
  # Shows the description for the region tree model
  output$description_text <- renderText({
    "This model is creating a pruned classification tree that predicts a region will yield the highest sales
    given regional sales in millions and a platform. This model is useful for companies in the game industry. One use
    would be for targeted marketing in which companies could prioritize resources to gain more money for a predicted region
    that would yield the highest sales. Another is for inventory management in which companies
    can minimize the amount of overstock or shortages for a new release. This model can also be useful for localization
    and partnerships to be able to make games for a specific language and also partner with distributors and retailers
    to gain more profit."
  })
  
  # Gets the tree model and plots it on the right panel
  output$tree_plot <- renderPlot({
    req(tree_model())  # Ensure tree_model() is available before rendering the plot
    rpart.plot(tree_model(), type = 3, extra = 101, fallen.leaves = TRUE, main = "Region Given Sales & Platform")
  })
  
  # This makes a prediction for the tree that is created
  observeEvent(input$predict, {
    req(tree_model())  # Ensure tree_model() is available
    
    # Checks for valid input else give error
    if (input$platform == "" || !input$platform %in% game_data_clean$Platform) {
      output$prediction_result <- renderText("Error: Please select a valid Platform.")
      return()
    }
    
    new_data <- data.frame(
      Platform = input$platform,
      NA_Sales = max(0, input$na_sales),
      EU_Sales = max(0, input$eu_sales),
      JP_Sales = max(0, input$jp_sales),
      Other_Sales = max(0, input$other_sales),
      stringsAsFactors = FALSE
    )
    
    prediction <- predict(tree_model(), new_data, type = "class")
    output$prediction_result <- renderText({
      paste("Predicted Region:", prediction)
    })
  })
}

shinyApp(ui, server)
