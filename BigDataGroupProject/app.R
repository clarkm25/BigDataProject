library(shiny)
library(bslib)

# About page text - We can use HTML :D
about_layout <- layout_columns(
  card(card_header(HTML("<h1><b>Exploring the Dataset</b></h1>")),
    HTML("
      As a group of gamers, we wanted to find a dataset related to gaming and 
      one from which we could make beneficial predictions. We found this dataset, 
      Video Game Sales, which contains 16,598 entries with 11 features, which 
      are listed below, as well as a 12th feature that we created called Region.
      We use these features to create a collection of decision trees, 
      classification trees, and regression trees. Each of the tabs, besides the 
      Visuals tab, contains a modifiable decision tree that can be used to 
      predict platforms, regions, and sales. We hope that companies and 
      individuals can find these tree models useful throughout the creation and 
      buying of video games.
      
      <b><font size=4>Features in the dataset: Video Game Sales</font></b>
      <ul>
        <li><b><u>Rank:</b></u> Ranking of all the video games based on sales.</li>
        <li><b><u>Name:</b></u> Name of the video game.</li>
        <li><b><u>Platform:</b></u> Main platform that the game was played on upon release.</li>
        <li><b><u>Year:</b></u> Year of the video game's release.</li>
        <li><b><u>Genre:</b></u> Main genre of the video game.</li>
        <li><b><u>Publisher:</b></u> Company or person that published the game.</li>
        <li><b><u>NA_Sales:</b></u> Sales in North America (by the millions)</li>
        <li><b><u>EU_Sales:</b></u> Sales in Europe (by the millions)</li>
        <li><b><u>JP_Sales:</b></u> Sales in Japan (by the millions)</li>
        <li><b><u>Other_Sales:</b></u> Sales in every other region besides 
            those listed above (by the millions)</li>
        <li><b><u>Global_Sales:</b></u> Total sales worldwide (by the millions)</li>
        <li><b><u>Region:</b></u> The region in which the game did the best in.</li>
        <br>
        <p>This dataset can be found on Kaggle at: 
        <i><u>https://www.kaggle.com/datasets/gregorut/videogamesales</i></u></p>
    ")
  )
)

visuals_layout <- layout_columns(
  card("Add Graphs Here")
)

# Creates the page for the platform classification tree
platform_model_layout <- page_fillable(
  layout_columns(
    card(card_header("Sliders"),
         sliderInput("control", 
                     "Enter Control:",
                     value = 0,
                     min = 0,
                     max = 0.01,
                     step = 0.0001
         ),
         numericInput("maximum_depth",
                   "Enter Maximum Depth:",
                   value = 4, 
                   min = 1,
                   max = 10,
                   step = 1
         ),
         actionButton("build_platform_tree", "Build Tree")
    ),
    layout_columns(
      card(
        card_header("Platform Classification Tree"),
        plotOutput("platform_tree_plot")
      ),
      card(
        card_header("Analysis"),
        textOutput("platform_tree_text")     
      ),
      col_widths = c(12,12),
      row_heights = c(9,3)
    ),
    card(card_header("Prediction"),
      # Selected Year gets sorted so it isn't out of order. The N/A is supposed to be there
      selectInput("pred_year", "Selected Year:", choices = sort(unique(vgsales$Year), decreasing = TRUE)),
      selectInput("pred_publisher", "Selected Publisher:", choices = unique(vgsales$Publisher)),
      selectInput("pred_region", "Selected Region:", choices = unique(vgsales$Region)),
      actionButton("pred_button", "Predict Platform"),
      textOutput("pred_result")
    ),
    col_widths = c(2,7,3)
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


ui <- page_fillable(
  navset_card_tab(
    nav_panel("About", about_layout),
    nav_menu(
      "Visuals - Graphs",
      nav_panel("Global Sales Based on Genre", "Graph Here"),
      nav_panel("Global Sales Based on Console", "Graph Here"),
      nav_panel("Global Sales Based on Publisher", "Graph Here"),
      nav_panel("Global Sales Per Genre Based on Publisher", "Graph Here"),
      nav_panel("Global Sales Based on Year", "Graph Here"),
      nav_panel("Sales Per Region Based on Genre", "Graph Here"),
      nav_panel("Sales Per Region Based on Top 10 Platforms of Each Region", "Graph Here")
    ),
    nav_panel("Platform Based on Year, Publisher, Region", platform_model_layout), 
    nav_panel("Region Based on Sales, Genre, Platform", region), 
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
  ########### START PLATFORM CLASSIFICATION TREE ###########
  platform_tree_model <- reactiveVal(NULL)
  
  observeEvent(input$build_platform_tree, {
    platform_tree <- build_platform_tree(input$control, input$maximum_depth)
    platform_tree_model(platform_tree)
  })
  
  output$platform_tree_text <- renderText({
    "This model creates a pruned classfication tree that predicts the platform that will
    be most likely required to play an original copy of a game given a year, publisher,
    and region. It can be used for people who are into collecting original copies of games
    or who want to play older games that have not been put on more recent platforms. 
    This will help the user know if they need to buy a new console when looking at
    games from a certain year and company. It can also be extremely useful for small companies
    that collect and sell old video games and consoles. They can use this model to predict
    what consoles or platforms they will need to buy when collecting games from a certain
    company."
  })
  
  output$platform_tree_plot <- renderPlot({
    req(platform_tree_model())
    rpart.plot(platform_tree_model(), type = 3, extra = 101, 
               fallen.leaves = TRUE, main = "Platform Based on Year, Publisher, & Region")
  })
  
  observeEvent(input$pred_button, {
    req(platform_tree_model())
    
    new_vgsales <- data.frame(
      Year = input$pred_year,
      Publisher = input$pred_publisher,
      Region = input$pred_region,
      stringsAsFactors = FALSE
    )
    
    platform_prediction <- predict(platform_tree_model(), new_vgsales, type = "class")
    output$pred_result <- renderText({
      paste("Predicted Platform:", platform_prediction)
    })
  })
  ########### END PLATFORM CLASSIFICATION TREE ###########

  ########### START REGION CLASSIFICATION TREE ###########
  tree_model <- reactiveVal(NULL)
  
  # Builds the tree using the chosen cp and max depth
  observeEvent(input$build_tree, {
    tree <- build_tree(input$cp, input$max_depth)
    tree_model(tree)
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
    req(tree_model())
    rpart.plot(tree_model(), type = 3, extra = 101, fallen.leaves = TRUE, main = "Region Given Sales & Platform")
  })
  
  # This makes a prediction for the tree that is created
  observeEvent(input$predict, {
    req(tree_model())
    
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
  ########### END REGION CLASSIFICATION TREE ###########
}

shinyApp(ui, server)