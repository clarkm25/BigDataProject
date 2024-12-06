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

# Creates the complete page
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
    nav_panel("Region Based on Sales, Genre, Platform", "Classification Tree"), 
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
}

shinyApp(ui, server)
