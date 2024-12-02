library(shiny)
library(bslib)

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
    nav_panel("Genre Based on Sales", model1_layout), 
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
}

shinyApp(ui, server)
