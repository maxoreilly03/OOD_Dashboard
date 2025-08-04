library(shiny)
library(bslib)
library(maps)
library(mapproj)
library(sf)
library(leaflet)
library(ggplot2)
library(arcgis)
library(dplyr)
source("helpers.R")
counties <- readRDS("data/counties.rds")


statesST <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/States_forR/FeatureServer/0/query?where=1=1&outFields=*&f=geojson")
statesMerged <- left_join(statesST, statesWikiTest, by = c("NAME" = "State"))
labels = sprintf("<strong>%s</strong><br/><a href='%s' target='_blank'>Link to Wikipedia Page</a>",
                 statesMerged$NAME, statesMerged$Wikipedia_Link) %>% lapply(htmltools::HTML)

statesWikiTest <- read.csv("/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/us_states_wikipedia_links.csv")



## THEME ##
my_theme <- bs_theme(
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish")
)


## UI START ##
ui <- page_navbar(
  theme = my_theme,
  title = "This is the first website I've ever made!",
  # sidebar = sidebar("Sidebar"),
  inverse = TRUE,
  nav_panel(
    title = "Splash Page",
    p("Here's where you'd find information on how to use the dashboard!"),
    card(
      layout_sidebar(
        sidebar = sidebar(
                          helpText(
                              "Create demographic maps with information from the 2010 US census."
                          ),
                          
                          selectInput(
                            "var",
                            "Choose a variable to display",
                            choices = c("Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"),
                            selected = "Percent White"
                          ),
                          
                          sliderInput(
                            "vals",
                            "Range of Interest",
                            min = 0,
                            max = 100,
                            value = c(0,100)
                          ),
      ),
      card(
        plotOutput("map")
      )
    )
    )
    ),
    
  nav_panel(
    title = "Graphs", 
    p("Graphs of OOD"),
    
  ),
  nav_panel(
    title = "Maps", 
    p("Maps of OOD"),
    card(
      card_header("Which State?"),
      selectInput(
        "state",
        "Select Option",
        choices=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
        ),
        selected = "Alabama",
      )
    ),
    card(
      leaflet(statesST) %>% addTiles()%>% 
        addPolygons(color = "gray", 
                    weight = 1,
                    highlightOptions = highlightOptions(weight = 5, "black"),
                    popup = labels)
    )
    
  ),
  nav_panel(
    title = "Sources and Acknowledgements", 
    p("Thanks Tom and rstudio documentation!"),
    #card(
        card_header("a reminder"),
        # card_image(
        #  file = "https://www.donttaptheglass.com/images/og-image.png",
      # can't figure out how to put my own image here
       #)
      #),
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  ),
)



## SERVER START ##
server <- function(input, output) {
  # pseudocode re: how to query by clicking // selecting a state
  # 2 options: 
  # 1: user selects state in the dropdown
  # 2: user clicks on state
  # 1: server fxn - output$popup is a card that contains the info they're looking for
    # zooms to state
  # 2: the popup thing contains some sort of reference to another table that has links to 
  #.   the info they're looking for
  # labels = sprintf("<strong>Label</strong><br/>%s",
    #.   statesST$NAME, "<br/>%s", tabelwithdata$LINK where states$name = table$name) %>% lapply(htmltools::HTML)
  

  
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var,
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var,
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend, input$vals[1], input$vals[2])
  })
}

shinyApp(ui, server)