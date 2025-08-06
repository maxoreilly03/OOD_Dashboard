## Loading Packages In
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

## Loading in data from ArcGIS Online
statesST <- st_read("https://services3.arcgis.com/iuNbZYJOrAYBrPyC/ArcGIS/rest/services/States_forR/FeatureServer/0/query?where=1=1&outFields=*&f=geojson")
statesWikiTest <- read.csv("/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/us_states_wikipedia_links.csv")
statesMerged <- left_join(statesST, statesWikiTest, by = c("NAME" = "State"))
labels = sprintf("<strong>%s</strong><br/><a href='%s' target='_blank'>Link to Wikipedia Page</a>",
                 statesMerged$NAME, statesMerged$Wikipedia_Link) %>% lapply(htmltools::HTML)

## Loading in Image
image_path_1 <- "/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/Figures/Average\ Drive\ Time\ for\ Methadone\ Provider\ -\ 2020.png"
image_path_2 <- "/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/Figures/SAMSHAOTPs.png"



## THEME ##
my_theme <- bs_theme(
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish")
)


## UI START ##
ui <- page_navbar(
  theme = my_theme,
  title = "Opioid Overdose & Treatment Access Dashboard - TUSM",
  inverse = TRUE,
  nav_panel(
    title = "Splash Page",
    p("Welcome to the dashboard"),
    card(
      layout_sidebar(
        sidebar = sidebar(
                          helpText(
                              "Helper Text."
                          ),
      ),
      card(
        card_header("Information about the dashboard"),
        helpText(
          "The opioid overdose epidemic continues to be one of the largest American public health crises of the 21st century. Between 2002 and 2022, opioid-related overdose deaths nearly quadrupled; an estimated 81,083 opioid-related overdose deaths occurred in 2023 [1]. Between 6.7 and 7.6 million people lived with opioid use disorder (OUD) nationwide in 2019 [2]. However, a minority of those with OUD receive treatment, with estimates ranging from 8% [3] to 25% [4], a figure that is lower among uninsured and minority communities [5]. "
        ),
        helpText(
          "To treat OUD and mitigate the risk of opioid-related overdose deaths, medications for opioid use disorder such as methadone are first line treatment [7]. Methadone treatment is highly effective, decreasing opioid-related overdose deaths by 59% during the 12-month period following a nonfatal overdose [8]. Yet, methadone is accessible almost exclusively via opioid treatment programs (OTPs) [9] [10] [11]. Among myriad barriers to accessing methadone treatment, travel time can be considerable, as most patients must travel to an OTP in-person every day to receive treatment [12]. Indeed, patients who traveled less than a mile to an OTP were 50% more likely to continue treatment compared to those traveling longer distances [13]."
        ),
        helpText(
          "This dashboard combines data from various publicly available sources to show areas that are disproportionately impacted by the opioid crisis or have low or inequitable access to harm reduction and treatment services. Using tabs at the top, you can explore trends in fatal and non-fatal overdose, access to services, etc."
        ),
        ## scrolling info about different aspects of OOD crisis. 
      )
    )
    )
    ),
    
  nav_panel(
    title = "Graphs", 
    p("Graphs of OOD"),
    navset_card_tab(
      height = 450,
      nav_panel(
        "Graph",
        card_title("graph showing MOUD access"),
        card_image(
          file = image_path_1,
                   height = 400,
                   width = 500)
      ),
      nav_panel(
        "Map",
        card_title("Map showing MOUD access"),
        card_image(
          file = image_path_2,
          height = 400,
          width = 500),
      ),
      #card_header(
      #  "how do we make this look not ridiculous"
      #),
      
    )
    
  ),
  # nav_panel(
   # title = "Census Data Tutorial",
  #  card(
      #layout_sidebar(
        #sidebar = sidebar(
        #  helpText(
        #    "Create demographic maps with information from the 2010 US census."
        #  ),
          
          # selectInput(
          #  "var",
          #  "Choose a variable to display",
         #   choices = c("Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"),
        #    selected = "Percent White"
          #),
          
          # sliderInput(
            #"vals",
            #"Range of Interest",
           # min = 0,
          #  max = 100,
         #   value = c(0,100)
        #  ),
       # ),
      #  card(
     #     plotOutput("map")
    #    )
   #   )
  #  )
 # ),
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