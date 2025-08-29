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
library(shinyBS)



Wonder_Full <- readRDS("Wonder_Full.rds")
States <- readRDS("States_noAK_noHI.rds")

## source("Data.R")

## Loading in Images
image_path_1 <- "/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/Figures/Average\ Drive\ Time\ for\ Methadone\ Provider\ -\ 2020.png"
image_path_2 <- "/Users/maxoreilly/Desktop/opioidDataDashboard/myApp/OODAttempt_oneFile/App-1/Figures/SAMSHAOTPs.png"



## THEME ##
my_theme <- bs_theme(
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish")
)

## INTERACTIVITY ##
if (interactive()) {
  shinyApp(

##################
##################
#### UI START ####
##################
##################
ui <- page_navbar(
  theme = my_theme,
  title = "Opioid Overdose & Treatment Access Dashboard - TUSM",
  inverse = TRUE,
  #nav_panel(
   # title = "I'm still trying",
  #  navset_underline(
  #    nav_panel(
  #      title = "tyler",
  #    tags$img(
  #      src = "https://www.donttaptheglass.com/images/og-image.png",
  #    )
       # ),
    #  nav_panel(
   #     title = "button",
  #      actionButton(
 #         "trigger", "Big Red Button"
#        )
#      ),
#    ),
 # ),
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
      ),
      card(
          title = "button",
          actionButton(
            "trigger", "Big Red Button"
          ),
        
      )
    )
    )
 ),

  nav_panel(
    ## make this a sidebar layout
    title = "Overdoses", 
    p("Overdoses"),
    card(
      layout_sidebar(
      sidebar = sidebar(
       card_header("Which Data?"),
       selectInput(
         "ODorCount",
         "Select Overdose Count or Rate",
         choices = c("Overdose Rate", "Overdose Count"),
         selected = "Overdose Rate"
       ),
       selectInput(
         "year",
         "Select Option",
         choices=c("2018", "2019", "2020",
                   "2021", "2022"),
         selected = "2022",
      )
      ),
      leafletOutput("leaflet_map")
      ),
    ),
  ),

 nav_panel(
   title = "Treatment Services", 
   p("Access to Treatment Services for Opioid Use Disorder"),
   navset_card_tab(
     height = 450,
     nav_panel(
       "Graphs",
       card_title("graph showing MOUD access"),
       ## can the image have a popup with more info?
       card_image(
         file = image_path_1,
         height = 400,
         width = 500)
     ),
     nav_panel(
       "Maps",
       card_title("Map showing MOUD access"),
       ## can the map have a popup with more info?
       card_image(
         file = image_path_2,
         height = 400,
         width = 500),
     )
   )
 ),
 
  nav_panel(
    title = "Sources", 
    p("Thanks Tom and rstudio documentation!"),
    p("Counties with 10 or fewer overdose deaths are suppresed to protect decedent & family anonymity")
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  ),
),


##################
##################
## SERVER START ##
##################
##################
server <- function(input, output, session) {
  map_data <- reactive({
    req(input$year, input$ODorCount)
    if (input$ODorCount == "Overdose Rate") {
      col = paste0("Crude_Rate_", input$year)
    }
    else {
      col = paste0("Deaths_", input$year)
    }
    Wonder_Full %>% 
      select(NAME, all_of(col))
    }
  )
  
  legend_title <- reactive({
    req(input$year, input$ODorCount)
    year <- input$year
    whichValue <- input$ODorCount
    paste0(whichValue, ", ", year)
  })
  
  reactive_maximum <- reactive({
    values <- as.numeric(map_data()[[2]])
    maximum = max(values, na.rm = TRUE)
    minimum = min(values, na.rm = TRUE)
    cat("mininum in reactive fxn is: ", minimum, "\n")
    cat("maximum in reactive fxn is: ", maximum, "\n")
    max(values, na.rm = TRUE)
  })
  
  reactive_pal <- reactive({
    maximum = reactive_maximum()
    cat("passed maximum is: ", maximum, "\n")
    values <- as.numeric(map_data()[[2]])
    vals <- map_data()[[2]]
    range = range(vals, na.rm = TRUE)
    cat("range is: ", range, "\n")
    colorBin(
      "Oranges", 
      domain = values, 
      bins = 5,
      pretty = FALSE,
      na.color = "white")
  })
  
  popups_2 <- reactive({
    vals <- map_data()[[2]]
    name <- map_data()[[1]]
    labels <- paste0(
      name, "<br/>", vals
    )
    lapply(labels, htmltools::HTML)
  })
  
  
  output$leaflet_map <- renderLeaflet({
    pal <- reactive_pal()
    legend_title <- legend_title()
    maximum <- reactive_maximum()
    valuesForLegend = map_data()[[2]]
    str(valuesForLegend)
    class(legend_title)
      leaflet(map_data()) %>%
      setView(lng = -98.274923, lat =  38.904775, zoom = 4) %>% 
      addPolygons(data = States,
                  color = "black",
                  weight = 2,
                  opacity = 0.9) %>%
      addPolygons(fillColor = ~pal(map_data()[[2]]), 
                    weight = 1,
                    color = "grey",
                    highlightOptions = highlightOptions(weight = 5, "black"),
                    popup = popups_2(),
                    fillOpacity = 0.8) %>%
      addLegend(
        pal = reactive_pal(),
        values = valuesForLegend,
        #str(valuesForLegend),
        #cat("values for legend in leaflet: ", valuesForLegend),
        position = "bottomright",
        title = legend_title,
        na.label = "Insufficient Data - see 'Sources' Tab"
      )
  })
  
  toggleModal(session, "BS", toggle = "toggle")
  
  observeEvent(input$trigger, {
    showModal(modalDialog(
      title = "Important",
      size = "xl",
      easyClose = TRUE,
      card(
            card_header("a reminder"),
            card_image(
              file = "https://www.donttaptheglass.com/images/og-image.png",
              # can't figure out how to put my own image here
           )
        )
    ))
  })
  
  observeEvent(input$popup, {
    showModal(modalDialog(
      title = "Graph!",
      "GRAPH GRAPH GRAPH"
    ))
  })

}
)
}
shinyApp(ui, server)



#    card(
#      height = 800,
#     leaflet(Wonder_21_Raw) %>% setView(lng = -98.274923, lat =  38.904775, zoom = 4) 
#    %>%
#       addPolygons(fillColor = ~bin_pal_CR(Crude_Rate), 
#                   weight = 1,
#                   color = "grey",
#                  highlightOptions = highlightOptions(weight = 5, "black"),
#                  popup = labels,
#                 fillOpacity = 0.8) 
#  %>%
#     addLegend(data = States, pal = bin_pal, title = "Legend", values = land, position = 'bottomright'
#     )
# )


  
  #nav_panel(
  # title = "bitchass",
  #p("fucking labels"),
  #card(
  #  height = 800,
  #  leaflet(Wonder_21_Raw) %>% setView(lng = -98.274923, lat =  38.904775, zoom = 4)  %>%
  #    addPolygons(data = States,
  #                color = "black",
  #               weight = 2,
  #              opacity = 0.8) %>%
  # addPolygons(fillColor = ~bin_pal_Deaths(Deaths), 
  #            weight = 1,
  #           color = "grey",
  #          highlightOptions = highlightOptions(weight = 5, "black"),
  #         popup = labels,
  #        fillOpacity = 0.8) 
  #)
  #),
  



## LINKS TO DOCUMENTATION ## 
# https://shiny.posit.co/r/reference/shiny/1.8.0/modaldialog.html

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

#coutput$map <- renderPlot({
  #data <- switch(input$var,
  #              "Percent White" = counties$white,
  #               "Percent Black" = counties$black,
   #              "Percent Hispanic" = counties$hispanic,
  #               "Percent Asian" = counties$asian)
 # 
#  color <- switch(input$var,
  #                "Percent White" = "darkgreen",
 #                 "Percent Black" = "black",
 #                 "Percent Hispanic" = "darkorange",
#                "Percent Asian" = "darkviolet")
 # 
#  legend <- switch(input$var,
  #                 "Percent White" = "% White",
  #                 "Percent Black" = "% Black",
  #                 "Percent Hispanic" = "% Hispanic",
  #                 "Percent Asian" = "% Asian")
  #
 # percent_map(data, color, legend, input$vals[1], input$vals[2])
#})
