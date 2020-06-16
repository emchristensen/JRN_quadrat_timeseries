library(shiny)
library(ggplot2)
library(dplyr)

# Data pre-processing ----
# dates = read.csv('data/quadrat_veg.csv',stringsAsFactors = F) %>%
#   dplyr::select(quadrat, year, month)

plantcover <- read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
# plantcovertot <- aggregate(plantcover$area, by=list(quadrat=plantcover$quadrat, 
#                                                           year=plantcover$year,
#                                                           month=plantcover$month,
#                                                           sp_code=plantcover$species_code),
#                            FUN=sum) 
#plantcoverdate = merge(plantcovertot, dates, by=c('quadrat','year','month'),all.y=T) 
plantcover$date = as.Date(paste(plantcover$year, plantcover$month, '01', sep='-'))


# Define UI  ----
ui <- fluidPage(
  
  # App title ----
  headerPanel(textOutput("caption")),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Select which quadrat to display data for
    selectInput("quadrat", "Quadrat:",
                sort(unique(plantcover$quadrat))),
    # Input: slider to select date range for y axis
    sliderInput("ylimits", "Y-axis limits:",
                min = 0, max = 1.2, 
                value=c(0,1.1), sep=''),
    # Input: slider to select date range for x axis
    sliderInput("daterange", "Date Range:",
                min = min(plantcover$year), max = max(plantcover$year), 
                value=c(min(plantcover$year),max(plantcover$year)), sep=''),
    # checkboxes for species
    uiOutput("speciesselection")
    ),
  
  
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Formatted text for caption ----
    h3("Perennial grass/shrub cover"),
    
    # Output: Plot of grass/shrub cover ----
    plotOutput("cover.ts")

    
  )
)

# Define server logic  ----
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression 
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    paste("Quadrat", input$quadrat)
  })
  
  # subset data based on user input
  selecteddatacover <- reactive({
    dplyr::filter(plantcover, quadrat==input$quadrat, !is.na(date)) %>%
      tidyr::complete(quadrat, date, fill=list(x=0)) #%>%
      #dplyr::filter(sp_code %in% input$selectedspecies, !is.na(sp_code))
  })

  # # render the checkboxes for species selection
  # output$speciesselection <- renderUI({
  #   splist <- dplyr::filter(plantcoverdate, quadrat==input$quadrat, !is.na(date)) %>%
  #     dplyr::pull(sp_code) %>%
  #     unique()
  #   checkboxGroupInput("selectedspecies", "Species:", choices=sort(splist), selected=sort(splist))
  # })


  #Generate a plot of grass/shrub cover ----
  output$cover.ts <- renderPlot({
    datemin=as.Date(paste0(input$daterange[1],'-01-01'))
    datemax=as.Date(paste0(input$daterange[2],'-12-31'))
    ggplot2::ggplot(data = selecteddatacover()) +
      geom_line(aes(x=date, y=total_shrub), color='steelblue', size=1.5) +
      geom_line(aes(x=date, y=total_grass), color='coral', size=1.5) +
      ylab('Cover (m^2)') +
      xlab("") +
      ylim(input$ylimits) +
      xlim(datemin,datemax) 

  })
}

shinyApp(ui = ui, server = server)