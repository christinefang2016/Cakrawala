#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinyWidgets)

# upload raw data of indonesia export countries
export_partners <- read.csv("data/Export_by_country.csv")
#export_partners<- mutate(export_partners, log_export = log10(export_partners$Export))

#sort the country ascendingly based on export value based on year
#sorted_export_partners <- export_partners[order(-export_partners$Export),]
filter_export_partners <- filter(export_partners, Year == 2018)
sorted_export_partners <- filter_export_partners[order(-filter_export_partners$Export),]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Indonesia Top Export Partners"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "order",
                        label = "Top K",
                        choices = c("All" = "All",
                                    "5" = "5",
                                    "10" = "10",
                                    "15" = "15",
                                    "20" = "20"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("LineExport")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$LineExport <- renderPlotly({
        inputOrder <- ifelse(input$order == "All", nrow(export_partners), input$order)
        #final_data <- sorted_export_partners[c(1:input$order), c(1:3)]
        final_data <- sorted_export_partners[c(1:inputOrder), c(1:3)]
        subset_data <- subset(export_partners, export_partners$Destination %in% final_data$Destination)
        
        plot_ly(source = "source") %>%
            add_lines(data = subset_data, x = ~Year, y = ~Export, color = ~Destination, mode= 'lines')%>%
            
            
            add_markers(x=~Year, y=~Export,color = ~Destination,
                        hoverinfo = 'text',
                        text = ~paste('<br> Country: ', Destination,
                                      '<br> Year: ', Year,
                                      '<br> Export Value: $ ', Export),
                        showlegend = FALSE
                        ) 
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
