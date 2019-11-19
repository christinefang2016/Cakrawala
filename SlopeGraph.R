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

#upload data
country_lists <- read.csv("data/SlopeGraphExportPartners.csv")
#rename column year from X2000 to 2000 and so on
colnames(country_lists) <- c("Destination", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")


ui <- fluidPage(
    titlePanel("Export Partners"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Year1",
                        label = "From:",
                        choices = c("2000" = "2000",
                                    "2001" = "2001",
                                    "2002" = "2002",
                                    "2003" = "2003",
                                    "2004" = "2004",
                                    "2005" = "2005",
                                    "2006" = "2006",
                                    "2007" = "2007",
                                    "2008" = "2008",
                                    "2009" = "2009",
                                    "2010" = "2010",
                                    "2011" = "2011",
                                    "2012" = "2012",
                                    "2013" = "2013",
                                    "2014" = "2014",
                                    "2015" = "2015",
                                    "2016" = "2016",
                                    "2017" = "2017",
                                    "2018" = "2018"
                                    ),
                        selected = "2000"),
            selectInput(inputId = "Year2",
                        label = "To:",
                        choices = c("2000" = "2000",
                                    "2001" = "2001",
                                    "2002" = "2002",
                                    "2003" = "2003",
                                    "2004" = "2004",
                                    "2005" = "2005",
                                    "2006" = "2006",
                                    "2007" = "2007",
                                    "2008" = "2008",
                                    "2009" = "2009",
                                    "2010" = "2010",
                                    "2011" = "2011",
                                    "2012" = "2012",
                                    "2013" = "2013",
                                    "2014" = "2014",
                                    "2015" = "2015",
                                    "2016" = "2016",
                                    "2017" = "2017",
                                    "2018" = "2018"
                        ),
                        selected = "2018")
        ),
        mainPanel(
            plotlyOutput("slopegraph")
        )
    )
)



server <- function(input, output) {
    
    output$slopegraph <- renderPlotly({
        # retrive the export for the Year1 filter
        Year_1_filter <- round(country_lists[, names(country_lists) == input$Year1])
        # attach the country name to the export value
        first_label <- paste(country_lists$Destination, Year_1_filter,sep=", ")

        # retrieve the export for the Year2 filter
        Year_2_filter <- round(country_lists[, names(country_lists) == input$Year2])
        #attach the country name to the export value 
        second_label <- paste(country_lists$Destination, Year_2_filter, sep=", ")
        
        # if second year export - first year export is less than 0, it will be in color red, otherwise it will be green
        country_lists$class <- ifelse(Year_2_filter - Year_1_filter < 0, "red", "green")
        
        slope_graph <- ggplot(country_lists) + geom_segment(aes(x=1, xend=2, y=Year_1_filter, yend=Year_2_filter, col=class), size=.75, show.legend=F) + 
            geom_vline(xintercept=1, linetype="dashed", size=.1) + 
            geom_vline(xintercept=2, linetype="dashed", size=.1) +
            scale_color_manual(labels = c("Up", "Down"), 
                               values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
            labs(x="", y="Export Value")   # Axis labels
            #xlim(.5, 2.5) + ylim(0,(2.5*(Year_2_filter - Year_1_filter)))  # X and Y axis limits
        
        slope_graph <- slope_graph + geom_text(label=first_label, y=Year_1_filter, x=rep(1, NROW(country_lists)), hjust=1.1, size=3.5)
        slope_graph <- slope_graph + geom_text(label=second_label, y=Year_2_filter, x=rep(2, NROW(country_lists)), hjust=-0.1, size=3.5)
        slope_graph <- slope_graph + geom_text(label="From", x=1, y=1.1*(1.1*(Year_2_filter - Year_1_filter)), hjust=1.2, size=5)  # title
        slope_graph <- slope_graph + geom_text(label="To", x=2, y=1.1*(1.1*(Year_2_filter - Year_1_filter)), hjust=-0.1, size=5)  # title
        
        slope_graph

    })
}
shinyApp (ui=ui, server=server)