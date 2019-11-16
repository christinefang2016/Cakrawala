packages = c('tinytex','plotly', 'RColorBrewer','classInt','ggthemes',
             'tidyverse', 'pivottabler', 'dplyr','shiny','shinythemes', 'lubridate')
for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}
data <- read.csv("data/ExportImportByCountries.csv")
tempdata = mutate(data, Importpercentile = ntile(data$Import.Value,100))
finaldata = mutate(tempdata, Exportpercentile = ntile(tempdata$Export.Value,100))
finaldata$Tradebalance <- finaldata$Export.Value - finaldata$Import.Value
#cleandata <- data %>%
   # group_by(Year = data$Year) %>%
    #summarise(TotalImport = sum(Import.Value), TotalExport = sum(Export.Value))

#finaldata <- cbind(data, TotalImportValue = cleandata$TotalImport,
                    #TotalExportValue = cleandata$TotalExport)

#finaldata$percentImport <- (finaldata$Import.Value / finaldata$TotalImportValue)* 100
#finaldata$percentExport <- (finaldata$Export.Value / finaldata$TotalExportValue)* 100

ui <- fluidPage(

    titlePanel("Export Import by Countries"),
    fluidRow(
        sliderInput(
            inputId = "FilterYear",
            label = "Year",
            min = 2002,
            max = 2018,
            value = 2002,
            sep = "",
            animate = animationOptions(loop = TRUE)),
            #choices = unique(finaldata$Year),
            #selected = finaldata$Year[0],
            #multiple = FALSE),
        column(6, plotlyOutput("scatter"), height = "600px")
        
    )
    
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter <- renderPlotly({
       plot_ly(data = finaldata)%>%
            filter(Year %in% input$FilterYear) %>%
            group_by(Year) %>%
            #data <- data[order(data$Import.Value),]%>%
            #data <- data[order(data$Export.Value),]%>%
            
            add_markers(x=~Exportpercentile, y=~Importpercentile,
                        hoverinfo = 'text',
                        text = ~paste('</br> Country: ', Countries,
                                      '</br> Import Value: $', Import.Value,
                                      '</br> Export Value: $', Export.Value), 
                          marker = list(colorbar=list(title = "Trade Balance"), color=~Tradebalance,colorscale='YlOrRd', showscale = TRUE))%>%
           
                        
            #add_segments(x=(max(finaldata$percentImport) - min(finaldata$percentImport))/2, xend = (max(finaldata$percentImport) - min(finaldata$percentImport)/2), y=0, yend = max(finaldata$percentExport))%>%
            #add_segments(x=0, xend = max(finaldata$percentImport), y=(max(finaldata$percentExport) - min(finaldata$percentExport))/2,yend = (max(finaldata$percentExport) - min(finaldata$percentExport))/2)
           #abline(importExportlm,h= mean(~percentExport), col = "blue", lwd=3, lty=2)
            layout(
                shapes=list(
                    list(type='line', x0= 50, x1= 50, y0=0, y1=100, line=list(dash='dot', width=1)),
                    list(type='line', x0= 0, x1=100, y0=50, y1=50, line=list(dash='dot', width=1)))
                    #list(type='line', x0= (max(finaldata$percentImport) - min(finaldata$percentImport))/2, x1= (max(finaldata$percentImport) - min(finaldata$percentImport))/2, y0=0, y1=max(finaldata$percentExport), line=list(dash='dot', width=1)),
                    #list(type='line', x0= 0, x1 = max(finaldata$percentImport), y0=(max(finaldata$percentExport) - min(finaldata$percentExport))/2, y1=(max(finaldata$percentExport) - min(finaldata$percentExport))/2, line=list(dash='dot', width=1)))
                
            )
            
        })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)