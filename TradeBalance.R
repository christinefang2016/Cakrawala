#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('tinytex','plotly', 'RColorBrewer','classInt','ggthemes',
             'tidyverse', 'pivottabler', 'dplyr','shiny','shinythemes', 'lubridate')
for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trade Balance"),

    # Sidebar with a slider input for number of bins 
    

        # Show a plot of the ge nerated distribution
        fluidRow(
           
            plotlyOutput(outputId = "timeseries", height = "600px") 
                   
        )
        
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ImportExport <- read.csv("data/IndonesiaExportImport.csv")
    
    #change month to date
    ImportExport$Year <- format(as.Date(ImportExport$Month, "%d/%m/%Y"), "%Y")
    
    #Clean Data
    CleanedData <- ImportExport %>%
        group_by(Year = ImportExport$Year) %>%
        summarise(TotalImport = sum(Total.Import), TotalExport = sum(Total.Export))
    
    # Add Trade Balance column
    Tradebalance = CleanedData$TotalExport - CleanedData$TotalImport
    CleanedData= cbind(CleanedData, Tradebalance)
    
    # Change year format to integer
    #CleanedData$Year <- as.integer(CleanedData$Year)
    
    #reshape
    ds <- reshape2::melt(CleanedData, id = "Year")
    ds2 <- filter(ds,variable == "Tradebalance")
    #ds3 <- filter(ds,variable == "TotalImport")
    #ds4 <- filter(ds,variable == "TotalExport")
    ds <- filter(ds,variable != "Tradebalance")
    
    # Set some colors
    plotcolor <- "#F5F1DA"
    papercolor <- "#E3DFC8"
    
    col <- reactive({ifelse(ds2$value >=0, "#5B84B1FF","FC766AFF")})
    output$timeseries <- renderPlotly({
        
      
        p <- plot_ly(source = "source") %>%
        
            add_lines(data = ds, x= ~Year, y=~value,color=~variable,
                       mode = 'lines', marker = list(width = 3))%>%
            add_trace(data = ds2,x= ~Year, y=~value,
                      marker =list(color = col(), width = 10),yaxis = "y2", name = "TradeBalance", opacity = 0.6)%>%
            #add_lines(data = ds2, x=~Year, y=~value, color=~variable,
                      #mode = 'lines', line=list(width=5),yaxis = "y2", opacity = 0.8)%>%
            layout(
                xaxis = list(title = "Year", domain = c(0, 0.98)),
                yaxis = list(title = "Amount (USD million)", domain = c(0, 0.98)),
                #plot_bgcolor = plotcolor,
                #paper_bgcolor = papercolor,
                hovermode = 'compare',
                yaxis2 = list(overlaying = "y",
                              title = "Trade Balance Amount (USD million)", side = "right")
            )
            
          #ds2$pos <- ifelse(ds2$value >=0, ds2$value, NA)
          #ds2$neg <- ifelse(ds2$value <0, ds2$value, NA)
       # q <- plot_ly(source = "source") %>%
       #      
       #     add_trace(data = ds2,x= ~Year, y=~value,
       #      marker =list(color = col()))%>%
       #     #add_trace(data = ds2,x= ~Year, y=~neg,
       #               #mode = 'markers', line = list(color = "red",width = 5))%>%
       #     layout(
       #         xaxis = list(title = "Year", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
       #         yaxis = list(title = "Amount (USD million)", gridcolor = "#bfbfbf"),
       #         plot_bgcolor = plotcolor,
       #         paper_bgcolor = papercolor,
       #         hovermode = 'compare'
       #         #yaxis2 = list(title = "Total Import", side = "right")
       #     )
       # 
       # z <- subplot(p,q, shareX = TRUE)
       p
             
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
