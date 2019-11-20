#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c('tinytex','plotly', 'RColorBrewer','classInt','ggthemes',
             'tidyverse','devtools','data.table', 'pivottabler', 'dplyr','shiny','shinythemes', 'lubridate')
for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}
devtools::install_github("mattflor/chorddiag")
#library(chorddiag)
#library(shiny)
#library(dplyr)
library(chorddiag)
#library(data.table)
data <- read_csv("Data/ExportImportByCountries.csv", locale = locale(encoding = "Latin1"))
#sorted_import_partners <- data[order(-data$`Import Value`),]
# Define UI for application that draws a histogram
ui <- fluidPage(

    fluidRow(
        
            sliderInput(
                inputId = "FilterYear",
                label = "Year",
                min = 2002,
                max = 2018,
                value = 2002,
                sep = "",
                animate = animationOptions(loop = TRUE)),
            mainPanel(
                column(12,chorddiagOutput("chord"),height = "1000px")
            )
        
        
        #column(6, plotOutput("sankey"), height = "1000px")
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$chord <- renderChorddiag({
       
        #plotOutput(data = sorted_import_partners)%>%
        testdata = filter(data, Year==input$FilterYear)
        destinationImport = cbind(testdata$Destination,testdata$`Import Value`, row.names=NULL)
        destinationExport = cbind(testdata$Destination,testdata$`Export Value`,row.names=NULL)
        destinationImport = as.data.frame(destinationImport)
        destinationExport = as.data.frame(destinationExport)
        #View(destination)
        destinationImport$V2  <- as.numeric(as.character(destinationImport$V2))
        destinationExport$V2  <- as.numeric(as.character(destinationExport$V2))
        tdestinationImport <- transpose(destinationImport)
        names(tdestinationImport) <- tdestinationImport[1,]
        tdestinationImport <- tdestinationImport[-1,]
        tdestination <- cbind(Indonesia = destinationExport$V2, tdestinationImport)
        tdestination[nrow(tdestination)+1,] <- 0
        row.names(tdestination) <- c("Indonesia",names(tdestinationImport))
        #colnames(tdestinationImport) <- c("Indonesia")
        #View(tdestination)
        tdestination<-as.matrix(tdestination)
        #glimpse(tdestination)
        tdestination[c(2:34),c(2:34)] <- 0
        tdestination[c(2:34)] <- tdestination[c(1:34)]
        tdestination[1,1] <- 0
        class(tdestination) <- "numeric"
 
        groupColor <- c("#ffb997","#f67e7d","#843b62","#ObO32d","#74546a",
                         "#247ba0", "#70c1b3", "#b2dbbf", "#f3ffbd", "#ff1654", "#fe938c", "#e6b89c", "#ead2ac", "#9cafb7", "#4281a4", "#50514f", "#f25f5c", "#ffe066", "#247ba0", "#70c1b3", "#773344", "#e3b5a4", "#f5e9e2", "#0b0014", "#d44d5c", "#c9e4ca", "#87bba2", "#55828b", "#3b6064", "#364958", "#071e22", "#1d7874", "#679289", "#ee2e31")
        #         #461220, #8c2f39, #b23a48, #fcb9b2, #fed0bb, #faf3dd, #c8d5b9, #8fc0a9, #68b0ab, #4a7c59, #faa275, #ff8c61, #ce6a85, #985277, #5c374c)
        # 
        
        #View(tdestination)
        chorddiag(
            tdestination, 
            showTicks = F, 
            groupColors = groupColor, 
            groupedgeColor = groupColor, 
            chordedgeColor = groupColor, 
            width = 10000,
            height = 10000, 
            groupnameFontsize = 8, 
            groupnamePadding = 1, 
            margin = 50,
            categorynamePadding = 10
            )
        
        
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
