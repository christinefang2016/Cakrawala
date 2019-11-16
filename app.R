library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinyWidgets)
library(sf)
library(tmap)
library(leaflet)
library(ggmosaic)
library(htmltools)
library(raster)
library(rgdal)
library(sf)
library(rgeos)
library('treemap')
library(RColorBrewer)
library(d3treeR)

#----------------------------------------Package installation--------------------------------------------
packages = c('tinytex','plotly', 'RColorBrewer','classInt','ggthemes',
             'tidyverse', 'pivottabler', 'dplyr','shiny','shinythemes', 'lubridate',
             'sf', 'tmap', 'shinyWidgets', 'leaflet', 'ggmosaic', 'htmltools', 'raster', 'rgdal', 'rgeos')
for(p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}
#--------------------------------------------------------------------------------------------------------

#---------------------------------------- Total Import and Export of Indonesia (Dashboard 1-1a)----------
#TOTAL EXPORT GROUP_BY YEAR
total_export <- read_csv("data/Total Export.csv")
export_by_year <- aggregate(total_export$Total,
                            by = list(total_export$Year),
                            FUN = sum,
                            na.rm = TRUE)
names(export_by_year)[names(export_by_year) == "Group.1"] <- "Year"
names(export_by_year)[names(export_by_year) == "x"] <- "EXPORT"

#TOTAL IMPORT GROUP_BY YEAR
total_import <- read_csv("data/Total Import.csv")
import_by_year <- aggregate(total_import$Total,
                            by = list(total_import$Year),
                            FUN = sum,
                            na.rm = TRUE)
names(import_by_year)[names(import_by_year) == "Group.1"] <- "Year"
names(import_by_year)[names(import_by_year) == "x"] <- "IMPORT"

#COMBINE IMPORT AND EXPORT DATA
export <- data.frame(cbind("Type" = "Import", "Year" = import_by_year$Year, "Total" = import_by_year$IMPORT))
import <- data.frame(cbind("Type" = "Export", "Year" = export_by_year$Year, "Total" = export_by_year$EXPORT))
export_import <- data.frame(cbind("Year" = import_by_year$Year, "Import" = import_by_year$IMPORT, 
                                  "Export" = export_by_year$EXPORT))
#-----------------------------------------------------------------------------------------------------------


#---------------------------------Proportion of Exported Goods (Dashboard 1-1b)-----------------------------
export_oilgas <- read_csv("data/Export_Oil_Gas.csv")
export_nonoilgas <- aggregate(total_export[,6:9],
                              by = list(total_export$Year),
                              FUN = sum,
                              na.rm=TRUE)
names(export_nonoilgas)[names(export_nonoilgas) == "Group.1"] <- "Year"
export_nonoilgas <- export_nonoilgas[-c(24),]

testing <- export_oilgas %>% gather("Subcategory", "Import", -Year)
testing$Category <- "Oil&Gas"
names(testing)[names(testing) == "Crude Oil"] <- "Crude.Oil"
names(testing)[names(testing) == "Oil Product"] <- "Oil.Product"

testing_nonoilgas <- export_nonoilgas %>% gather("Subcategory", "Import", -Year)
testing_nonoilgas$Category <- "NonOil&Gas"

export_proportion <- data.frame(rbind(testing_nonoilgas, testing))
#-----------------------------------------------------------------------------------------------------------


#---------------------------Proportion of Imported Goods (Dashboard 1-1c)-----------------------------------
import_proportion <- aggregate(total_import[,4:6],
                               by = list(total_import$Year),
                               FUN = sum,
                               na.rm=TRUE)
names(import_proportion)[names(import_proportion) == "Group.1"] <- "Year"
names(import_proportion)[names(import_proportion) == "Consumption Goods"] <- "ConsumptionGoods"
names(import_proportion)[names(import_proportion) == "Raw Material Support"] <- "RawMaterialSupport"
names(import_proportion)[names(import_proportion) == "Capital Goods"] <- "CapitalGoods"

#-----------------------------------------------------------------------------------------------------------

#---------------------------------------Dashboard 1-2a------------------------------------------------------
export_partners <- read.csv("data/Export_by_country.csv")
filter_export_partners <- filter(export_partners, Year == max(export_partners$Year))
sorted_export_partners <- filter_export_partners[order(-filter_export_partners$Export),]

# upload raw data of indonesia export countries
import_partners <- read.csv("data/Import_by_country.csv")
filter_import_partners <- filter(import_partners, Year == max(import_partners$Year))
sorted_import_partners <- filter_import_partners[order(-filter_import_partners$Import),]
#-----------------------------------------------------------------------------------------------------------

#---------------------------------------Dashboard 1-2b------------------------------------------------------
export_import_by_country <- read_csv("data/ExportImportByCountries.csv")
names(export_import_by_country)[names(export_import_by_country) == "Import Value"] <- "Import.Value"
names(export_import_by_country)[names(export_import_by_country) == "Export Value"] <- "Export.Value"
#-----------------------------------------------------------------------------------------------------------

#---------------------------------------Dashboard 1-2c------------------------------------------------------
#Export_Import by country on map
export_import_map <- read_csv("data/ExportImportByCountriesLongLat.csv")
global_map <- map_data("world")
#-----------------------------------------------------------------------------------------------------------

#-----------------------------Data Preprocessing for Product Category---------------------------------------
exportCategory <- read_csv("data/Export_Goods_Category.csv")
exportCategory$label <- paste(exportCategory$Destination, ", US$ ", exportCategory$Export, " Millions")
importCategory <- read_csv("data/Import_Goods_Category.csv")
importCategory$label <- paste(importCategory$Origin, ", US$ ", importCategory$Import, " Millions")
#-----------------------------------------------------------------------------------------------------------

#-----------------------------Data preprocessing for Dashboard 3 (Trade Balance)----------------------------
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

#reshape
ds <- reshape2::melt(CleanedData, id = "Year")
ds2 <- filter(ds,variable == "Tradebalance")
ds <- filter(ds,variable != "Tradebalance")

# Set some colors
plotcolor <- "#F5F1DA"
papercolor <- "#E3DFC8"
col <- reactive({ifelse(ds2$value >=0, "#5B84B1FF","FC766AFF")})

data <- read.csv("data/ExportImportByCountries.csv")
tempdata = mutate(data, Importpercentile = ntile(data$Import.Value,100))
finaldata = mutate(tempdata, Exportpercentile = ntile(tempdata$Export.Value,100))
finaldata$Tradebalance <- finaldata$Export.Value - finaldata$Import.Value
#-----------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Cakrawala",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Sales Dept",
                                     message = "Sales are steady this month."
                                 ),
                                 messageItem(
                                     from = "New User",
                                     message = "How do I register?",
                                     icon = icon("question"),
                                     time = "13:45"
                                 ),
                                 messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2014-12-01"
                                 )
                    ),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                     text = "5 new users today",
                                     icon("users")
                                 ),
                                 notificationItem(
                                     text = "12 items delivered",
                                     icon("truck"),
                                     status = "success"
                                 ),
                                 notificationItem(
                                     text = "Server load at 86%",
                                     icon = icon("exclamation-triangle"),
                                     status = "warning"
                                 )
                    )),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard 1-1 Overall", tabName = "dashboard1-1", icon = icon("dashboard")),
            menuItem("Dashboard 1-2 Export", tabName = "dashboard1-2", icon = icon("dashboard")),
            menuItem("Dashboard 1-2 Import", tabName = "dashboard1-2b", icon = icon("dashboard")),
            menuItem("Dashboard 3", tabName = "dashboard3", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            #--------------------------------------------Dashboard 1-1 Tab-------------------------------------------------
            tabItem(tabName = "dashboard1-1",
                    fluidRow(
                        #box(title = "Import/Export of Indonesia",
                        #    mainPanel(
                        #    plotlyOutput("ImportExport", height = 500))),
                        
                        column(6, plotlyOutput("ImportExport"), height = "600px"),
                        
                        sliderInput(
                            inputId = "FilterYear",
                            label = "Year",
                            min = min(export_proportion$Year),
                            max = max(export_proportion$Year),
                            value = max(export_proportion$Year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)),
                        
                        
                        box(title = "Proportion of Exported Goods",
                            plotlyOutput("ExportProportion", height = 500)),
                        
                        box(title = "Proportion of Imported Goods",
                            plotlyOutput("ImportProportion", height = 500))
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------
            
            #-------------------------------------------Dashboard1-2 Export------------------------------------------------------
            tabItem(tabName = "dashboard1-2",
                    fluidRow(
                        sidebarPanel(
                            selectInput(inputId = "order",
                                        label = "Top K",
                                        choices = c("All" = "All",
                                                    "5" = "5",
                                                    "10" = "10",
                                                    "15" = "15",
                                                    "20" = "20"))
                        ),
                        
                        column(6, plotlyOutput("LineExport"), height = "500px"),
                        
                        sliderInput(
                            inputId = "FilterYearMap",
                            label = "Year",
                            min = min(export_import_map$Year),
                            max = max(export_import_map$Year),
                            value = max(export_import_map$Year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)),
                        
                        column(6, plotlyOutput("ExportPartnerMap"), height = "600px")
                    )
            ),
            #--------------------------------------------------------------------------------------------------------------
            
            
            #--------------------------------------------Dashboard 1-2b Import---------------------------------------------
            tabItem(tabName = "dashboard1-2b",
                    fluidRow(
                        sidebarPanel(
                            selectInput(inputId = "orderImport",
                                        label = "Top K",
                                        choices = c("All" = "All",
                                                    "5" = "5",
                                                    "10" = "10",
                                                    "15" = "15",
                                                    "20" = "20"))
                        ),
                        column(6, plotlyOutput("LineImport"), height = "500px"),
                        
                        sliderInput(
                            inputId = "FilterYearMapImport",
                            label = "Year",
                            min = min(export_import_map$Year),
                            max = max(export_import_map$Year),
                            value = max(export_import_map$Year),
                            sep = "",
                            animate = animationOptions(loop = TRUE)),
                        
                        
                        column(6, plotlyOutput("ImportPartnerMap"), height = "600px")
                        #radioButtons(
                        #    inputId = "ProductCategory",
                        #    label = "Category:",
                        #    choices = import_proportion$Year,
                        #    selected = NULL
                        #),
                        #mainPanel(plotlyOutput("exportCountry"))
                    )
            ),
            #-----------------------------------------------------------------------------------------------------------------------------------
            
            
            # Second tab content
            tabItem(tabName = "dashboard3",
                    h2("Dashboard 3 - Trade Balance"),
                    fluidRow(
                        column(6, plotlyOutput(outputId = "timeseries", height = "600px")),
                        
                        sliderInput(
                            inputId = "FilterYear3",
                            label = "Year",
                            min = 2002,
                            max = 2018,
                            value = 2002,
                            sep = "",
                            animate = animationOptions(loop = TRUE)),
                        column(6, plotlyOutput("scatter"), height = "600px")
                    )
            )
            
            
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    #----------------------------------------------------Dashboard 1-1 Overall-----------------------------------------------------
    output$ImportExport <- renderPlotly({
        melt_export_import <- melt(export_import, id.vars = "Year")
        p <- plot_ly(source = "source") %>% 
            add_lines(data = melt_export_import, x= ~Year, y=~value,color=~variable,
                      mode = 'lines', line = list(width = 4),opacity = 0.5)%>%
            layout(
                title = "Indonesia's Import and Export over years",
                xaxis = list(title = "Year", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
                yaxis = list(title = "Amount (USD million)", gridcolor = "#bfbfbf"), 
                plot_bgcolor = plotcolor, 
                paper_bgcolor = papercolor,
                hovermode = 'compare',
                yaxis2 = list(overlaying = "y", 
                              title = "Amount (USD million)", side = "right")
            )
    })
    
    output$ExportProportion <- renderPlotly({
        filtered_export_proportion <- filter(export_proportion, Year == input$FilterYear)
        YearValue <- paste("Export at", filtered_export_proportion$Year)
        a <- ggplot(data = filtered_export_proportion, label="Year") +
            geom_mosaic(aes(x = product(Subcategory, Category), fill=Subcategory, weight = Import), divider = ddecker(), na.rm=TRUE, offset = 0.002) +
            #scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac", "#d8b365", "#f5f5f5", "#5ab4ac", "#5ab4ac"))+
            scale_y_continuous(labels=scales::percent)+
            #theme(axis.text.x=element_text(angle=35))+
            #scale_x_productlist("Age", labels=labels)+
            labs(x = YearValue, title='Proportion of Exported Goods') +
            theme(plot.background = element_rect(fill = papercolor),
                  panel.background = element_rect(fill = plotcolor))
        
        ggplotly(a)
    })
    
    output$ImportProportion <- renderPlotly({
        plot_ly(data = import_proportion) %>%
            filter(Year %in% input$FilterYear) %>%
            group_by(Year) %>%
            add_trace(x = "Consumption Goods", y = ~ConsumptionGoods, name = "Consumption Goods", 
                      text = ~ConsumptionGoods, textposition = 'auto') %>%
            add_trace(x = "Raw Material Support", y = ~RawMaterialSupport, name = "Raw Material Support",
                      text = ~RawMaterialSupport, textposition = 'auto') %>%
            add_trace(x = "Capital Goods", y = ~CapitalGoods, name = "Capital Goods",
                      text = ~CapitalGoods, textposition = 'auto') %>%
            layout(yaxis = list(title = "Amount (USD Million)", range = c(0, 200000)), barmode = NULL,
                   plot_bgcolor = plotcolor, 
                   paper_bgcolor = papercolor)
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #-----------------------------------------------------------Dashboard 1-2---------------------------------------------------
    output$LineExport <- renderPlotly({
        inputOrder <- ifelse(input$order == "All", nrow(export_partners), input$order)
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
    
    output$ExportPartnerMap <- renderPlotly({
        mapExport <- filter(export_import_map, Year == input$FilterYearMap)
        
        map <- ggplot()+
            geom_polygon(data=global_map, aes(x=long, y=lat, group=group)) +  
            geom_point(data=mapExport, aes(x=Longitude, y=Latitude, size=ExportValue,
                                           label=Countries, label2=Year, label3=ExportValue), color="#F9665E")
        
        ggplotly(map)
    })
    
    output$ExportGoodsCategory <- renderD3tree2({
        category <- filter(exportCategory, Year == input$FilterYearMap)
        newTitle <- paste0("Category of Product exported in ", input$FilterYearMap)
        treemap <- treemap(category,
                           index = c("Type", "label"),
                           vSize="Export",
                           vColor="Type",
                           palette=brewer.pal(n=8, "Set3"),
                           title=newTitle,
                           title.legend = "Amount (Million US$)"
        )
        d3tree2(treemap, rootname = "Export of product (Million US$)")
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #-----------------------------------------------------------Dashboard 1-2b Import-----------------------------------------------------
    output$LineImport <- renderPlotly({
        inputOrder <- ifelse(input$orderImport == "All", nrow(import_partners), input$orderImport)
        final_data <- sorted_import_partners[c(1:inputOrder), c(1:3)]
        subset_data <- subset(import_partners, import_partners$Destination %in% final_data$Destination)
        
        plot_ly(source = "source") %>%
            add_lines(data = subset_data, x = ~Year, y = ~Import, color = ~Destination, mode= 'lines')%>%
            add_markers(x = ~Year, y = ~Import, color = ~Destination,
                        hoverinfo = 'text',
                        text = ~paste('<br> Country: ', Destination,
                                      '<br> Year: ', Year,
                                      '<br> Import Value: $ ', Import),
                        showlegend = FALSE
            )
    })
    
    output$ImportPartnerMap <- renderPlotly({
        mapExport <- filter(export_import_map, Year == input$FilterYearMapImport)
        
        map <- ggplot()+
            geom_polygon(data=global_map, aes(x=long, y=lat, group=group)) +  
            geom_point(data=mapExport, aes(x=Longitude, y=Latitude, size=ImportValue,
                                           label=Countries, label2=Year, label3=ImportValue), color="#44D362")
        
        ggplotly(map)
    })
    
    output$ImportGoodsCategory <- renderD3tree2({
        category <- filter(importCategory, Year == input$FilterYearMapImport)
        newTitle <- paste0("Category of Product imported in ", input$FilterYearMapImport)
        treemap <- treemap(category,
                           index = c("Type", "label"),
                           vSize="Import",
                           vColor="Type",
                           palette=brewer.pal(n=8, "Set3"),
                           title=newTitle,
                           title.legend = "Amount (Million US$)"
        )
        d3tree2(treemap, rootname = "Export of Import (Million US$)")
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    
    #-----------------------------------------------------------Dashboard 3 (Trade Balance)-------------------------------------
    output$timeseries <- renderPlotly({
        plot_ly(source = "source") %>%
            add_lines(data = ds, x= ~Year, y=~value,color=~variable,
                      mode = 'lines', marker = list(width = 3))%>%
            add_trace(data = ds2,x= ~Year, y=~value,
                      marker =list(color = col(), width = 10),yaxis = "y2", name = "TradeBalance", opacity = 0.6)%>%
            layout(
                xaxis = list(title = "Year", domain = c(0, 0.98)),
                yaxis = list(title = "Amount (USD million)", domain = c(0, 0.98)),
                hovermode = 'compare',
                yaxis2 = list(overlaying = "y",
                              title = "Trade Balance Amount (USD million)", side = "right")
            )
    })
    
    output$scatter <- renderPlotly({
        plot_ly(data = finaldata)%>%
            filter(Year %in% input$FilterYear3) %>%
            group_by(Year) %>%
            add_markers(x=~Exportpercentile, y=~Importpercentile,
                        hoverinfo = 'text',
                        text = ~paste('</br> Country: ', Countries,
                                      '</br> Import Value: $', Import.Value,
                                      '</br> Export Value: $', Export.Value), 
                        marker = list(colorbar=list(title = "Trade Balance"), 
                                      color=~Tradebalance,colorscale='YlOrRd', showscale = TRUE)) %>%
            layout(
                shapes=list(
                    list(type='line', x0= 50, x1= 50, y0=0, y1=100, line=list(dash='dot', width=1)),
                    list(type='line', x0= 0, x1=100, y0=50, y1=50, line=list(dash='dot', width=1)))
            )
        
    })
    #---------------------------------------------------------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)