library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinyWidgets)

#----------------------------------------Package installation--------------------------------------------
packages = c('tinytex','RColorBrewer','classInt','ggthemes',
             'tidyverse', 'pivottabler', 'dplyr', 'lubridate', 'gapminder')
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

export_proportion <- data.frame(cbind(export_oilgas, export_nonoilgas[,2:5]))
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

# upload raw data of indonesia export countries
import_partners <- read.csv("data/Import_by_country.csv")
#-----------------------------------------------------------------------------------------------------------

#---------------------------------------Dashboard 1-2b------------------------------------------------------
export_import_by_country <- read_csv("data/ExportImportByCountries.csv")
names(export_import_by_country)[names(export_import_by_country) == "Import Value"] <- "Import.Value"
names(export_import_by_country)[names(export_import_by_country) == "Export Value"] <- "Export.Value"
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
            menuItem("Dashboard 1-1", tabName = "dashboard1-1", icon = icon("dashboard")),
            menuItem("Dashboard 1-2", tabName = "dashboard1-2", icon = icon("dashboard")),
            menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard")),
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
                        
                        selectizeInput(
                            inputId = "FilterYear",
                            label = "Year",
                            choices = unique(export_proportion$Year),
                            selected = export_proportion$Year[0],
                            multiple = FALSE),
                        
                        box(title = "Proportion of Exported Goods",
                            plotlyOutput("ExportProportion", height = 500)),
                        
                        box(title = "Proportion of Exported Goods",
                            plotlyOutput("ImportProportion", height = 500))
                    )
            ),
            #-------------------------------------------------------------------------------------------------------------
            
            # Third tab content
            tabItem(tabName = "dashboard1-2",
                    fluidRow(
                        sliderInput("obs", 
                                    "Number of observations:",
                                    min = min(export_import_by_country$Year), 
                                    max = max(export_import_by_country$Year), 
                                    value = c(min(export_import_by_country$Year), max(export_import_by_country$Year)),
                                    step = 1,
                                    sep = ""
                        ),
                        
                        column(6, plotlyOutput("ImportPartner"), height = "600px"),
                        column(6, plotlyOutput("ExportPartner"), height = "600px")
                    )
            ),
            
            # Third tab content
            tabItem(tabName = "dashboard2",
                    fluidRow(
                        radioButtons(
                            inputId = "ProductCategory",
                            label = "Category:",
                            choices = import_proportion$Year,
                            selected = NULL
                        ),
                        mainPanel(plotlyOutput("exportCountry"))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "dashboard3",
                    h2("Dashboard 3 - Trade Balance"),
                    fluidRow(
                        column(6, plotlyOutput(outputId = "timeseries", height = "600px"))
                    )
            )
            
            
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    #----------------------------------------------------Dashboard 1-1-----------------------------------------------------
    output$ImportExport <- renderPlotly({
        melt_export_import <- melt(export_import, id.vars = "Year")
        p <- ggplot(melt_export_import, aes(x = Year, y = value, fill=variable)) + 
            geom_bar(stat = 'identity', position=position_dodge(), colour = "black") +
            ylab("Amount (US$)") + xlab("Year") 
        ggplotly(p)
    })
    
    output$ExportProportion <- renderPlotly({
        plot_ly(data = export_proportion, x = ~Year) %>%
            filter(Year %in% input$FilterYear) %>%
            group_by(Year) %>%
            add_trace(x = "Oil & Gas", y = ~Crude.Oil, name = "Crude Oil", 
                      text = ~Crude.Oil, textposition = 'auto') %>%
            add_trace(x = "Oil & Gas", y = ~Oil.Product, name = "Oil Product",
                      text = ~Oil.Product, textposition = 'auto') %>%
            add_trace(x = "Oil & Gas", y = ~Gas, name = "Gas",
                      text = ~Gas, textposition = 'auto') %>%
            add_trace(x = "Non-Oil & Gas", y = ~Agriculture, name = "Agriculture",
                      text = ~Agriculture, textposition = 'auto') %>%
            add_trace(x = "Non-Oil & Gas", y = ~Industry, name = "Industry",
                      text = ~Industry, textposition = 'auto') %>%
            add_trace(x = "Non-Oil & Gas", y = ~Mining, name = "Mining",
                      text = ~Mining, textposition = 'auto') %>%
            add_trace(x = "Non-Oil & Gas", y = ~Others, name = "Others",
                      text = ~Others, textposition = 'auto') %>%
            layout(yaxis = list(title = "Proportion", range = c(0, 200000)), 
                   barmode = "stack", plot_bgcolor = plotcolor,
                   paper_bgcolor = papercolor)
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
            layout(yaxis = list(title = "Proportion", range = c(0, 200000)), barmode = NULL)
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #-----------------------------------------------------------Dashboard 1-2---------------------------------------------------
    output$ImportPartner <- renderPlotly({
        #import partners of indonesia
        plot_ly(source = "source") %>%
            add_lines(data = import_partners, x = ~Year, y = ~Import, color = ~Country.of.Origin, mode= 'lines' )%>%
            add_markers(data = import_partners, x = ~Year, y = ~Import, color = ~Country.of.Origin, mode= 'markers') %>%
            layout(
                text=paste("country:", import_partners$Country.of.Origin)
            )
    })
    
    output$ExportPartner <- renderPlotly({
        plot_ly(source = "source") %>%
            add_lines(data = export_partners, x = ~Year, y = ~Export, color = ~Destination, mode= 'lines')%>%
            add_markers(data = export_partners, x = ~Year, y = ~Export, color = ~Destination, mode= 'markers')
    })
    #---------------------------------------------------------------------------------------------------------------------------
    
    #-----------------------------------------------------------Dashboard 3 (Trade Balance)-------------------------------------
    output$timeseries <- renderPlotly({
        plot_ly(source = "source") %>% 
            add_lines(data = ds, x= ~Year, y=~value,color=~variable,
                      mode = 'lines+markers', line = list(width = 3))%>% 
            add_markers(data = ds2, x= ~Year, y=~value,color=~variable)%>%
            layout(
                xaxis = list(title = "Year", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
                yaxis = list(title = "Amount", gridcolor = "#bfbfbf"), 
                plot_bgcolor = plotcolor,
                paper_bgcolor = papercolor
            )
    })
    #---------------------------------------------------------------------------------------------------------------------------
}

shinyApp(ui = ui, server = server)