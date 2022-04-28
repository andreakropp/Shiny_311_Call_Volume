#
# This is a Shiny web application created by Andrea Kropp at DataRobot
# as an example of lightweight apps that solve specific end-user needs
#
#

library(datarobot)
library(shiny)
library(shinythemes)
library(dplyr)
library(data.table)
library(plotly)
library(httr)
library(readr)


### Connect to DataRobot

ConnectToDataRobot(endpoint = "https://app.datarobot.com/api/v2", token = "NjE2ZGQzYmUyZDM1YzNiMTNmMGU3ZWY5Oi80REt4VzdZQVEzKzRGNkVudCs1TDZQZUZtN3VySk9pYUYyekNoOXlYL1U9")


### Helper Functions

#This function retrieves a dataset from DataRobot's AI Catalog usign the datasetID number
downloadDataFromCatalog <- function(catalogId) {
    routeString <- datarobot:::UrlJoin("datasets", catalogId, "file")
    response <- datarobot:::DataRobotGET(routeString, returnRawResponse = TRUE)
    csvFile = httr::content(response)
    return (csvFile)
}


### Non-Reactive Code to Execute at Start Up

# Retrieve data sets using helper function above
# These datasets have scheduled refreshes in AI Catalog 
# The Shiny app retrieves the most recent version rather than loading static data from a file

training <- downloadDataFromCatalog("626816a346b19d76d1ad4859")  #file used to train the model; has roughly 800 days of historical data
last50days <- downloadDataFromCatalog("6268169ca3e08d8483abd545") #file with the most recent 50 days of known values
last50days$UID <- gsub("_","",last50days$UID) #remove underscore in UniqueID to match up with training data

#combine all actual known values
total_known <- rbind(training,last50days)
total_known <- total_known[is.na(total_known$CALLS) == FALSE,] #drop rows with NA for CALLS
total_known <- total_known[duplicated(total_known) == FALSE,] #drop any duplicates introduced by joining
total_known <- total_known[order(total_known$AGENCY, total_known$CREATED_DATE),] #reorder by agency and date
total_known <- total_known[total_known$CREATED_DATE != max(total_known$CREATED_DATE),] #remove most recent day reported because it is typically only a partial day 


### Important dates
max_date_training <- max(training$CREATED_DATE)  #The max date in the training file is the date the model has last re-trained
date_today <- Sys.Date()

### Full names of Agencies
agency_df <- data.frame(AGENCY = c("DOT","NYPD","HPD","DSNY","DPR"),
                        AGENCY_NAME = c("Department of Transportation", "NY Police Department","Department of Housing Preservation and Development",
                                        "Department of Sanitation","Department of Parks and Recreation"))




# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("readable"),

    # Application title
    titlePanel("NYC 311 Call Volume and Duration Forecasting"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("agency", label = h3("Agency selection"), 
                        choices = list("NYPD - Police" = "NYPD", "HPD - Housing" = "HPD", "DOT - Transportation" = "DOT",
                                       "DPR - Parks & Rec" = "DPR", "DSNY - Sanitation" = "DSNY"), 
                        selected = "HPD"),
            
            dateRangeInput("dates", label = h3("Date range"), start = (date_today-90), end = (date_today+28)),
            HTML("<br>"),
            HTML("<br>"),
            
            checkboxGroupInput("checkGroup", label = h3("Forecasts to Plot"), 
                               choices = list("Long-term trend" = 2, "Naive model" = 1, "DataRobot ML next 7 days model" = 3, "DataRobot ML days 8 to 28 model" = 4),
                               selected = 2),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Historical + Forecast",
                         HTML("<br>"),
                         h2("Historical Calls Plus Forecasts"),
                         HTML("<br>"),
                         plotlyOutput("main_line_chart"),
                         HTML("<br>"),
                         textOutput("model_train_date"),
                         h2("How to Use this Chart"),
                         HTML("<b>The heavy blue-gray line shows the actual calls each day.</b> This line stops a few days before today's date (where the shaded area begins) because the actual call volumes are not immediately reported."),
                         HTML("The options in the left side-panel control which forecast lines are plotted on top of the actual values. All of the options are forecasts for what will happen in the future."),
                         h4("Forecast Options"),
                         HTML("<ul><li>Long-term trend (light grey) - Text to come</li><li>Naive model (light orange) - Text to come</li><li>DataRobot 7-day model (blue) - Text to come</li><li>DataRobot 28-day model (light blue) - Text to come</li></ul>"),
                         HTML("<br>"),
                         h2("When and How these Forecast were Created"),
                         HTML("The dotted vertical line indicates the date that the machine learning model was last retrained. It is good practice to re-train machine learning models such as the one used here roughly every 30-45 days to ensure that they are aware of the most recent patterns and trends in the outside world."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("The model labeled 'naive' is based on simple math looking back at the recent past. The models labled 'DataRobot ML' are machine learning models developed using DataRobot software."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<br>")
                ),
                tabPanel("Model Accuracy",
                         HTML("<br>"),
                         h4("Evaluate Model Accuracy")
                ),
                tabPanel("Forecast Explanations",
                         HTML("<br>"),
                         h4("Understand the Major Factors Impacting the Forecast")
                ),
                tabPanel("Methodology",
                         HTML("<br>"),
                         h4("Methodology")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #Known data for the AGENCY selected
    agency_known <- reactive({
        total_known[total_known$AGENCY == input$agency,]
    })
    
    
    
    #Calculate naive model forecasts
    agency_line_chart_data_all <- reactive({
        
        #known data
        data <- agency_known()
        
        future_rows_needed <- 28 + as.numeric((date_today - max(data$CREATED_DATE)))
        
        #blank rows for 28 days into the future 
        future_data <- data.frame("AGENCY" = rep(input$agency, future_rows_needed),
                                  "CREATED_DATE" = seq.Date(from = max(data$CREATED_DATE)+1, by = 1, length.out = future_rows_needed),
                                  "CALLS" = rep(NA, future_rows_needed),
                                  "SEGMENT" = rep(input$agency, future_rows_needed))
        future_data$UID <- paste0(future_data$AGENCY,future_data$CREATED_DATE)
        
        #combine dataframes
        data <- rbind(data, future_data)
        
        #calculate inputs for naive forecast and long term trend
        data$lag7 <- lag(data$CALLS, 7)
        data$lag14 <- lag(data$CALLS, 14)
        data$lag21 <- lag(data$CALLS, 21)
        data$lag28 <- lag(data$CALLS, 28)
        data$lag35 <- lag(data$CALLS, 35)
        data$lag42 <- lag(data$CALLS, 42)
        data$lag49 <- lag(data$CALLS, 49)
        data$lag56 <- lag(data$CALLS, 56)
        data$roll7 <- lag(frollmean(data$CALLS,7),7)
        data$roll14 <- lag(frollmean(data$CALLS,14),14)
        data$roll21 <- lag(frollmean(data$CALLS,21),21)
        data$roll28 <- lag(frollmean(data$CALLS,28),28)
        data$naive1 <- rowMeans(data[which(grepl("lag", colnames(data)) == TRUE)], na.rm = TRUE)  
        data$naive2 <- rowMeans(data[which(grepl("roll", colnames(data)) == TRUE)], na.rm = TRUE)  
        
        
        #merge machine learning forecasts from DataRobot
        
        #data <- merge(data, DRModel_FW1to8, by = c("CREATED_DATE","AGENCY"), all.x = TRUE)
        
        data
        
    })
    
    
    agency_line_chart_data_date_bound <- reactive({
        
        data <- agency_line_chart_data_all()
        data <- data[data$CREATED_DATE >= input$dates[1] & data$CREATED_DATE <= input$dates[2],]
        data
        
    })
    
    
    output$main_line_chart <- renderPlotly({
        
        data <- agency_line_chart_data_date_bound()
        ymax = max(c(data$CALLS, data$naive1, data$naive2), na.rm = TRUE)
        dynamic_title = paste0("Calls to ",agency_df$AGENCY_NAME[agency_df$AGENCY == input$agency])
        
        
        fig <- plot_ly(data, x = ~CREATED_DATE, y = ~CALLS, name = "Actual CALLS", type = 'scatter', mode = 'lines', 
                       hoverinfo = "x+y", line = list(color = '#53718F', width = 4))
        
        if ("2" %in% input$checkGroup ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive2, name = "Long-term trend", type = 'scatter', mode = 'lines',
                                     hoverinfo = "x+y", line = list(color = '#BAC5CE', width = 2))
        }
        
        if ("1" %in% input$checkGroup ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive1, name = "Naive model", type = 'scatter', mode = 'lines',
                                     hoverinfo = "x+y", line = list(color = '#ffb68f', width = 2))
        }
        
       # if ("3" %in% input$checkGroup ) {
        #    fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR7day, type = 'scatter', mode = 'lines',
        #                             hoverinfo = "x+y", line = list(color = '#2d8fe2', width = 2))
        #}
        
       # if ("4" %in% input$checkGroup ) {
        #    fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive2, type = 'scatter', mode = 'lines',
       #                              hoverinfo = "x+y", line = list(color = '#8ebcee', width = 2))
       # }
        
        fig <- fig %>% layout(showlegend = TRUE,
                              title = dynamic_title,
                              xaxis = list(title = "Date"),
                              yaxis = list(title = "Call Volume", range = list(0,ymax)),
                              shapes = list(
                                  list(type = "rect",
                                       fillcolor = "#777777",
                                       line = list(color = "#777777"),
                                       opacity = 0.3,
                                       y0 = 0, y1 = ymax*1.1, x0 = date_today, x1 =date_today+28),
                                  list(type = "line",
                                       y0 = 0, y1 = ymax, x0 = max_date_training, x1 = max_date_training,
                                       line = list(color = "#777777", dash="dot"))
                              )
        )
        
        fig
        
    })
    
    
    
    output$model_train_date <- renderText({
            paste0("Machine learning models were last retrained on ", max_date_training," as indicated by the dotted vertical line.")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
