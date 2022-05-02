#
# This is a Shiny web application created by Andrea Kropp at DataRobot
# as an example of lightweight apps that solve specific end-user needs
#

library(datarobot)
library(shiny)
library(shinythemes)
library(dplyr)
library(data.table)
library(plotly)
library(httr)
library(readr)
library(DT)

date_today <- Sys.Date()
training_date <- as.Date("2022-04-18")


### Connect to DataRobot
ConnectToDataRobot(endpoint = "https://app.datarobot.com/api/v2", token = "NjE2ZGQzYmUyZDM1YzNiMTNmMGU3ZWY5Oi80REt4VzdZQVEzKzRGNkVudCs1TDZQZUZtN3VySk9pYUYyekNoOXlYL1U9")

### API
source("keys.R")

API_URL       <- "https://cfds-ccm-prod.orm.datarobot.com"
API_TOKEN     <-  API_TOKEN     #'YOUR API TOKEN'
DATAROBOT_KEY <-  DATAROBOT_KEY #'YOUR DATAROBOT KEY'
USERNAME      <-  USERNAME      #"YOUR USE NAME"

DEPLOYMENT_ID_1to7 = '626c43f9ba5db8e203fed447' #deployment that makes 1 to 7 day forecasts
DEPLOYMENT_ID_8to28 = '626b036fdbfb7cbfeb7520ea' #deployment that makes 8 to 28 day forecasts


### Define Custom Functions

#This function retrieves a dataset from DataRobot's AI Catalog using the datasetID number
downloadDataFromCatalog <- function(catalogId) {
    routeString <- datarobot:::UrlJoin("datasets", catalogId, "file")
    response <- datarobot:::DataRobotGET(routeString, returnRawResponse = TRUE)
    csvFile = httr::content(response)
    return (csvFile)
}

#This function sends a local dataframe to the DataRobot real-time prediction API
timeseriesPredictionsFromLocalDataframe <- function(df, deploymentID, forecastPoint = Sys.Date()) {
    
    URL <- paste0(API_URL, "/predApi/v1.0/deployments/", deploymentID, "/predictions","?forecastPoint=",forecastPoint,"T00:00:00")
    response <- POST(URL,
         body = jsonlite::toJSON(df, pretty=T, na = "string"),
         add_headers("datarobot-key" = DATAROBOT_KEY),
         httr::content_type_json(),
         authenticate(USERNAME, API_TOKEN, type = "basic"))
    response <- httr::content(response, simplifyVector=TRUE)
    return(response$data)
}



### Retrieve Data Sets

# These datasets have scheduled refreshes in AI Catalog 
# The Shiny app retrieves the most recent version rather than loading static data from a file

training <- downloadDataFromCatalog("626816a346b19d76d1ad4859")  #file used to train the model; has roughly 800 days of historical data
last50days <- downloadDataFromCatalog("6268169ca3e08d8483abd545") #file with the most recent 50 days of known values
last50days$UID <- gsub("_","",last50days$UID) #remove underscore in UniqueID to match up with training data

# Combine all actual known values
total_known <- rbind(training,last50days)
total_known <- total_known[is.na(total_known$CALLS) == FALSE,] #drop rows with NA for CALLS
total_known <- total_known[duplicated(total_known$UID) == FALSE,] #drop any duplicates introduced by joining
total_known <- total_known[order(total_known$AGENCY, total_known$CREATED_DATE),] #reorder by agency and date
total_known <- total_known[total_known$CREATED_DATE != max(total_known$CREATED_DATE),] #remove most recent day reported because it is typically only a partial day 


### Full names of Agencies
agency_df <- data.frame(AGENCY = c("DOT","NYPD","HPD","DSNY","DPR"),
                        AGENCY_NAME = c("Department of Transportation", "NY Police Department","Department of Housing Preservation and Development",
                                        "Department of Sanitation","Department of Parks and Recreation"))

### Load training predictions and format for later merging
training_preds_1to7 <- read.csv("datasets/Training_Predictions_FW_1to7_Model_626b18edfa1e740219b571d6.csv", stringsAsFactors = FALSE)
training_preds_8to28 <- read.csv("datasets/Training_Predictions_FW_8to28_Model_626aeac66312bc4d8a9491c5.csv", stringsAsFactors = FALSE)
training_preds_1to7$Timestamp <- as.Date(strftime(training_preds_1to7$Timestamp, format = "%Y-%m-%d", usetz = FALSE), format = "%Y-%m-%d")
training_preds_8to28$Timestamp <- as.Date(strftime(training_preds_8to28$Timestamp, format = "%Y-%m-%d", usetz = FALSE), format = "%Y-%m-%d")

colnames(training_preds_1to7) <- c("row_id","Partition","CREATED_DATE","Forecast.Distance","Forecast.Point","AGENCY","Prediction1to7")
colnames(training_preds_8to28) <- c("row_id","Partition","CREATED_DATE","Forecast.Distance","Forecast.Point","AGENCY","Prediction8to28")

training_preds_1to7$UID <- paste0(training_preds_1to7$CREATED_DATE,training_preds_1to7$AGENCY)
training_preds_8to28$UID <- paste0(training_preds_8to28$CREATED_DATE,training_preds_8to28$AGENCY)


### Create Local DF to be sent for Scoring

# Creates the file structure that the time series deployment expects to returns predictions
# This local df can be used for the 1 to 7 and the 8 to 28 day deployments
# These two models were trained with a -35 to -4 day feature derivation window
# You must provide at least 35 days of history, but the most recent 3 days are not required

unknown28 <- data.frame("AGENCY" = c(rep("HPD",28), rep("NYPD",28),rep("DOT",28),rep("DPR",28),rep("DSNY",28)),
                        "CREATED_DATE" = rep(seq.Date(from = date_today+1, by = 1, length.out = 28),5),
                        "CALLS" = rep(NA, 5*28),
                        "SEGMENT" = c(rep("HPD",28), rep("NYPD",28),rep("DOT",28),rep("DPR",28),rep("DSNY",28)))
unknown28$UID <- paste0(unknown28$CREATED_DATE,unknown28$AGENCY)
unknown28 <- rbind(total_known, unknown28) #combine known history with unknown dates being requested
unknown28 <- unknown28[order(unknown28$AGENCY, unknown28$CREATED_DATE),]


### Generate Predictions

# Run predictions for the 1 to 7 day model and the 8 to 28 day model
response7 <- timeseriesPredictionsFromLocalDataframe(unknown28,DEPLOYMENT_ID_1to7)
response28 <- timeseriesPredictionsFromLocalDataframe(unknown28,DEPLOYMENT_ID_8to28)
    

# Pull specific columns out of the response and rename them to make them easier to merge later
predictions7 <- data.frame(AGENCY = response7$seriesId,
                          CREATED_DATE = as.Date(strftime(response7$timestamp, format = "%Y-%m-%d", usetz = FALSE), format = "%Y-%m-%d"),
                          DR7day = response7$prediction,
                          UID = response7$passthroughValues)

predictions28 <- data.frame(AGENCY = response28$seriesId,
                           CREATED_DATE = as.Date(strftime(response28$timestamp, format = "%Y-%m-%d", usetz = FALSE), format = "%Y-%m-%d"),
                           DR28day = response28$prediction,
                           UID = response28$passthroughValues)    


# Define UI for the application
ui <- fluidPage(theme = shinytheme("readable"),

    # Application title
    titlePanel("NYC 311 Call Volume Forecasting"),

    # Sidebar section
    sidebarLayout(
        sidebarPanel(
            
            selectInput("agency", label = h3("Agency selection"), 
                        choices = list("NYPD - Police" = "NYPD", "HPD - Housing" = "HPD", "DOT - Transportation" = "DOT",
                                       "DPR - Parks & Rec" = "DPR", "DSNY - Sanitation" = "DSNY"), 
                        selected = "DOT"),
            
            dateRangeInput("dates", label = h3("Date range"), start = (date_today-90), end = (date_today+28)),
            HTML("<br>"),
            HTML("<br>"),
            
            checkboxGroupInput("checkGroupHistory", label = h3("Historical Predictions to Plot"), 
                               choices = list("Long-term trend" = 2, "Naive model" = 1, "DataRobot ML next 7 days model" = 3, "DataRobot ML days 8 to 28 model" = 4),
                               selected = 2),
            
            checkboxGroupInput("checkGroupFuture", label = h3("Forecasts to Plot"), 
                               choices = list("Long-term trend" = 2, "Naive model" = 1, "DataRobot ML next 7 days model" = 3, "DataRobot ML days 8 to 28 model" = 4),
                               selected = 2),
            
            numericInput("uncertainity", label = h3("Percent Uncertainty"), width = '30%',
                         value = 10, min = 0, max = 50, step = 1
                         )
            
        ),

        # Main section
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
                         HTML("<ul><li>Long-term trend (light grey) - A blend of rolling averages across 1 to 4 week intervals. </li><li>Naive model (light orange) - The average value for the same day of the week across the last 8 weeks. </li><li>DataRobot 7-day model (blue) - Top performing machine learning model across hundreds of experiments focused on accuracy for the next 1 to 7 days. </li><li>DataRobot 28-day model (light blue) - Top performing machine learning model across hundreds of experiments focused on accuracy 8 to 28 days from today.</li></ul>"),
                         HTML("<br>"),
                         h2("When and How these Forecasts were Created"),
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
                tabPanel("Forecast Tables",
                         HTML("<br>"),
                         h2("Forecast Table"),
                         HTML("<br>"),
                         downloadButton("downloadData", "Download"),
                         HTML("<br>"),
                         DTOutput("displayed_forecast_table"),
                         HTML("<br>"),
                         HTML("<br>")
                ),
                tabPanel("Methodology",
                         HTML("<br>"),
                         h2("Purpose"),
                         HTML("This web app is a prototype to demonstrate how time series algorithms can support critical business decisions that hinge on anticipating future volumes. It allows business decision makers to interact with the output of advanced machine learning algorithms in a point-and-click interface."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("Time series forecasting is a specialty area within machine learning that requires different data prepeartion and different modeling approaches  - and therefore different data science skill sets - than traditional machine learning. What makes forecasting hard is that there are often multiple series to forecast that have different underlying patterns and drivers and the data science team is often trying to forecast far into the future."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("In this application we make forecasts for five very different governement agencies as far as 28 days into the future."),
                         HTML("<br>"),
                         h2("Methodology"),
                         HTML("<br>"),
                         h4("Source of the Data"),
                         HTML("The data for this application comes from the NYC OpenData project. As part of that projetc the city of New York has made available data on 311 Service Requests from 2010 to Present. Learn more <a href='https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9'>here</a>."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("The original data set contains calls for about 30 agencies. For this application we have selected 5 that have relatively high call volumes."),
                         HTML("<br>"),
                         HTML("<br>"),
                         h4("Data Prepartion"),
                         HTML("We use the NYC OpenData API service to retrieve new call data daily. The data is restructured to 'long' format so that each agency and each day is one row. We create a unique identifier combining the agency name and the date. This identifier can be used to link the actual call volumes to the volume predictions made in advance."),
                         HTML("<br>"),
                         HTML("<br>"),
                         h4("Machine Learning"),
                         HTML("The algorithms in this prototype were created using <a href = 'http://www.datarobot.com'>DataRobot</a> software."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("DataRobot software evaluated hundreds of algorithms in it’s blueprint repository to find the approach that fits this data set best including ARIMA, Vector Autoregressive Models (VAR), Prophet, AUTOETS (Error-Trend-Seasonal), Ridge Regression, Elastic-Net, Random Forest, Eureqa 'Genetic' Algorithms', eXtreme Gradient Boosted Trees (XGBoost) and Keras Slim Residual Neural Networks."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("All of these techniques require that lagged features are engineered from the data provided. Many of these techniques require a separate model for each forecast distance such as 1 day out, 2 days out, 3 days outs. Many of these techniques also require that long-term trends be removed prior to modeling using differencing techniques. DataRobot software masks all of this complexity and automatically generates the lagged features, pairs the right data preprocessing techniques with the right modeling techniques."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("After examining the top performing algorithms, DataRobot software was used to blend the top 2 to 4 models into a final ensemble model and to calculate the accuracy metrics associated with the final blended model."),
                         HTML("<br>"),
                         HTML("<br>"),
                         h4("Deployment and Making Predictions"),
                         HTML("DataRobot software was used to one-click deploy the blender model to a DataRobot prediction server and to create an API endpoint for batch scoring."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("Each time a user loads the app, a prediction request is sent to the DataRobot real-time API requesting predictions for 1 to 28 days in the future. Those predictions are typically returned in a few milliseconds."),
                         HTML("<br>"),
                         HTML("<br>"),
                         h2("Questions"),
                         HTML("Questions about this work should be directed to Andrea Kropp (andrea.kropp@datarobot.com) or Greg Ricker (greg.ricker@datarobot.com)."),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<br>"),
                         HTML("<i>Thank you for viewing this prototype.</i>")
 
                         )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {

    #Known data for the AGENCY selected
    agency_known <- reactive({
        total_known[total_known$AGENCY == input$agency,]
    })
    
    training_known_1to7 <- reactive({
        df <- training_preds_1to7[training_preds_1to7$AGENCY == input$agency,]
        df <- df[df$Forecast.Distance == 1,]
        df
    })
    
    training_known_8to28 <- reactive({
        df <- training_preds_8to28[training_preds_8to28$AGENCY == input$agency,]
        df <- df[df$Forecast.Distance == 8,]
        df
    })
    
    
    #Pulls all the data needed for the line chart into one dataframe
    #This includes the historical known data and all of the forecasts that can be plotted as overlays
    agency_line_chart_data_all <- reactive({
        
        data <- agency_known() #known data
        
        future_rows_needed <- 28 + as.numeric((date_today - max(data$CREATED_DATE)))
        
        #blank rows for 28 days into the future 
        future_data <- data.frame("AGENCY" = rep(input$agency, future_rows_needed),
                                  "CREATED_DATE" = seq.Date(from = max(data$CREATED_DATE)+1, by = 1, length.out = future_rows_needed),
                                  "CALLS" = rep(NA, future_rows_needed),
                                  "SEGMENT" = rep(input$agency, future_rows_needed))
        future_data$UID <- paste0(future_data$CREATED_DATE,future_data$AGENCY)
        
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
        
        data$naive1_history <- data$naive1
        data$naive1_future <- data$naive1
        data$naive2_history <- data$naive2
        data$naive2_future <- data$naive2
        
        data$naive1_history[data$CREATED_DATE >= max(total_known$CREATED_DATE)] <- NA
        data$naive1_future[data$CREATED_DATE <= date_today] <- NA
        data$naive2_history[data$CREATED_DATE >= max(total_known$CREATED_DATE)] <- NA
        data$naive2_future[data$CREATED_DATE <= date_today] <- NA
        
        #merge machine learning forecasts from DataRobot
        data <- merge(data, predictions7, by = c("CREATED_DATE","AGENCY","UID"), all.x = TRUE)
        data <- merge(data, predictions28, by = c("CREATED_DATE","AGENCY","UID"), all.x = TRUE)
        
        #merge training predictions
        
        train_1to7 <- training_known_1to7()
        train_8to28 <- training_known_8to28()
        
        data <- merge(data, train_1to7, by = c("CREATED_DATE","AGENCY","UID"), all.x = TRUE)
        data <- merge(data, train_8to28, by = c("CREATED_DATE","AGENCY","UID"), all.x = TRUE)
        
        data$DR7day_low <- data$DR7day*(1 - (input$uncertainity/100))
        data$DR7day_high <- data$DR7day*(1 + (input$uncertainity/100))
        
        data$DR28day_low <- data$DR28day*(1 - (input$uncertainity/100))
        data$DR28day_high <- data$DR28day*(1 + (input$uncertainity/100))
        
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
        
        if ("2" %in% input$checkGroupHistory ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive2_history, name = "Long-term trend", type = 'scatter', mode = 'lines',
                                     hoverinfo = "x+y", line = list(color = '#BAC5CE', width = 2))
        }
        
        if ("1" %in% input$checkGroupHistory ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive1_history, name = "Naive model", type = 'scatter', mode = 'lines',
                                     hoverinfo = "x+y", line = list(color = '#ffb68f', width = 2))
        }
        
        if ("3" %in% input$checkGroupHistory ) {
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~Prediction1to7, name = "DataRobot 1 to 7 day", type = 'scatter', mode = 'lines',
                                     hoverinfo = "x+y", line = list(color = '#2d8fe2', width = 2))
        }
        
        if ("4" %in% input$checkGroupHistory ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~Prediction8to28, name = "DataRobot 8 to 28 day", type = 'scatter', mode = 'lines',
                                     hoverinfo = "x+y", line = list(color = '#8ebcee', width = 2))
        }
        
        if ("2" %in% input$checkGroupFuture ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive2_future, name = "Long-term trend", type = 'scatter', mode = 'lines+markers',
                                     hoverinfo = "x+y", line = list(color = '#BAC5CE', width = 2), marker = list(color = '#BAC5CE'))
        }
        
        if ("1" %in% input$checkGroupFuture ) {
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~naive1_future, name = "Naive model", type = 'scatter', mode = 'lines+markers',
                                     hoverinfo = "x+y", line = list(color = '#ffb68f', width = 2), marker = list(color = '#ffb68f'))
        }
        
        if ("3" %in% input$checkGroupFuture ) {
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR7day_high, name = "High 1 to 7 day", type = 'scatter', mode = 'lines',
                                     line = list(color = 'transparent'), showlegend = FALSE)
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR7day_low, type = 'scatter', mode = 'lines',
                                     fill = 'tonexty', fillcolor='rgba(45,143,226,0.2)', line = list(color = 'transparent'),
                                     showlegend = FALSE, name = 'Low 1 to 7 day') 
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR7day, name = "DataRobot 1 to 7 day", type = 'scatter', mode = 'lines+markers',
                                     hoverinfo = "x+y", line = list(color = '#2d8fe2', width = 2), marker = list(color = '#2d8fe2'))
            
        }
        
        if ("4" %in% input$checkGroupFuture ) {
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR28day_high, name = "High 1 to 7 day", type = 'scatter', mode = 'lines',
                                     line = list(color = 'transparent'), showlegend = FALSE)
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR28day_low, type = 'scatter', mode = 'lines',
                                     fill = 'tonexty', fillcolor='rgba(142,188,238,0.2)', line = list(color = 'transparent'),
                                     showlegend = FALSE, name = 'Low 1 to 7 day') 
            
            fig <- fig %>% add_trace(x = ~CREATED_DATE, y = ~DR28day, name = "DataRobot 8 to 28 day", type = 'scatter', mode = 'lines+markers',
                                     hoverinfo = "x+y", line = list(color = '#8ebcee', width = 2), marker = list(color = '#8ebcee'))
        }
        
        
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
                                       y0 = 0, y1 = ymax, x0 = training_date, x1 = training_date,
                                       line = list(color = "#777777", dash="dot"))
                              )
        )
        
        fig
        
    })
    
    
    
    
    forecast_table_data <- reactive({
        data <- agency_line_chart_data_all()
        
        data <- data[data$AGENCY == input$agency & data$CREATED_DATE > date_today,] #limit by agency and date after today

        data <- data[,c(2,1,24,25)]
        data$Low_Estimate <- (1-(input$uncertainity/100)) * rowSums(data[,c(3,4)], na.rm=TRUE)
        data$High_Estimate <- (1 + (input$uncertainity/100)) * rowSums(data[,c(3,4)], na.rm=TRUE)
        
        colnames(data) <- c("Agency","Date Forecasted","Short-term Model Forecast", "Long-term Model Forecast", "Low Estimate","High Estimate")
        
        data
    })
    
    
    output$displayed_forecast_table <- renderDT({
        
        data <- forecast_table_data()
        
        datatable(data, 
                  colnames = c("Agency","Date Forecasted","Short-term Model Forecast", "Long-term Model Forecast", "Low Estimate","High Estimate"),
                  rownames= FALSE, 
                  options = list(
                      dom = 't',
                      autoWidth = FALSE,
                      pageLength = 50,
                      lengthMenu = c(20, 50, 100, 200))
        ) %>%
            formatRound('Short-term Model Forecast', 0) %>%
            formatRound('Long-term Model Forecast', 0) %>%
            formatRound('Low Estimate', 0) %>%
            formatRound('High Estimate', 0)
        
    })
    
    output$model_train_date <- renderText({
            paste0("Machine learning models were last retrained using data up to ", training_date," as indicated by the dotted vertical line.")
    })

    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Call forecast ",input$agency, " created ",date_today,".csv", sep = "")
        },
        content = function(file) {
            write.csv(forecast_table_data(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
