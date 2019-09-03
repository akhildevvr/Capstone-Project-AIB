library(shiny)
library(sp)
library(rgdal)
library(geojson)
library(geojsonio)
library(leaflet)
library(dplyr)
library(plotly)
library(listviewer)
library(shinydashboard)


datac<-read.csv("counties-no-geom.csv" , header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))
newdata8<- read.csv("RPZdata.csv" , header = TRUE, stringsAsFactors = TRUE, na.strings = c("", "NA"))
newdata8$Date.of.Purchase<- as.Date(as.character(newdata8$Date.of.Purchase), format = "%Y-%m-%d")
#newdata8$dateyear<-as.factor(newdata8$dateyear)
newdata8<-newdata8[,-c(1,2)]
newdata8$ID<- seq.int(nrow(newdata8))
newdata8<-na.omit(newdata8)
newdata8<-newdata8[newdata8$Price > quantile(newdata8$Price, .25) - 1.5*IQR(newdata8$Price) & 
                     newdata8$Price < quantile(newdata8$Price, .75) + 1.5*IQR(newdata8$Price), ]
b<-newdata8 %>% group_by(County) %>% summarise(Count = n())%>% arrange(desc(Count)) %>% top_n(5)

#b<-head(newdata8 %>% group_by(County) %>% summarise(Count = n())%>% arrange(desc(Count)),5)
postcodes <- "map.geojson"
states <- rgdal::readOGR(dsn = postcodes,
                         #layer = "OGRGeoJSON",
                         require_geomType = "wkbPolygon",
                         encoding = "UTF-8")
#states <- geojsonio::geojson_read("dataMap.geojson", what = "sp")
bins <- c(5, 10, 15, 20, 25, Inf)
pal <- colorBin("YlGn", domain = as.numeric(states$Price), bins = bins)

labels <- sprintf(
  "<strong>%s</strong>",
  states$NAME_TAG
) %>% lapply(htmltools::HTML)



header<-dashboardHeader(title = "Ireland House Price Prediction")
sidebar<-dashboardSidebar(
  selectInput(inputId = "County",
            label = "County: ",
            choices = c(     "Dublin",
                             "Limerick",
                             "Galway",
                             "Waterford",
                             "Louth",
                             "Cavan",
                             "Longford",
                             "Cork",
                             "Kerry",
                             "Donegal",
                             "Wicklow",
                             "Westmeath",
                             "Roscommon",
                             "Meath",
                             "Kildare",
                             "Offaly",
                             "Tipperary",
                             "Carlow",
                             "Sligo",
                             "Wexford",
                             "Monaghan",
                             "Kilkenny",
                             "Clare",
                             "Mayo",
                             "Laois"),selected = "Cork"),


  sidebarMenu(
    menuItem("Choropleth Map", tabName = "ModelParameters"),
    menuItem("Year Wise Summary", tabName = "Top"),
    menuItem("County Wise Summary", tabName = "Summary"),
    menuItem("Actual Vs Predicted", tabName = "ActualVsPredicted"),
    menuItem("Rent Pressure Zones", tabName = "Plot")
    
    
  
  
)
)
body<-dashboardBody(
tabItems(
  tabItem("ModelParameters",
        fluidPage(
          fluidRow(
           
            box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Predicted Sale Price",
              textOutput("summary")
            ),
            
          
            
            box(
              width = 4, status = "info", solidHeader = TRUE,
              title = "Actual Sale Price",
              textOutput("summary_sale")
            )
          ),
          fluidRow(
            box(
              width = 15, status = "info",
              title = "Choropleth Map of Ireland",
              leafletOutput("mymap",width = "100%", height = 600))
            )
        )
  ),
         
  tabItem("Top",
          fluidRow(
            box(
              width = 12, status = "info",
              title = "House Sale Price Summary ",
              plotlyOutput("top"))
            
          )),
  
  tabItem("Plot",
        fluidPage(
          fluidRow(
            
            box(
              width = 15, status = "info",
              title = "House Sale Price VS Rent Pressure Zone ",
              plotlyOutput("RPZ",width = "100%",height = 500))
            )
          
    
            
      )
      
  ),
          
          
tabItem("ActualVsPredicted",
      fluidPage( 
        fluidRow(
          box(
            
            width = 15, status = "info",
            title = "Actual Vs Predicted",
            plotlyOutput("AVP",width = "100%", height = 600))
          )
      )
    ),
tabItem("Summary",
      fluidPage(
        fluidRow(
          box(
            width = 15, status = "info",
            title = "County Summary",
            plotlyOutput("sum",width = "100%", height = 600))
          )
      )
    )



  )
)




ui<-dashboardPage(header,sidebar, body, skin = "blue")


#names(fit$coefficients) <- c("Intercept", input$var2)
#summary(fit)     
server <- function(input, output){
  
  
  
  
  
  
  a<- reactive({
    newdata8%>%filter(County==input$County)})
  
  output$summary <- reactive({
    progress<-shiny::Progress$new()
    progress$set(message='Predicting',value=0)
    on.exit(progress$close())
    model<-lm(a()$Price ~ a()$Number.of.Bathrooms+a()$No.of.Bedrooms+a()$Description.of.Property+a()$latitude+a()$longitude+a()$VAT.Exclusive+a()$dateyear+a()$month+a()$day+a()$Rent.Pressure.Zones)
    progress$set(message='finalizing',value=0.5)
    #a()$predicted<-model$fitted.values
    format(round(model$fitted.values[[1]],2),nsmall = 2)
    
  })
  
  Regression<-reactive({
    model1<-lm(a()$Price~a()$Number.of.Bathrooms)
    
  })
  
  output$summary_sale <- reactive({
    format(round(mean(a()$Price),2),nsmall = 2)})
  output$RPZ <- renderPlotly({
    
    p<-ggplot(data = a(), aes(x = Date.of.Purchase, y = Price, group =Rent.Pressure.Zones ))+
      geom_smooth(method = "lm", size = 0.5, aes(colour = Rent.Pressure.Zones))
    p+ggtitle("Price Vs Rent Pressure Zones")
    p<- p + labs(y = "Price (Euro)")
    ggplotly(p)
    
      
      
    
  })
  output$sum<-renderPlotly({
    
    
    s <- schema()
    
    agg <- s$transforms$aggregate$attributes$aggregations$items$aggregation$func$values
    l = list()
    for (i in 1:length(agg)) {
      ll = list(method = "restyle",
                args = list('transforms[0].aggregations[0].func', agg[i]),
                label = agg[i]) 
      l[[i]] = ll
    }
    p <-  plot_ly( type = 'bar', x = a()$County, y = a()$Price, mode = 'markers', marker = list(size = 20,color = a()$dateyear, opacity = 0.8), transforms = list(list( type = 'aggregate', groups = a()$dateyear,aggregations = list(list(target = 'y', func = 'avg', enabled = T) ) ) )) %>%layout(title = '<b> Aggregations </b><br>Use Dropdown to change aggregation<br><b>  </b>',  xaxis = list(title = 'Year'), yaxis = list(title = 'Price (Euro)'),     updatemenus = list(list(x = 0.2,y = 1.2, xref = 'paper', yref = 'paper',yanchor = 'top', buttons = l )))
    p
  })
  
  output$AVP<- renderPlotly({
    
    model<-lm(a()$Price ~ a()$Number.of.Bathrooms+a()$No.of.Bedrooms+a()$Description.of.Property+a()$latitude+a()$longitude+a()$VAT.Exclusive+a()$dateyear+a()$month+a()$day+a()$Rent.Pressure.Zones)
    r<-ggplot(data = a(), aes(x = Date.of.Purchase))+
       geom_smooth(aes( y = a()$Price,color = "Actual"),method = "lm", size = 0.5) + geom_smooth(aes(color = "Predicted",y= model$fitted.values ))
    r <- r + labs(y = "Price")
    ggplotly(r)
    
    
    #r <- plot_ly(a(), x = ~Date.of.Purchase, y = ~Price,color = , name = 'Actual', type = 'scatter', mode = 'lines')%>% 
          #add_trace(y = ~model$fitted.values, name = 'Predicted', mode = 'lines+markers')
  })
  
  
  output$top<-renderPlotly({
    
    p <- a() %>%ggplot( aes(x=as.factor(dateyear), y=Price, fill= as.factor(dateyear))) +
      geom_violin(trim=TRUE) + labs(fill = "Year")
    p<- p + geom_boxplot(width= 0.5)
    p<- p + labs(x = "Year", "Price (Euro)")
    ggplotly(p)
      
    
  })
  
  output$mymap <- renderLeaflet({
    progress<-shiny::Progress$new()
    progress$set(message='stage1',value=0)
    on.exit(progress$close()) 
    town <- datac[datac$NAME_TAG == input$County,]
    town_lon <- town$LONGITUDE[1]
    town_lat <- town$LATITUDE[1]
    progress$set(message='stage2',value=0.5)
    leaflet(states) %>% addTiles() %>%
      setView(town_lon, town_lat, 8) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(as.numeric(Price)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addMarkers(~town$LONGITUDE, ~town$LATITUDE, popup = ~as.character(town$NAME_TAG), label = ~as.character(town$NAME_TAG)) %>%
      addLegend(pal = pal, values = ~Price, opacity = 0.7, title = NULL,
                position = "bottomright")
    
    
  })    
}

shinyApp(ui, server)  


