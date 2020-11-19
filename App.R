library(readr)
library(shinythemes)
library(tidyverse)
library(rccdates)
library(epitools)


#Loading In the Two Datasets
IPO <- read_csv("IPOList.csv")
StockData<-read_csv("StockDetails.csv")%>%
  mutate(Date=as.Date(Date, format="%b%d,%Y"))

#--------------------------------------------------------------------------------------------------------------------------------------------------

#Creating vectors of all the different regions
Countires<-c("Canada","China","Switzerland","Brazil","Belgium","Bermuda","Ireland",
             "Israel","Sweden","Sweden","Mexico","the United Arab Emirates",
             "Uruguay", "Greece","the Netherlands","Colombia","Denmark","Taiwan",
             "India","Chile","Argentina","Australia","Puerto Rico","Indonesia",
             "Netherlands","France","South Africa","Cayman Islands","Germany",
             "Japan","Monaco","Peru","South Korea","Spain","Marshall Islands",
             "Cyprus","the United Kingdom","United Kingdom","Norway","the Bahamas",
             "Russia","Turkey","The Netherlands","US Virgin Islands")
West<-c("CA","OR","WA","ID","UT","AZ","NV","MT","WY","CO","NM")
Midwest<-c("ND","SD","NE","MN","WI","KS","MO","IL","IA","MI","OH","IN")
South<-c("TX","OK","AR","LA","MS","AL","TN","KY","WV","GA","FL","NC","VA","MD","DE","DC","SC")
Northeast<-c("PA","NY","VT","NH","ME","MA","CT","NJ","RI")

#Filtering histogram, organizing into regions and getting rid of missing data
IPOHist <- IPO %>%
  filter(stateCountry != "NYArconic") %>%
  filter(stateCountry != "N/A") %>%
  filter(stateCountry != "HI") %>%
  filter(stateCountry != "AK") %>%
  filter(Sector != "N/A") %>%
  filter(Industry != "N/A") %>%
  filter(Revenue != "N/A") %>%
  mutate(Year = as.character(Year), 
         Region = ifelse(stateCountry %in% Countries, "International",
                         ifelse(stateCountry %in% West, "West",
                                ifelse(stateCountry %in% Midwest, "Midwest",
                                       ifelse(stateCountry %in% South,"South",
                                              ifelse(stateCountry %in% Northeast, "Northeast",0))))))
                            
                          
#Creating Abbreviations of the Month
IPOHist$Month <- month.abb[IPOHist$Month]


#Histogram Variables Chosen
categories <- as.list(names(IPOHist)[c(12, 19, 5:6)])
categoryNames <- c("Sector", "Region","Year", "Month")
names(categoryNames) <- categoryNames

#Sector filter
div_choices <- (unique(IPO$Sector))

#List of all stocks for trend line
line_choices<-(unique(StockData$Stock))

#----------------------------------------------------------------------------------------------------------------------------------------------
# ui 

ui <- fluidPage(
  
  h1("Analysis of IPOs"),
  
  #Three SelectInputs: One for Variable of Interest for Bar Graph, 
  #one for filtering based on sector, and one for choosing which stock for trend line
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "hist", 
                  label = "Choose a Category Variable of Interest: (For Bar Graph)", 
                  choices = categories, 
                  selected = "Sector"),
      selectInput(inputId = "div"
                  , label = "Include Sectors: (For Bar Graph)"
                  , choices = c("All", div_choices)
                  , selected = "All"),
      selectInput(inputId = "line"
                  , label = "Choose an IPO (For Stock Trend Line):"
                  , choices = (line_choices)
                  , selected = "ACIA")
    ),
    
    #Naming the different tabs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Bar Graph", plotOutput(outputId = "barG")),
                  tabPanel("Stock Trends", plotOutput(outputId = "Line"))))
  )
)

#-------------------------------------------------------------------------------------------------------------------------------------------------
# server
#Use_data checks whether the div is "All"; if it is not, filter the data by the
#sector to ensure that that sector is the only one in the scatterplot
server <- function(input,output){
  
  use_data <- reactive({
    data<-IPOHist
    req(input$div)
    if(input$div!="All"){
      data<-data%>%
        filter(Sector==input$div)
    }else{
      data<-data
    }
  })
  #Use high and lows dependent on slected stock
  use_data2 <- reactive({
    data<-StockData%>%
      filter(Stock==input$line)
  })
  
  #Creating a bar graph dependent on the predictor variable input  
  output$barG <- renderPlot({
    ggplot(data = use_data(), aes_string(x = input$hist, fill = input$hist)) +
      geom_bar() +
      labs(x = categoryNames[categories == input$hist], 
           y = "Count") +
      ggtitle(aes("IPO's BY CATEGORY (1996 - 2018)")) +
      coord_flip()
  })
  
  #Creating a time series of the Date and price of the stock  
  output$Line <- renderPlot({
    ggplot(data = use_data2(), aes(x = Date, y = as.numeric(Volume), color=HighLow)) +
      geom_line() +
      labs(x = "Date"
           , y = "Price of IPO Stock")
  })
  
}

# call to shinyApp
shinyApp(ui = ui, server = server)
