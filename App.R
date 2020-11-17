library(readr)
library(shinythemes)
library(tidyverse)
library(rccdates)
library(epitools)

#Loading In the Two Datasets

path_in <- "/Users/zachostrow/Desktop/git/Blog-MoneyMovers"

IPO <- read_csv(paste0(path_in,"/IPOList.csv"))
StockData<-read_csv(paste0(path_in,"/StockDetails.csv"))%>%
  mutate(Date=as.Date(Date, format="%b%d,%Y"))

#--------------------------------------------------------------------------------------------------------------------------------------------------

#Will: in order to have a better presentation of the data, I created a new variable called "Region" and categorized that by the state or country
#the company was based in. I also filtered out the "N/A" data points in the set rather than outliers like in Zach's case, because the bar graph
#is looking at one qualitative variable rather than something quantitative like Revenue or Net Income.
IPOHist <- IPO %>%
  filter(stateCountry != "NYArconic") %>%
  filter(stateCountry != "N/A") %>%
  filter(stateCountry != "HI") %>%
  filter(stateCountry != "AK") %>%
  filter(Sector != "N/A") %>%
  filter(Industry != "N/A") %>%
  filter(Revenue != "N/A") %>%
  mutate(Year = as.character(Year), 
         Region = ifelse(stateCountry == "Canada" |
                           stateCountry == "China" |
                           stateCountry == "Switzerland" |
                           stateCountry == "Brazil" |
                           stateCountry == "Belgium" |
                           stateCountry == "Bermuda" |
                           stateCountry == "Ireland" |
                           stateCountry == "Israel" |
                           stateCountry == "Sweden" |
                           stateCountry == "Mexico" |
                           stateCountry == "the United Arab Emirates" |
                           stateCountry == "Uruguay" |
                           stateCountry == "Greece" |
                           stateCountry == "the Netherlands" |
                           stateCountry == "Colombia" |
                           stateCountry == "Denmark" |
                           stateCountry == "Taiwan" |
                           stateCountry == "India" |
                           stateCountry == "Chile" |
                           stateCountry == "Argentina" |
                           stateCountry == "Australia" |
                           stateCountry == "Puerto Rico" |
                           stateCountry == "Indonesia" |
                           stateCountry == "Netherlands" |
                           stateCountry == "France" |
                           stateCountry == "South Africa" |
                           stateCountry == "Cayman Islands" |
                           stateCountry == "Germany" |
                           stateCountry == "Japan" |
                           stateCountry == "Monaco" |
                           stateCountry == "Peru" |
                           stateCountry =="South Korea" |
                           stateCountry == "Spain" |
                           stateCountry == "Marshall Islands" |
                           stateCountry == "Cyprus" |
                           stateCountry == "the United Kingdom" |
                           stateCountry == "United Kingdom" |
                           stateCountry == "Norway" |
                           stateCountry == "the Bahamas" |
                           stateCountry == "Russia" |
                           stateCountry == "Turkey" |
                           stateCountry == "The Netherlands" |
                           stateCountry == "US Virgin Islands",
                         "International",
                         ifelse(stateCountry == "CA" |
                                  stateCountry == "OR" |
                                  stateCountry == "WA" |
                                  stateCountry == "ID" |
                                  stateCountry == "UT" |
                                  stateCountry == "AZ" |
                                  stateCountry == "NV" |
                                  stateCountry == "MT" |
                                  stateCountry == "WY" |
                                  stateCountry == "CO" |
                                  stateCountry == "NM",
                                "West",
                                ifelse(stateCountry == "ND" |
                                         stateCountry == "SD" |
                                         stateCountry == "NE" |
                                         stateCountry == "MN" |
                                         stateCountry == "WI" |
                                         stateCountry == "KS" |
                                         stateCountry == "MO" |
                                         stateCountry == "IL" |
                                         stateCountry == "IA" |
                                         stateCountry == "MI" |
                                         stateCountry == "OH" |
                                         stateCountry == "IN",
                                       "Midwest",
                                       ifelse(stateCountry == "TX" |
                                                stateCountry == "OK" |
                                                stateCountry == "AR" |
                                                stateCountry == "LA" |
                                                stateCountry == "MS" |
                                                stateCountry == "AL" |
                                                stateCountry == "TN" |
                                                stateCountry == "KY" |
                                                stateCountry == "WV" |
                                                stateCountry == "GA" |
                                                stateCountry == "FL" |
                                                stateCountry == "NC" |
                                                stateCountry == "VA" |
                                                stateCountry == "MD" |
                                                stateCountry == "DE" |
                                                stateCountry == "DC" |
                                                stateCountry == "SC",
                                              "South",
                                              ifelse(stateCountry == "PA" |
                                                       stateCountry == "NY" |
                                                       stateCountry == "VT" |
                                                       stateCountry == "NH" |
                                                       stateCountry == "ME" |
                                                       stateCountry == "MA" |
                                                       stateCountry == "CT" |
                                                       stateCountry == "NJ" |
                                                       stateCountry == "RI",
                                                     "Northeast", 0)
                                       )
                                )
                         )
         )
  )
#Creating Abbreviations of the Month
IPOHist$Month <- month.abb[IPOHist$Month]


#Histogram
categories <- as.list(names(IPOHist)[c(12, 19, 5:6)])
categoryNames <- c("Sector", "Region","Year", "Month")
names(categoryNames) <- categoryNames
div_choices <- (unique(IPO$Sector))
line_choices<-(unique(StockData$Stock))

#----------------------------------------------------------------------------------------------------------------------------------------------
# ui 

ui <- fluidPage(
  
  h1("Analysis of IPOs"),
  
  #Three SelectInputs: One for predictor variable, one for response variable,
  #and One for Choosing which sectors to include
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "hist", 
                  label = "Choose a Category Variable of Interest:", 
                  choices = categories, 
                  selected = "Sector"),
      selectInput(inputId = "div"
                  , label = "Include Sectors:"
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
                  tabPanel("Stock Trends", plotOutput(outputId = "Line")),
                  tabPanel("SpatialMap", tableOutput(outputId = "Map"))))
  )
)

#-------------------------------------------------------------------------------------------------------------------------------------------------
# server
#Use_data checks whether the div is "All"; if it is not, filter the data by the
#sector to ensure that that sector is the only one in the scatterplot
server <- function(input,output){
  
  #Use data 2 does same thing but with different dataset for bar graphs 
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
  
  #Creating a scatterplot depending on the predictor and response varaible input  
  output$Line <- renderPlot({
    ggplot(data = use_data2(), aes(x = Date, y = as.numeric(Volume), color=HighLow)) +
      geom_line() +
      labs(x = "Date"
           , y = "Price of IPO Stock")
  })
  

  #In addition to response and predictor variable inputs, also gives info on
  #Name, symbol, sector, and other present day info that was webscraped
}

# call to shinyApp
shinyApp(ui = ui, server = server)
