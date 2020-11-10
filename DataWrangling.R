library(tidyverse) 
library(rvest)


#Full Data Can be Found in Git Repo, May Need to Adjust Path for it to work
#For a different User
path_in <- "/Users/zachostrow/Desktop/git/Blog-MoneyMovers"
IPOData <- read_csv(paste0(path_in,"/IPOFullData.csv"))
#Creating and Selecting the desired Columns
AllIPOs<-IPOData%>%
  mutate(closeDay_Mean = rowMeans(select(IPOData, starts_with("closeDay")), 
                                  na.rm = TRUE))%>%
  mutate(openDay_Mean = rowMeans(select(IPOData, starts_with("openDay")), 
                                 na.rm = TRUE))%>%
  mutate(highDay_Mean = rowMeans(select(IPOData, starts_with("highDay")), 
                                 na.rm = TRUE))%>%
  mutate(lowDay_Mean = rowMeans(select(IPOData, starts_with("lowDay")), 
                                na.rm = TRUE))%>%
  select(Symbol, DaysBetterThanSP, daysProfit, daysProfitGrouped, Year, Month,
         closeDay_Mean, highDay_Mean, openDay_Mean, lowDay_Mean,MarketCap,
         Sector, Industry, stateCountry, Revenue, netIncome, employees, Name) 

#Uninterested in Companies Which Are Funds or ETFs
FilteredIPOs<-AllIPOs%>%
  mutate(
    isIPO=case_when(
      str_detect(Name,"Fund") ~ FALSE,
      str_detect(Name,"ETF") ~ FALSE,
      TRUE ~ TRUE
    ), 
    employees=parse_number(employees),
    Revenue=case_when(
      str_detect(Revenue, "M")~parse_number(Revenue)*1000000,
      str_detect(Revenue, "B")~parse_number(Revenue)*1000000000,
      TRUE~parse_number(Revenue)
    ),
    netIncome=case_when(
      str_detect(netIncome, "M")~parse_number(netIncome)*1000000,
      str_detect(netIncome, "B")~parse_number(netIncome)*1000000000,
      TRUE~parse_number(netIncome)
    )
  )%>%
  filter(isIPO)%>%
  select(-isIPO)
#There will be some NAs found in this data. However, since we want to keep the
#stock for other graphs, we instead chose to keep those rows that contain these 
#NA's, rather than lose the other data found in the rest of the row


####POSSIBLY COULD CHOOSE RANDOM 200
set.seed(100)
RandomIPOs<-FilteredIPOs[sample(nrow(FilteredIPOs), 200), ]
ChosenIPOs<-RandomIPOs

#Data Frame which helps make transporting data easier
#####ASK HOW TO ADD CERTAIN POINTS IN THE MOST EFFICIENT WAY
StockPrices<-data.frame(matrix(ncol = 7, nrow = 0))
colnames(StockPrices)<- c("Date","Open","High","Low","Close*",
                          "Volume","Stock")

#For Loop to Webscrape, then add webscraped data to Added Characteristics
for(i in 1:1){
  url<-paste("https://finance.yahoo.com/quote/",ChosenIPOs$Symbol[i],
             "/history?p=",ChosenIPOs$Symbol[i], sep="")
  
  tables<-url%>%
    read_html()%>%
    html_nodes("table")
  tables<-html_table(tables[[1]])
  
  IPOStockInfo<-tables%>%
    mutate(Stock=ChosenIPOs$Symbol[i])%>%
    select(-`Adj Close**`)
  StockPrices<-rbind(StockPrices, IPOStockInfo)
  
}

#Added New Data to Specific IPOs
SpecificIPOs<-inner_join(SpecificIPOs, AddedCharacteristics, by="Symbol")

#Might Need to Change Out Path Depending On Who is Coding
out_path<-"/Users/zachostrow/Desktop/git/Shiny-MoneyMovers"
write_csv(SpecificIPOs, paste0(out_path, "/SpecificIPOs.csv"))
write_csv(FilteredIPOs, paste0(out_path, "/FilteredIPOs.csv"))