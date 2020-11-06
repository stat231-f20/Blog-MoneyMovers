# code to scroll down yahoo finance webpage for MMM to get all data
# had to update Chrome to latest version

library(tidyverse)
library(rvest)
library(RSelenium)

rD <- rsDriver(port = 4837L, browser = "chrome")
remDr <- rD$client
remDr$navigate("https://finance.yahoo.com/quote/MMM/history?p=MMM")

for(i in 1:5){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  Sys.sleep(5)    
}

#get the page html
page_source <- remDr$getPageSource()

# scrape it
MMM_data <- (read_html(page_source[[1]]) %>% 
  html_nodes("table") %>% 
  html_table)[[1]] 
 
# this will close the driver/browser
#remDr$close()

# confirming last date (Oct 6, 2019) is in the dataset
tail(MMM_data)
tail(MMM_data$Date)

# resources:
# https://stackoverflow.com/questions/52394733/using-rselenium-to-open-chrome-browser-getting-unable-to-create-new-service-c
# https://www.rdocumentation.org/packages/RSelenium/versions/1.7.7/topics/rsDriver
# https://github.com/NanWen-CU/Web-Scraping-Scrolling-Website-Using-R/blob/master/Script
