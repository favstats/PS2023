
library(tidyverse)

didyoudoit <- read_lines("youdidit.txt")

if(didyoudoit != "youdidit"){
  
library(RSelenium)

  # https://adstransparency.google.com/advertiser/AR09355418985304162305?political&region=NL&preset-date=Last%207%20days
  
  
  library(netstat)
  library(RSelenium)
  # port <- netstat::free_port()
  podf <- sample(4000L:5000L,1)
  rD <- rsDriver(browser = "firefox"
                 ,chromever=NULL
                 ,check = F
                 ,port = podf
                 ,verbose = T
  )
  
  
  library(rvest)
  
  remDr <- rD$client
  
  remDr$navigate("https://www.facebook.com/ads/library/report/")
  
  
  thth <- remDr$getPageSource() %>% .[[1]] %>% rvest::read_html()
  
  
  still14th <- thth %>% 
    str_detect("14 Mar")
  
  if(still14th){
    # remDr$closeServer()
    remDr$closeall()
    
  } else {
    source("provincies.R")
    write_lines("youdidit","youdidit.txt")
  }
  
} else {
  print("its been done")
}
