
library(RSelenium)

# https://adstransparency.google.com/advertiser/AR09355418985304162305?political&region=NL&preset-date=Last%2030%20days


library(netstat)
library(RSelenium)
port <- netstat::free_port()
podf <- sample(4000L:5000L,1)
rD <- rsDriver(browser = "firefox"
                    ,chromever=NULL
                ,check = F
                ,port = podf
                ,verbose = T
)


library(rvest)

remDr <- rD$client


# ggl_spend

retrieve_spend <- function(id) {

    # id <- "AR18091944865565769729"
    url <- glue::glue("https://adstransparency.google.com/advertiser/{id}?political&region=NL&preset-date=Last%2030%20days")
    remDr$navigate(url)

    Sys.sleep(1)

    thth <- remDr$getPageSource() %>% .[[1]] %>% read_html()

    Sys.sleep(3)
    
    root5 <- "/html/body/div[3]" 
    root3 <- "/html/body/div[5]" 
    ending <- "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"

    try({
      insights <<- remDr$findElement(value = paste0(root5, ending))
      it_worked <- T
    })
    
    if(!exists("it_worked")){
      
      print("throwed an error")
      
      try({
        insights <<- remDr$findElement(value = paste0(root3, ending))
        
      })
      
      root <- root3
      
    } else {
      root <- root5
    }
    
    print("click now")
    insights$clickElement()

    Sys.sleep(1)

    pp <- remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    ending_eur <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
    ending_ads <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"
    
    print("retrieve numbers")
    # try({
    eur_amount <- pp %>%
        html_elements(xpath = paste0(root, ending_eur)) %>%
        html_text()
    
    num_ads <- pp %>%
        html_elements(xpath = paste0(root, ending_ads)) %>%
        html_text()
    
    # })
    
    fin <- tibble(advertiser_id = id, eur_amount, num_ads)
    
    print(fin)

    return(fin)

}

ggl_spend <- readRDS("data/ggl_spend.rds")

# retrieve_spend(unique(ggl_spend$Advertiser_ID)[1])
# fvd <- retrieve_spend("AR09355418985304162305")



ggl_sel_sp <- unique(ggl_spend$Advertiser_ID) %>%
    map_dfr_progress(retrieve_spend)

# ggl_sel_sp %>% 
  # filter(advertiser_id %in% "AR09355418985304162305")
# 
# # ggl_spend %>% 
#   # filter(Advertiser_ID %in% "AR09355418985304162305")
# 
# ggl_sel_sp$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)
#   filter(!(advertiser_id %in% unique(ggl_spend$Advertiser_ID)))

# ggl_sel_sp <- ggl_sel_sp %>% 
  # bind_rows(fvd) %>% 
  # distinct(advertiser_id, .keep_all = T)


saveRDS(ggl_sel_sp, file = "data/ggl_sel_sp.rds")

