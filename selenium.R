
library(RSelenium)

# https://adstransparency.google.com/advertiser/AR09355418985304162305?political&region=NL&preset-date=Last%2030%20days


library(netstat)
library(RSelenium)
port <- netstat::free_port()
rD <- rsDriver(browser = "firefox"
                    ,chromever=NULL
                ,check = F
                ,port = port
                ,verbose = T
)


library(rvest)

remDr <- rD$client


ggl_spend

retrieve_spend <- function(id) {


    url <- glue::glue("https://adstransparency.google.com/advertiser/{id}?political&region=NL&preset-date=Last%2030%20days")
    remDr$navigate(url)

    Sys.sleep(1)

    thth <- remDr$getPageSource() %>% .[[1]] %>% read_html()

    Sys.sleep(3)

    insights <- remDr$findElement(value = "/html/body/div[5]/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple")

    insights$clickElement()

    Sys.sleep(1)

    pp <- remDr$getPageSource() %>% .[[1]] %>% read_html()

    eur_amount <- pp %>%
        html_elements(xpath = "/html/body/div[5]/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div") %>%
        html_text()

    num_ads <- pp %>%
        html_elements(xpath = "/html/body/div[5]/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div") %>%
        html_text()



    return(tibble(advertiser_id = id, eur_amount, num_ads))

}

ggl_spend <- readRDS("data/ggl_spend.rds")

ggl_sel_sp <- unique(ggl_spend$Advertiser_ID) %>%
    map_dfr_progress(retrieve_spend)

saveRDS(ggl_sel_sp, file = "data/ggl_sel_sp.rds")

