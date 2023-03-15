source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
library(tidyverse)

tstamp <- Sys.time()

more_data <- dir("data/reports", full.names = T) %>%
    map_dfr(~read_csv(.x) %>% mutate(path = .x)) %>%
    mutate(date_produced = str_remove_all(path, "data/reports/FacebookAdLibraryReport_|_NL_yesterday_advertisers\\.csv")) %>%
    mutate(date_produced = lubridate::ymd(date_produced)) %>%
    janitor::clean_names()%>% #rename(advertiser_id = page_id) %>%
    mutate(spend = readr::parse_number(amount_spent_eur)) %>%
    mutate(spend = ifelse(spend == 100, 50, spend)) %>%
    distinct(page_id, .keep_all = T)  %>%
    mutate(party1 = case_when(
        str_detect(page_name, "VVD") ~ "VVD",
        str_detect(page_name, "\\bCDA\\b") ~ "CDA",
        str_detect(page_name, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(page_name, "D66|Jonge Democraten") ~ "D66",
        str_detect(page_name, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(page_name, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(page_name, "\\bSP\\b") ~ "SP",
        str_detect(page_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(page_name, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(page_name, "\\bSGP\\b") ~ "SGP",
        str_detect(page_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(page_name, "PVV") ~ "PVV",
        str_detect(page_name, "DENK") ~ "DENK",
        str_detect(page_name, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(page_name, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(page_name, "BVNL") ~ "BVNL",
        str_detect(page_name, "Ja21") ~ "JA21",
        str_detect(page_name, "Alliantie") ~ "Alliantie",
        str_detect(page_name, "BBB") ~ "BBB",
        T ~ NA_character_
    )) %>%
    mutate(party2 = case_when(
        str_detect(disclaimer, "VVD") ~ "VVD",
        str_detect(disclaimer, "\\bCDA\\b") ~ "CDA",
        str_detect(disclaimer, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(disclaimer, "D66|Jonge Democraten") ~ "D66",
        str_detect(disclaimer, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(disclaimer, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(disclaimer, "\\bSP\\b") ~ "SP",
        str_detect(disclaimer, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(disclaimer, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(disclaimer, "\\bSGP\\b") ~ "SGP",
        str_detect(disclaimer, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(disclaimer, "PVV") ~ "PVV",
        str_detect(disclaimer, "DENK") ~ "DENK",
        str_detect(disclaimer, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(disclaimer, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(disclaimer, "BVNL") ~ "BVNL",
        str_detect(disclaimer, "Ja21") ~ "JA21",
        str_detect(disclaimer, "BBB") ~ "BBB",
        T ~ NA_character_
    )) %>%
    mutate(party = ifelse(is.na(party1), party2, party1)) %>%
    drop_na(party) %>%
    distinct(page_id, .keep_all = T) %>%
    filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T)) %>%
    mutate(page_id = as.character(page_id))


source("utils.R")

unlink("provincies/7", recursive = T, force = T)
unlink("provincies/30", recursive = T, force = T)

dir.create("provincies/7")
dir.create("provincies/30")

# rawadvertisers <- read_csv("data/advertisers - advertisers.csv")  %>%
#   mutate(party_lab = case_when(
#     str_detect(advertiser_name, "VVD") ~ "VVD",
#     str_detect(advertiser_name, "\\bCDA\\b") ~ "CDA",
#     str_detect(advertiser_name, "PvdA|Jonge Socialisten") ~ "PvdA",
#     str_detect(advertiser_name, "D66|Jonge Democraten") ~ "D66",
#     str_detect(advertiser_name, "GroenLinks") ~ "GroenLinks",
#     str_detect(advertiser_name, "ChristenUnie") ~ "ChristenUnie",
#     str_detect(advertiser_name, "\\bSP\\b") ~ "SP",
#     str_detect(advertiser_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
#     str_detect(advertiser_name, "50PLUS") ~ "50PLUS",
#     str_detect(advertiser_name, "\\bSGP\\b") ~ "SGP",
#     str_detect(advertiser_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
#     str_detect(advertiser_name, "PVV") ~ "PVV",
#     str_detect(advertiser_name, "DENK") ~ "DENK",
#     str_detect(advertiser_name, "Volt") ~ "Volt Nederland",
#     str_detect(advertiser_name, "BIJ1") ~ "BIJ1",
#     str_detect(advertiser_name, "BVNL") ~ "BVNL",
#     str_detect(advertiser_name, "Ja21") ~ "Ja21",
#     T ~ ""
#   ))



# internal_page_ids <- read_csv("data/nl_advertisers.csv") %>%
#   mutate(page_id = as.character(page_id))

internal_page_ids <- read_csv("https://raw.githubusercontent.com/favstats/ProvincialeStatenverkiezingen2023/main/data/nl_advertisers.csv") %>%
    mutate(page_id = as.character(page_id))

# internal_page_ids %>%
#     count(party, sort = T) %>% View

wtm_data <- read_csv("data/wtm-advertisers-nl-2023-03-13.csv") %>% #names
    select(page_id = advertisers_platforms.advertiser_platform_ref,
           page_name = name, party = entities.short_name)  %>%
    mutate(page_id = as.character(page_id)) %>%
    # filter(party == "And") %>% #View
    # count(party, sort = T)  %>%
  mutate(party = case_when(
    str_detect(party, "VVD") ~ "VVD",
    str_detect(party, "\\bCDA\\b") ~ "CDA",
    str_detect(party, "PvdA|Jonge Socialisten") ~ "PvdA",
    str_detect(party, "D66|Jonge Democraten") ~ "D66",
    str_detect(party, "GroenLinks|GL") ~ "GroenLinks",
    str_detect(party, "ChristenUnie|CU") ~ "ChristenUnie",
    str_detect(party, "\\bSP\\b") ~ "SP",
    str_detect(party, "FvD|FVD|Forum voor Democratie") ~ "FvD",
    str_detect(party, "50Plus|50PLUS") ~ "50PLUS",
    str_detect(party, "\\bSGP\\b") ~ "SGP",
    str_detect(party, "PvdD|Partij voor de Dieren") ~ "PvdD",
    str_detect(party, "PVV") ~ "PVV",
    str_detect(party, "DENK") ~ "DENK",
    str_detect(party, "Volt|VOLT") ~ "Volt Nederland",
    str_detect(party, "BIJ1|BiJ") ~ "BIJ1",
    str_detect(party, "BVNL") ~ "BVNL",
    str_detect(party, "Ja21") ~ "JA21",
    str_detect(page_name, "Alliantie") ~ "Alliantie",
    str_detect(page_name, "Partij voor de Dieren") ~ "PvdD",
    str_detect(page_name, "Christine Govaert") ~ "BBB",
    str_detect(page_name, "BVNL|Belang van Nederland") ~ "BVNL",
    T ~ party
  )) #%>% #View
    # count(party, sort = T)

# wtm_data %>% 
#   filter(party == "And") %>% View

# wtm_data %>% count(party)

rep <- read_csv("data/FacebookAdLibraryReport_2023-03-05_NL_last_30_days_advertisers.csv") %>% janitor::clean_names()  %>%
    mutate(page_id = as.character(page_id)) %>%
    mutate(party1 = case_when(
        str_detect(page_name, "VVD") ~ "VVD",
        str_detect(page_name, "\\bCDA\\b") ~ "CDA",
        str_detect(page_name, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(page_name, "D66|Jonge Democraten") ~ "D66",
        str_detect(page_name, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(page_name, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(page_name, "\\bSP\\b") ~ "SP",
        str_detect(page_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(page_name, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(page_name, "\\bSGP\\b") ~ "SGP",
        str_detect(page_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(page_name, "PVV") ~ "PVV",
        str_detect(page_name, "DENK") ~ "DENK",
        str_detect(page_name, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(page_name, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(page_name, "BVNL") ~ "BVNL",
        str_detect(page_name, "Ja21") ~ "Ja21",
        str_detect(page_name, "Alliantie") ~ "Alliantie",
        str_detect(page_name, "BBB") ~ "BBB",
        T ~ NA_character_
    )) %>%
    mutate(party2 = case_when(
        str_detect(disclaimer, "VVD") ~ "VVD",
        str_detect(disclaimer, "\\bCDA\\b") ~ "CDA",
        str_detect(disclaimer, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(disclaimer, "D66|Jonge Democraten") ~ "D66",
        str_detect(disclaimer, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(disclaimer, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(disclaimer, "\\bSP\\b") ~ "SP",
        str_detect(disclaimer, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(disclaimer, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(disclaimer, "\\bSGP\\b") ~ "SGP",
        str_detect(disclaimer, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(disclaimer, "PVV") ~ "PVV",
        str_detect(disclaimer, "DENK") ~ "DENK",
        str_detect(disclaimer, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(disclaimer, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(disclaimer, "BVNL") ~ "BVNL",
        str_detect(disclaimer, "Ja21") ~ "Ja21",
        str_detect(disclaimer, "BBB") ~ "BBB",
        T ~ NA_character_
    )) %>%
    mutate(party = ifelse(is.na(party1), party2, party1)) %>%
    drop_na(party) %>%
    distinct(page_id, .keep_all = T) %>%
    filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T))

# 338750440106782

all_dat <- #read_csv("nl_advertisers.csv") %>%
    # mutate(page_id = as.character(page_id)) %>%
    bind_rows(internal_page_ids) %>%
    bind_rows(wtm_data) %>%
    bind_rows(rep) %>%
    bind_rows(more_data %>% mutate(source = "new")) %>%
    distinct(page_id, .keep_all = T) %>%
    add_count(page_name, sort  =T) %>%
    mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
    filter(!remove_em) %>%
    # filter(n >= 2) %>%
    # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
    select(-n)  %>%
    mutate(party = case_when(
        str_detect(party, "VVD") ~ "VVD",
        str_detect(party, "\\bCDA\\b") ~ "CDA",
        str_detect(party, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(party, "D66|Jonge Democraten") ~ "D66",
        str_detect(party, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(party, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(party, "\\bSP\\b") ~ "SP",
        str_detect(party, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(party, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(party, "\\bSGP\\b") ~ "SGP",
        str_detect(party, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(party, "PVV") ~ "PVV",
        str_detect(party, "DENK") ~ "DENK",
        str_detect(party, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(party, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(party, "BVNL") ~ "BVNL",
        str_detect(party, "Ja21|JA21") ~ "Ja21",
        str_detect(party, "Alliantie") ~ "Alliantie",
        str_detect(party, "BBB") ~ "BBB",
        T ~ party
    ))

all_dat %>% filter(source == "new") %>% View


# all_dat %>%
#     filter(party == "And") %>% View
#     count(party, sort = T) %>% View

#
# all_dat %>% View
#     filter(page_id == 1519997944789250)
#
# all_dat %>%
#     add_count(page_name, sort  =T) %>%
#     filter(n >= 2) %>% View
#     filter(str_ends(page_id, "0", negate = T)) %>% View

# all_dat %>%
#     mutate(source = "already_there") %>%
#     # filter(str_detect(party, "FvD")) %>% View
#     bind_rows(more_data) %>%
#     distinct(page_id, .keep_all = T) %>%
#     # filter(page_id == 609816282477420) %>%
#     # filter(page_id == 609816282477420) %>% View
#
#     filter(is.na(source)) %>% View

# all_dat %>%
#     count(party, sort  =T)

# all_dat %>%
#     bind_rows(rep %>% select(page_name, page_id, disclaimer, party))  %>%
#     distinct(page_id, .keep_all = T) %>%
#     filter(!(page_id %in% all_dat$page_id)) %>%
#     filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T)) %>% View

# all_dat %>% filter(str_detect(page_name, "BBB")) %>% View

# write_csv(all_dat, file = "data/nl_advertisers.csv")

# janitor::clean_names() %>%
# arrange(desc(amount_spent_usd)) %>%
# mutate(spend_upper = amount_spent_usd %>% as.numeric()) %>%
# arrange(-spend_upper) %>%
# mutate_all(as.character)C

# internal_page_ids %>% count(party, sort =T) %>% slice(11:17)
#
# internal_page_ids %>%
#   filter(party == "Politiek Op Maat")
#
# rawadvertisers %>%
#   # filter(category == "Political Organization") %>% View
#   # filter(str_detect(category, "Party|Politician|Candidade")) %>%
#   rename(page_id = advertiser_id) %>%
#   select(page_id, page_name = advertiser_name, party = party_lab)
#   left_join(internal_page_ids) %>%
#   # drop_na(party) %>%
#   filter(!is.na(party) | party_lab != "") %>%
#   # filter(party == "PvdA" & party_lab == "")
#   count(party, party_lab, sort = T)  %>% View
#
#
#
#   internal_page_ids %>%
#     bind_rows(
#       rawadvertisers %>%
#         rename(page_id = advertiser_id) %>%
#         select(page_id, page_name = advertiser_name, party = party_lab) %>%
#         filter(party != "") %>%
#         filter(str_starts(page_id, "AR", negate = T)) %>%
#         mutate(source = "yo")
#     ) %>%
#     distinct(page_id, .keep_all = T) %>%
#     write_csv("data/nl_advertisers.csv")


# georgia_wtm <- readr::read_csv("data/wtm-advertisers-us-2022-11-28T14_22_01.338Z.csv") %>%
#   select(page_name = name,
#          page_id = advertisers_platforms.advertiser_platform_ref) %>%
#   mutate(page_id = as.character(page_id))

# options(scipen = 999999)

# georgia_wtm

# internal_page_ids <- georgia_wtm %>%
#   mutate_all(as.character) %>%
#   bind_rows(last90days)  %>%
#   distinct(page_id, .keep_all = T)

# get_targeting(internal_page_ids$page_id[1], timeframe = "LAST_30_DAYS")
# debugonce(get_targeting)
# get_targeting("121264564551002", timeframe = "LAST_30_DAYS")

scraper <- function(.x, time = "7") {

  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

  yo <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)

  if(nrow(yo)!=0){
    path <- paste0(glue::glue("provincies/{time}/"),.x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(yo %>% bind_rows(ol), file = path)
    # } else {

    saveRDS(yo, file = path)
    # }
  }

  # print(nrow(yo))
  # })

}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("provincies/7", full.names
# }
# da30 <- readRDS("data/election_dat30.rds")
# da7 <- readRDS("data/election_dat7.rds")

### save seperately
yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
  # filter(!(page_id %in% unique(da7$page_id))) %>%
  # filter(cntry == "GB") %>%
  # slice(1:10) %>%
  split(1:nrow(.)) %>%
  map_dfr_progress(scraper, 7)

yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    # filter(!(page_id %in% unique(da30$page_id))) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper, 30)

# saveRDS(yo, file = )
library(tidyverse)
da30  <- dir("provincies/30", full.names = T) %>%
  map_dfr_progress(readRDS)  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

# da30 %>%
#     count(party, sort = T) %>% View

da30 %>% count(ds)
da7 %>% count(ds)

da7  <- dir("provincies/7", full.names = T) %>%
    map_dfr_progress(readRDS) %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

saveRDS(da30, "data/election_dat30.rds")
saveRDS(da7, "data/election_dat7.rds")



# bbb %>% filter(str_detect(funding_, "Strijker"))

# da7 %>%
#   distinct(internal_id, .keep_all = T) %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids) %>%
#   group_by(party) %>%
#   summarize(total_spend = sum(total_spend))
#
#
# amgna <- da7 %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids)
#
#
# amgna %>%
#   filter(type == "gender") %>%
#   filter(value == "Women") %>%
#   # mutate(total_spend = total_spend*total_spend_pct) %>%
#   ggplot(aes(party, total_spend_pct)) +
#   geom_boxplot() #+
#   # scale_y_log10()
#
#
#
# amgna %>%
#   filter(type == "detailed")



wk_spend <- read_csv("data/google-political-ads-advertiser-weekly-spend.csv")

ggl_spend <- wk_spend  %>%
    mutate(party1 = case_when(
        str_detect(Advertiser_Name, "VVD|Volkspartij voor Vrijheid en Democratie") ~ "VVD",
        str_detect(Advertiser_Name, "\\bCDA\\b|Christen Democratisch Appèl") ~ "CDA",
        str_detect(Advertiser_Name, "PvdA|Jonge Socialisten|Partij van de Arbeid") ~ "PvdA",
        str_detect(Advertiser_Name, "\\bD66\\b|Jonge Democraten|Democraten 66") ~ "D66",
        str_detect(Advertiser_Name, "GroenLinks|\\bGL\\b") ~ "GroenLinks",
        str_detect(Advertiser_Name, "ChristenUnie|\\bCU\\b") ~ "ChristenUnie",
        str_detect(Advertiser_Name, "\\bSP\\b|Socialistische Partij") ~ "SP",
        str_detect(Advertiser_Name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(Advertiser_Name, "50.lus|50PLUS|VLG") ~ "50PLUS",
        str_detect(Advertiser_Name, "\\bSGP\\b|Staatkundig Gereformeerde Partij") ~ "SGP",
        str_detect(Advertiser_Name, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(Advertiser_Name, "PVV|Partij Voor de Vrijheid") ~ "PVV",
        str_detect(Advertiser_Name, "DENK") ~ "DENK",
        str_detect(Advertiser_Name, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(Advertiser_Name, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(Advertiser_Name, "BVNL|Belang Van Nederland") ~ "BVNL",
        str_detect(Advertiser_Name, "Ja21|JA21|Conservatieve Liberalen") ~ "JA21",
        str_detect(Advertiser_Name, "Alliantie") ~ "Alliantie",
        str_detect(Advertiser_Name, "BBB|Marc-Michel Strijker") ~ "BBB",
        T ~ NA_character_
    )) %>%
    # distinct(Advertiser_Name, .keep_all = T) %>%
    filter(!(str_detect(Advertiser_Name, "Gleichheitspartei|Nieuw-Vlaamse|SP Digital LLC|MURRAY|REVOLT|Angelenos Against Higher Property Taxes|ITALIA|Volt Deutschland"))) %>%
    drop_na(party1) %>%
    mutate(Week_Start_Date = lubridate::ymd(Week_Start_Date)) %>%
    filter(Week_Start_Date >= as.Date("2023-02-05"))
    # count(Week_Start_Date)

saveRDS(ggl_spend, "data/ggl_spend.rds")

# all_ads <- vroom::vroom("C:/Users/fabio/Downloads/skeptic/google-political-ads-creative-stats.csv")

source("selenium.R")

ggl_sel_sp <- readRDS("data/ggl_sel_sp.rds")

tt_ads <- ggl_sel_sp %>%
    rename(Advertiser_ID = advertiser_id) %>%
    left_join(ggl_spend %>% distinct(Advertiser_ID, party1))  %>%
    # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
    # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
    group_by(party1) %>%
    summarize(total_num_ads = sum(as.numeric(num_ads))) %>%
    # count(party1, name = "total_num_ads") %>%
    mutate(total_num_ads = scales::comma(total_num_ads)) %>%
    pivot_wider(names_from = party1, values_from = total_num_ads) %>%
    mutate(`Coalizione/Partito` = "Number of Ads")


ttl_spn <- ggl_sel_sp %>%
    rename(Advertiser_ID = advertiser_id) %>%
    left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>%
    mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
    group_by(party1) %>%
    summarize(Spend_EUR = sum(Spend_EUR)) %>%
    arrange(desc(Spend_EUR)) %>%
    select(party = party1, spend = Spend_EUR) %>%
    mutate(spend = scales::comma(spend)) %>%
    mutate(spend = paste0("€", spend)) %>%
    drop_na() %>%
    pivot_wider(names_from = party, values_from = spend) %>%
    mutate(`Coalizione/Partito` = "Total Spend")



tp_spnders <- ggl_sel_sp %>%
    rename(Advertiser_ID = advertiser_id) %>%
    left_join(ggl_spend %>% distinct(Advertiser_ID, party1, .keep_all = T) %>% select(Advertiser_ID, party1, Advertiser_Name)) %>%
    mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
    group_by(Advertiser_Name, party1) %>%
    summarize(Spend_EUR = sum(Spend_EUR)) %>%
    ungroup() %>%
    group_by(party1) %>%
    arrange(desc(Spend_EUR)) %>%
    slice(1:3) %>%
    mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
    mutate(n_words = str_count(Advertiser_Name, " ")) %>%
    # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
    mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
    select(party1, lab) %>%
    drop_na() %>%
    summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
    pivot_wider(names_from = party1, values_from = lab) %>%
    mutate(`Coalizione/Partito` = "Top Spenders")

ggl_all <- tt_ads %>%
    bind_rows(tp_spnders) %>%
    bind_rows(ttl_spn) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Coalizione/Partito") %>%
    set_names(.[nrow(.),] %>% as.character()) %>%
    slice(1:(n()-1))


saveRDS(ggl_all, file = "data/ggl_all.rds")





all_ads <- vroom::vroom("C:/Users/fabio/Downloads/skeptic/google-political-ads-creative-stats.csv")

all_ads %>%
  filter(Advertiser_ID %in% ggl_sel_sp$advertiser_id) %>%
  mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
  filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
  mutate(Spend_Range_Max_EUR = as.numeric(Spend_Range_Max_EUR)) %>% 
  left_join(ggl_spend %>% distinct(Advertiser_ID, .keep_all = T) %>% select(Advertiser_ID, party1)) %>% 
  group_by(party1) %>% 
  mutate(total = sum(Spend_Range_Max_EUR)) %>% 
  mutate(perc = Spend_Range_Max_EUR/total) %>% 
  ungroup() %>% 
  # arrange(desc(Spend_Range_Max_EUR)) %>% View
  # count(Age_Targeting, sort = T) %>% View
  count(Geo_Targeting_Included, sort = T) %>%
  slice()
  # sample_n(5) %>% dput()
  mutate(Geo_Targeting_Included = str_remove_all(Geo_Targeting_Included, ",Netherlands")) %>% View

  

  Drenthe,Netherlands
  North Holland,Netherlands
  South Holland,Netherlands
  Utrecht,Netherlands
  North Brabant,Netherlands
  Overijssel,Netherlands
  Zeeland,Netherlands
  Limburg,Netherlands
  Flevoland,Netherlands
  Friesland,Netherlands
  Groningen,Netherlands
  Gelderland,Netherlands


  

  
  
  
  library(stringr)
  
  # your list of Dutch province names
  province_names <- c("Drenthe,Netherlands", "North Holland,Netherlands", "South Holland,Netherlands", "Utrecht,Netherlands", "North Brabant,Netherlands", "Overijssel,Netherlands", "Zeeland,Netherlands", "Limburg,Netherlands", "Flevoland,Netherlands", "Friesland,Netherlands", "Groningen,Netherlands", "Gelderland,Netherlands")
  
  # example strings to match
  
  # example strings to match
  example_strings <- c("South Holland,Netherlands",
                       "Zoeterwoude,Zoeterwoude,South Holland,Netherlands",
                       "Zuidhorn,Groningen,Netherlands", 
                       "Zuidplas,South Holland,Netherlands",
                       "North Brabant,Netherlands, Zwartewaterland,Overijssel,Netherlands, Zwijndrecht,Zwijndrecht,South Holland,Netherlands, het Bildt,Friesland,Netherlands", 
                       "North Brabant,Netherlands", 
                       "South Holland,Netherlands, Groningen,Netherlands")
  
  # pattern to match province names
  pattern <- paste0("(?<=^|,\\s)(?:", paste(province_names, collapse = "|"), ")(?=,|$)")
  
  # extract province names from example strings
  province_matches <- str_extract_all(example_strings, pattern)
  
  # print matches
  province_matches
  
  
sp_all_ads <- all_ads %>%
    filter(Advertiser_ID %in% ggl_sel_sp$advertiser_id) %>%
    mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
    filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
    mutate(Spend_Range_Max_EUR = as.numeric(Spend_Range_Max_EUR)) %>% 
    left_join(ggl_spend %>% distinct(Advertiser_ID, .keep_all = T) %>% select(Advertiser_ID, party1)) 

geo_sp_all <- sp_all_ads %>% 
    group_by(party1) %>% 
    mutate(total = sum(Spend_Range_Max_EUR)) %>% 
    mutate(perc = Spend_Range_Max_EUR/total) %>% 
    ungroup() %>% 
    # arrange(desc(Spend_Range_Max_EUR)) %>% View
    # count(Age_Targeting, sort = T) %>% View
    # count(Geo_Targeting_Included, sort = T) %>% 
    rowwise() %>% 
    mutate(provinces = paste0(unlist(str_extract_all(Geo_Targeting_Included, pattern)), collapse = "---")) %>% 
    ungroup() %>% 
    separate_rows(provinces, sep = "---") %>% 
    mutate(sep_entities_count = str_count(Geo_Targeting_Included, ", ")) %>% 
    mutate(sep_entities_count = sep_entities_count+1) %>% 
    mutate(province_count = str_count(Geo_Targeting_Included, provinces)) %>% 
    mutate(same_provinces = sep_entities_count==province_count) %>% 
    # sample_n(20) %>% 
    mutate(partial_spend = case_when(
      !same_provinces ~ Spend_Range_Max_EUR/sep_entities_count,
      same_provinces ~ Spend_Range_Max_EUR
    )) %>% 
    select(Spend_Range_Max_EUR, partial_spend, everything()) %>% 
    arrange(desc(partial_spend)) %>% 
    group_by(party1, provinces) %>% 
    summarize(partial_spend = sum(partial_spend)) %>% 
    mutate(provinces = str_remove_all(provinces, ",Netherlands")) %>% 
    drop_na(partial_spend) %>% 
    ungroup()


hc_geo <- sp_all_ads %>% 
  filter(Geo_Targeting_Included == "Netherlands") %>% 
  group_by(party1) %>% 
  summarize(Spend_Range_Max_EUR = sum(Spend_Range_Max_EUR)) %>% 
  mutate(partial_spend = Spend_Range_Max_EUR/12) %>% 
  expand_grid(province_names) %>% 
  select(party1, partial_spend, provinces = province_names) %>% 
  mutate(provinces = str_remove_all(provinces, ",Netherlands")) %>% 
  bind_rows(geo_sp_all) %>% 
  group_by(party1, provinces) %>%
  summarize(partial_spend = sum(partial_spend)) %>% 
  mutate(name = case_when(
    str_detect(provinces, "North Holland") ~ "Noord-Holland",
    str_detect(provinces, "South Holland") ~ "Zuid-Holland",
    str_detect(provinces, "North Brabant") ~ "Noord-Brabant",
    T ~ provinces
  ))



hc_geo %>% 
  group_split(party1) %>% 
  map(~{chart_maps(.x, F, mapdata)}) %>% hw_grid(ncol = 4) %>% 
  htmltools::browsable()

chart_maps(hc_geo %>% filter(party1 == "Volt Nederland"), F, mapdata)

library(highcharter)
mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/nl/nl-all.js"))

mapdata %>% count(name, sort =T)

chart_maps <- function(x, download_data = T, mapdata) {
  hc <- hcmap2(
    "https://code.highcharts.com/mapdata/countries/nl/nl-all.js",
    custom_map = mapdata,
    data = x,
    download_map_data = T,
    value = "partial_spend ",
    joinBy = c("name", "name"),
    # name = trans_internal$plot_tooltip_geo,
    dataLabels = list(enabled = TRUE, format = "{point.name}"),
    borderColor = "#FAFAFA",
    borderWidth = 0.2,
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "€"
    )
  ) %>% 
    # hc_colorAxis(
    #   minColor = "white",
    #   maxColor = unique(x$colorful),
    #   min = 0,
    #   max = 40
    # )%>% 
    hc_title(
      text = unique(x$party1)
    ) %>%
    hc_exporting(
      enabled = TRUE
    )
  
  # download_data <<- F
  
  return(hc)
}



fb_aggr %>% 
  hc_plotter(filters = dutch_parties_fb,
             plot_type = unlist_it(trans$choices, 4),
             plot_type_sub = unlist_it(trans$targeted_ads_choices, 3),
             platform = "Facebook",
             mapdata = map_data,
             trans_internal = trans,
             last_updated = update_time, minmax = "Minimum"
  )
