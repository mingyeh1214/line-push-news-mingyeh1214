library(rvest)
library(httr)
library(tidyverse)
library(readxl)
library(writexl)


update_data <- function(){
  print("開始爬網")
  data_path <- "data.xlsx"
  crawl_url_main <- "https://ctee.com.tw/category/news/insurance/page/"
  if(nrow(readxl::read_excel(data_path)) == 0) {
    crawl_url_page <- paste(1:10)
    } else {
      crawl_url_page <- paste(1:2)
    }
  crawl_url <- paste0(crawl_url_main, crawl_url_page, "/")
  search_strings <- c("中華郵政", "壽險")
  watting_sec <- 0.5
  
  for(i in 1:length(crawl_url)){
    print(paste("i=",i))
    crawl_html <- rvest::read_html(crawl_url[i], encoding = "UTF-8")
    
    news_header <- crawl_html %>%
      rvest::html_nodes(".post-title , .post-url") %>%
      rvest::html_text() %>%
      gsub('[\t\n]', '',.) %>%
      .[1:10]
    
    news_date <- crawl_html %>%
      rvest::html_nodes(".updated") %>%
      rvest::html_text() %>%
      gsub('[\t\n]', '',.) %>%
      .[1:10]
    
    news_link <- crawl_html %>%
      rvest::html_nodes(".post-title , .post-url") %>%
      html_attr('href') %>%
      .[1:10]
    
    context <- match_key_1 <- match_key_2 <- match_key_all <- c()
    
    for(j in 1:10){
      print(paste("j=",j))
      crawl_context <- rvest::read_html(news_link[j], encoding = "UTF-8")
      context_temp <- crawl_context %>%
        rvest::html_nodes(".entry-content") %>%
        rvest::html_nodes("p") %>%
        rvest::html_text() 
      context[j] <- context_temp[!context_temp %>% grepl( "\t", ., fixed = TRUE)] %>%
        paste(collapse = "")
      match_key_1[j] <- grepl(search_strings[1], context[j], fixed = TRUE)
      match_key_2[j] <- grepl(search_strings[2], context[j], fixed = TRUE)
      match_key_all[j] <- match_key_1[j] & match_key_2[j]
      Sys.sleep(watting_sec)
    }
    
    news <- data.frame(
      header = news_header,
      date = news_date,
      link = news_link,
      crawl_time = Sys.time(),
      context = context,
      match_key_1 = match_key_1,
      match_key_2 = match_key_2,
      match_key_all = match_key_all,
      if_push_line = FALSE,
      push_line_time = NA
    )
    
    data <- readxl::read_excel(data_path)
    
    if(nrow(data) == 0){
      data <- news 
    } else {
      news <- news %>%
        filter(!header %in% data$header)
      
      data <- news %>%
        bind_rows(data)
    }
    
    data %>% 
      arrange(link, desc = FALSE) %>%
      writexl::write_xlsx(data_path)
    
  }
  
}

push_line <- function(){
  
  data_path <- "data.xlsx"
  
  token <- "V0b0tmio80RGomvTsRSKDijv3Y8bJLkWY0gzk1ihS4Q"
  
  url <- "https://notify-api.line.me/api/notify"
  
  auth <- paste0("Bearer ", token)
  
  data <- readxl::read_excel(data_path)
  
  ready_to_push <- data %>%
    filter(match_key_all == 1) %>%
    filter(if_push_line == 0)
  
  if(nrow(ready_to_push) != 0){
    for(i in 1:nrow(ready_to_push)){
      message = ready_to_push$link[i]
      
      resp <- POST(url,
                   httr::add_headers(Authorization = auth),
                   body = list(message = message),
                   encode = "multipart")
      
      update_row <- data %>%
        filter(link == ready_to_push$link[i]) %>%
        mutate(if_push_line = TRUE) %>%
        mutate(push_line_time = Sys.time())
      
      data %>%
        filter(link != ready_to_push$link[i]) %>%
        bind_rows(update_row) %>%
        arrange(link, desc = FALSE) %>%
        writexl::write_xlsx(data_path)
    }
  }else if(nrow(ready_to_push) == 0){
    message = "無新增新聞"
    resp <- POST(url,
                 httr::add_headers(Authorization = auth),
                 body = list(message = message),
                 encode = "multipart")
  }
}

update_data()

push_line()



