### Author: Insang Song (isong@uoregon.edu)
### Version 0.1
### Description: to scrape the spatiotemporal trajectory of COVID-19 patients in Seoul, Republic of Korea
### Note: The original data source is the government of Seoul Metropolitan City, Republic of Korea.
### Disclaimer: Any consequence by using this code will be subject to the user.
library(pacman)
p_load(rvest, tidyverse, curl, jsonlite)

read_html('http://www.seoul.go.kr/coronaV/coronaStatus.do?tab=1') %>% 
  html_table(fill =T) -> tab1

# Should be changed when the number of cases exceeds 300
bind_rows(
  tab1[[2]] %>% mutate_all(as.character), 
  tab1[[3]] %>% mutate_all(as.character), 
  tab1[[4]] %>% mutate_all(as.character)) -> tab10


#### Function ####
corona_parse_seoul <- function(pids, dfmode = TRUE){
  corpars <- list()
  get_retcode <- function(pid_a){
    corp <- list()
    #counter <- 1
    for (i in 1:36){
      corp[[i]] <- list()
      pid_1 <- sprintf('%04d', as.integer(pid_a))
      pid_2 <- sprintf('%03d', i)
      pid <- str_c(pid_1, pid_2, sep = '_')
      # The curl call should be modified as the user environment is changed
      curl0 <- sprintf('curl "https://map.seoul.go.kr/smgis/apps/poi.do" -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:74.0) Gecko/20100101 Firefox/74.0" -H "Accept: text/javascript, application/javascript, application/ecmascript, application/x-ecmascript, */*; q=0.01" -H "Accept-Language: en-US,en;q=0.5" --compressed -H "Content-Type: application/x-www-form-urlencoded; charset=UTF-8" -H "X-Requested-With: XMLHttpRequest" -H "Origin: https://map.seoul.go.kr" -H "Connection: keep-alive" -H "Referer: https://map.seoul.go.kr/smgis/webs/theme/themeMapCopy.do?mode=themeMapCopy&thm_theme_id=11101856&map_type=1&list_type=0&order=0&xpoint=127.5&ypoint=37.5&level=6&oldmap=&tp=0.7&maptile=BASEMAP_GEN&utid=&copy_mode=theme_url&subcateIds=1_1&map_b=smCb_boundarySeoul" -H "Cookie: JSESSIONID=shBHrlCrHeezhmwUC1V6JMI2gouRS1HiCS9LBcQRs8giPbUz4qBhcwYJH7t75lwE.amV1c19kb21haW4vc2VvdWxjb3AxMDQ=; WL_PCID=15843776211962128161796; WMONID=gabec3jr_Xs; JSESSIONID=w0DERgiBvrwqvykGCmuzeVqW60riZyMiCfC129X488EJ0071423gLrqKpJfMtHsk.M_NAP2_servlet_engine1" --data "callback=jQuery1122023672073495102508_1584378036035&cmd=getNewContentsDetail&key=bf5ab1d4a3294328a5957ac14244eb25&theme_id=11101856&conts_id=%s&lan_type=KOR"', pid)
      #tryCatch({
      curl_return <- system(curl0, intern = TRUE)
      curl_return[which(sapply(curl_return, nchar)>0)] %>% str_sub(45, nchar(.)-3) -> js
      if (dfmode){
        js %>% fromJSON -> js
        if (js$head$RETCODE != '0'){ corp[[i]] <- NA } else {corp[[i]] <- js$body
        }
      } else {
        corp[[i]] <- js
      }
      #},
      #       error = function(e) corp[[i]] <- NA )
    }
    return(corp)
  }
  for (k in 1:length(pids)){
    corpars[[k]] <- get_retcode(pids[k])
  }
  return(corpars)
}

cha3 <- corona_parse_seoul(unlist(tab10[,2]))

cha3.df <- cha3 %>% 
  lapply(function(x) tryCatch({x %>% do.call(rbind, .) %>% data.frame}, error = function(e) print(e))) %>% 
  Reduce(bind_rows, .)
