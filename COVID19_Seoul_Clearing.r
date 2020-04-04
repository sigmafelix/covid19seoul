library(pacman)
devtools::install_github('hrbrmstr/curlconverter')
p_load(rvest, tidyverse, curl, curlconverter, glue, tidyjson, jsonlite)

read_html('http://www.seoul.go.kr/coronaV/coronaStatus.do?tab=1') %>% 
  html_table(fill =T) -> tab1
bind_rows(
  tab1[[2]] %>% mutate_all(as.character), 
  tab1[[3]] %>% mutate_all(as.character), 
  tab1[[4]] %>% mutate_all(as.character),
  tab1[[5]] %>% mutate_all(as.character),
  tab1[[6]] %>% mutate_all(as.character),
  tab1[[7]] %>% mutate_all(as.character)
  ) -> tab10


tab200 <- tab20 %>% separate(col = 3, sep = '→|⇒|/|일 ', into = str_c('M', sprintf('%02d', 1:100)))
colnames(tab200)[1:2] <- c('pid', 'pinfo')
colnames(tab200)[103:105] <- c('day_diag', 'residence', 'place_q')
tab201 <- tab200 %>%
  mutate(pinfo = ifelse(nchar(pinfo)>40, NA, pinfo),
         day_diag = ifelse(nchar(day_diag)>40, NA, day_diag),
         residence = ifelse(nchar(residence)>40, NA, residence),
         place_q = ifelse(nchar(place_q)>40, NA, place_q)) %>%
  pivot_longer(cols = 2:102) %>%
  filter(!is.na(value)) %>%
  mutate(date1 = str_extract_all(value, '\\d{1,}월 \\d{1,}일|\\d{1,}월 \\d{1,}', simplify = TRUE)[,1],
         date2 = str_extract_all(value, '\\d{1,}월 \\d{1,}일|\\d{1,}월 \\d{1,}', simplify = TRUE)[,2],
         date = str_replace(date1, '월 |월', '/') %>% str_replace(., '일', ''),
         date2 = str_replace(date2, '월 |월', '/') %>% str_replace(., '일', ''),
         t_start = str_extract_all(value, '\\d{1,}:\\d{1,}', simplify = TRUE)[,1],
         t_end = str_extract_all(value, '\\d{1,}:\\d{1,}', simplify = TRUE)[,2],
         pid_seoul = str_extract_all(pid, '\\d{1,3}', simplify = TRUE)[,1],
         pid_korea = str_replace(str_extract(pid, '(#\\d{1,})'), '\\(|\\)', '') %>% str_replace('#', ''),
         value = trimws(value),
         ismask = ifelse(str_extract_all(value, '(마스크 착용)|(마스크착용)', simplify = TRUE)[,1] == '', 0, 1),
         address = str_extract_all(value, '\\w{2,10}로 \\d{1,}|\\w{2,10}길 \\d{1,}|\\w{2,10}로 \\d{1,}-\\d{1,}|\\w{2,10}길 \\d{1,}-\\d{1,}|\\w{2,10}로\\d{1,}|\\w{2,10}길\\d{1,}|\\w{2,10}로\\d{1,}-\\d{1,}|\\w{2,10}길\\d{1,}-\\d{1,}', simplify = TRUE)[,1],
         address2 = str_extract_all(value, '\\w{2,10}로 \\d{1,}|\\w{2,10}길 \\d{1,}|\\w{2,10}로 \\d{1,}-\\d{1,}|\\w{2,10}길 \\d{1,}-\\d{1,}|\\w{2,10}로\\d{1,}|\\w{2,10}길\\d{1,}|\\w{2,10}로\\d{1,}-\\d{1,}|\\w{2,10}길\\d{1,}-\\d{1,}', simplify = TRUE)[,2],
         transport = str_extract_all(value, '버스|도보|자차|KTX|자가용|지하철|전철|\\d{1}호선|\\w{2}선|오토바이|택시', simplify = TRUE)[,1],
         transport2 = str_extract_all(value, '버스|도보|자차|KTX|자가용|지하철|전철|\\d{1}호선|\\w{2}선|오토바이|택시', simplify = TRUE)[,2]) %>%
  group_by(pid) %>%
  mutate(day_diag = day_diag[1],
         residence = residence[1],
         place_q = place_q[1]) %>%
  ungroup
