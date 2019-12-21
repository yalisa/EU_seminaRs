library(tidyverse)

https://agricolamz.github.io/2019.12.21_EU_purrr/index.html

got_chars <- jsonlite::read_json("https://raw.githubusercontent.com/agricolamz/2019.12.21_EU_purrr/master/data/got_chars.json")
got_chars[1]
listviewer::jsonedit


got_chars %>% 
  map("gender") %>% 
  unlist() %>% 
  table()


dict <- read_tsv("https://raw.githubusercontent.com/agricolamz/DS_for_DH/master/data/freq_dict_2011.csv")

dict$lemma %>%
  str_split("") %>% 
  unlist() %>% 
  enframe() %>% 
  mutate(value = str_to_lower(value)) %>% 
  filter(str_detect(value, "\\w")) %>% 
  count(value) %>% 
  ggplot(aes(x = fct_reorder(value, n), y = n))+
  geom_col()+
  coord_flip()+
  labs(x = NULL, y = NULL, caption = "данные из [Ляшевская, Шаров 2011]")
  
