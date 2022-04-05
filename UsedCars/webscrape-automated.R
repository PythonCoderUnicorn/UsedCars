
# ======= web scraping for car data 
# Sys.Date()  "2021-10-15"

library(rvest)
library(tidyverse)

url = "https://www.kijiji.ca/b-alberta/used-vehicles-by-owner/k0l9003"

page = read_html(url)



# .dealer-badges , .detail-price-area , .used , span
# .description , .price , .location span , .title

title2 = page %>% 
  html_nodes(".title") %>% 
  html_text2()

title2

price = page %>% 
  html_nodes(".price") %>% 
  html_text2()

price

location = page %>% 
  html_nodes('.location') %>% 
  html_text2()

location

info = page %>% 
  html_nodes('.info') %>% 
  html_text2()

info

description = page %>% 
  html_nodes('.description') %>% 
  html_text2()

description


# make a dataframe
cars_kiji = data.frame(title2,
                       price,
                       location,
                       info,
                       description,
                       stringsAsFactors = FALSE
                       )

cars_kiji

write_csv2(cars_kiji, "kijiji_cars.csv",na="NA", append = F)










library(janitor)
library(tidyverse)

df = read_csv2('kijiji_cars.csv')

df = df %>% clean_names()

glimpse(df)

skimr::skim(df)

summary(df)

df = df %>% distinct()
df



df = df %>% 
  mutate(title = str_replace_all(title2, "- PRICE REDUCED - low mileage", "")
         )
df$title %>% unique()



pattern.2 = ".+(?=\\s(?=[[:digit:]]{4,}))|[[:alnum:]]+"
df= df %>% 
  mutate(car_year = str_extract(title2, pattern.2),
         title = str_remove(title2, "For Sale"),
         # title = str_extract(title2, "[[:PUNCT:]]" )
         
         )
df$car_year

alphas = "[[:alpha:]]"
df$car_year = str_remove_all(df$car_year, alphas )

df$car_year = str_remove_all(df$car_year, "105")
df$car_year = as.factor(as.numeric(df$car_year) )
df$car_year





df$title

digits = "[[:digit:]]"
df$location = str_remove_all(df$location, digits)


df$location= str_remove_all(df$location, "<  minutes ago$")
df$location= str_remove_all(df$location, "//$")
df$location = str_replace_all(df$location, "\\s+","")

df$location = as.factor(df$location)






df$price = as.factor(gsub(",","", df$price))
df$price = as.numeric(gsub("\\$","",df$price))

df$price = as.integer(df$price)

max(df$price, na.rm = T)
min(df$price, na.rm = T)

# =================================






df = read_csv2('kijiji_cars_v2.csv')

df$location = as.factor(df$location)

library(stringr)

df$title %>% unique()

cars_types = c('[Tt]oyota','BMW','[Cc]hevrolet','[Mm]azda','HONDA','[Hh]yundai','[Ff]ord','[Ss]ubaru',
               '[Jj]eep','[Ll]incoln','[Nn]issan')

cars_types2 = c('Toyota','BMW','Chevrolet','Mazda',
                'HONDA','Hyundai','Ford','Subaru',
               'Jeep','Lincoln','Nissan')

str_extract(df$title2,"Nissan?")

car_type_counts = tribble(
  ~type, ~count,
  'Ford',12,
  'Toyota',2,
  'BMW',8,
  'Chevrolet',2,
  'Mazda',4,
  'HONDA',8,
  'Hyundai',4,
  'Subaru',4,
  'Jeep',2,
  'Lincoln',2,
  'Nissan',2
  
)


str_detect(df$title, pattern = fixed("BMW"), negate = F)

str_subset(df$title2, cars_types)

str_extract_all(df$title, cars_types2, simplify = T)

df$title2[1:10]

str_extract(cars_types2,"^H?")


# RETURN TO GET CAR COUNTS 


df$description[1:6]

df$title2[1:11]
str_detect(df$title2, "2016")

df %>% 
  select(car_year, info) %>% 
  filter(str_detect(info, "Ford")) %>% unique()

df %>% 
  select(car_year, title2) %>% 
  filter(str_detect(title2, "Ford")) %>% unique()

df %>% 
  select(car_year, description) %>% 
  filter(str_detect(description, "Ford")) %>% unique()


# 5 - BMW (info)/ 3 (title2) /  1 (description)
# 5 - HONDA (INFO)/  4      / 4
# 10 Fords / 4 / 6 
# 4 - 2013 Fords


# ============================





df$description[1:5]


library(tidytext)

text_df = tibble(line=1:90, text= df$description)

info_txt_df = tibble(line=1:90, text= df$info)

text_df = text_df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

info_txt_df =info_txt_df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)


text_df %>% 
  count(word, sort = T) %>% 
  filter(n > 10,
         word != c('15799','116ave')) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot( aes(x= n, y= word))+
  geom_col(fill="#4287f5")+
  labs(title = "Used Cars on Kijiji.ca - Word Counts", 
       subtitle = "Data Webscraped on Oct. 15, 2021",
       y= "",
       x="Count")+
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(face = 'bold', size=11)
  )


# ======= info section
df$info[1:6]



info_txt_df$word = str_remove_all(info_txt_df$word, digits )  
punct = "[[:punct:]]"
info_txt_df$word = str_remove_all(info_txt_df$word, punct )  
blanks = "[[:blank:]]"
info_txt_df$word = str_remove_all(info_txt_df$word, blanks ) 

ctrl_chars = "[[:cntrl:]]" 
info_txt_df$word = str_remove_all(info_txt_df$word, c('1','l')) 




info_txt_df %>% 
  count(word, sort = T) %>% 
  drop_na(word) %>%
  filter(n < 554 & n > 10
         ) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot( aes(x= n, y= word))+
  geom_col(fill="#3ADAC6")+
  labs(title = "Used Cars on Kijiji.ca - Word Counts (from info section)", 
       subtitle = "Data Webscraped on Oct. 15, 2021",
       y= "",
       x="Count")+
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(face = 'bold', size=11)
  )




















# ==========
df %>% 
  group_by(location) %>% 
  count() %>% 
  ggplot( aes(x= n, y= reorder(location, n)) )+
  geom_col(fill='#4287f5')+
  labs(title = "Used Cars on Kijiji.ca - Location Counts", 
       subtitle = "Data Webscraped on Oct. 15, 2021",
       y= "",
       x="Count")+
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(face = 'bold', size=11)
  )




df %>% 
  count(car_year, sort = T) %>% 
  drop_na() %>% 
  ggplot( aes(x= n, y= reorder(car_year, n)) )+
  geom_col(fill='#4287f5')+
  labs(title = "Used Cars on Kijiji.ca - Car Year Counts", 
       subtitle = "Data Webscraped on Oct. 15, 2021",
       y= "",
       x="Count")+
  theme(
    plot.title = element_text(size = 14, face = 'bold'),
    axis.text.y = element_text(face = 'bold', size=11)
  )







write_csv2(df, "kijiji_cars_v2.csv",na="NA", append = F)

























# df= df %>% 
#   mutate(title = str_extract_all("[[:digit:]]+{4}", title2))
# 
# 
# 
# 
# dx = c("aaa7864x88","56wd32v5","2%4r54ed44")
# dx
# as.numeric( gsub(".*?([0-9]+).*", "\\1", df$title2) )
# 
# x_numbers = regmatches(df$title2, gregexpr("[[:digit:]]+", df$title2) )   # Apply gregexpr & regmatches
# x_numbers
# x_numbers = as.numeric(unlist(x_numbers))                            # Convert characters to numeric
# x_numbers


# extract
# ex = unname(df$title2) %>% unique()
# ex 






# my_basket %>% 
#   mutate(Price_band = case_when(Price>=50 & Price <=70   ~ "Medium", 
#                                 Price > 70 ~ "High", TRUE ~ "Low"))











































# Automated Web Scraping in R using rvest
# Youtube - feb 14 2019

# === reddit text 
# https://code.datasciencedojo.com/rebeccam/tutorials/blob/master/web_scraping_R-master/r_web_scraping_meetup_share.r


# scenario: web scrape for political news for sentiment analysis
# --> customer satisfaction
