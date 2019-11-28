library(tidyverse)
library(readxl)

Trump <- read_excel("C:/Users/Desktop/Trump/Trump tweets by day of week.xlsx", 
                    col_types = c("text", "text", "date", "numeric", "numeric", "text", "text"))
# Plot it!
ggplot() +
  geom_bar(data = Trump_new, aes(x = created_at), color = "red", fill = "red") +
  #facet_wrap(vars(Name,Mentioned), ncol = 2, nrow = 8) +  Use this if you want to faced based on key words, 
  #and if that is the case, change the data from Trump to Trump_mentions
 #geom_hline(yintercept = 11.15, color = "navy") +
  labs(
    x = "Date",
    y = "Number of tweets/day",
    title = "President Trumps' number of tweets per day",
    caption = "Data from https://docs.google.com/spreadsheets/d/1X-Osf6lYJbMZ_Ry4RDD4Wd59pH4A-D8dJAbM4lExGKg/edit?usp=sharing"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0,80), breaks = seq(0,80, by = 10))

ggplot(data = df_avg) +
  geom_point(aes(x = created_at, y = n), color = "dark red") +
  stat_smooth(aes(x = created_at, y = n), method = "lm", color = "blue", se = F, size = 1, fullrange = T)

number_trump <- Trump %>% 
  group_by(created_at) %>% 
  count(created_at) %>% 
  filter(n > 60)

#Find tweets about candidates:
Trump <- Trump %>% 
  mutate(text = str_to_lower(text))

# Find key words
#Making a function
names <- function(x) {
  as.data.frame(str_detect(Trump$text, coll("x", ignore_case = TRUE)))
}

hillary <- names(hillary)
sanders <- names(sanders)
fake_news <- names("fake news")
clinton <- names(clinton)
wall <- names(wall)
mexico <- names(mexico)

renaming <- function(x, y, z) {
  y %>% select(z = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)
}


#rename the variables:
hillary <- hillary %>% 
  select(Hillary = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)

sanders <- sanders %>% 
  select(Sanders = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)

wall <- wall %>% 
  select(wall = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)

mexico <- mexico %>% 
  select(mexico = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)

fake_news <- fake_news %>% 
  select(Fake_news = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)

clinton <- clinton %>% 
  select(Clinton = `str_detect(Trump$text, coll("x", ignore_case = TRUE))`)

#And bind the columns to the Trump df
Trump_new <- cbind(Trump, hillary, sanders, fake_news, clinton, wall, mexico)

#------------------------------------------------------------

Trump_mentions <- Trump_new %>% 
  gather(key = "Name", value = "Mentioned", Hillary, Sanders, Fake_news, mexico, wall)


#No. of original Trump tweets to date:
Trump_new%>% 
  filter(is_retweet == 0) %>% 
  count()
