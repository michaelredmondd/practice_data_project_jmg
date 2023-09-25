library(tidyverse)
library(ggthemes)

theme_set(theme_minimal(base_size = 12))

## Tornados

# From Tidy Tuesday https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-05-16/readme.md

# can download cleaned data
#tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')
# but go through actual cleaning
# done in the get_tornados_data.R file
tornados <- read_csv("data/tornados.csv")

glimpse(tornados)

skimr::skim(tornados)

tornados %>%
  ggplot(aes(x = st)) + 
  geom_bar() + 
  coord_flip()

tornados %>%
  ggplot(aes(x=slon,y=slat)) + 
  geom_point()

tornados %>%
  ggplot(aes(x=slon,y=slat)) + 
  geom_point(alpha=0.1)

unique(tornados$st)

states_df <- map_data("state")

ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray")

tornados_49 <- tornados %>%
  filter(st != "AK", st != "HI",st!= "PR", st!= "VI")

tornados_49 %>%
  ggplot(aes(x = st)) + 
  geom_bar() + 
  coord_flip()

tornados_49 %>%
  group_by(st) %>%
  summarise(per_state = n()) %>%
  arrange(desc(per_state))

tornados_49 %>%
  group_by(st) %>%
  summarise(per_state = n()) %>%
  arrange(desc(per_state)) %>%
  ggplot(aes(x=st,y=per_state)) + 
  geom_bar(stat = "identity")


tornados_49 %>%
  group_by(st) %>%
  summarise(per_state = n()) %>%
  arrange(desc(per_state)) %>%
  ggplot(aes(x=reorder(st,per_state),y=per_state)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

tornados_49 %>%
  ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray") + 
  geom_segment(aes(x=slon,y=slat,xend=elon,yend=elat))

tornados_49 %>%
  ggplot(aes(x=elat)) + 
  geom_histogram()


tornados_49 <- tornados_49 %>%
  filter(elat > 0 & elon < 0)


tornados_49 %>%
  ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray") + 
  geom_segment(aes(x=slon,y=slat,xend=elon,yend=elat))


tornados_49 %>%
  ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray") + 
  geom_segment(aes(x=slon,y=slat,xend=elon,yend=elat))



tornados_49 <- tornados_49 %>%
  filter(slon >= -105)

states_df <- states_df %>%
  filter(long >= -105)

tornados_49 %>%
  ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray") + 
  geom_segment(aes(x=slon,y=slat,xend=elon,yend=elat))


tornados_49 %>%
  ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray") + 
  geom_segment(aes(x=slon,y=slat,xend=elon,yend=elat),alpha=0.5,color="darkgreen")

tornados_49 %>%
  ggplot(aes(x= yr)) + 
  geom_bar()

tornados_49 %>%
  ggplot(aes(x=mo)) + 
  geom_bar()


tornados_49 %>%
  ggplot() + 
  geom_polygon(data=states_df,
               mapping = aes(long,lat,group=group),
               fill="white",color="darkgray") + 
  geom_point(aes(x=slon,y=slat,color=mag))






