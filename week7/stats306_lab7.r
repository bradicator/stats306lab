
library(tidyverse)
options(repr.plot.width=8)

rand_idx = runif(1000, min=1, max=dim(diamonds)[1])
dm = diamonds[rand_idx, ]
names(dm)
dim(dm)

# p1 = ggplot() + 
#     geom_point() + 
#     facet_wrap()
# print(p1)

# p2 = ggplot() +
#     geom_point() +
#     geom_smooth()
# print(p2)

# p4 = ggplot() + 
#     geom_point() + 
#     geom_smooth()
# print(p4)

# ggplot(data = dm) + 
#     stat_summary()

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

# ggplot() + 
#   geom_bar()

# ggplot() + 
#   geom_bar()

ggplot(data = dm) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# ggplot() + 
#   geom_bar()

# Problem 6
ggplot(data=diamonds, mapping=aes(x=clarity, fill=clarity)) +
    geom_bar()

# Problem 7
table7 = diamonds %>% mutate(Volume = x*y*z)
ggplot(data=table7, mapping=aes(x=Volume, y=carat)) + 
    geom_bin2d() + xlim(0, 1000) + 
    xlab('Volume(x*y*z)') + ylab('Weight (carats)')

# Loading football data
load(url('https://github.com/terhorst/stats306/raw/master/ps4/cfb.RData'))

# Reproducing table problem
year_mpg = mpg 
head(year_mpg)

# Most 4WD problem
mpg %>% filter(year==1999, drv=='4') %>% 
    group_by(manufacturer, year, drv) %>% summarize(total = n()) %>% 
    arrange(desc(total))

colnames(cfb)

# Finding Big 10 teams
cfb %>% filter(Year==2018) %>% unite(wconf, `Winning Conference`, Winning, sep="_") %>%
        unite(lconf, `Losing Conference`, Losing, sep="_") %>%
        gather(wconf, lconf, key="wl", value="conf_team") %>%
        separate(conf_team, into=c("Conference", "Team"), sep="_") %>%
        filter(Conference == "Big 10") %>% distinct(Team, Conference) %>% print

require(nycflights13)

# Rainyday
rainy_days <- group_by(weather, origin, year, month, day) %>% 
              summarize(t=min(temp, na.rm=T), p=max(precip, na.rm=T)) %>%
              filter(p>0, t>32)
semi_join(flights, rainy_days) %>% nrow
