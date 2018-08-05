#lesson 6

ggplot(aes(x = price, y = x), data = diamonds) +
  geom_point()

cor(d0$price, d0$z)

ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point()

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks=seq(min(d0$depth), max(d0$depth),2))

cor(d0$depth, d0$price)

ggplot(aes(x = price, y =carat), data = diamonds) + 
  geom_point() +
  xlim(min(diamonds$price), quantile(diamonds$price, 0.99)) +
  ylim(min(diamonds$carat), quantile(diamonds$carat, 0.99)) 

volumne<- d0$x * d0$y * d0$z
d0$volumne <- volumne
ggplot(aes(x = price, y =volumne), data = d0) + 
  geom_point()

d1 = subset(d0, d0$volumne > 0 & d0$volumne <800)
cor(d1$price, d1$volumne)

ggplot(aes(x = price, y =volumne), data = d1) + 
  geom_point(alpha = 1/100) +
  geom_smooth(method = 'lm', color = 'red')

library(dplyr)
diamondsByClarity <- diamonds%>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)

head(diamondsByClarity)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")
p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")

library(gridExtra)
grid.arrange(p1,p2)

###lesson8
#q2
d0 <- diamonds 
ggplot(aes(x =price, fill = cut), data = d0) +
  geom_histogram()+
  facet_wrap(~color) +
  scale_fill_brewer(type = 'qual')+
  scale_x_log10()
#q3
ggplot(aes(x =table, y = price), data = d0) +
  geom_point(aes(colour = cut), position = "jitter")+ 
  scale_fill_brewer(type = 'qual') +
  scale_x_continuous(breaks = seq(50,80,2),
                     limits = c(50,80)) +
  theme_minimal()
#q4
d0$volume <- d0$x * d0$y * d0$z
ggplot(aes(x =volume, y = price), data = subset(d0, !is.na(d0$volume))) +
  geom_jitter(aes(colour = clarity))+
  scale_fill_brewer(type = 'div')+
  scale_y_log10() +
  xlim(0, quantile(d0$volume, 0.99)) 

#q5
pf <- read.delim('pseudo_facebook.tsv', header = TRUE)

pf$prop_initiated <-ifelse(pf$friend_count > 0,
                           pf$friendships_initiated/pf$friend_count, 0)
#q6
pf$year_joined <- floor(2014 - pf$tenure/365)

pf$year_joined.bucket <- cut(pf$year_joined, 
                             c(2004,2009,2011,2012,2014))

ggplot(aes(x = tenure, y = prop_initiated),
       data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) 

#q7
ggplot(aes(x = tenure, y = prop_initiated),
       data = subset(pf, tenure > 0)) +
  geom_smooth(aes(color = year_joined.bucket)) 

#q9
library(dplyr)
pf %>%
  filter(pf$year_joined.bucket == "(2012,2014]") %>%
  summarise(avg = mean(pf$prop_initiated))


#Q10
ggplot(aes(x = cut, y = price/carat) , data = d0) +
  geom_jitter(aes(color = color))+
  facet_wrap(~clarity) +
  scale_color_brewer(type = 'div')
  