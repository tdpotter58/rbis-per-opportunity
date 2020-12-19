library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

theme_set(theme_bw())

rbis_2020 <- read.xlsx("C:/Users/Tim/tim-project/rbis-per-opportunity/2020_rbis.xlsx", sheetIndex = 1)
rbis_2020 <- read.csv("C:/Users/Tim/tim-project/rbis-per-opportunity/2020_rbis.csv")
rbis_2020 <- rbis %>%
  mutate(rbi_percentage = round(RBI / Chances, digits = 4))

rbis_2020 %>%
  filter(Chances > 5) %>%
  ggplot(aes(x = Chances, y = rbi_percentage, label = Player)) + 
  #geom_point() +
  geom_point(data = subset(rbis_2020, Chances >= 30 | (Chances > 5 & rbi_percentage >= 0.6))) + 
  geom_text(data = subset(rbis_2020, Chances < 30 & (Chances > 5 & rbi_percentage < 0.6))) +
  geom_label_repel(data = subset(rbis_2020, Chances >= 30 | (Chances > 5 & rbi_percentage >= 0.6)), aes(fill = RBI)) +
  scale_fill_gradient(low = 'blue', high = 'red')

rbis_anal <- rbis %>%
  mutate(rbi_percentage = round(RBI / Chances, digits = 4))

rbis_anal %>%
  filter(Chances > 5) %>%
  sort(rbi_percentage)

plot <- rbis_anal %>%
  filter(Chances > 5) %>%
  ggplot(aes(x = Chances, y = rbi_percentage, label = Player)) + 
  #geom_point() +
  geom_point(data = subset(rbis_anal, Chances >= 30 | (Chances > 5 & rbi_percentage >= 0.6))) + 
  geom_text(data = subset(rbis_anal, Chances < 30 & (Chances > 5 & rbi_percentage < 0.6))) +
  geom_label_repel(data = subset(rbis_anal, Chances >= 30 | (Chances > 5 & rbi_percentage >= 0.6)), aes(fill = RBI))
plot + scale_fill_gradient(low = 'blue', high = 'red')

rbis_anal %>%
  filter(Chances > 5) %>%
  ggplot(aes(x = Chances, y = rbi_percentage, label = Player)) + 
  #geom_point() +
  geom_point(data = subset(rbis_anal, Chances >= 30 | (Chances > 5 & rbi_percentage >= 0.6))) + 
  geom_text(data = subset(rbis_anal, Chances < 30 & (Chances > 5 & rbi_percentage < 0.6))) +
  geom_label_repel(data = subset(rbis_anal, Chances >= 30 & rbi_percentage <= 0.4), aes(fill = 'blue')) +
  geom_label_repel(data = subset(rbis_anal, Chances >= 30 & rbi_percentage > 0.4), aes(fill = 'red')) +
  geom_label_repel(data = subset(rbis_anal, Chances > 5 & Chances < 30 & rbi_percentage >= 0.6), aes(fill = 'green'))
  

rbis_anal %>%
  filter(Chances > 5) %>%
  ggplot(aes(x = Chances, y = rbi_percentage, label = Player)) + 
  #geom_point() +
  geom_point(data = subset(rbis_anal, Chances > 5 & RBI >= 10)) + 
  geom_text(data = subset(rbis_anal, Chances > 5 & RBI < 10)) +
  geom_label_repel(data = subset(rbis_anal, RBI >=10), aes(fill = RBI)) +
  scale_fill_gradient(low = 'blue', high = 'red')

rbis_anal %>%
  filter(Chances > 20) %>%
  ggplot(aes(x = Chances, y = rbi_percentage, label = Player)) + 
  geom_text()
  
