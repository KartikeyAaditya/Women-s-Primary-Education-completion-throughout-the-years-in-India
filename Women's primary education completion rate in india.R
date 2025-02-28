library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(skimr)
library(tidyr)
library(ggthemes)



Women_education <- read_csv("E:/Aadikartikey/P1/API_SE.PRM.CMPT.FE.ZS_DS2_en_csv_v2_3095.csv", skip = 3)
head(Women_education)
colnames(Women_education)
str(Women_education)


na_count <- colSums(is.na(Women_education))

threshold <- nrow(Women_education)/2

df <- Women_education%>%
  select(where(function(k) sum(is.na(k)) <= threshold))

df <- df%>%
  mutate(across(where(is.numeric),function(k) ifelse(is.na(k), mean(k, na.rm = TRUE), k)))%>%
  mutate(across(where(is.numeric), function(k) ifelse(is.nan(k), 0, k)))

head(df)
colnames(df)
str(df)


india <- df%>%
  group_by(`Country Name`)%>%
  filter(`Country Name` == "India")%>%
  select(`Country Name`, `Country Code`, everything())

head(india)
str(india)
colnames(india)

india_long <- india%>%
  pivot_longer(cols = c( starts_with("19"),starts_with("20")),
               names_to = "YEAR",
               values_to = "completion rate")

india_long$YEAR <- as.numeric(india_long$YEAR)

india_long <- india_long%>%
  select(-c(`Indicator Name`, `Indicator Code`))


india_long


ggplot(india_long, aes(x = YEAR, y = `completion rate`, fill = factor(YEAR)))+
  geom_bar(stat = "identity", width = 1, show.legend = TRUE)+
  scale_fill_viridis_d(option = "H")+
  geom_hline(yintercept = seq(0, 100, by = 10), color = "pink", linetype = "dashed")+
  theme_fivethirtyeight()+
  labs(
       title = "women's primary education completion rate",
       x = "year",
       y ="completion rate by %"
    )
  
  
  
