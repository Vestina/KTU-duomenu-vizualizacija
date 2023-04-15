library(readr)
library(dplyr)
library(ggplot2)

data = read_csv("../data/lab_sodra.csv")
my_data = data %>% 
  filter(ecoActCode == 620000 & !is.na(avgWage))

#1 vidutinio atlyginimo histograma

my_data %>%
  ggplot(aes(x = avgWage)) +
  geom_histogram(bins = 100, alpha = 0.65, color = "darkmagenta", fill = "cyan", linetype = "dashed") + 
  theme_classic() +
  labs(title = "Awerage Wage of Employees", x = "Average Wage", y = "Count")

#2 vidutinis atlyginimas didžiausias

top5 = my_data %>%
  group_by(code) %>% 
  summarise(averageWage = mean(avgWage)) %>% 
  arrange(desc(averageWage)) %>%
  head(5)

top5_data = my_data %>%
  filter(code %in% top5$code) 

top5_data %>%
  ggplot(aes(x = month, y = avgWage, group = name)) +
  geom_line(aes(color = name), linewidth = 0.8) +
  geom_point(aes(color = name), size = 2) +
  theme_light() +
  labs(title = "Top 5 Firm Wages of 2022", x = "Month", y = "Average Wage", color = "Firm")

#3 maksimalus apdraustų darbuotojų skaičius

maxInsured = my_data %>%
  filter(code %in% top5$code) %>%
  group_by(name) %>%
  summarise(maxIns = max(numInsured)) %>%
  arrange(desc(maxIns))

maxInsured %>%
  ggplot(aes(x = reorder(name, -maxIns), y = maxIns)) +
  geom_col(aes(fill = name)) +
  theme_classic() + 
  scale_fill_manual(values = c("orange","lightgreen", "pink", "darkred", "lightblue"), name = "Firm") +
  labs(title = "Highest Wage and Number of Insured Eployees in 2022", x = "Firm", y = "Insured", fill = "Firm")