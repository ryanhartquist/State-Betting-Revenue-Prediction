



library(dplyr)
library(tidyr)
library(ggplot2)

demo_data <- read.table("/Users/admin/OneDrive - Oklahoma A and M System/Documents/MSIS 5223 - R-Python II/Group Project/demographic_data.txt", header=T, sep=",")

##Prep and pivot data for stacked bar chart
demo_piv <- demo_data %>% select(1:7)
demo_piv_bettax <- demo_data %>% select(1,29,30,31)

demo_piv <- demo_piv %>% 
  pivot_longer(!StateName, names_to = "Population", values_to = "sum")

#Stacked bar chart for change in % of population
ggplot(demo_piv, aes(fill=Population, y=sum, x=StateName)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x="", y = "% of Population" )


##Prep and pivot data for % of taxes bar chart
demo_piv_bettax <- demo_data %>% select(1,29,30,31) %>% rename(
  '2018' = Percent_of_Taxes_from_Betting_2018,
  '2019' = Percent_of_Taxes_from_Betting_2019,
  '2020' = Percent_of_Taxes_from_Betting_2020
  ) 
  
demo_piv_bettax <- demo_piv_bettax %>% 
  pivot_longer(!StateName, names_to = "Year", values_to = "sum")

demo_piv_bettax <- filter(demo_piv_bettax,sum > 0)

#demo_piv_bettax$sum = demo_piv_bettax$sum*100

#Stacked bar chart for change in % of population
ggplot(demo_piv_bettax, aes(fill=Year, y=sum, x = reorder(StateName, sum))) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) +
  labs(x="", y = "% of Taxes from Betting", title="Percent of Taxes from Betting" ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_flip()
