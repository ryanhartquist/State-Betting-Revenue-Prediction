library(rvest)
library(stringr)
library(RSelenium)
library(psych)
library(tidyverse)
library(dplyr)
# Demographic Data URL
rD = rsDriver(verbose=FALSE, browser = 'firefox', port = 4566L)
remDr = rD$client 
#URL for State Demographic Data from the Kaiser Family Foundation
#KFF Data for 2018 based on US Census American Community Survey Data
web_url = 'https://www.kff.org/other/state-indicator/distribution-by-age/?currentTimeframe=0&print=true&sortModel={"colId":"Location","sort":"asc"}'
remDr$open()
remDr$navigate(web_url)

#Pulling in State Name
state_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(1) > span:nth-child(1)'
state_elem = remDr$findElements(using = "css selector", value = state_css)
state_name = unlist(sapply(state_elem, function(x) {x$getElementText()}))
state_name

#Pulling in State Population Percent Ages 0-18
eighteen_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(2) > span:nth-child(1)'
eighteen_elem = remDr$findElements(using = "css selector", value = eighteen_css)
under_18 = unlist(sapply(eighteen_elem, function(x) {x$getElementText()}))
under_18

#Pulling in State Population Percent Ages 19-25
twentyfive_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(3) > span:nth-child(1)'
twentyfive_elem = remDr$findElements(using = "css selector", value = twentyfive_css)
nineteen_twentyfive = unlist(sapply(twentyfive_elem, function(x) {x$getElementText()}))
nineteen_twentyfive

#Pulling in State Population Percent Ages 26-34
thirtyfour_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(4) > span:nth-child(1)'
thirtyfour_elem = remDr$findElements(using = "css selector", value = thirtyfour_css)
twentysix_thirtyfour = unlist(sapply(thirtyfour_elem, function(x) {x$getElementText()}))
twentysix_thirtyfour

#Pulling in State Population Percent Ages 35-54
fiftyfour_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(5) > span:nth-child(1)'
fiftyfour_elem = remDr$findElements(using = "css selector", value = fiftyfour_css)
thirtyfive_fiftyfour = unlist(sapply(fiftyfour_elem, function(x) {x$getElementText()}))
thirtyfive_fiftyfour

#Pulling in State Population Percent Ages 55-64
sixtyfour_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(6) > span:nth-child(1)'
sixtyfour_elem = remDr$findElements(using = "css selector", value = sixtyfour_css)
fiftyfive_sixtyfour = unlist(sapply(sixtyfour_elem, function(x) {x$getElementText()}))
fiftyfive_sixtyfour

#Pulling in State Population Percent Ages 65+
sixtyfive_css = 'div.ag-row:nth-child(-n+52) > div:nth-child(7) > span:nth-child(1)'
sixtyfive_elem = remDr$findElements(using = "css selector", value = sixtyfive_css)
over_65 = unlist(sapply(sixtyfive_elem, function(x) {x$getElementText()}))
over_65

#Pulling in State Populations based on 2019 estimates
population_url = 'https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population'
population = read_html(population_url)
find_table = html_nodes(population, xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]")
population_table = html_table(find_table, fill=T)
population_table = data.frame(population_table)
population_table = population_table[c(3,4)]
population_table = population_table[-c(1, 32, 51, 54:61), ]
population_table = population_table[order(population_table$State),]
population_table = population_table[-c(1)]

#Creating data frame with Age Brackets
demographic_data = data.frame(state_name,under_18,nineteen_twentyfive,twentysix_thirtyfour,thirtyfive_fiftyfour,fiftyfive_sixtyfour,over_65)

#Dropping Columns for Overall and D.C.
demographic_data = demographic_data[-c(1), ]
demographic_data = demographic_data[-c(9), ]

#Adding State Population Column
demographic_data = cbind(demographic_data, 'State Population' = population_table)

#Pulling in State Median Income based on 2018 estimates
income_url = 'https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_income'
income = read_html(income_url)
find_table2 = html_nodes(income, xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]")
income_table = html_table(find_table2, fill=T)
income_table = data.frame(income_table)
income_table = income_table[-c(1, 21, 48, 54:57), ]
income_table = income_table[order(income_table$State.or.territory),]
income_table = income_table[c(2,3)]

#Pulling in State Per Capita Income based on 2014 estimates
income2_url = 'https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_income'
income2 = read_html(income2_url)
find_table3 = html_nodes(income2, xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[4]")
income2_table = html_table(find_table3, fill=T)
income2_table = data.frame(income2_table)
income2_table = income2_table[-c(1, 22, 52, 54:57), ]
income2_table = income2_table[order(income2_table$State.or.territory),]
income2_table = income2_table[c(2,3)]

#Adding Income Columns to main dataframe
demographic_data = cbind(demographic_data, 'Median Household Income' = income_table[c(2)], 'Per Capita Income' = income2_table[c(2)])
colnames(demographic_data) = c('State Name', '0-18', '19-25', '26-34', '35-54', '55-64','65+', 'Population', 'Median Household Income', 'Per Capita Income')

########################## 2019 Data
url_2019 <- 'https://www.taxadmin.org/2019-state-tax-revenue'
rev_html_2019 <- read_html(url_2019)

#Get total taxes for 2019
Total_Taxes_2019 <- rev_html_2019 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[2]/span') %>%
  html_text() 
#Get per capita for 2019
Per_Capita_2019 <- rev_html_2019 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[3]/span') %>%
  html_text() 

########################## 2018 Data
url_2018 <- 'https://www.taxadmin.org/2018-state-tax-revenue'
rev_html_2018 <- read_html(url_2018)
#Get total taxes for 2018
Total_Taxes_2018 <- rev_html_2018 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[2]/span') %>%
  html_text() 
#Get per capita for 2018
Per_Capita_2018 <- rev_html_2018 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[3]/span') %>%
  html_text() 

########################## 2017 Data
url_2017 <- 'https://www.taxadmin.org/2017-state-tax-revenue'
rev_html_2017 <- read_html(url_2017)
#Get State from 2017 and use for row header
State <- rev_html_2017 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[1]/span') %>%
  html_text() 
#Get total taxes for 2017
Total_Taxes_2017 <- rev_html_2017 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[2]/span') %>%
  html_text() 
#Get per capita for 2017
Per_Capita_2017 <- rev_html_2017 %>%
  html_nodes(xpath='//table/tbody/tr[position()<54]/td[3]/span') %>%
  html_text() 

#Create dataframe
revenue_data <- data.frame(State,Total_Taxes_2017,Per_Capita_2017,Total_Taxes_2018,Per_Capita_2018,Total_Taxes_2019,Per_Capita_2019) 


#Data Cleaning
demographic_data$`0-18` = as.numeric(gsub("[,$%]","",demographic_data$`0-18`))
demographic_data$`19-25` = as.numeric(gsub("[,$%]","",demographic_data$`19-25`))
demographic_data$`26-34` = as.numeric(gsub("[,$%]","",demographic_data$`26-34`))
demographic_data$`35-54` = as.numeric(gsub("[,$%]","",demographic_data$`35-54`))
demographic_data$`55-64` = as.numeric(gsub("[,$%]","",demographic_data$`55-64`))
demographic_data$`65+` = as.numeric(gsub("[,$%]","",demographic_data$`65+`))
demographic_data$`Population` = as.numeric(gsub("[,$%]","",demographic_data$`Population`))
demographic_data$`Median Household Income` = as.numeric(gsub("[,$%]","",demographic_data$`Median Household Income`))
demographic_data$`Per Capita Income` = as.numeric(gsub("[,$%]","",demographic_data$`Per Capita Income`))
revenue_data$Total_Taxes_2017 <- as.numeric(str_trim(str_replace(revenue_data$Total_Taxes_2017,',','')))
revenue_data$Per_Capita_2017 = as.numeric(str_trim(str_replace(revenue_data$Per_Capita_2017,',','')))
revenue_data$Total_Taxes_2018 = as.numeric(str_trim(str_replace(revenue_data$Total_Taxes_2018,',','')))
revenue_data$Per_Capita_2018 = as.numeric(str_trim(str_replace(revenue_data$Per_Capita_2018,',','')))
revenue_data$Total_Taxes_2019 = as.numeric(str_trim(str_replace(revenue_data$Total_Taxes_2019,',','')))
revenue_data$Per_Capita_2019 = as.numeric(str_trim(str_replace(revenue_data$Per_Capita_2019,',','')))

#Merging both data frames into one
revenue_data = revenue_data[-c(9), ]
demographic_data = cbind(demographic_data, revenue_data$Total_Taxes_2017, revenue_data$Per_Capita_2017, revenue_data$Total_Taxes_2018, revenue_data$Per_Capita_2018, revenue_data$Total_Taxes_2019, revenue_data$Per_Capita_2019)
colnames(demographic_data) = c('State Name', '0-18', '19-25', '26-34', '35-54', '55-64','65+', 'Population', 'Median Household Income', 'Per Capita Income', 'Total Taxes 2017', 'Per Capita Taxes 2017', 'Total Taxes 2018', 'Per Capita Taxes 2018', 'Total Taxes 2019', 'Per Capita Taxes 2019')


str(demographic_data)



remDr$close()
rD$server$stop()

####Descriptive Stats

##Mean, Median, Min, Max in summary
summary(demographic_data)

##Standard Deviation
sd(demographic_data$`0-18`, na.rm=TRUE)
sd(demographic_data$`19-25`, na.rm=TRUE)
sd(demographic_data$`26-34`, na.rm=TRUE)
sd(demographic_data$`35-54`, na.rm=TRUE)
sd(demographic_data$`55-64`, na.rm=TRUE)
sd(demographic_data$`65+`, na.rm=TRUE)
sd(demographic_data$Population, na.rm=TRUE)
sd(demographic_data$`Median Household Income`, na.rm=TRUE)
sd(demographic_data$`Per Capita Income`, na.rm=TRUE)
sd(demographic_data$`Total Taxes 2017`, na.rm=TRUE)
sd(demographic_data$`Per Capita Taxes 2017`, na.rm=TRUE)
sd(demographic_data$`Total Taxes 2018`, na.rm=TRUE)
sd(demographic_data$`Per Capita Taxes 2018`, na.rm=TRUE)
sd(demographic_data$`Total Taxes 2019`, na.rm=TRUE)
sd(demographic_data$`Per Capita Taxes 2019`, na.rm=TRUE)

######################################################################################################################Gambling Revenue Data
###Identify Website
sportsbet_url = 'https://www.legalsportsreport.com/sports-betting/revenue/'
sportsbet = read_html(sportsbet_url)

######Identify Selectors

######Handle: Amount wagered over the time period.
######Bet Revenue: Amount of money kept by sportsbooks out of the amount wagered.
######Hold %: How much revenue sportsbooks keep as a function of handle.
######Taxes/state revenue: Taxes collected by state and local jurisdictions; or state share of proceeds in revenue-sharing markets.


#######Overall Table
#Table headings
overviewtitle_xp = '//*[@id="tablepress-160"]/thead/tr/th/text()'
find_overviewtitle = html_nodes(sportsbet, xpath=overviewtitle_xp)
overviewtitletext = html_text(find_overviewtitle)

#States
states_xp = '//*[@id="tablepress-160"]/tbody/tr/td[1]'
find_states = html_nodes(sportsbet, xpath=states_xp)
states_text = html_text(find_states)

#Handle
handle_xp = '//*[@id="tablepress-160"]/tbody/tr/td[2]'
find_handle = html_nodes(sportsbet, xpath=handle_xp)
handle_text = html_text(find_handle)

#Bet Revenue
revenue_xp = '//*[@id="tablepress-160"]/tbody/tr/td[3]'
find_revenue = html_nodes(sportsbet, xpath=revenue_xp)
revenue_text = html_text(find_revenue)

#Hold
hold_xp = '//*[@id="tablepress-160"]/tbody/tr/td[4]'
find_hold = html_nodes(sportsbet, xpath=hold_xp)
hold_text = html_text(find_hold)

#Taxes/State Revenue
tax_xp = '//*[@id="tablepress-160"]/tbody/tr/td[5]'
find_tax = html_nodes(sportsbet, xpath=tax_xp)
tax_text = html_text(find_tax)

dfTotal = data.frame(states_text, handle_text, revenue_text, hold_text, tax_text)
colnames(dfTotal) = c(overviewtitletext)

##Data Cleaning
dfTotal[dfTotal == "$ -"] = NA
dfTotal[dfTotal == "--"] = NA
dfTotal$Handle = as.numeric(gsub("[,$%]","",dfTotal$Handle))
dfTotal$Revenue = as.numeric(gsub("[,$%]","",dfTotal$Revenue))
dfTotal$Hold = as.numeric(gsub("[,$%]","",dfTotal$Hold))
dfTotal$`Taxes/Jurisdiction Revenue` = as.numeric(gsub("[,$%]","",dfTotal$`Taxes/Jurisdiction Revenue`))

###############Each States' Data
header = c('Month/Year', 'Handle', 'Bet Revenue', 'Hold', 'Taxes')

######New Jersey Table
##NJ sports betting is the biggest outside Nevada. 
##That performance is on the merits of its strong online betting presence:
##about 80% of all handle is generated through sportsbook apps and websites.

#Month/Year
njdate_xp = '//*[@id="tablepress-153"]/tbody/tr/td[1]'
find_njdate = html_nodes(sportsbet, xpath=njdate_xp)
njdate_text = html_text(find_njdate)

#NJ Handle
njhandle_xp = '//*[@id="tablepress-153"]/tbody/tr/td[2]'
find_njhandle = html_nodes(sportsbet, xpath=njhandle_xp)
njhandle_text = html_text(find_njhandle)

#NJ Revenue
njrevenue_xp = '//*[@id="tablepress-153"]/tbody/tr/td[3]'
find_njrevenue = html_nodes(sportsbet, xpath=njrevenue_xp)
njrevenue_text = html_text(find_njrevenue)

#NJ Hold
njhold_xp = '//*[@id="tablepress-153"]/tbody/tr/td[4]'
find_njhold = html_nodes(sportsbet, xpath=njhold_xp)
njhold_text = html_text(find_njhold)

#NJ Taxes
njtax_xp = '//*[@id="tablepress-153"]/tbody/tr/td[5]'
find_njtax = html_nodes(sportsbet, xpath=njtax_xp)
njtax_text = html_text(find_njtax)

dfNewJersey = data.frame(njdate_text, njhandle_text, njrevenue_text, njhold_text, njtax_text)
colnames(dfNewJersey) = header

dfNewJersey["State"] = "New Jersey"

######Pennsylvania Table
##PA sports betting is poised to become one of the biggest markets in the near future.
##So far, the state has mostly relied on retail sportsbooks, but sportsbook apps started 
##going live in May 2019, and should dramatically increase both handle and revenue.
##The only hiccup is the high cost of doing business in the state: an effective tax rate of 36% and a $10 million licensing fee.

#Month/Year
penndate_xp = '//*[@id="tablepress-156"]/tbody/tr/td[1]'
find_penndate = html_nodes(sportsbet, xpath=penndate_xp)
penndate_text = html_text(find_penndate)

#Pennsylvania Handle
pennhandle_xp = '//*[@id="tablepress-156"]/tbody/tr/td[2]'
find_pennhandle = html_nodes(sportsbet, xpath=pennhandle_xp)
pennhandle_text = html_text(find_pennhandle)

#Pennsylvania Revenue
pennrevenue_xp = '//*[@id="tablepress-156"]/tbody/tr/td[3]'
find_pennrevenue = html_nodes(sportsbet, xpath=pennrevenue_xp)
pennrevenue_text = html_text(find_pennrevenue)

#Pennsylvania Hold
pennhold_xp = '//*[@id="tablepress-156"]/tbody/tr/td[4]'
find_pennhold = html_nodes(sportsbet, xpath=pennhold_xp)
pennhold_text = html_text(find_pennhold)

#Pennsylvania Taxes
penntax_xp = '//*[@id="tablepress-156"]/tbody/tr/td[5]'
find_penntax = html_nodes(sportsbet, xpath=penntax_xp)
penntax_text = html_text(find_penntax)

dfPenn = data.frame(penndate_text, pennhandle_text, pennrevenue_text, pennhold_text, penntax_text)
colnames(dfPenn) = header

dfPenn["State"] = "Pennsylvania"

######Delaware Table
##Delaware sports betting was the first to go live outside Nevada in June 2018. Wagering only takes place at three state casinos, 
##and parlay cards for football betting available at lottery retailers. There is no mobile betting although state law allows for it.
##Delaware's monthly reporting schedule ends on the last Sunday of every month. For exact month-end dates, see the breakdown at the Delaware Lottery website.
##There was no sports betting data reported for April through June 2020 because of the coronavirus pandemic.

###Delware shares revenue, for ease of consolidation called 'Tax'

#Month/Year
deldate_xp = '//*[@id="tablepress-250"]/tbody/tr/td[1]'
find_deldate = html_nodes(sportsbet, xpath=deldate_xp)
deldate_text = html_text(find_deldate)

#Delware Handle
delhandle_xp = '//*[@id="tablepress-250"]/tbody/tr/td[2]'
find_delhandle = html_nodes(sportsbet, xpath=delhandle_xp)
delhandle_text = html_text(find_delhandle)

#Delware Revenue
delrevenue_xp = '//*[@id="tablepress-250"]/tbody/tr/td[3]'
find_delrevenue = html_nodes(sportsbet, xpath=delrevenue_xp)
delrevenue_text = html_text(find_delrevenue)

#Delware Hold
delhold_xp = '//*[@id="tablepress-250"]/tbody/tr/td[4]'
find_delhold = html_nodes(sportsbet, xpath=delhold_xp)
delhold_text = html_text(find_delhold)

#Delware Taxes
deltax_xp = '//*[@id="tablepress-250"]/tbody/tr/td[5]'
find_deltax = html_nodes(sportsbet, xpath=deltax_xp)
deltax_text = html_text(find_deltax)

dfDel = data.frame(deldate_text, delhandle_text, delrevenue_text, delhold_text, deltax_text)
colnames(dfDel) = header

dfDel["State"] = "Delaware"

######Mississippi Table
##Mississippi sports betting went live in 2018 at casinos around the state. There are now more than two dozen sportsbooks.
##Online sports betting is not available off the grounds of casinos.
##There was no sports betting activity in April 2020 as casinos were closed because of the coronavirus pandemic.

#Month/Year
missdate_xp = '//*[@id="tablepress-154"]/tbody/tr/td[1]'
find_missdate = html_nodes(sportsbet, xpath=missdate_xp)
missdate_text = html_text(find_missdate)

#Mississippi Handle
misshandle_xp = '//*[@id="tablepress-154"]/tbody/tr/td[2]'
find_misshandle = html_nodes(sportsbet, xpath=misshandle_xp)
misshandle_text = html_text(find_misshandle)

#Mississippi Revenue
missrevenue_xp = '//*[@id="tablepress-154"]/tbody/tr/td[3]'
find_missrevenue = html_nodes(sportsbet, xpath=missrevenue_xp)
missrevenue_text = html_text(find_missrevenue)

#Mississippi Hold
misshold_xp = '//*[@id="tablepress-154"]/tbody/tr/td[4]'
find_misshold = html_nodes(sportsbet, xpath=misshold_xp)
misshold_text = html_text(find_misshold)

#Mississippi Taxes
misstax_xp = '//*[@id="tablepress-154"]/tbody/tr/td[5]'
find_misstax = html_nodes(sportsbet, xpath=misstax_xp)
misstax_text = html_text(find_misstax)

dfMiss = data.frame(missdate_text, misshandle_text, missrevenue_text, misshold_text, misstax_text)
colnames(dfMiss) = header

dfMiss["State"] = "Mississippi"

######Nevada Table
##Nevada has had legal wagering for decades. Billions of dollars are wagered annually, generating hundreds of millions in revenue.
##Here's a look at Nevada sports betting handle and revenue since June 2018, the month that the first sportsbooks opened in other states.
##Nevada has one of the lowest tax rates in the country at 6.75%.
##Note: Results for April and May 2020 are combined. The state did not release monthly sports betting results while sportsbooks and casinos were shut down.

#Month/Year
nevdate_xp = '//*[@id="tablepress-155"]/tbody/tr/td[1]'
find_nevdate = html_nodes(sportsbet, xpath=nevdate_xp)
nevdate_text = html_text(find_nevdate)

#Nevada Handle
nevhandle_xp = '//*[@id="tablepress-155"]/tbody/tr/td[2]'
find_nevhandle = html_nodes(sportsbet, xpath=nevhandle_xp)
nevhandle_text = html_text(find_nevhandle)

#Nevada Revenue
nevrevenue_xp = '//*[@id="tablepress-155"]/tbody/tr/td[3]'
find_nevrevenue = html_nodes(sportsbet, xpath=nevrevenue_xp)
nevrevenue_text = html_text(find_nevrevenue)

#Nevada Hold
nevhold_xp = '//*[@id="tablepress-155"]/tbody/tr/td[4]'
find_nevhold = html_nodes(sportsbet, xpath=nevhold_xp)
nevhold_text = html_text(find_nevhold)

#Nevada Taxes
nevtax_xp = '//*[@id="tablepress-155"]/tbody/tr/td[5]'
find_nevtax = html_nodes(sportsbet, xpath=nevtax_xp)
nevtax_text = html_text(find_nevtax)

dfNev = data.frame(nevdate_text, nevhandle_text, nevrevenue_text, nevhold_text, nevtax_text)
colnames(dfNev) = header

dfNev["State"] = "Nevada"

######Rhode Island Table
##RI sports betting started in late 2018, after the state legalized it earlier in the year. 
##The state actually runs sports betting through a pair of casinos, and with the help of sportsbook company William Hill. 
##RI gets a share of revenue, rather than taxing it.
##Rhode Island did not launch with online betting, but a 2019 law authorized it.

###Rhode Island shares revenue, for ease of consolidation called 'Tax'

#Month/Year
ridate_xp = '//*[@id="tablepress-159"]/tbody/tr/td[1]'
find_ridate = html_nodes(sportsbet, xpath=ridate_xp)
ridate_text = html_text(find_ridate)

#Rhode Island Handle
rihandle_xp = '//*[@id="tablepress-159"]/tbody/tr/td[2]'
find_rihandle = html_nodes(sportsbet, xpath=rihandle_xp)
rihandle_text = html_text(find_rihandle)

#Rhode Island Revenue
rirevenue_xp = '//*[@id="tablepress-159"]/tbody/tr/td[3]'
find_rirevenue = html_nodes(sportsbet, xpath=rirevenue_xp)
rirevenue_text = html_text(find_rirevenue)

#Rhode Island Hold
rihold_xp = '//*[@id="tablepress-159"]/tbody/tr/td[4]'
find_rihold = html_nodes(sportsbet, xpath=rihold_xp)
rihold_text = html_text(find_rihold)

#Rhode Island Taxes
ritax_xp = '//*[@id="tablepress-159"]/tbody/tr/td[5]'
find_ritax = html_nodes(sportsbet, xpath=ritax_xp)
ritax_text = html_text(find_ritax)

dfRI = data.frame(ridate_text, rihandle_text, rirevenue_text, rihold_text, ritax_text)
colnames(dfRI) = header

dfRI["State"] = "Rhode Island"

######West Virginia Table
##WV sports betting went live in 2018, thanks to a law passed even before the federal ban was struck down.
##The state has legalized online betting, too, but there were problems at first. 
##Delaware North, the only live operator, had to shut down. So too did two retail sportsbooks, leaving just three places to bet on sports in the state.
##Sportsbook apps returned in August 2020.
##West Virginia reports results on a weekly basis, so results for some months may include days from other months. Visit the West Virginia Lottery’s website for a breakdown of specific reporting dates.

#Month/Year
wvdate_xp = '//*[@id="tablepress-251"]/tbody/tr/td[1]'
find_wvdate = html_nodes(sportsbet, xpath=wvdate_xp)
wvdate_text = html_text(find_wvdate)

#West Virginia Handle
wvhandle_xp = '//*[@id="tablepress-251"]/tbody/tr/td[2]'
find_wvhandle = html_nodes(sportsbet, xpath=wvhandle_xp)
wvhandle_text = html_text(find_wvhandle)

#West Virginia Revenue
wvrevenue_xp = '//*[@id="tablepress-251"]/tbody/tr/td[3]'
find_wvrevenue = html_nodes(sportsbet, xpath=wvrevenue_xp)
wvrevenue_text = html_text(find_wvrevenue)

#West Virginia Hold
wvhold_xp = '//*[@id="tablepress-251"]/tbody/tr/td[4]'
find_wvhold = html_nodes(sportsbet, xpath=wvhold_xp)
wvhold_text = html_text(find_wvhold)

#West Virginia Taxes
wvtax_xp = '//*[@id="tablepress-251"]/tbody/tr/td[5]'
find_wvtax = html_nodes(sportsbet, xpath=wvtax_xp)
wvtax_text = html_text(find_wvtax)

dfWV = data.frame(wvdate_text, wvhandle_text, wvrevenue_text, wvhold_text, wvtax_text)
colnames(dfWV) = header

dfWV["State"] = "West Virginia"

######New York Table
##New York sports betting began in July 2019 at the upstate casinos. All four commercial properties have sportsbooks open for betting
##as do a few tribal casinos but the existing law does not make provision for statewide online/mobile wagering. 
##As such, expectations for NY sports betting revenue should be mitigated..

#Month/Year
nydate_xp = '//*[@id="tablepress-174"]/tbody/tr/td[1]'
find_nydate = html_nodes(sportsbet, xpath=nydate_xp)
nydate_text = html_text(find_nydate)

#New York Handle
nyhandle_xp = '//*[@id="tablepress-174"]/tbody/tr/td[2]'
find_nyhandle = html_nodes(sportsbet, xpath=nyhandle_xp)
nyhandle_text = html_text(find_nyhandle)

#New York Revenue
nyrevenue_xp = '//*[@id="tablepress-174"]/tbody/tr/td[3]'
find_nyrevenue = html_nodes(sportsbet, xpath=nyrevenue_xp)
nyrevenue_text = html_text(find_nyrevenue)

#New York Hold
nyhold_xp = '//*[@id="tablepress-174"]/tbody/tr/td[4]'
find_nyhold = html_nodes(sportsbet, xpath=nyhold_xp)
nyhold_text = html_text(find_nyhold)

#New York Taxes
nytax_xp = '//*[@id="tablepress-174"]/tbody/tr/td[5]'
find_nytax = html_nodes(sportsbet, xpath=nytax_xp)
nytax_text = html_text(find_nytax)

dfNY = data.frame(nydate_text, nyhandle_text, nyrevenue_text, nyhold_text, nytax_text)
colnames(dfNY) = header

dfNY["State"] = "New York"

######Iowa Table
##Iowa sports betting began in August 2019 across both retail and online/mobile channels in tandem.

#Month/Year
iowadate_xp = '//*[@id="tablepress-171"]/tbody/tr/td[1]'
find_iowadate = html_nodes(sportsbet, xpath=iowadate_xp)
iowadate_text = html_text(find_iowadate)

#Iowa Handle
iowahandle_xp = '//*[@id="tablepress-171"]/tbody/tr/td[2]'
find_iowahandle = html_nodes(sportsbet, xpath=iowahandle_xp)
iowahandle_text = html_text(find_iowahandle)

#Iowa Revenue
iowarevenue_xp = '//*[@id="tablepress-171"]/tbody/tr/td[3]'
find_iowarevenue = html_nodes(sportsbet, xpath=iowarevenue_xp)
iowarevenue_text = html_text(find_iowarevenue)

#Iowa Hold
iowahold_xp = '//*[@id="tablepress-171"]/tbody/tr/td[4]'
find_iowahold = html_nodes(sportsbet, xpath=iowahold_xp)
iowahold_text = html_text(find_iowahold)

#Iowa Taxes
iowatax_xp = '//*[@id="tablepress-171"]/tbody/tr/td[5]'
find_iowatax = html_nodes(sportsbet, xpath=iowatax_xp)
iowatax_text = html_text(find_iowatax)

dfIowa = data.frame(iowadate_text, iowahandle_text, iowarevenue_text, iowahold_text, iowatax_text)
colnames(dfIowa) = header

dfIowa["State"] = "Iowa"

######Indiana Table
##Indiana sports betting began in September 2019, with the first online/mobile apps rolling out about a month later.

#Month/Year
indidate_xp = '//*[@id="tablepress-181"]/tbody/tr/td[1]'
find_indidate = html_nodes(sportsbet, xpath=indidate_xp)
indidate_text = html_text(find_indidate)

#Indiana Handle
indihandle_xp = '//*[@id="tablepress-181"]/tbody/tr/td[2]'
find_indihandle = html_nodes(sportsbet, xpath=indihandle_xp)
indihandle_text = html_text(find_indihandle)

#Indiana Revenue
indirevenue_xp = '//*[@id="tablepress-181"]/tbody/tr/td[3]'
find_indirevenue = html_nodes(sportsbet, xpath=indirevenue_xp)
indirevenue_text = html_text(find_indirevenue)

#Indiana Hold
indihold_xp = '//*[@id="tablepress-181"]/tbody/tr/td[4]'
find_indihold = html_nodes(sportsbet, xpath=indihold_xp)
indihold_text = html_text(find_indihold)

#Indiana Taxes
inditax_xp = '//*[@id="tablepress-181"]/tbody/tr/td[5]'
find_inditax = html_nodes(sportsbet, xpath=inditax_xp)
inditax_text = html_text(find_inditax)

dfIndiana = data.frame(indidate_text, indihandle_text, indirevenue_text, indihold_text, inditax_text)
colnames(dfIndiana) = header

dfIndiana["State"] = "Indiana"

######Oregon Table
##Oregon sports betting began with the tribes in August 2019. Shortly thereafter, the state lottery began 
##offering its own online/mobile product. The reported data does not include tribal sports betting.

#Month/Year
ordate_xp = '//*[@id="tablepress-212"]/tbody/tr/td[1]'
find_ordate = html_nodes(sportsbet, xpath=ordate_xp)
ordate_text = html_text(find_ordate)

#Oregon Handle
orhandle_xp = '//*[@id="tablepress-212"]/tbody/tr/td[2]'
find_orhandle = html_nodes(sportsbet, xpath=orhandle_xp)
orhandle_text = html_text(find_orhandle)

#Oregon Revenue
orrevenue_xp = '//*[@id="tablepress-212"]/tbody/tr/td[3]'
find_orrevenue = html_nodes(sportsbet, xpath=orrevenue_xp)
orrevenue_text = html_text(find_orrevenue)

#Oregon Hold
orhold_xp = '//*[@id="tablepress-212"]/tbody/tr/td[4]'
find_orhold = html_nodes(sportsbet, xpath=orhold_xp)
orhold_text = html_text(find_orhold)

#Oregon Taxes
ortax_xp = '//*[@id="tablepress-212"]/tbody/tr/td[5]'
find_ortax = html_nodes(sportsbet, xpath=ortax_xp)
ortax_text = html_text(find_ortax)

dfOregon = data.frame(ordate_text, orhandle_text, orrevenue_text, orhold_text, ortax_text)
colnames(dfOregon) = header

dfOregon["State"] = "Oregon"

######New Hampshire Table
##New Hampshire sports betting launched in late December 2019 through its DraftKings Sportsbook mobile app.
##DraftKings Sportsbook can also open retail locations throughout the state. Intralot will eventually launch
##sports betting through the New Hampshire Lottery.

###New Hampshire shares revenue, for ease of consolidation called 'Tax'

#Month/Year
nhdate_xp = '//*[@id="tablepress-237"]/tbody/tr/td[1]'
find_nhdate = html_nodes(sportsbet, xpath=nhdate_xp)
nhdate_text = html_text(find_nhdate)

#New Hampshire Handle
nhhandle_xp = '//*[@id="tablepress-237"]/tbody/tr/td[2]'
find_nhhandle = html_nodes(sportsbet, xpath=nhhandle_xp)
nhhandle_text = html_text(find_nhhandle)

#New Hampshire Revenue
nhrevenue_xp = '//*[@id="tablepress-237"]/tbody/tr/td[3]'
find_nhrevenue = html_nodes(sportsbet, xpath=nhrevenue_xp)
nhrevenue_text = html_text(find_nhrevenue)

#New Hampshire Hold
nhhold_xp = '//*[@id="tablepress-237"]/tbody/tr/td[4]'
find_nhhold = html_nodes(sportsbet, xpath=nhhold_xp)
nhhold_text = html_text(find_nhhold)

#New Hampshire Taxes
nhtax_xp = '//*[@id="tablepress-237"]/tbody/tr/td[5]'
find_nhtax = html_nodes(sportsbet, xpath=nhtax_xp)
nhtax_text = html_text(find_nhtax)

dfNH = data.frame(nhdate_text, nhhandle_text, nhrevenue_text, nhhold_text, nhtax_text)
colnames(dfNH) = header

dfNH["State"] = "New Hampshire"

######Michigan Table
##Michigan sports betting launched retail betting in March 2020, right before sports leagues began to shut down because of the coronavirus pandemic.
##Casinos did not reopen until August 5 at 15% maximum occupancy.

#Month/Year
michdate_xp = '//*[@id="tablepress-249"]/tbody/tr/td[1]'
find_michdate = html_nodes(sportsbet, xpath=michdate_xp)
michdate_text = html_text(find_michdate)

#Michigan Handle
michhandle_xp = '//*[@id="tablepress-249"]/tbody/tr/td[2]'
find_michhandle = html_nodes(sportsbet, xpath=michhandle_xp)
michhandle_text = html_text(find_michhandle)

#Michigan Revenue
michrevenue_xp = '//*[@id="tablepress-249"]/tbody/tr/td[3]'
find_michrevenue = html_nodes(sportsbet, xpath=michrevenue_xp)
michrevenue_text = html_text(find_michrevenue)

#Michigan Hold
michhold_xp = '//*[@id="tablepress-249"]/tbody/tr/td[4]'
find_michhold = html_nodes(sportsbet, xpath=michhold_xp)
michhold_text = html_text(find_michhold)

#Michigan Taxes
michtax_xp = '//*[@id="tablepress-249"]/tbody/tr/td[5]'
find_michtax = html_nodes(sportsbet, xpath=michtax_xp)
michtax_text = html_text(find_michtax)

dfMich = data.frame(michdate_text, michhandle_text, michrevenue_text, michhold_text, michtax_text)
colnames(dfMich) = header

dfMich["State"] = "Michigan"

######Colorado Table
##Colorado sports betting launched May 1, 2020 in the middle of the coronavirus pandemic.
##Net sports betting proceeds, which is revenue minus excise taxes paid and free bets, are taxed at 10%.

#Month/Year
coldate_xp = '//*[@id="tablepress-264"]/tbody/tr/td[1]'
find_coldate = html_nodes(sportsbet, xpath=coldate_xp)
coldate_text = html_text(find_coldate)

#Colorado Handle
colhandle_xp = '//*[@id="tablepress-264"]/tbody/tr/td[2]'
find_colhandle = html_nodes(sportsbet, xpath=colhandle_xp)
colhandle_text = html_text(find_colhandle)

#Colorado Revenue
colrevenue_xp = '//*[@id="tablepress-264"]/tbody/tr/td[3]'
find_colrevenue = html_nodes(sportsbet, xpath=colrevenue_xp)
colrevenue_text = html_text(find_colrevenue)

#Colorado Hold
colhold_xp = '//*[@id="tablepress-264"]/tbody/tr/td[4]'
find_colhold = html_nodes(sportsbet, xpath=colhold_xp)
colhold_text = html_text(find_colhold)

#Colorado Taxes
coltax_xp = '//*[@id="tablepress-264"]/tbody/tr/td[5]'
find_coltax = html_nodes(sportsbet, xpath=coltax_xp)
coltax_text = html_text(find_coltax)

dfCol = data.frame(coldate_text, colhandle_text, colrevenue_text, colhold_text, coltax_text)
colnames(dfCol) = header

dfCol["State"] = "Colorado"

######Illinois Table
##Illinois sports betting launched March 9 but was suspended March 16. The first online bet wasn't accepted until June 18.

#Month/Year
illdate_xp = '//*[@id="tablepress-274"]/tbody/tr/td[1]'
find_illdate = html_nodes(sportsbet, xpath=illdate_xp)
illdate_text = html_text(find_illdate)

#Illinois Handle
illhandle_xp = '//*[@id="tablepress-274"]/tbody/tr/td[2]'
find_illhandle = html_nodes(sportsbet, xpath=illhandle_xp)
illhandle_text = html_text(find_illhandle)

#Illinois Revenue
illrevenue_xp = '//*[@id="tablepress-274"]/tbody/tr/td[3]'
find_illrevenue = html_nodes(sportsbet, xpath=illrevenue_xp)
illrevenue_text = html_text(find_illrevenue)

#Illinois Hold
illhold_xp = '//*[@id="tablepress-274"]/tbody/tr/td[4]'
find_illhold = html_nodes(sportsbet, xpath=illhold_xp)
illhold_text = html_text(find_illhold)

#Illinois Taxes
illtax_xp = '//*[@id="tablepress-274"]/tbody/tr/td[5]'
find_illtax = html_nodes(sportsbet, xpath=illtax_xp)
illtax_text = html_text(find_illtax)

dfIll = data.frame(illdate_text, illhandle_text, illrevenue_text, illhold_text, illtax_text)
colnames(dfIll) = header

dfIll["State"] = "Illinois"

######Arkansas Table
#Arkansas sports betting began in July 2019 at Oaklawn Casino. Saracen Casino added sports betting in October 2019 
#and Southland Casino took its first bet in January 2020.
#Sports betting is only available at land-based casinos.
#Casinos closed in March through May 18 because of the coronavirus pandemic.

#Month/Year
arkdate_xp = '//*[@id="tablepress-293"]/tbody/tr/td[1]'
find_arkdate = html_nodes(sportsbet, xpath=arkdate_xp)
arkdate_text = html_text(find_arkdate)

#Arkansas Handle
arkhandle_xp = '//*[@id="tablepress-293"]/tbody/tr/td[2]'
find_arkhandle = html_nodes(sportsbet, xpath=arkhandle_xp)
arkhandle_text = html_text(find_arkhandle)

#Arkansas Revenue
arkrevenue_xp = '//*[@id="tablepress-293"]/tbody/tr/td[3]'
find_arkrevenue = html_nodes(sportsbet, xpath=arkrevenue_xp)
arkrevenue_text = html_text(find_arkrevenue)

#Arkansas Hold
arkhold_xp = '//*[@id="tablepress-293"]/tbody/tr/td[4]'
find_arkhold = html_nodes(sportsbet, xpath=arkhold_xp)
arkhold_text = html_text(find_arkhold)

#Arkansas Taxes
arktax_xp = '//*[@id="tablepress-293"]/tbody/tr/td[5]'
find_arktax = html_nodes(sportsbet, xpath=arktax_xp)
arktax_text = html_text(find_arktax)

dfArk = data.frame(arkdate_text, arkhandle_text, arkrevenue_text, arkhold_text, arktax_text)
colnames(dfArk) = header

dfArk["State"] = "Arkansas"

######Tennessee Table
##Tennessee sports betting launched November 1 as a mobile-only market.

#Month/Year
tendate_xp = '//*[@id="tablepress-298"]/tbody/tr/td[1]'
find_tendate = html_nodes(sportsbet, xpath=tendate_xp)
tendate_text = html_text(find_tendate)

#Tennessee Handle
tenhandle_xp = '//*[@id="tablepress-298"]/tbody/tr/td[2]'
find_tenhandle = html_nodes(sportsbet, xpath=tenhandle_xp)
tenhandle_text = html_text(find_tenhandle)

#Tennessee Revenue
tenrevenue_xp = '//*[@id="tablepress-298"]/tbody/tr/td[3]'
find_tenrevenue = html_nodes(sportsbet, xpath=tenrevenue_xp)
tenrevenue_text = html_text(find_tenrevenue)

#Tennessee Hold
tenhold_xp = '//*[@id="tablepress-298"]/tbody/tr/td[4]'
find_tenhold = html_nodes(sportsbet, xpath=tenhold_xp)
tenhold_text = html_text(find_tenhold)

#Tennessee Taxes
tentax_xp = '//*[@id="tablepress-298"]/tbody/tr/td[5]'
find_tentax = html_nodes(sportsbet, xpath=tentax_xp)
tentax_text = html_text(find_tentax)

dfTen = data.frame(tendate_text, tenhandle_text, tenrevenue_text, tenhold_text, tentax_text)
colnames(dfTen) = header

dfTen["State"] = "Tennessee"

######Virginia Table
##Virginia sports betting launched Jan. 21 when FanDuel Sportsbook went live. 
#Since then, four other operators went live in January. There could still be more than a dozen operators waiting for a license.

#Month/Year
virdate_xp = '//*[@id="tablepress-307"]/tbody/tr/td[1]'
find_virdate = html_nodes(sportsbet, xpath=virdate_xp)
virdate_text = html_text(find_virdate)

#Virginia Handle
virhandle_xp = '//*[@id="tablepress-307"]/tbody/tr/td[2]'
find_virhandle = html_nodes(sportsbet, xpath=virhandle_xp)
virhandle_text = html_text(find_virhandle)

#Virginia Revenue
virrevenue_xp = '//*[@id="tablepress-307"]/tbody/tr/td[3]'
find_virrevenue = html_nodes(sportsbet, xpath=virrevenue_xp)
virrevenue_text = html_text(find_virrevenue)

#Virginia Hold
virhold_xp = '//*[@id="tablepress-307"]/tbody/tr/td[4]'
find_virhold = html_nodes(sportsbet, xpath=virhold_xp)
virhold_text = html_text(find_virhold)

#Virginia Taxes
virtax_xp = '//*[@id="tablepress-307"]/tbody/tr/td[5]'
find_virtax = html_nodes(sportsbet, xpath=virtax_xp)
virtax_text = html_text(find_virtax)

dfVir = data.frame(virdate_text, virhandle_text, virrevenue_text, virhold_text, virtax_text)
colnames(dfVir) = header

dfVir["State"] = "Virginia"

#Create Master Merged Dataframe
#dfStatesMerged = do.call("rbind", list(dfCol,dfDel,dfIll,dfIndiana,dfIowa,dfMich,dfMiss,dfNev,dfNewJersey,dfNH,dfNY,dfOregon,dfPenn,dfRI,dfWV))
dfStatesMerged = do.call("rbind", list(dfArk,dfCol,dfDel,dfIll,dfIndiana,dfIowa,dfMich,dfMiss,dfNev,dfNewJersey,dfNH,dfNY,dfOregon,dfPenn,dfRI,dfTen,dfVir,dfWV))
###Cleaning Merged Data
dfStatesMerged[dfStatesMerged == "$ -"] = 0
dfStatesMerged[dfStatesMerged == "--"] = 0
dfStatesMerged[dfStatesMerged == "N/A"] = 0
dfStatesMerged$Taxes = as.character(gsub("[,$%)]","",dfStatesMerged$Taxes))
dfStatesMerged$Taxes = as.numeric(gsub("[(]","-",dfStatesMerged$Taxes))
dfStatesMerged$Hold = as.character(gsub("[,$%)]","",dfStatesMerged$Hold))
dfStatesMerged$Hold = as.numeric(gsub("[(]","-",dfStatesMerged$Hold))
dfStatesMerged$'Bet Revenue' = as.character(gsub("[,$%)]","",dfStatesMerged$'Bet Revenue'))
dfStatesMerged$'Bet Revenue' = as.numeric(gsub("[(]","-",dfStatesMerged$'Bet Revenue'))
dfStatesMerged$Handle = as.character(gsub("[,$%)]","",dfStatesMerged$Handle))
dfStatesMerged$Handle = as.numeric(gsub("[(]","-",dfStatesMerged$Handle))

#Merging both main data frames to make calculations on % of states income from gambling
dfStatesMerged2 = data.frame(do.call("rbind", strsplit(as.character(dfStatesMerged$`Month/Year`), ' ', fixed = TRUE)))
dfStatesMerged3 = cbind(dfStatesMerged, 'Month' = dfStatesMerged2$X1, 'Year' = dfStatesMerged2$X2)
dfStatesMerged3 = data.frame(transform(dfStatesMerged3, StateYear=paste(dfStatesMerged3$`State`, dfStatesMerged3$`Year`, sep=" ")))
dfStatesMerged3$Handle = as.character(gsub("[,$%)]","",dfStatesMerged3$Handle))
dfStatesMerged3$Handle = as.numeric(gsub("[(]","-",dfStatesMerged3$Handle))
dfStatesMerged3 = dfStatesMerged3[c(2:5, 9)]
dfStatesMerged3 <- aggregate(dfStatesMerged3[-5], by = list(dfStatesMerged3$StateYear), FUN = sum)
dfStatesMerged4 = data.frame(do.call("rbind", strsplit(as.character(dfStatesMerged3$Group.1), '[A-z ]+\\s')))
dfStatesMerged5 = data.frame(do.call("rbind", strsplit(as.character(dfStatesMerged3$Group.1), '\\s[0-9]+')))
dfStatesMerged6 = cbind(dfStatesMerged3, 'State' = dfStatesMerged5, 'Year' = dfStatesMerged4$X2)
dfStatesMerged6 = dfStatesMerged6[c(2, 3, 5, 6, 7)]
colnames(dfStatesMerged6) = c('Handle', 'Bet Revenue', 'Taxes', 'State Name', 'Year')
dfStatesMerged6 = dfStatesMerged6[, c(4, 5, 1, 2, 3)]
dfStatesMerged7 = subset(dfStatesMerged6, dfStatesMerged6$Year == 2018)
demographic_data2 = left_join(demographic_data, dfStatesMerged7, by = "State Name")
dfStatesMerged8 = subset(dfStatesMerged6, dfStatesMerged6$Year == 2019)
demographic_data3 = left_join(demographic_data2, dfStatesMerged8, by = "State Name")
dfStatesMerged9 = subset(dfStatesMerged6, dfStatesMerged6$Year == 2020)
demographic_data4 = left_join(demographic_data3, dfStatesMerged9, by = "State Name")
dfStatesMerged10 = subset(dfStatesMerged6, dfStatesMerged6$Year == 2021)
demographic_data5 = left_join(demographic_data4, dfStatesMerged10, by = "State Name")
demographic_data5 = demographic_data5[-c(17, 21, 25, 29)]
colnames(demographic_data5) = c('State Name', '0-18', '19-25', '26-34', '35-54', '55-64','65+', 'Population', 'Median Household Income', 'Per Capita Income', 
                                'Total Taxes 2017', 'Per Capita Taxes 2017', 'Total Taxes 2018', 'Per Capita Taxes 2018', 'Total Taxes 2019', 'Per Capita Taxes 2019',
                                'Total Handle 2018', 'Total Bet Revenue 2018', 'Total Taxes Collected 2018', 'Total Handle 2019', 
                                'Total Bet Revenue 2019', 'Total Taxes Collected 2019', 'Total Handle 2020', 'Total Bet Revenue 2020', 'Total Taxes Collected 2020',
                                'Total Handle 2021', 'Total Bet Revenue 2021', 'Total Taxes Collected 2021')
demographic_data5$`Total Taxes Collected 2018` = (demographic_data5$`Total Taxes Collected 2018`)/1000000
demographic_data5$`Total Taxes Collected 2019` = (demographic_data5$`Total Taxes Collected 2019`)/1000000
demographic_data5$`Total Taxes Collected 2020` = (demographic_data5$`Total Taxes Collected 2020`)/1000000
demographic_data5$`Total Taxes Collected 2021` = (demographic_data5$`Total Taxes Collected 2021`)/1000000
demographic_data5$`Percent of Taxes from Betting 2018` = ((demographic_data5$`Total Taxes Collected 2018`)/(demographic_data5$`Total Taxes 2018`))*100
demographic_data5$`Percent of Taxes from Betting 2019` = ((demographic_data5$`Total Taxes Collected 2019`)/(demographic_data5$`Total Taxes 2019`))*100
demographic_data5$`Percent of Taxes from Betting 2020` = ((demographic_data5$`Total Taxes Collected 2020`)/(demographic_data5$`Total Taxes 2019`))*100
demographic_data5$`Projected Percent of Taxes from Betting 2021` = ((demographic_data5$`Total Taxes Collected 2021`)/(demographic_data5$`Total Taxes 2019`))*100
####Descriptive Stats

##Mean, Median, Min, Max in summary
summary(demographic_data5)

##Standard Deviation

sd(demographic_data5$`Percent of Taxes from Betting 2018`, na.rm=TRUE)
sd(demographic_data5$`Percent of Taxes from Betting 2019`, na.rm=TRUE)
sd(demographic_data5$`Percent of Taxes from Betting 2020`, na.rm=TRUE)
sd(demographic_data5$`Projected Percent of Taxes from Betting 2021`, na.rm=TRUE)

#Exporting two main dataframes as csv for external visualiation construction
#write.csv(demographic_data5,"C:\\Users\\Andrew\\Documents\\R 5193\\Team_Project_MA_JB_RH_AK\\demographic_data.csv", row.names = T)
#write.csv(dfStatesMerged,"C:\\Users\\Andrew\\Documents\\R 5193\\Team_Project_MA_JB_RH_AK\\state_betting_data.csv", row.names = T)

#################################################
#==========Principal Component Analysis=========#

#na to 0 values
demographic_data5[is.na(demographic_data5)] <- 0

pcamodel_reduc = princomp(na.omit(demographic_data5[,2:32]),cor=TRUE)		
pcamodel_reduc$sdev^2				


plot(pcamodel_reduc,main="State Betting Data - Scree Plot",col='blue', horiz=T, las=1)					


#################################################
#==========Factor Analysis=========#

#=====manipulate column names to cooporate with factor analysis

colnames(demographic_data5) <-
  c('StateName'
    ,'p0_18'
    ,'p19_25'
    ,'p26_34'   
    ,'p35_54'
    ,'p55_64'
    ,'p65_plus' 
    ,'Population'
    ,'Median_Household_Income'
    ,'Per_Capita_Income'
    ,'Total_Taxes_2017'
    ,'Per_Capita_Taxes_2017'
    ,'TotalTaxes2018'
    ,'PerCapitaTaxes2018'
    ,'TotalTaxes2019'
    ,'PerCapitaTaxes2019'
    ,'TotalHandle2018'
    ,'TotalBetRevenue2018'
    ,'TotalTaxesCollected2018'
    ,'TotalHandle2019'
    ,'TotalBetRevenue2019'
    ,'TotalTaxesCollected2019'
    ,'TotalHandle2020'
    ,'TotalBetRevenue2020'
    ,'TotalTaxesCollected2020'
    ,'Total_Handle_2021'
    ,'Total_Bet_Revenue_2021'
    ,'Total_Taxes_Collected_2021'
    ,'Percent_of_Taxes_from_Betting_2018' 
    ,'Percent_of_Taxes_from_Betting_2019'
    ,'Percent_of_Taxes_from_Betting_2020'
    ,'Projected_Percent_of_Taxes_from_Betting_2021')

reduction_data.FA = factanal(~p0_18+p19_25
                             +p26_34   
                             +p35_54
                             +p55_64
                             +p65_plus 
                             +Population
                             +Median_Household_Income
                             +Per_Capita_Income,
                             factors=4,
                             rotation="varimax",
                             scores="none",
                             data=demographic_data5)		

reduction_data.FA


