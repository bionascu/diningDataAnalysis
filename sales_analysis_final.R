# Read data
sales_path = '/Users/nyuad/Documents/repos/diningDataAnalysis/'

sales_full_k = read.csv(paste(sales_path, 'sales_3-kmistry.csv', sep=''), stringsAsFactors=F)
sales_full_4 = read.csv(paste(sales_path, 'sales_4.csv', sep=''), stringsAsFactors=F)

# Pre-processing
sales_k = na.omit(sales_full_k)
sales_4 = na.omit(sales_full_4)

# Add weekday and month data
sales_4$date = as.Date(sales_4$date, '%Y-%m-%d')
sales_4$weekday = weekdays(sales_4$date)


# Restrict dates
sales_5 = sales_4[sales_4$date >= as.Date('2015-09-30', '%Y-%m-%d') & sales_4$date <= as.Date('2015-12-30', '%Y-%m-%d'),]
sales_4 = sales_5


# Separate dining hall food using family names in selected_families
selected_families = read.csv(paste(sales_path, 'unique_families3.csv', sep=''), header=F)
colnames(selected_families) = 'Family'

dh_food = sales_4[sales_4$family %in% selected_families$Family,]
dh_food$qnty.sold = as.numeric(dh_food$qnty.sold)


library(dplyr)
dh_food = mutate(dh_food, weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                          "Thursday", "Friday", "Saturday")))

gs_food = dh_food[dh_food$item == 'GLOBAL STREET MAIN',]


# Separate Market Place food
mp_food = sales_k[sales_k$place2 == 'Marketplace',]
mp_food = mp_food[,c('item', 'family','group','qnty.sold','revenue','date')]
mp_food$date = as.Date(mp_food$date, '%m/%d/%y')
mp_food$weekday = weekdays(mp_food$date)
mp_food = mp_food[mp_food$date >= as.Date('2015-09-30', '%Y-%m-%d') & mp_food$date <= as.Date('2015-12-30', '%Y-%m-%d'),]

mp_food = mutate(mp_food, weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                          "Thursday", "Friday", "Saturday")))




pref_dh <-  group_by(dh_food, weekday) %>% summarize(qnty_dh = mean(sum(qnty.sold)),
                                                   var_dh = var(qnty.sold),
                                                   sd_dh = sd(qnty.sold),
                                                   n_dh = n(),
                                                   n_weeks = length((qnty.sold)))

arrange(pref_dh, weekday)

pref_gs <-  group_by(gs_food, weekday) %>% summarize(qnty_gs = mean(sum(qnty.sold)),
                                                   var_gs = var(qnty.sold),
                                                   sd_gs = sd(qnty.sold),
                                                   n_gs = n())
arrange(pref_gs, weekday)

pref_mp <-  group_by(mp_food, weekday) %>% summarize(qnty_mp = mean(sum(qnty.sold)),
                                                   var_mp = var(qnty.sold),
                                                   sd_mp = sd(qnty.sold),
                                                   n_mp = n())
arrange(pref_mp, weekday)


pref_dh = pref_dh[-which(pref_dh$weekday == 'Saturday'),]
pref_gs = pref_gs[-which(pref_gs$weekday == 'Saturday'),]
pref_mp = pref_mp[-which(pref_mp$weekday == 'Saturday'),]


pref_merged = merge(pref_dh,pref_gs,by="weekday")
pref_merged$percentage = pref_merged$qnty_gs / pref_merged$qnty_dh
pref_merged$scaled_dh = pref_merged$qnty_dh / pref_merged$n_gs


pref_merged2 = merge(pref_merged,pref_mp,by="weekday")
pref_merged2$scaled_mp = pref_merged2$qnty_mp / pref_merged2$n_gs
pref_merged2$scaled_gs = pref_merged2$qnty_gs / pref_merged2$n_gs


arrange(pref_merged2, weekday)



# BAR PLOTS
library(ggplot2)

ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=qnty_gs),stat="identity") +
  geom_errorbar(aes(x=weekday, ymax=qnty_gs+1.96*sd_gs, ymin=qnty_gs-1.96*sd_gs))


# DINING HALL
ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=scaled_dh),stat="identity") + ggtitle("Average Amount of Dining Hall Food Sold Per Weekday")

# GLOBAL STREET
ggplot(data = pref_gs) + 
  geom_bar(aes(x=weekday, y=qnty_gs),stat="identity") + ggtitle("Average Amount of Global Street Food Sold Per Weekday")

# MARKET PLACE
ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=scaled_mp),stat="identity") + ggtitle("Average Amount of Market Place Food Sold Per Weekday")

# PERCENTAGE GLOBAL STREET FROM DINING HALL 
p = ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=percentage*100),stat="identity") + ggtitle("Percentage of Global Street Food Sold Per Weekday") + coord_cartesian(ylim=c(4, 7))

