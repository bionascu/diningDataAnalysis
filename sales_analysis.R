# Read data

sales_path = '/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/'

sales_full = read.csv(paste(sales_path, 'sales.csv', sep=''), stringsAsFactors=F)
sales_full_3 = read.csv(paste(sales_path, 'sales_3.csv', sep=''), stringsAsFactors=F)
sales_full_k = read.csv(paste(sales_path, 'sales_3-kmistry-f-yacout-remix.csv', sep=''), stringsAsFactors=F)

# Pre-processing
sales = na.omit(sales_full)
sales_3 = na.omit(sales_full_3)
sales_k = na.omit(sales_full_k)

# Recover FOOD family from sales
food_fam = sales[sales$family=='FOOD',]
split_name_list = lapply(strsplit(food_fam$item, ","), function(x) gsub("^\\s+|\\s+$", "", x))
split_name = data.frame(matrix(ncol = 2, nrow = length(split_name_list)))

for(ii in 1:length(split_name_list)){
	split_name[ii,1] = split_name_list[[ii]][1]
	split_name[ii,2] = split_name_list[[ii]][2]
}
colnames(split_name) = c('item', 'family')

food_fam$revenue = food_fam$qnty.sold
food_fam$qnty.sold = food_fam$group
food_fam$group = food_fam$family
food_fam$family = split_name$family
food_fam$item = split_name$item

# Add recovered data to sales_3
sales_3[sales_3$family == 'FOOD',] = food_fam
sales_4 = sales_3[-which(sales_3$family == 'FOOD'),]
sales_4 = rbind(sales_4, food_fam)


sales_full_4 = read.csv(paste(sales_path, 'sales_4.csv', sep=''), stringsAsFactors=F)
sales_4 = na.omit(sales_full_4)

# Add weekday and month data
sales_4$date = as.Date(sales_4$date, '%Y-%m-%d')
sales_4$weekday = weekdays(sales_4$date)
#sales_4$month = month(sales_4$date)


# Restrict dates

sales_5 = sales_4[sales_4$date >= as.Date('2015-09-30', '%Y-%m-%d') & sales_4$date <= as.Date('2015-12-30', '%Y-%m-%d'),]


#unique_families = unique(sales$family)
#write.csv(unique_families,'/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/unique_families.csv',row.names=F)


# Separate dining hall food
selected_families = read.csv('/Users/nyuad/Documents/repos/diningDataAnalysis/unique_families3.csv',stringsAsFactors=F, header=F)
colnames(selected_families) = 'Family'

dh_food = sales_4[sales_4$family %in% selected_families$Family,]
dh_food$qnty.sold = as.numeric(dh_food$qnty.sold)

dh_food = mutate(dh_food, weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                          "Thursday", "Friday", "Saturday")))

gs_food = dh_food[dh_food$item == 'GLOBAL STREET MAIN',]

dh_food_oct  = dh_food[dh_food$date >= as.Date('2015-10-01', '%Y-%m-%d') & mp_food$date < as.Date('2015-11-01', '%Y-%m-%d'),]
dh_food_nov  = dh_food[dh_food$date >= as.Date('2015-11-01', '%Y-%m-%d') & mp_food$date < as.Date('2015-12-01', '%Y-%m-%d'),]
dh_food_dec  = dh_food[dh_food$date >= as.Date('2015-12-01', '%Y-%m-%d') & mp_food$date < as.Date('2016-01-01', '%Y-%m-%d'),]



# Separate Market Place food
mp_food = sales_k[sales_k$place2 == 'Marketplace',]
mp_food = mp_food[,c('item', 'family','group','qnty.sold','revenue','date')]
mp_food$date = as.Date(mp_food$date, '%m/%d/%y')
mp_food$weekday = weekdays(mp_food$date)
mp_food = mp_food[mp_food$date >= as.Date('2015-09-30', '%Y-%m-%d') & mp_food$date <= as.Date('2015-12-30', '%Y-%m-%d'),]

mp_food = mutate(mp_food, weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                                          "Thursday", "Friday", "Saturday")))

mp_food_per_date = aggregate(mp_food$qnty.sold, by=list(mp_food$date),FUN=sum)
colnames(mp_food_per_date) = c('date','tot.qnty.sold')
mp_food_per_date$weekday = weekdays(mp_food_per_date$date)

mp_food_per_weekday = aggregate(mp_food$qnty.sold, by=list(mp_food$weekday),FUN=sum)

# Dining Hall Aggregates
dh_food_per_date = aggregate(dh_food$qnty.sold, by=list(dh_food$date),FUN=sum)
colnames(dh_food_per_date) = c('date','tot.qnty.sold')
dh_food_per_date$weekday = weekdays(dh_food_per_date$date)

gs_food_per_date = aggregate(gs_food$qnty.sold, by=list(gs_food$date),FUN=sum)
colnames(gs_food_per_date) = c('date','tot.qnty.sold')
gs_food_per_date$weekday = weekdays(gs_food_per_date$date)


# Plot Global Street per day
library(ggplot2)
library("RColorBrewer") 



#Create a custom color scale
myColors <- brewer.pal(7,"Set1")
names(myColors) <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
colScale <- scale_colour_manual(name = "Weekday",values = myColors)

# One plot with the GS data
p <- ggplot(gs_food_per_date,aes(date,tot.qnty.sold,colour = weekday)) + geom_line()
p_ <- p + colScale + ggtitle("Average Amount of Global Street Food Sold (Oct - Dec)")

#One plot with all the data
p2 <- ggplot(dh_food_per_date,aes(date,tot.qnty.sold,colour = weekday)) + geom_line()
p2_ <- p2 + colScale + ggtitle("Average Amount of Dining Hall Food Sold (Oct - Dec)") 

# One plot with the MP data
p3 <- ggplot(mp_food_per_date,aes(date,tot.qnty.sold,colour = weekday)) + geom_line()
p3_ <- p3 + colScale + ggtitle("Average Amount of Market Place Food Sold (Oct - Dec)")




pref_dh <-  group_by(dh_food, weekday) %>% summarize(qnty_dh = mean(sum(qnty.sold)),
                                                   var_dh = var(qnty.sold),
                                                   sd_dh = sd(qnty.sold),
                                                   n_dh = n(),
                                                   n_weeks = length((qnty.sold)))

#pref_dh$n_weeks <- 
arrange(pref_dh, weekday)

#aggregate(dh_food$weekday, by=list(dh_food$date),FUN=n)

#count(dh_food$weekday, dh_food$date)


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



pref_merged = merge(pref_dh,pref_gs,by="weekday")
pref_merged$percentage = pref_merged$qnty_gs / pref_merged$qnty_dh
pref_merged$scaled_dh = pref_merged$qnty_dh / pref_merged$n_gs


pref_merged2 = merge(pref_merged,pref_mp,by="weekday")
pref_merged2$scaled_mp = pref_merged2$qnty_mp / pref_merged2$n_gs
pref_merged2 = pref_merged2[-which(pref_merged2$weekday == 'Saturday'),]
pref_merged2$scaled_gs = pref_merged2$qnty_gs / pref_merged2$n_gs


#pref_merged = mutate(pref_merged, weekday = factor(weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", 
#  "Thursday", "Friday", "Saturday")))

arrange(pref_merged2, weekday)


pref_dh = pref_dh[-which(pref_dh$weekday == 'Saturday'),]
pref_gs = pref_gs[-which(pref_gs$weekday == 'Saturday'),]
pref_mp = pref_mp[-which(pref_mp$weekday == 'Saturday'),]



# BAR PLOTS!!!

ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=qnty),stat="identity") +
  geom_errorbar(aes(x=weekday, ymax=qnty+1.96*sd, ymin=qnty-1.96*sd))




ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=scaled_dh),stat="identity") + ggtitle("Average Amount of Dining Hall Food Sold Per Weekday")

ggplot(data = pref_gs) + 
  geom_bar(aes(x=weekday, y=qnty_gs),stat="identity") + ggtitle("Average Amount of Global Street Food Sold Per Weekday")


ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=scaled_mp),stat="identity") + ggtitle("Average Amount of Market Place Food Sold Per Weekday")


p = ggplot(data = pref_merged2) + 
  geom_bar(aes(x=weekday, y=percentage*100),stat="identity") + ggtitle("Percentage of Global Street Food Sold Per Weekday") + coord_cartesian(ylim=c(4, 7))

#Create a custom color scale
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)


#One plot with all the data
p <- ggplot(dat,aes(x,y,colour = grp)) + geom_point()
p1 <- p + colScale

#A second plot with only four of the levels
p2 <- p %+% droplevels(subset(dat[4:10,])) + colScale



#3########
all_dates = 

friday = subset(sales, weekday == 'Friday')

burger_friday = subset(friday, family == 'Burgers')
str(friday)
sum(burger_friday$qnty.sold)/sum(friday$qnty.sold)


table(sales$date)

# Find number of missing dates between data points
unique_dates = unique(sales$date)
day_intervals = difftime(unique_dates[2:end(unique_dates)],unique_dates[1:end(unique_dates)-1])



unique_families = unique(sales$family)
write.csv(unique_families,'/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/unique_families.csv',row.names=F)
write.csv(sales_4,'/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/sales_4.csv',row.names=F)

#November 10 - spike in all food



#sales$qnty.sold = abs(sales$qnty.sold)
#sales$qnty.sold = round(sales$qnty.sold)


#subset data based on food and date

