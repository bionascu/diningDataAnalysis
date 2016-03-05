# Read data

sales_path = '/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/'

sales_full = read.csv(paste(sales_path, 'sales.csv', sep=''), stringsAsFactors=F)
sales_full_3 = read.csv(paste(sales_path, 'sales_3.csv', sep=''), stringsAsFactors=F)
sales_full_k = read.csv(paste(sales_path, 'sales-kmistry-hot-n-heavy-remix.csv', sep=''), stringsAsFactors=F)
sales_full_k = sales_full_k[c('item', 'family', 'group', 'qnty.sold', 'revenue', 'date', 'place')]

# Pre-processing

sales = na.omit(sales_full)
sales_3 = na.omit(sales_full_3)
sales_k = na.omit(sales_full_k)

sales = 

sales$date = as.Date(sales$date, '%Y-%m-%d')
sales$weekday = weekdays(sales$date)
sales$month = month(sales$date)

# Keep only dining hall food
# whatever appears less than 5 time
# in food family clean out cs
selected_families = read.csv('/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/unique_families.csv',stringsAsFactors=F, header=F)
colnames(selected_families) = 'Family'

food = sales[sales$family %in% selected_families$Family,]
food_3 = sales_3[sales_3$family %in% selected_families$Family,]




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

#November 10 - spike in all food



#sales$qnty.sold = abs(sales$qnty.sold)
#sales$qnty.sold = round(sales$qnty.sold)


#subset data based on food and date