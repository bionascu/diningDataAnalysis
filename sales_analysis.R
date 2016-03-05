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

# Add weekday and month data
sales_4$date = as.Date(sales_4$date, '%Y-%m-%d')
sales_4$weekday = weekdays(sales_4$date)
sales_4$month = month(sales_4$date)


# whatever appears less than 5 time
# in food family clean out cs

#unique_families = unique(sales$family)
#write.csv(unique_families,'/Users/nyuad/Documents/workspace/Datathon/datathon data/sales/unique_families.csv',row.names=F)

# Separate dining hall food

selected_families_dh = read.csv('/Users/nyuad/Documents/repos/diningDataAnalysis/unique_families3.csv',stringsAsFactors=F, header=F)
colnames(selected_families) = 'Family'

dh_food = sales_4[sales_4$family %in% selected_families$Family,]

# Separate Market Place
mp_food = sales_k[]


# Separate Market Place food


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

