
#Convert into dates

daily_activity$New_Date = as.Date(daily_activity$dateActivity)

in_app_Purchases$New_Date = as.Date(in_app_Purchases$dateActivity)

#sort the two dates

daily_sorted <- daily_activity[order(as.Date(daily_activity$New_Date, increasing = TRUE)),]
in_app_sorted <- in_app_Purchases[order(as.Date(in_app_Purchases$New_Date, increasing = TRUE)),]

#Remove old date
daily_sorted$dateActivity <- NULL
in_app_sorted$dateActivity <- NULL

#Remove N/A Values

daily_sorted_NA <- na.omit(daily_sorted)

#Merge Dataframes

daily_in_app <- merge(daily_sorted_NA, in_app_sorted, by = c('userId'))

#Rename NewDate.x = date for daily_activity, and NewDate.y = date for in app purchases
#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
names(daily_in_app)[names(daily_in_app) == 'New_Date.x'] <- 'activityDate'
names(daily_in_app)[names(daily_in_app) == 'New_Date.y'] <- 'purchaseDate'



#Make two dataframes,  for the group_control and one for the group_test

#colnames(daily_in_app)
group_control <- dplyr::filter(daily_in_app, abTestGroup =='group_control')
group_test <- dplyr::filter(daily_in_app, abTestGroup =='group_test')

#Identify the data range

max(group_control$cost, na.rm = TRUE)
min(group_control$cost, na.rm = TRUE)

max(group_test$cost, na.rm = TRUE)
min(group_test$cost, na.rm = TRUE)

#IMPORTANT NOTE: The data has a wide range (0.198 to 19.998) in both tables, this is because each in app purchase has a set price.
#The mean is off because of the activity date, drop the column

group_control$activityDate <- NULL
group_test$activityDate <- NULL

#Now remove duplicate data
group_clean <- group_control[!duplicated(group_control), ]
test_clean <- group_test[!duplicated(group_test), ]
#Clean all NA numbers
group_clean2 <- na.omit(group_clean)
test_clean2 <- na.omit(test_clean)
#t-test for the two clean data sets(Note, this is not the high seller data)

t.test(group_clean2$cost)
t.test(test_clean2$cost)
test0 <- bayesTest(group_clean2$cost, test_clean2$cost, distribution = "normal", priors = c("mu" = 5, "lambda" = 1, 'alpha' = 3, 'beta' = 1))



#High variance can have an impact on the data results. Need to remove observations that are too high. 
#To counter this, conduct analysis on product(s) id with highest volume of sales.

#Make Table then Bar Chart For Control
group_clean_table <-table(group_clean$productId)
barplot(group_clean_table[order(group_clean_table, decreasing = T)])

#Now for Test
test_clean_table <-table(test_clean$productId)
barplot(test_clean_table[order(test_clean_table, decreasing = T)])

#Next, put conditions and only compair high sellin products in effort to reduce data varience (Any item that sells over 1000)

#Top sellers for Control Group (group_clean_table)
#coinpack001
#coinpack002
#poolcash001
#goldenspin
#specialpack
#specialpack1_0
#specialpack1_2
#specialpack1_3
#specialpack12_0
#specialpack2_0

#Top sellers for Test Group (test_clean_table)
#coinpack001
#coinpack002
#goldenspin
#poolcash001
#specialpack1_3
#specialpack1_2
#specialpack1_0
#specialpack
#specialpack12_0
#specialpack2_0




control_HighSeller <- group_clean[!(group_clean$productId != "coinpack001")|!(group_clean$productId != "coinpack002")|!(
  group_clean$productId != "poolcash001")|!(group_clean$productId != 'goldenspin')|!(group_clean$productId != "specialpack")|!(
    group_clean$productId != "specialpack1_0")|!(group_clean$productId != "specialpack1_2")|!(group_clean$productId != "specialpack1_3")|!
    (group_clean$productId != "specialpack12_0")|!(group_clean$productId != "specialpack2_0"), ]

t.test(control_HighSeller$cost) #mean of x is .59

test_HighSeller <- group_clean[!(test_clean$productId != "coinpack001")|!(test_clean$productId != "coinpack002")|!(
  test_clean$productId != "poolcash001")|!(test_clean$productId != 'goldenspin')|!(test_clean$productId != "specialpack")|!(
    test_clean$productId != "specialpack1_0")|!(test_clean$productId != "specialpack1_2")|!(test_clean$productId != "specialpack1_3")|!
    (test_clean$productId != "specialpack12_0")|!(test_clean$productId != "specialpack2_0"), ]

#Get rid of NAs

test_HighSeller_Clean <- na.omit(test_HighSeller)
control_HighSeller_Clean <- na.omit(control_HighSeller)

t.test(test_HighSeller_Clean$cost) #mean of x is .88
t.test(control_HighSeller_Clean$cost)#mean of x is .59


test1 <- bayesTest(control_HighSeller_Clean$cost, test_HighSeller_Clean$cost, distribution = "normal", priors = c("mu" = 5, "lambda" = 1, 'alpha' = 3, 'beta' = 1))
plot(test1)