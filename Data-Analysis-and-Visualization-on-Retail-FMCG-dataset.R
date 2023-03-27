
# Pre-Processing of data

# Import the required libraries
library(dplyr)
library(ggplot2)
library(readr)

# Import the dataset
Train <- read_csv("Train.csv")

# Have a look at the data
head(Train)
tail(Train)

# Check the data summary for future use
summary(Train)

# See the data
Train


# The columns ‘Item_Identifier’ and ‘Outlet_Identifier’ will be dropped from the dataset as they are not useful.
Train <- Train[ , -c( 1, 7)]

Train

# Let’s Conduct an exploratory data analysis (EDA) on our dataset before we can address any questions.

# Get the number of missing values in each column
colSums(is.na(Train))

# Plotting a box plot
ggplot(Train, aes(y = Item_Weight)) + 
  geom_boxplot()

# Replace missing values with mean to the 'Item_Weight' column.
Train$Item_Weight[is.na(Train$Item_Weight)] <- mean(Train$Item_Weight, na.rm = TRUE)

# Check if the NULL values are gone.

sum(is.na(Train$Item_Weight))

#Replace missing values with mode to the 'Outlet_Size' column.

Train <- Train %>% 
  mutate(Outlet_Size = ifelse(is.na(Outlet_Size), names(which.max(table(Outlet_Size))), Outlet_Size))


# Check if the Null values are gone

sum(is.na(Train$Outlet_Size))

colSums(is.na(Train))

# We are good to go now.

# Analysis of sales associated with each factor.

# Outlet Type v/s Sales

# Group data by 'Outlet_Type' and sum of 'Item_Outlet_Sales'

outlet_type_sales <- Train %>% 
  group_by(Outlet_Type) %>% 
  summarise(Item_Outlet_Sales = sum(Item_Outlet_Sales)) %>% 
  arrange(desc(Item_Outlet_Sales)) %>% 
  ungroup()

outlet_type_sales

# Create bar chart using ggplot2

outlet_type_sales <- outlet_type_sales[order(-outlet_type_sales$Item_Outlet_Sales),]

ggplot(outlet_type_sales, aes(reorder(Outlet_Type, -Item_Outlet_Sales), Item_Outlet_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Outlet Type", x = "Outlet Type", y = "Total Sales")

## Sales per outlet location

location_type_sales <- Train %>%
  group_by(Outlet_Location_Type) %>%
  summarize(Item_Outlet_Sales = sum(Item_Outlet_Sales)) %>%
  arrange(desc(Item_Outlet_Sales))

location_type_sales

# Plot a bar chart

ggplot(location_type_sales, aes(x=Outlet_Location_Type, y=Item_Outlet_Sales)) + 
  geom_bar(stat="identity") +
  labs(x="Outlet Location Type", y="Total Sales") +
  ggtitle("Total Sales by Outlet Location Type") +
  theme_bw()

# Outlet size and it’s sales

outlet_size_sales <- aggregate(Train$Item_Outlet_Sales, by = list(Train$Outlet_Size), FUN = sum)
names(outlet_size_sales) <- c("Outlet_Size", "Item_Outlet_Sales")
outlet_size_sales <- outlet_size_sales %>%
  arrange(Item_Outlet_Sales)

outlet_size_sales

# Create a bar chart using ggplot2

ggplot(outlet_size_sales, aes(x = Outlet_Size, y = Item_Outlet_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Outlet Size", x = "Outlet Size", y = "Total Sales")

# Sorting the bars

ggplot(outlet_size_sales, aes(x = reorder(Outlet_Size, Item_Outlet_Sales, sum), y = Item_Outlet_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Outlet Size", x = "Outlet Size", y = "Total Sales")

# Item type and Item Outlet Sales

train_max_sales <- Train %>%
  group_by(Item_Type) %>%
  summarize(Max_Item_Outlet_Sales = max(Item_Outlet_Sales)) %>%
  arrange(desc(Max_Item_Outlet_Sales))

train_max_sales


#Create a bar chart using ggplot2

ggplot(train_max_sales, aes(x=reorder(Item_Type, Max_Item_Outlet_Sales), y=Max_Item_Outlet_Sales)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Max Item Outlet Sales by Item Type", x="Item Type", y="Max Item Outlet Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Item Visibility v/s Sales
# Uniqueness in the item visibility
unique(Train$Item_Visibility)

#Finding and removing the values having item visibility equal to 0
Train <- Train[Train$Item_Visibility != 0, ]

Train$Item_Visibility

item_visibility_sales <- aggregate(Train$Item_Outlet_Sales, by = list(Train$Item_Visibility), FUN = sum)
names(item_visibility_sales) <- c("Item_Visibility", "Item_Outlet_Sales")
item_visibility_sales <- item_visibility_sales[order(-item_visibility_sales$Item_Outlet_Sales),]

item_visibility_sales

# Visualize it using ggplot2 with a scatter plot

ggplot(Train, aes(x = Item_Visibility, y = Item_Outlet_Sales, color = Item_Visibility)) +
  geom_point() +
  labs(x = "Item Visibility", y = "Item Outlet Sales", color = "Item Visibility")+ 
  scale_color_gradient(low = "blue", high = "red")

## Item Weight v/s Sales

min(Train$Item_Weight)

item_weight_sales <- Train %>%
  group_by(Item_Weight) %>%
  summarize(Total_Item_Outlet_Sales = sum(Item_Outlet_Sales)) %>%
  arrange(desc(Total_Item_Outlet_Sales))

item_weight_sales

# Scatter plot using ggplot2

ggplot(Train, aes(x = Item_Weight, y = Item_Outlet_Sales, color = Item_Weight)) +
  geom_point() +
  labs(title = "Item Weight vs Item Outlet Sales", x = "Item Weight", y = "Item Outlet Sales") +
  scale_color_gradient(low = "blue", high = "red")


# Item MRP v/s Sales

train_max_sales <- Train %>%
  group_by(Item_MRP) %>%
  summarize(Max_Item_Outlet_Sales = max(Item_Outlet_Sales)) %>%
  arrange(desc(Max_Item_Outlet_Sales))

train_max_sales


subset(Train, Item_MRP == 234.9958)$Item_Type

subset(Train, Item_MRP == 35.0558)$Item_Type

# Scatter plot using ggplot2

ggplot(train_max_sales, aes(x = Item_MRP, y = Max_Item_Outlet_Sales, color = Max_Item_Outlet_Sales)) +
  geom_point() +
  xlab("Item MRP") +
  ylab("Maximum Item Outlet Sales") +
  ggtitle("Maximum Outlet Sales vs Item MRP") +
  scale_color_gradient(low = "blue", high = "red")


# Products that sell better in Tier 1 cities as compared to Tier 2 and Tier 3 cities

#splitting the data in categories

Train_tier1 <- subset(Train, Outlet_Location_Type == "Tier 1")

Train_tier2 <- subset(Train, Outlet_Location_Type == "Tier 2")

Train_tier3 <- subset(Train, Outlet_Location_Type == "Tier 3")

Train_cities <- rbind(Train_tier2, Train_tier3)

#tier 1 sales

Train_tier1_list <- Train_tier1 %>%
  group_by(Item_Type) %>%
  summarize(Sales_tier_1 = max(Item_Outlet_Sales)) %>%
  arrange(desc(Sales_tier_1)) %>%
  rename(Items_Tier_1 = Item_Type, Sales_tier_1 = Sales_tier_1)

Train_tier1_list

#Tier 2 and 3 sales

Train_cities_list <- Train %>%
  filter(Outlet_Location_Type %in% c("Tier 2", "Tier 3")) %>%
  group_by(Item_Type) %>%
  summarize(Sales_tier2_3 = max(Item_Outlet_Sales)) %>%
  arrange(desc(Sales_tier2_3)) %>%
  rename(Items_Tier2_3 = Item_Type)

Train_cities_list

# combining the data frames
Train_sales <- cbind(Train_tier1_list, Train_cities_list)
Train_sales

Train_sales <- Train_sales %>%
  arrange(Sales_tier_1) 

# Plotting the graph to show the differennce of sale between the cities

ggplot(Train_sales, aes(x = reorder(Items_Tier_1, Sales_tier_1), y = Sales_tier_1)) +
  geom_line(color = "blue", aes(group = 1)) +
  geom_point(color = "blue") +
  geom_line(aes(x = Items_Tier_1, y = Sales_tier2_3, group = 1), color = "red") +
  geom_point(aes(x = Items_Tier_1, y = Sales_tier2_3), color = "red") +
  labs(title = "Maximum Sales by Item Type and Location Type",
       x = "Item Type",
       y = "Maximum Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
