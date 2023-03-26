All the codes goes as follows:

# Import required libraries

library(tidyverse)
library(ggplot2)
library(plotly)
library(readr)

# Import the dataset and see if it is imported.

Train <- read_csv("Train.csv")

View(Train)

# Top/bottom values of the dataset

head(Train)
tail(Train)

# Summary of the dataset

summary(Train)

# Drop ‘Item_Identifier” and “Outlet_Identifier” columns 

Train <- Train[ , -c( 1, 7)]

View(Train)
summary(Train)

# Get the number of missing values in each column

colSums(is.na(Train))

# Plotting a box plot

ggplot(Train, aes(y = Item_Weight)) + 
  geom_boxplot()

# Replace missing values with mode to the Item_Weight column to remove the Null values

Train$Item_Weight[is.na(Train$Item_Weight)] <- mean(Train$Item_Weight, na.rm = TRUE)

# Check if the Null values are gone

sum(is.na(Train$Item_Weight))

# Replace missing values with mode to the Outlet_Size to remove all the Null values

Train <- Train %>% 
  mutate(Outlet_Size = ifelse(is.na(Outlet_Size), names(which.max(table(Outlet_Size))), Outlet_Size))

# Check if the Null values are gone

sum(is.na(Train$Outlet_Size))
colSums(is.na(Train))

# Group data by Outlet_Type and sum Item_Outlet_Sales

outlet_type_sales <- Train %>% 
  group_by(Outlet_Type) %>% 
  summarise(Item_Outlet_Sales = sum(Item_Outlet_Sales)) %>% 
  arrange(desc(Item_Outlet_Sales)) %>% 
  ungroup()

outlet_type_sales


# Create bar chart using ggplot2

ggplot(outlet_type_sales, aes(x = Outlet_Type, y = Item_Outlet_Sales)) +
  geom_bar(stat = "identity") +
  xlab("Outlet Type") +
  ylab("Total Sales") +
  ggtitle("Total Sales by Outlet Type")

# Sorting the values of bar chart in descending order

outlet_type_sales <- outlet_type_sales[order(-outlet_type_sales$Item_Outlet_Sales),]

ggplot(outlet_type_sales, aes(reorder(Outlet_Type, -Item_Outlet_Sales), Item_Outlet_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Outlet Type", x = "Outlet Type", y = "Total Sales")

# See the sales per outlet location

options(digits=3)
location_type_sales <- Train %>%
  group_by(Outlet_Location_Type) %>%
  summarize(Item_Outlet_Sales = sum(Item_Outlet_Sales)) %>%
  arrange(desc(Item_Outlet_Sales))

location_type_sales

# Create a bar chart using ggplot2

ggplot(location_type_sales, aes(x=Outlet_Location_Type, y=Item_Outlet_Sales)) + 
  geom_bar(stat="identity") +
  labs(x="Outlet Location Type", y="Total Sales") +
  ggtitle("Total Sales by Outlet Location Type") +
  theme_bw()

# Outlet_size and it's sales

options(digits = 3)

outlet_size_sales <- aggregate(Train$Item_Outlet_Sales, by = list(Train$Outlet_Size), FUN = sum)
names(outlet_size_sales) <- c("Outlet_Size", "Item_Outlet_Sales")
outlet_size_sales <- outlet_size_sales %>%
  arrange(Item_Outlet_Sales)

outlet_size_sales

# Create a bar chart using ggplot2

ggplot(outlet_size_sales, aes(x = Outlet_Size, y = Item_Outlet_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Outlet Size", x = "Outlet Size", y = "Total Sales")

# 'Item_type' and 'Item_Outlet_Sales'

train_max_sales <- Train %>%
  group_by(Item_Type) %>%
  summarize(Max_Item_Outlet_Sales = max(Item_Outlet_Sales)) %>%
  arrange(desc(Max_Item_Outlet_Sales))

train_max_sales

# Create a bar chart using ggplot2

ggplot(train_max_sales, aes(x=reorder(Item_Type, Max_Item_Outlet_Sales), y=Max_Item_Outlet_Sales)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Max Item Outlet Sales by Item Type", x="Item Type", y="Max Item Outlet Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Item Visibility v/s Sales

unique(Train$Item_Visibility)

Train <- Train[Train$Item_Visibility != 0, ]

Train$Item_Visibility

item_visibility_sales <- aggregate(Train$Item_Outlet_Sales, by = list(Train$Item_Visibility), FUN = sum)
names(item_visibility_sales) <- c("Item_Visibility", "Item_Outlet_Sales")
item_visibility_sales <- item_visibility_sales[order(-item_visibility_sales$Item_Outlet_Sales),]

# Visualize it using ggplot2

ggplot(Train, aes(x = Item_Visibility, y = Item_Outlet_Sales, color = Item_Visibility)) +
  geom_point() +
  labs(x = "Item Visibility", y = "Item Outlet Sales", color = "Item Visibility")+ 
  scale_color_gradient(low = "blue", high = "red")

# Item Weight v/s Sales

min(Train$Item_Weight)

item_weight_sales <- Train %>%
  group_by(Item_Weight) %>%
  summarize(Total_Item_Outlet_Sales = sum(Item_Outlet_Sales)) %>%
  arrange(desc(Total_Item_Outlet_Sales))

item_weight_sales

# Visualize it using ggplot2

ggplot(Train, aes(x = Item_Weight, y = Item_Outlet_Sales, color = Item_Weight)) +
  geom_point() +
  labs(title = "Item Weight vs Item Outlet Sales", x = "Item Weight", y = "Item Outlet Sales") +
  scale_color_gradient(low = "blue", high = "red")

# Item MRP v/s Sales
# Sales for the products of different MRP
train_max_sales <- Train %>%
  group_by(Item_MRP) %>%
  summarize(Max_Item_Outlet_Sales = max(Item_Outlet_Sales)) %>%
  arrange(desc(Max_Item_Outlet_Sales))

train_max_sales
summary(train_max_sales)
subset(Train, Item_MRP == 234.9958)$Item_Type
subset(Train, Item_MRP == 31.290)$Item_Type

#Visualize it using ggplot2

ggplot(train_max_sales, aes(x = Item_MRP, y = Max_Item_Outlet_Sales, color = Max_Item_Outlet_Sales)) +
  geom_point() +
  xlab("Item MRP") +
  ylab("Maximum Item Outlet Sales") +
  ggtitle("Maximum Outlet Sales vs Item MRP") +
  scale_color_gradient(low = "blue", high = "red")

# Products that sell better in Tier 1 cities as compared to Tier 2 and Tier 3 cities

Train_tier1 <- subset(Train, Outlet_Location_Type == "Tier 1")

Train_tier2 <- subset(Train, Outlet_Location_Type == "Tier 2")

Train_tier3 <- subset(Train, Outlet_Location_Type == "Tier 3")

Train_cities <- rbind(Train_tier2, Train_tier3)


Train_tier1_list <- Train_tier1 %>%
  group_by(Item_Type) %>%
  summarize(Sales_tier_1 = max(Item_Outlet_Sales)) %>%
  arrange(desc(Sales_tier_1)) %>%
  rename(Items_Tier_1 = Item_Type, Sales_tier_1 = Sales_tier_1)

Train_tier1_list

Train_cities_list <- Train %>%
  filter(Outlet_Location_Type %in% c("Tier 2", "Tier 3")) %>%
  group_by(Item_Type) %>%
  summarize(Sales_tier2_3 = max(Item_Outlet_Sales)) %>%
  arrange(desc(Sales_tier2_3)) %>%
  rename(Items_Tier2_3 = Item_Type)

Train_cities_list

Train_sales <- cbind(Train_tier1_list, Train_cities_list)
Train_sales


dim(Train_sales)

# Sort by Sales_tier_1 in ascending order

Train_sales <- Train_sales %>%
  arrange(Sales_tier_1) 

# Plot a line chart using plotly

fig <- plot_ly(Train_sales, x = ~reorder(Items_Tier_1, Sales_tier_1), y = ~Sales_tier_1, 
               name = "Tier 1 Sales", type = 'scatter', mode = 'lines+markers',
               line = list(color = 'blue')) %>% 
  add_trace(y = ~Sales_tier2_3, name = "Tier 2&3 Sales", line = list(color = 'red')) %>% 
  layout(title = "Maximum Sales by Item Type and Location Type", 
         xaxis = list(title = "Item Type"),
         yaxis = list(title = "Maximum Sales"))

fig
