setwd('F:\\MY PROJECTS\\R\\Data Analysis\\Datathon')
library(tidyverse)

datathon <- read.csv("F:\\MY PROJECTS\\R\\Data Analysis\\Datathon\\datathon.csv")
datathon <- datathon %>% select(-1) %>%
  rename(Row_ID = "Row.ID",Order_ID = "Order.ID",Order_Date = "Order.Date",Ship_date="Ship.Date",Ship_mode="Ship.Mode",
         Customer_ID = "Customer.ID",Customer_Name = "Customer.Name",Product_ID = "Product.ID",Sub_Category = "Sub.Category",
         Product_Name = "Product.Name",Shipping_Cost = "Shipping.Cost",Order_Priority = "Order.Priority")
datathon <- transform(datathon,
                      Order_Date= ymd(datathon$Order_Date),
                      Ship_date = ymd(datathon$Ship_date),
                      Segment = as.factor(datathon$Segment),
                      Ship_mode = as.factor(datathon$Ship_mode),
                      City = as.factor(datathon$City),
                      State = as.factor(datathon$State),
                      Market = as.factor(datathon$Market),
                      Region = as.factor(datathon$Region),
                      Category = as.factor(datathon$Category),
                      Sub_Category = as.factor(datathon$Sub_Category),
                      Order_Priority = as.factor(datathon$Order_Priority))

new_columns <- c(#"Row_ID", 
                 "Order_ID", "Order_Date", "Ship_date", "Ship_mode", 
                 "Customer_ID", #"Customer_Name", "Segment", 
                 "City", "State", "Country", 
                 "Market", "Region", "Product_ID", "Category", "Sub_Category", 
                 #"Product_Name",
                 "Sales", "Quantity", "Discount", "Profit", "Shipping_Cost", 
                "Order_Priority")

datathon_new <- datathon[,new_columns]
dim(datathon_new)

colSums(is.na(datathon_new))

#most frequent customers
customer_frequency <- datathon_new %>%
  group_by(Customer_ID) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  filter(Frequency>=77)

ggplot(data =customer_frequency,aes(x = Frequency, y = reorder(Customer_ID,Frequency),label = Frequency))+
  geom_point()+
  theme_bw()+
  geom_text(nudge_x = .3,
            nudge_y = .3)+
  labs(title = "Most Frequent Customers",
       y = 'Customer ID')+
  theme(plot.title = element_text(face = 'bold',hjust = .5,color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(face = 'bold',color = 'black'),
        axis.text.y =  element_text(face = 'bold',color = 'black'))

#Most popular market Regions
market_Region <-datathon_new %>%
  group_by(Region) %>%
  summarise(Frequency = n())

 ggplot(data = market_Region, aes(x = reorder(Region,-Frequency), y =Frequency, fill = Region, label = Frequency))+
  geom_col(width = .5)+
  theme_classic()+
  geom_text(vjust = -.3)+
  scale_x_discrete(guide = guide_axis(n.dodge =2 ))+
  labs(title = 'Total Customers In Each Region',
       x = 'Region')+
  theme(plot.title = element_text(face = 'bold',hjust = .5,color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(face = 'bold',color = 'black'),
        axis.text.x =  element_text(face = 'bold',color = 'black'),
        legend.position = 'none')
 


#Defining high and low buyers
high_buyers <- datathon_new %>%
   group_by(Customer_ID) %>%
   summarise(Frequency = n()) %>%
   arrange(desc(Frequency)) %>%
   filter(Frequency>=60)

high_buyers_IDs <- high_buyers$Customer_ID

datathon_new <- datathon_new %>%
  mutate(buyer_category = ifelse(Customer_ID %in% high_buyers_IDs,'High Buyer','Low Buyer'))

#Profit between the groups
datathon_new %>%
  group_by(buyer_category) %>%
  summarise(profit = sum(Profit)) %>%
  ggplot(aes(x = buyer_category,y = profit,label = round(profit),fill = buyer_category ))+
  geom_col(width = .4)+
  theme_classic()+
  geom_text(vjust = 2,size=4,)+
  labs(title = 'Profit Margin Between The Two\n Buyer Groups')+
  scale_fill_brewer(palette = 'Set1')+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        axis.title = element_text(face = 'bold',colour = 'black'),
        axis.text.x = element_text(face = 'bold',color = 'black'),
        plot.title = element_text(face = 'bold',colour = 'black',hjust = .5))

#Regions Among the high Buyers
datathon_new %>%
  group_by(Region,buyer_category) %>%
  summarise(Frequencies = n()) %>%
  ggplot(aes(x=reorder(Region,-Frequencies),y=Frequencies,fill=buyer_category,label= Frequencies)) +
  geom_col() +
  geom_text(position = position_stack(vjust=.6))+
  labs(title = 'Buyer Category By Region',x='Region')+
  scale_fill_brewer(palette = 'Set1') +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #legend.position = 'none',
        axis.title = element_text(face = 'bold',colour = 'black'),
        axis.text.x = element_text(face = 'bold',color = 'black'),
        plot.title = element_text(face = 'bold',colour = 'black',hjust = .5))

#category of goods bought by the different buyers
prc <- datathon_new %>%
  group_by(Category,buyer_category) %>%
  summarise(Totals = n())

prc$percentage <- ''

frntr <- sum(prc$Totals[prc$Category=='Furniture'])
office <- sum(prc$Totals[prc$Category=='Office Supplies'])
Technology <- sum(prc$Totals[prc$Category=='Technology'])

for (row in 1:nrow(prc)) {
   prc$percentage[row] <- ifelse(prc$Category[row] == 'Furniture',round(prc$Totals[row]/frntr*100),
                                 ifelse(prc$Category[row] == 'Office Supplies',round(prc$Totals[row]/office *100),round(prc$Totals[row]/Technology *100)))
}

percentage <- prc %>%
  ggplot(aes(x = Category, y = percentage,label = paste(percentage,'%'),fill = buyer_category))+
  geom_col(width = .3)+
  geom_text(position = position_stack(vjust = .5))+
  scale_fill_brewer(palette = 'Set1')+
  theme_classic()+
  labs(title =  'Percentage Of Products Bought By\n Each Buyer Group')+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold',colour = 'black'),
        axis.text.x = element_text(face = 'bold',color = 'black'),
        plot.title = element_text(face = 'bold',colour = 'black',hjust = .5))
