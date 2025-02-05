library(lubridate)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(caret)
library(forecast)


df <- read.csv("ecommerceData.csv")
colnames(df)<-c('UID','PID','Category','Price','Discount','FP','PM','Date')

summary(df)

str(df)

df$Date <- dmy(df$Date)

any(is.na(df))

# unique users
length(unique(df[,1]))

# unique products
length(unique(df[,2]))

# unique categories 
unique(df[,3])

# unique payment methods 
unique(df[,'PM'])

# unique Discount values
unique(df[,'Discount'])


# Category vs Discount
df$Discount<- as.factor(df$Discount)
df %>% 
  count(Category, Discount) %>% 
  ggplot(aes(x = Category, y = n, fill = Discount)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Discount Distribution by Category", x = "Category", y = "Discount") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

# payment method vs category 
df %>% 
  count(Category, PM) %>% 
  ggplot(aes(x = Category, y = n, fill = PM)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Payment Method Distribution by Category", x = "Category", y = "Payment Method")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Price by category
ggplot(df, aes(x = Category, y = `Price`, fill = Category)) +
  geom_boxplot() +
  labs(title = "Price by Category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Discounted price by category
ggplot(df, aes(x = Category, y = `FP`, fill = Category)) +
  geom_boxplot() +
  labs(title = "Sales' Price by Category", y = "Sales price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# daily price sum
df_summary <- df %>%
  group_by(Date) %>%
  summarize(Total_Price = sum(FP, na.rm = TRUE))

ggplot(df_summary, aes(x = Date, y = Total_Price)) +
  geom_line() +
  labs(title = "Daily Revenue", x = "Date", y = "Total Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# to get a clear view we will select 60 days to examine our data
ggplot(df_summary[1:60, ], aes(x = Date, y = Total_Price)) +
  geom_line() +
  labs(title = "Daily Revenue", x = "Date", y = "Total Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# we notice a weekly trend

df$YearMonth <- format(df$Date, "%Y-%m")

df_monthly_summary <- df %>%
  group_by(YearMonth) %>%
  summarize(Monthly_Price = sum(FP, na.rm = TRUE))

ggplot(df_monthly_summary, aes(x = YearMonth, y = Monthly_Price)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Monthly Total of Purchases", x = "Month", y = "Total Revenu") +
  theme_minimal()

df <- df %>% select(-UID)
df <- df %>% select(-PID)