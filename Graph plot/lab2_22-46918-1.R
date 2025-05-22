library(tidyverse)

if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
library(e1071)




data <- read.csv("C:/Users/User/Downloads/House_Rent_Dataset.csv",header = TRUE, sep= ',' )
new_data <- data[data$Rent <= 300000,]


mean_rent <- mean(new_data$Rent, na.rm = TRUE)
median_rent <- median(new_data$Rent, na.rm = TRUE)
mode_rent <- as.numeric(names(sort(table(new_data$Rent), decreasing = TRUE))[1]) # Mode


hist(new_data$Rent, prob = TRUE, main = paste("Histogram with Density (Skewness =", round(skewness_value, 2), ")"),
     xlab = "Rent", col = "skyblue", border = "black")
lines(density(new_data$Rent, na.rm = TRUE), col = "red", lwd = 2)
 

abline(v = mean_rent, col = "green", lwd = 2, lty = 2)
abline(v = median_rent, col = "red", lwd = 2, lty = 2)
abline(v = mode_rent, col = "blue", lwd = 2, lty = 2)  
legend("topright", legend = c("Mean", "Median", "Mode"), 
       col = c("green","red","blue"), lty = 2, lwd = 2)




skewness_value <- skewness(new_data$Rent, na.rm = TRUE)
print(skewness_value)


ggplot(new_data, aes(x = BHK)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Room_Number", x = "Category", y = "Frequency") +
  theme_minimal()


ggplot(new_data, aes(x = City)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of CITY", x = "Category", y = "Frequency") +
  theme_minimal()


new_data <- data[data$Size <= 5000,]
new_data %>%
  ggplot(aes(Size))+
  geom_boxplot()

new_data <- data[data$Rent <= 100000,]
new_data %>%
  ggplot(aes(Rent))+
  geom_boxplot()



plot(x= new_data$Size, new_data$Rent)


ggplot(data =new_data, mapping= aes(x= Size, y= Rent, colour = City))+
  geom_point() + geom_smooth(se= F)+
  theme_dark()

ggplot(data =new_data, mapping= aes(x= Size, y= Rent, colour = Area.Type))+
  geom_point() + geom_smooth(method =lm, se= F)+
  theme_dark()


ggplot(data =new_data, mapping= aes(x= Size, y= Rent, colour = Tenant.Preferred))+
  geom_point() + geom_smooth(method =lm, se= F)+
  theme_dark()

ggplot(data =new_data, mapping= aes(x= City, y= BHK))+
  geom_point() + geom_smooth(se= F)+
  theme_dark()



new_data %>%
  ggplot(data =new_data, mapping= aes(x= BHK, y= Bathroom, colour = City))+
  geom_point(sixe=5, alpha= .3)+
  geom_line(size =1)+
  theme_minimal()
  


new_data %>%
  ggplot(aes(City, Rent))+
  geom_boxplot()+
  geom_point(aes(colour= Area.Type))


