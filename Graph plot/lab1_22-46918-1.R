install.packages("infotheo")
install.packages("emmeans")
library(emmeans)
library(infotheo)



ds = data <- read.csv("C:/Users/User/Downloads/House_Rent_Dataset.csv",header = TRUE, sep= ',' )


data = na.omit(data)
colSums(is.na(data))



cor.test(ds$Rent, ds$Bathroom)

cor.test(ds$Rent, data$Size, method = "spearman", exact = FALSE)




anova_result <- aov(Rent ~ City, data=ds)
summary(anova_result)
emmeans_results <- emmeans(anova_result, ~ City)
pairwise_results <- pairs(emmeans_results)
print(pairwise_results)
plot(emmeans_results)

anova_result <- aov(Size ~ Posted.On, data=ds)
summary(anova_result)
emmeans_results <- emmeans(anova_result, ~ Posted.On)
pairwise_results <- pairs(emmeans_results)
print(pairwise_results)
plot(emmeans_results)







contingency_table <- table(data$Floor, data$Posted.On)
chi_squared_test <- chisq.test(contingency_table)
cat("Chi-Squared Test Results:\n")
print(chi_squared_test)

contingency_table <- table(data$BHK, data$Tenant.Preferred)
chi_squared_test <- chisq.test(contingency_table)
cat("Chi-Squared Test Results:\n")
print(chi_squared_test)





ds$Area.Type <- as.factor(ds$Area.Type)
ds$Tenant.Preferred <- as.factor(ds$Tenant.Preferred)
data_discretized <- discretize(data)
mi_value <- mutinformation(ds$Area.Type, ds$Tenant.Preferred)
cat("Mutual Information between Area.Type and Tenant.Preferred:", mi_value, "\n")



ds$Area.Locality <- as.factor(ds$Area.Locality)
ds$Tenant.Preferred <- as.factor(ds$Tenant.Preferred)
# Discretize the data (required for mutual information calculation)
data_discretized <- discretize(data)
mi_value <- mutinformation(ds$Area.Locality, ds$Tenant.Preferred)
cat("Mutual Information between Area.Type and Tenant.Preferred:", mi_value, "\n")

