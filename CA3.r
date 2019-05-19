df <- read.csv("20193200544011566783NQQ2503814615961.csv")
names(df)[names(df) == "X"] <- "Type"
View(df)

str(df)

# To simolify the Type variable, transform funnction is used
transformed_gdp_data <- transform(df, Type = factor(Type, labels = c("non-seasonal", "seasonal")))
str(transformed_gdp_data)

# Subsets are created to select the appropriate variables need for data analysis
sub <- subset(transformed_gdp_data, select = c(1,4,7))
View(sub)
str(sub)
sub$Type <- as.character(sub$Type)


# creating new dataset containing the GDPs of the two sectors Industry and Transportation 
# from the original dataset
new_dataset <- data.frame(Sector = rep(c("Industry","Transportation"), each = 152),
                      GDP = c(sub$Industry, sub$Distribution.Transport.Software.and.Communication),
                      Type = c(sub$Type))

View(new_dataset)
str(new_dataset)

# ggplot2 library is called to create a ggplot comparing Industry and Transportation business sectors
library(ggplot2)
ggplot(data = new_dataset, mapping = aes(x = Sector, y = GDP)) +
  geom_boxplot()

# The subset of the industry business sector is created
industry_subset <- new_dataset[ which(new_dataset$Sector == "Industry"),]
View(industry_subset)
str(industry_subset)

# The subset of the transportation business sector is created.
transportation_subset <- new_dataset[which(new_dataset$Sector == "Transportation"),]
View(transportation_subset)
str(transportation_subset)

# Shapiro test is performed to check the p value of GDP 
# of Industry business sector
normality_test <- shapiro.test(industry_subset$GDP)
normality_test$p.value


# Shapiro test is performed to check the p value of GDP 
# of Transportation business sector
normality_test1 <- shapiro.test(transportation_subset$GDP)
normality_test1$p.value

# Wilcox test is selected as both the variables are numerical
# and p values of both the variables is below 0.05
res <- wilcox.test(industry_subset$GDP, transportation_subset$GDP)
res
# Wilcox test shows that p value is below 0.05 
# and the alternative hypothesis will be selected

# Means of the GDPs of both Industry and Transportation sectors
# will be calculated to determine standard deviation which is 
# then used to compute delta value
mean1 <- mean(industry_subset$GDP)
mean2 <- mean(transportation_subset$GDP)
sd <- sd(new_dataset$GDP)
delta1 <- (mean1 - mean2)/sd
delta1

# The pwr package is installed and its library is called
# because it is needed to perform power test
install.packages("pwr")
library(pwr)

# power.t.test is used to examine the sample number of the data
power.t.test(delta = delta1, 
             n = NULL, 
             sig.level = 0.05, 
             power = 0.90, 
             type = "two.sample", 
             alternative = "two.sided")

# pwr.t.test id used to examine the effect size and also
# visualize it using plot function 
power_information <- pwr.t.test(d = delta1, 
                                sig.level = 0.05,
                                power = 0.99,
                                type = "two.sample",
                                alternative = "two.sided")

power_information
plot(power_information)

# The spearmen correlation is found to examine the correlation
# between the GDPs of both Industry and Transportation sectors
cohen.ES(test = c("p"), size = c("small"))
res2 <- cor.test(industry_subset$GDP, transportation_subset$GDP,
                 method = "spearman")
res2
# the rho spearman's rank correlation is 0.506827