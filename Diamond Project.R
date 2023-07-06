#loading the data
library(tidyverse)
diamonds<-read.csv("diamonds4.csv", header=TRUE)


#Scatter plot of Carat vs. Price
diamonds %>%
  ggplot(aes(x=carat, y=price))+
  geom_point(alpha=0.2)+
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x="Diamond Carat", y="Price ($)", caption="Figure 2.2.1 Scatter Plot of Diamond Carat Against Price")


#Box Plot of Cut vs Price
#Factoring cut
diamonds$cut <-factor(diamonds$cut, levels=c("Poor/Fair","Good","Very Good","Ideal","Astor Ideal"))

#proportion of each category
#table(diamonds$cut)

##Explore Cut
#Cut vs log(Price)
ggplot(diamonds, aes(x=cut, y=log(price)))+
  geom_boxplot()+
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x="Diamond Cut", y="Log(Price)", caption="Figure 2.3.1 Box Plot of Cut Against log(Price)")


diamond_price_color <- diamonds %>%
  group_by(color) %>%
  summarise(mean_price = mean(price), median_price = median(price))

ggplot(data = diamond_price_color) +
  aes(x = color, y = mean_price) +
  geom_bar(stat = "identity", fill = "#16a085") +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Color", y = "Price (USD)", caption = "Figure 2.4.1. Bar graph of mean Price by Color") +
  guides(fill = "none")

## EXPLORE CLARITY ----------
# find the number of diamonds for each clarity grade
#table(diamonds$clarity)

# create a scatterplot of price against clarity
diamonds %>% 
  ggplot(aes(x = clarity, y = price))+
  geom_point(alpha = 0.4) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Clarity", 
       y = "Price (USD)", 
       caption = "Figure 2.5.1. Scatterplot of Price against Clarity") +
  scale_x_discrete(limits = c("SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF", "FL"))+
  scale_y_continuous(labels = scales::dollar)


diamonds$cut<- factor(diamonds$cut, levels = c("Astor Ideal", "Ideal", "Very Good", "Good"))

diamonds %>%
  ggplot(aes(x=log(carat), y=log(price), col=cut))+
  geom_point(alpha = 0.4)+
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x="log(Carat)", y="Log(Price)", caption="Figure 2.6.1.1.1. Scatterplot of log(Price) Against log(Carat)")


diamonds$Color_Category <- NA

diamonds$Color_Category[diamonds$color %in% c('D', 'E', 'F')] <- 1
diamonds$Color_Category[diamonds$color %in% c('G', 'H')] <- 2
diamonds$Color_Category[diamonds$color %in% c('I', 'J')] <- 3

#convert to a factor
diamonds$Color_Category <- factor(diamonds$Color_Category, levels = 1:3, labels = c("Good Color Grade (D, E, F)", "Okay Color Grade (G, H)", "Poor Color Grade (I, J)"))

diamonds %>%
  ggplot(aes(x=log(carat), y=log(price), col=Color_Category))+
  geom_point(alpha = .4)+
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x="Log(Carat)", y="Log(Price)", caption="Figure 2.6.1.2.1. Scatterplot of log(Price) Against log(Carat)")

ggplot(diamonds, aes(x=color, fill=clarity))+
  geom_bar()+
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x="Color", y="Clarity", caption="Figure 2.6.1.3.1. Boxplot of Color with Clarity frequency")


ggplot(data = diamonds) +
  aes(x = carat, y = price) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Carat", y = "Price (USD)", caption="Figure 3.1.1. Scatter Plot of Diamond Price (USD) Against Carat")



# create linear model
diamond_result <- lm(price ~ carat, data = diamonds)
#summary(diamond_result)

# plot residuals
par(mfrow = c(2,2))
plot(diamond_result, which=c(1,2,3,5), caption=list("Figure 3.1.2.1. Residuals vs Fitted", "Figure 3.1.2.3. Normal Q-Q", "Figure 3.1.2.2. Scale-Location", NULL, "Figure 3.1.2.4. Residuals vs Leverage"))


# assumptions 1 and 2 are not met, so we will first transform price and then carat if neccessary

# create box cox plot
MASS::boxcox(diamond_result, 
             lambda = (seq(0, 0.5, 0.05)),
             xlab = "λ
Figure 3.1.3. Box Cox Plot of Residuals based on Figure 3.1.2.")

# we will proceed with lamba = 0.3

ystar_price <- (diamonds$price^0.3)
diamonds <- data.frame(diamonds, ystar_price)

ggplot(data = diamonds) +
  aes(x = carat, y = ystar_price) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Carat", y = "Transformed Price (USD)", caption="Figure 3.1.4. Scatter Plot of Transformed Price against Carat")

# assumption 2 seems to still not be met so we will re-transform price with a log transformation for variable interpretation purposes


# transform price with log
ystar_price_log <- log(diamonds$price)
diamonds <- data.frame(diamonds, ystar_price_log)

ggplot(data = diamonds) +
  aes(x = carat, y = ystar_price_log) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Carat", y = "log(Price (USD))", caption = "Figure 3.1.5. Scatter Plot of Transformed Price Against Carat")

# assumption 2 seems to be met so we will proceed with transforming carat with a log transformation for interpretation purposes

# transform carat
xstar_carat_log <- log(diamonds$carat)
diamonds <- data.frame(diamonds, xstar_carat_log)

# create linear model for ystar_price_log ~ xstar_carat_log
diamond_final_result <- lm(ystar_price_log ~ xstar_carat_log, data = diamonds)
#summary(diamond_final_result)

# Our R^2 value of 0.9547 indicates that 95.5% of the variability in log(price) can be accounted for by the variability in log(carat)

# create linear model
diamond_result1 <- lm(ystar_price ~ carat, data = diamonds)
#summary(diamond_result1)

# plot residuals
par(mfrow = c(2,2))
plot(diamond_result1, which=c(1,2,3,5), caption=list("Figure 3.1.6.1. Residuals vs Fitted", "Figure 3.1.6.3. Normal Q-Q", "Figure 3.1.6.2. Scale-Location", NULL, "Figure 3.1.6.4. Residuals vs Leverage"))

ggplot(data = diamonds) +
  aes(x = xstar_carat_log, y = ystar_price_log) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "log(Carat)", y = "log(Price (USD))", caption = "Figure 3.1.7. Scatter Plot of Transformed Price Against Transformed Carat")

diamond_final_result <- lm(ystar_price_log ~ xstar_carat_log, data = diamonds)

par(mfrow = c(2,2))
plot(diamond_final_result, which=c(1,2,3,5), caption=list("Figure 3.1.8.1. Residuals vs Fitted", "Figure 3.1.8.3. Normal Q-Q", "Figure 3.1.8.2. Scale-Location", NULL, "Figure 3.1.8.4. Residuals vs Leverage"))
