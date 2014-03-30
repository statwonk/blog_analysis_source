# Author: Christopher Peters
# Twitter: @statwonk
set.seed(1234)
# Let's generate some reasonable ARPS data.

# Number of respondents
consumers <- 45
businesses <- 30

# Assumed average price response for each group
# Consumers: $15 per month
# Businesses: $300 per month
consumer_average <- 15
business_average <- 300

# E(x) of EXP(1/rate)
consumer_responses <- rexp(consumers, rate = 1/consumer_average)
business_responses <- rexp(businesses, rate = 1/business_average) 

df <- data.frame(group = c(rep("Consumers", consumers), rep("Businesses", businesses)),
                 value = c(consumer_responses, business_responses))

names(df) <- c("group", "willingness_to_pay")

# install.packages("ggplot2") # uncomment to install
library(ggplot2)

# install.packages("ggthemes")
library(ggthemes)

# install.packages("scales")
library(scales)

ggplot(df, aes(x = mean(willingness_to_pay), y = 1)) +
  geom_point(size = 0) +
  geom_vline(aes(xintercept = mean(willingness_to_pay)), colour = "red") +
  scale_x_continuous(labels = dollar_format(), breaks = seq(0, 399, 50)) +
  theme_economist() +
  ggtitle("Average Willingness to Pay\nMarch 2014\nXYZ Company") +
  ylab("") +
  xlab("Average Willingness to Pay") +
  theme(axis.title.x = element_text(size = 16, vjust = 0.3, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20)) +
  coord_cartesian(xlim = c(0, 400))

ggplot(df, aes(x = willingness_to_pay)) +
  geom_histogram(alpha = 0.4, colour = "black") +
  geom_vline(data = df, aes(xintercept = mean(willingness_to_pay)), colour = "red") +
  scale_x_continuous(labels = dollar_format(), breaks = seq(0, 1200, 100)) +
  theme_economist() +
  ggtitle("Average Willingness to Pay\nMarch 2014\nXYZ Company") +
  ylab("Respondents in each bin") +
  xlab("Willingness to pay") +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(vjust = 0.3),
        axis.title.x = element_text(vjust = 0.3),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20, angle = 90, vjust = 1, hjust = 1),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16)) +
  facet_wrap(~ group, ncol = 1)

# Let's bootstrap that average

# 1. create a container to store our results
bootstrap_iterations <- 10000 # more is always better
bootstrapped_willingness_to_pay <- rep(NA, bootstrap_iterations)
number_to_sample_in_each_iteration <- 30
all_responses <- df$willingness_to_pay

for(i in 1:bootstrap_iterations) {
  print(paste(i, " of ", bootstrap_iterations, sep = ""))
  responses_of_sample <- all_responses[sample(1:length(all_responses),
                                       number_to_sample_in_each_iteration,
                                       replace = TRUE)]
  bootstrapped_willingness_to_pay[i] <- mean(responses_of_sample)
}

qplot(bootstrapped_willingness_to_pay, geom = 'blank') +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4,
                 binwidth = 10, position = "identity", colour = "black") +
  stat_function(fun = dnorm, aes(colour = 'Normal'),
                args = list(mean = mean(bootstrapped_willingness_to_pay),
                            sd = sd(bootstrapped_willingness_to_pay))) +
  scale_colour_manual(name = 'Distribution', values = c('red')) + 
  geom_vline(data = df, aes(xintercept = mean(bootstrapped_willingness_to_pay)), colour = "red") +
  scale_x_continuous(labels = dollar_format(), breaks = seq(0, 399, 25)) +
  theme_economist() +
  ggtitle("Average Willingness to Pay\nMarch 2014\nXYZ Company") +
  ylab("") +
  xlab("Average Willingness to Pay") +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, angle = 90, vjust = 1, hjust = 1),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0, 400))

sd(bootstrapped_willingness_to_pay)
