library(tidyverse)
library(knitr)
library(ggplot2)
library(reshape2)
library(viridis)

data <- readRDS("/Users/kelsey/Desktop/Sports Analytics/pbp2014-2024.rds")

# Cut out certain time in the quarter
# Randomize where in the 5 yard increments

quarter1<-data %>% filter(qtr==1) %>% filter(down==3) %>% filter(yardline_100<=75) %>% filter(yardline_100>=25)

# Assuming we have a EP model, input down, field position, and yards to go (filtering by quarter)
# Create function to calculate the decision boundary
descision_fcn<-function(EPs,EPf){
  p<-EPf/(EPs+EPf)
  p
}


# Create matrix to store decision boundaries
decision_boundaries_df <- data.frame(matrix(NA, nrow = 10, ncol = 5))

colnames(decision_boundaries_df) <- c("Yard Range","Quarter1", "Quarter2", "Quarter3", "Quarter4")
# Check these ranges b/c the last group has 6 yards not 5

ranges<-c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-75")

decision_boundaries_df$`Yard Range`<- ranges

ranges<-c(25:29,30:34,35:39,40:44,45:49,50:54,55:59,60:64,65:69,70:74)
yards<-c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-75")

quarter1<-c(0.4949594, 0.4975449, 0.4956879, 0.4992837, 0.4971841, 0.5000513, 0.4992649, 0.5015776, 0.5044593, 0.5050420)
quarter2<-c(0.4951310, 0.4974376, 0.4956930, 0.4992149, 0.4971974, 0.5000000, 0.4993505, 0.5013630, 0.5043730, 0.5047816)
quarter3<-c(0.4950628, 0.4973561, 0.4956416, 0.4993694, 0.4972354, 0.4999829, 0.4993675, 0.5014317, 0.5044923, 0.5050035)
quarter4<-c(0.4953265, 0.4975323, 0.4957096, 0.4992319, 0.4971667, 0.4999102, 0.4994700, 0.5011788, 0.5042107, 0.5047256)

df<-data.frame(yards,quarter1,quarter2,quarter3,quarter4)
df

## Create kable output of decision boundary probabilities 
kable(df, caption = "Decision Boundary Probabilites by Quarter and Field Position")

### Create Visualizations

# Pivot dataframe for visualizations
df_melt <- melt(df, id.vars = "yards")

## Create the heatmap
ggplot(df_melt, aes(x = variable, y = yards, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "blue") + 
  scale_x_discrete(labels = c("quarter1" = "Q1", "quarter2" = "Q2", "quarter3" = "Q3", "quarter4" = "Q4")) +
  theme_minimal() + 
  labs(title = "Heatmap of Yards vs. Quarter", x = "Quarter", y = "Yards Range", fill = "Decision Boundary") +
  theme(plot.title = element_text(hjust=0.5, size=16), 
        axis.text = element_text(size=10), 
        axis.title = element_text(size=12)
        )

## Create line plot 
df_melt$yards <- factor(df_melt$yards, levels = unique(df_melt$yards), ordered = TRUE)

ggplot(df_melt, aes(x = yards, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Decision Boundary Probabilities\nby Yard Range and Quarter",
       x = "Yard Range",
       y = "Decision Bounary Probability",
       color = "Quarter") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_viridis_d()

