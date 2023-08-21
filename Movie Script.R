#Import library
library(tidyverse)

#Check datatypes
str(df)
dim(df)

#Check for missing values
colSums(is.na(df))

#Remove missing values
df <- na.omit(df)
df<-df%>%  filter(!row_number() %in% c(38))

#Check for duplicate values
df <- df[!duplicated(df$Film), ]

#Round to 2 decimal places
df$Profitability <- round(df$Profitability ,digit=2)
df$Worldwide.Gross <- round(df$Worldwide.Gross ,digit=2)

#Check for outliers using a boxplot
library(ggplot2)
ggplot(df,aes(x=Profitability, y=Worldwide.Gross))+geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))


#Remove outliers in Profitability
Q1 <- quantile(df$Profitability, .25)
Q3 <- quantile(df$Profitability, .75)
#Interquartile Range
IQR <- IQR(df$Profitability)

no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR))


#Remove outliers in Worldwide.Gross
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
#InterQuartile Range
IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross>(Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))
dim(df1)

#scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 300))

#BarChart
ggplot(df1, aes(x=Year)) + geom_bar()
ggplot(df1, aes(x=Genre)) + geom_bar()

#PieChart
data(df1)
counts <- table(df1$Genre)
pie(counts, col=c("red", "green", "blue", "pink","purple","orange","yellow","cyan","turquoise"))

View(df1)

#Export data to CSV file
write.csv(df1, "clean_df.csv")
