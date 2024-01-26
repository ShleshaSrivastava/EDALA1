  ---
title: "EDA Learning Activity-1"
output: pdf_document
---

**World University Ranking Dataset**

***Submitted by - Shlesha(1NT21IS153), Sem: 5-'C'***

**Introduction**

The World University Ranking dataset is a comprehensive collection of information that assesses and ranks higher education institutions globally based on various criteria. This dataset serves as a valuable resource for researchers, educators, and policymakers, offering insights into the comparative performance of universities worldwide. By examining factors such as academic reputation, faculty quality, research output, and international diversity, the dataset provides a nuanced understanding of the educational landscape on a global scale. These rankings play a pivotal role in shaping perspectives on academic excellence, aiding prospective students in making informed decisions about their educational journeys and contributing to discussions on the state of higher education internationally. Analyzing the World University Ranking dataset enables a deeper exploration of trends, patterns, and disparities within the global academic community, fostering a better understanding of the evolving dynamics in the realm of higher education.
```{r}
#read data into a dataframe
df<- read.csv("~/Desktop/cwurData.csv") 
#printing the summary of the dataframe
summary(df)                            
```
```{r}
#viewing named objects in the workspace
 ls()

```

```{r}
#checking the structure of a data frame
str(df)   
```
```{r}
#display first few rows of the data frame
head(df)  

```
```{r}
#accessing a column in a dataframe
df$world_rank  

```
```{r}
# Retrieves the names of all the columns present in the dataset
names(df)
```

```{r}
#Calculate mean of a specific column
 mean(df$world_rank) 

```
```{r}
#checking data type of a column
class(df$country) 

```


```{r}
# Create a subset of data frame where World rank is less than 25
subset_df <- df[df$world_rank < 10, ]
subset_df
```

```{r}
# Add a new column "Gender" to the data frame
df$Gender <- c("F", "M", "M", "M", "F")

```
```{r}
# Remove the "Patents" column from the data frame
df$Patents <- NULL
summary(df)
```
```{r}

# Rename the "Score" column to "Points"
names(df)[names(df) == "score"] <- "points"
summary(df)

```
```{r}
# Assuming "Gender" is a factor column, change level names
levels(df$Gender) <- c("Male", "Female")
str(df$Gender)
```

```{r}
# Adding Means to a Box Plot
boxplot(df$world_rank, mean = TRUE, col = "red", main = "Box Plot with Means")

```


```{r}
#Visualizing the Top Countries in Higher Learning

height<- sort(table(df$country), decreasing = TRUE)

barplot(height[1:10], las = 3, main = "Top Countries in University Rankings 2015")

```
```{r}
#Calculates the standard deviation a dataset
sd(df$national_rank)

```
```{r}
#Building a subset dataframe that contains information about only the schools of USA
usa <- subset(df, df$country == "USA")

```
**Analysis**

Looking at the data, it is clear that national ranking is determined by multiple factors. To determine how different factors correlate with National rank, we first begin by visualizing National Rank against different variables.

I chose these three variables for comparison.

1. Quality of Faculty

2. Influence

3. Citations
```{r}
#To determine how different factors correlate with National rank, we begin by visualizing 

#National Rank against the influence variable.
plot (usa$influence, usa$national_rank, xlab ="Influence", ylab="National Rank", main = "Influnce vs National Rank")
c <- lm(national_rank ~ influence, data = usa)
abline(c)
```
```{r}
summary(c) #regression model
```

```{r}
#To determine how different factors correlate with National rank, we then visualize National Rank 
#against the quality of faculty variable.
plot (usa$quality_of_faculty, usa$national_rank, xlab ="Quality of Faculty", ylab = "National Rank", main = "Quality of Faculty vs National Rank")
c <- lm(national_rank ~ quality_of_faculty, data = usa)
abline(c)
```
```{r}
summary(c) #regression model
```
```{r}
#To determine how different factors correlate with National rank, we then visualize National Rank 
#against the citations variable.
plot (usa$citations, usa$national_rank, xlab = "Citations", ylab = "National Rank", main = "Citations vs National Rank")
c <- lm(national_rank ~ citations, data = usa)
abline(c)
```

```{r}
summary(c) #regression model
```

```{r}

# Making a Dot Plot of World_Rank column

dotchart(df$world_rank, pch = 19, col = "brown", main = "Dot Plot")
```
```{r}

# Creating a Scatter Plot for world rank vs national rank
library(ggplot2)
ggplot(df, aes(x = world_rank, y = national_rank)) + geom_point()
```

```{r}
# Creating a Histogram for the world_rank column
ggplot(df, aes(x = world_rank)) + geom_histogram(binwidth = 5, fill = "blue", color = "black")

```

```{r}
# Plotting a Function Curve for the given dataset
curve(x^2, from = -10, to = 10, col = "blue", lwd = 2)
```
```{r}
# Making a Frequency Polygon for the national_rank column
plot(df$national_rank, type = "l", lty = 1, col = "pink", main = "Frequency Polygon")
```

```{r}
#converting data type
as.numeric(df$broad_impact) 

```

```{r}
# Recode "Year" into categories (e.g., "New", "Middle-aged", "Old")
df$YearGroup <- cut(df$year, breaks = c(0, 2014, 2016, Inf), labels = c("Old", "Middle-aged", "New"))
df$YearGroup
```
```{r}
# Transform "Points" by squaring each value
df$Squared <- df$broad_impact^2
df$broad_impact
```








