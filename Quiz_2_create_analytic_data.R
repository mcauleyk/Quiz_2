bfi_data <- psych::bfi

library(tidyverse)

View(bfi_data)

categorical_variables <- select(bfi_data, gender, education)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Males"=1, "Females"=2)

#Check these have been changed
str(categorical_variables)

#create new data set with single set of items (ex. scale for extroversion)
agreeableness_items <- select (bfi_data, A1, A2, A3, A4, A5)
extraversion_items <- select (bfi_data, E1, E2, E3, E4, E5)
neuroticism_items <- select (bfi_data, N1, N2, N3, N4, N5)
gender <- select (bfi_data, gender)
education <- select (bfi_data, education)
age <- select (bfi_data, age)

#check for out of value ranges (any responses outside of what range should be)
psych::describe(agreeableness_items)
psych::describe(extraversion_items)
psych::describe(neuroticism_items)

#reverse key
agreeableness_items <- mutate(agreeableness_items,A1=7-A1)
extraversion_items <- mutate(extraversion_items,E1=7-E1)
extraversion_items <- mutate(extraversion_items,E2=7-E2)

#create a single score for each participant
agreeableness <- psych::alpha(as.data.frame(agreeableness_items) ,check.keys=FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion_items) ,check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism_items) ,check.keys=FALSE)$scores

#combine all columns into new data frame called analytic_data
analytic_data <- cbind(categorical_variables,age,agreeableness,neuroticism,extraversion)
## to view the data frame
analytic_data


#Saving the data
##.CSV
write_csv(analytic_data,path="analytic_data.csv")

##Creating Table #1
#download data from URL
my.data <- read_csv("analytic_data.csv")

#extract relevant columns for analytic data
analytic.data <- select(my.data, agreeableness, extraversion, neuroticism, education, age)

#View correlations between multiple variables, converting data set to data frame
library(apaTables)
apa.cor.table(as.data.frame(analytic.data))
#Save table
apa.cor.table(analytic.data, filename ="Table1.doc")

###############
#Correlation table with Men over the age of 40
my.data <- read_csv("analytic_data.csv")

#extract relevant columns for analytic data

analytic.data.gender <- my.data %>% filter(gender=="Males") %>% select(-gender)
analytic.data.40 <- filter(analytic.data.gender, age>40)

analytic.data <- select(analytic.data.40, agreeableness, extraversion, neuroticism, education, age)

#View correlations between multiple variables, converting data set to data frame
library(apaTables)
apa.cor.table(as.data.frame(analytic.data))
#Save table
apa.cor.table(analytic.data, filename ="Table2.doc")

###########
#Make scatter plot
plot.men.40 <- qplot(agreeableness,extraversion,data=analytic.data.40)
plot.men.40 <- plot.men.40 + theme_classic()
plot.men.40 <- plot.men.40 + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
print(plot.men.40)
ggsave("Figure1.pdf",plot=plot.men.40, width=6,height=6)

#### Correlation
analytic.data.40$agreeableness
analytic.data.40$extraversion
cor.test(x=analytic.data.40$agreeableness, y=analytic.data.40$extraversion)