df <- read.csv('/Users/sneha/Downloads/final.csv')
df
install.packages("foreign")
install.packages("ggplot2")
install.packages("MASS")
install.packages("Hmisc")
install.packages("reshape2")

## one at a time, table apply, pared, and public
lapply(df[, c('stringency_index','contact_tracing','facial_coverings','income_support','restrictions_internal_movements','international_travel_controls','restriction_gatherings','school_closures','stay_home_requirements','workplace_closures')], table)

require(MASS)
m <- polr(as.factor(res) ~ stringency_index+contact_tracing+facial_coverings+income_support+restrictions_internal_movements+international_travel_controls+restriction_gatherings+school_closures+stay_home_requirements+workplace_closures, data = df, method = c('logistic'),Hess=TRUE)

## view a summary of the model
summary(m)

(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m))

confint.default(m)

exp(coef(m))

exp(cbind(OR = coef(m), ci))

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(df, summary(as.factor(res) ~ stringency_index+contact_tracing+facial_coverings+income_support+restrictions_internal_movements+international_travel_controls+restriction_gatherings+school_closures+stay_home_requirements+workplace_closures, fun=sf)))

install.packages('caTools')
library(caTools)
set.seed(150)
split = sample.split(df$res, SplitRatio = 0.70)
 
# Create training and testing sets
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)
 
dim(train); dim(test)


n <- polr(as.factor(res) ~ stringency_index+contact_tracing+facial_coverings+income_support+restrictions_internal_movements+international_travel_controls+restriction_gatherings+school_closures+stay_home_requirements+workplace_closures, data = train, method = c('logistic'),Hess=TRUE)
summary(n)

library(dplyr)

x <- select(test, stringency_index,contact_tracing,facial_coverings,income_support,restrictions_internal_movements,international_travel_controls,restriction_gatherings,school_closures,stay_home_requirements,workplace_closures)
y <- select(test, res)


newdat <- cbind(x, predict(n, x, type = "class"))
head(newdat)
newdat

install.packages('Metrics')
library(Metrics)
install.packages('MLmetrics')
library(MLmetrics)

Precision(y_true = y$res, y_pred = newdat$`predict(n, x, type = "class")`)

length(newdat$`predict(n, x, type = "class")`)
length(y$res)



