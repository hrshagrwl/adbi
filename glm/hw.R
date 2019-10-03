# Load the Data

library('readxl')
df <- read_excel('eBayAuctions.xls')
colnames(df)[which(names(df) == "Competitive?")] <- "competitive"
names(df) <- tolower(names(df))
summary(df)

# Create Pivot tables and merge the columns
library(reshape)

# Function to generate Pivot table for a colum
generatePivotTable <- function(df, col) {
    # Melt the column we want    
    df_melt = melt(df, id.vars = c(col), measure.vars = 'competitive')
    # Cast to pivot form
    p_table <- cast(df_melt, paste(paste(col), "~", "variable"), mean)
    # Duplicate the first column so we can merge
    p_table['merge'] <- p_table[1]
    # Number of rows in the table
    len <- dim(p_table[1])
    # Threshold of ratio/mean to use for merging the categorical varaibles
    threshold <- 0.05
    # Merge
    for (i in 1:(len-1)) {
        for (j in (i+1):len){
            if (abs(p_table[i,2] - p_table[j,2]) < threshold) {
                p_table[j,3] = p_table[i,3]
            }
        }
    }
    return (p_table)
}

# Function to generate dummy columns
createDummy <- function(x, col) {
    for (level in unique(x[,col])) {
        x[paste('d', col, level, sep = "_")] <- ifelse(x[,col] == level, 1, 0)
    }
    return(x)
}

# Columns to check and merge
columns <- c('category', 'currency', 'endday', 'duration')

for (col in columns) {
    # Generate Pivot table for col
    p_table <- generatePivotTable(df, col)
    print(p_table)
    # Merge Rows
    rows <- dim(p_table[1])
    for (i in 1:rows) {
      df[df[paste(col)] == p_table[i,1], paste(col)] = p_table[i,3]
    }
    # Create dummy columns     
    df <- createDummy(df, col)
    # Drop the column
    df[, paste(col)] <- NULL
}

# Print the data once
head(df)

# Split the data into train and test
## 60% of the sample size
smp_size <- floor(0.60 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

fit.all <- glm(`competitive` ~., family = binomial(link = 'logit'), data = train)
summary(fit.all)

# Predict the result and check the accuracy
predicted <- predict(fit.all, test, type = 'response')
predicted <- ifelse(predicted > 0.5, 1, 0)
accuracy <- mean(test$competitive == predicted)
print(accuracy)

# Checking the coefficients
coef = fit.all$coefficients
print(coef)

# Function to get the highest absolute value predictor variable
max = 0
name = names(coef)[1]
index = 1;
for (i in 2:length(coef)) {
    val = abs(as.numeric(coef[i]))
    if (!is.na(val) && val > abs(as.numeric(max))) {
        name = names(coef)[i]
        if (name != '(Intercept)') {
            max = coef[i]
            index = i
        }
    }  
}

print(max)
print(name)

# Train fit.single using the above found variable
subset = c("competitive", name)
fit.single = glm(competitive ~., family = binomial(link='logit'), data = train[subset])

# Find most significant predictors using p-value
significance_level = 0.05
coefs = summary(fit.all)$coefficients
significant_predictors = coefs[coefs[,4] < significance_level,]
print(significant_predictors)

# Train Fit.reduced using the above found predictors
subset = names(significant_predictors[,1])
subset = c('competitive', subset)
fit.reduced = glm(competitive ~., family = binomial(link='logit'), data = train[subset])
summary(fit.reduced)

# Perform the anova chi-square test
anova(fit.reduced, fit.all, test='Chisq')

# Perform the over-dispersion test
library(qcc)
s = rep(length(train$competitive), length(train$competitive))
qcc.overdispersion.test(train$competitive, size = s, type="binomial")