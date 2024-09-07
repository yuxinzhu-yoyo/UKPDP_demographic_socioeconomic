knitr::opts_chunk$set(echo = FALSE, warning = FALSE)

library(glmnet)
library(gt)
library(rpart)
library(maptools)
library(raster)
library(plotrix)
library(kableExtra)
library(caret)
library(gam)
library(splines)
library(tidyverse)
library(here) # CN addition for working within project directory structure

source(here("scripts/visualize_map.R"))
data.sub <- readRDS(here("data/data.sub.RDS"))

# data.sub %>% 
#   summarytools::descr() %>% 
#   t() %>% 
#   round(2)

data.sub %>%
    summarytools::descr() %>%
    t() %>%
    round(2) %>%
    kbl(caption = "Table. Data Summary", digits = 2)%>%
    kable_classic(full_width = F, font_size = 12)
data.sub %>% 
    gather(key = "key", value = "value", -Dogs_per_Household) %>% 
    ggplot() + 
    geom_point(aes(x = value, y = Dogs_per_Household)) + 
    geom_smooth(aes(x = value, y = Dogs_per_Household), 
                method = "lm", formula = "y ~ x") + 
    facet_wrap(~key, scales = "free", ncol = 4)
data.sub %>% 
    cor() %>% 
    corrplot::corrplot()
cor(data.sub)[,1] %>% round(3) %>% 
    data.frame() %>% 
    kbl(caption = "Table. Data Summary", digits = 2)%>%
    kable_classic(full_width = F, font_size = 12)
age_group <- readRDS(here("data/age_group.RDS"))
age_group %>% 
    select(-postal_area_code, -District, -Country, -Region) %>% 
    as.matrix() -> observed

rownames(observed) <- age_group$postal_area_code

# Perform the chi-square proportion test
test.age <- chisq.test(observed)

size_group <- readRDS(here("data/size_group.RDS"))
size_group %>% 
    select(-postal_area_code) %>% 
    as.matrix() -> observed

rownames(observed) <- size_group$postal_area_code

# Perform the chi-square proportion test
test.size <- chisq.test(observed)

brachy <- readRDS(here("data/brachy.RDS"))
brachy %>% 
    select(-postal_area_code) %>% 
    as.matrix() -> observed
rownames(observed) <- brachy$postal_area_code

# Perform the chi-square proportion test
test.brachy <- chisq.test(observed)
data.frame(Type = c("Age Group", "Size Group", "Brachy Group"), 
           X_squared = c(test.age$statistic, test.size$statistic, test.brachy$statistic), 
           DF = c(test.age$parameter, test.size$parameter, test.brachy$parameter), 
           P_value = c("< 2.2e-16", "< 2.2e-16", "< 2.2e-16")) -> test.df
test.df %>%   
    kbl(caption = "Table. Proportion Test", digits = 2)%>%
    kable_classic(full_width = F, font_size = 12)
set.seed(1)
# Fit LASSO regression model using cross-validation to determine penalty parameter
cv_fit <- cv.glmnet(data.sub %>% dplyr::select(-Dogs_per_Household) %>% as.matrix(), 
                    data.sub$Dogs_per_Household, 
                    alpha = 1)

plot(cv_fit)

cv_fit
# Choose value of lambda that minimizes CV error
lambda_min <- cv_fit$lambda.min

# Fit LASSO regression model using chosen lambda
fit <- glmnet(data.sub %>% dplyr::select(-Dogs_per_Household) %>% as.matrix(), 
              data.sub$Dogs_per_Household, 
              alpha = 1, lambda = lambda_min)
names(coef(fit)[coef(fit)[,1] != 0,]) -> features_selected_demographic
features_selected_demographic[2:length(features_selected_demographic)] -> features_selected_demographic

data.sub %>% 
    dplyr::select(c("Dogs_per_Household", features_selected_demographic)) -> data.sub1

model.lm <- lm(formula = Dogs_per_Household ~ ., data = data.sub1)
model.lm.sum <- summary(model.lm)

model.lm.sum$coefficients %>% 
    data.frame() %>% 
    kbl(caption = "Table. Estimated Coefficients", digits = 2)%>%
    kable_classic(full_width = F, font_size = 12)
set.seed(1)
# Split the data into training and testing sets (70%-30% split)
train_idx <- sample(nrow(data.sub1), floor(nrow(data.sub1) * 0.7))
train_data <- data.sub1[train_idx, ]
test_data <- data.sub1[-train_idx, ]
# Define the tuning grid
cp_values <- 10^seq(-4, -1, by = 0.2)
tune_grid <- data.frame(cp = cp_values)

# Create the train control object with 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the decision tree model with cross-validation
set.seed(1)
dt_model <- train(
    Dogs_per_Household ~ ., 
    data = train_data[,1:30], 
    method = "rpart", 
    tuneGrid = tune_grid, 
    trControl = ctrl
)

# Print the best model and its performance metric
dt_model
# Build the decision tree model using the "rpart" function
model.rpart <- rpart(Dogs_per_Household ~ . , data = train_data, cp = 0.001)

# Print the decision tree model
model.rpart
rpart.plot::rpart.plot(model.rpart)
Demographic <- read.csv(here("data/core_data.csv"))
Demographic[2:nrow(Demographic),] %>% 
    rename("Area" = "X") %>% 
    mutate(Dogs_per_Household = Dogs.Per.people * 2.4) -> Demographic
Demographic %>% 
    rename("postal_area_code" = "Area") -> Demographic

Demographic %>% 
    dplyr::select(postal_area_code, Dogs_per_Household) %>% 
    rename("value" = "Dogs_per_Household") -> df

visualize_map(df)
data.sub1 %>% 
    dplyr::select(Dogs_per_Household, lat, long) %>% 
    ggplot() + 
    geom_point(aes(x = lat, y = Dogs_per_Household)) + 
    geom_smooth(aes(x = lat, y = Dogs_per_Household), 
                method = "loess", formula = "y ~ x") + 
    theme_minimal() + 
    xlab("Latitude") + 
    ylab("Dogs Per Household") + 
    ggtitle("Relationship between Dogs Per Household and Latitude") -> A

data.sub1 %>% 
    dplyr::select(Dogs_per_Household, lat, long) %>% 
    ggplot() + 
    geom_point(aes(x = long, y = Dogs_per_Household)) + 
    geom_smooth(aes(x = long, y = Dogs_per_Household), 
                method = "loess", formula = "y ~ x") + 
    theme_minimal() + 
    xlab("Longitude") + 
    ylab("Dogs Per Household") + 
    ggtitle("Relationship between Dogs Per Household and Longitude") -> B
ggpubr::ggarrange(A, B, nrow = 2)
lm(formula = Dogs_per_Household ~ poly(lat, 3) + poly(long, 3), 
   data = data.sub1 %>% 
       dplyr::select(Dogs_per_Household, lat, long)) -> model.lm2

summary(model.lm2)
