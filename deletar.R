library(tidyverse)
library(caret)
library(pls)

data(Boston, package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Build the model on training set
set.seed(123)
model <- train(
  medv~., data = train.data, method = "pcr",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model RMSE vs different values of components
plot(model)
# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune

summary(model$finalModel)

# Make predictions
predictions <- model %>% predict(test.data)
# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, test.data$medv),
  Rsquare = caret::R2(predictions, test.data$medv)
)

df <- scale(Boston)
autoplot(prcomp(df), data = Boston, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5)


# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/152-principal-component-and-partial-least-squares-regression-essentials/
#   https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
#   https://stats.stackexchange.com/questions/87037/which-variables-explain-which-pca-components-and-vice-versa
# https://www.r-bloggers.com/performing-principal-components-regression-pcr-in-r/
#   https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
# https://stats.stackexchange.com/questions/143905/loadings-vs-eigenvectors-in-pca-when-to-use-one-or-another
# https://stats.stackexchange.com/questions/92499/how-to-interpret-pca-loadings
# 
# 
# http://rstatistics.net/principal-component-analysis/
#   https://stats.stackexchange.com/questions/4093/interpreting-pca-scores
