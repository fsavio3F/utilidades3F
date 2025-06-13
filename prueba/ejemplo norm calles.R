{library(tidyverse)
library(stringdist)}
library(caret)          # For training/testing models
library(randomForest)   # Random Forest implementation
library(pROC)           # For ROC-AUC evaluation
library(dplyr)  

wd <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(wd)

{calles_ejemplo <- read.csv("direcciones_pruebas.csv")
  
sample <- dplyr::sample_n(calles_ejemplo, 500)

sample_3f <- calles_ejemplo %>%
  filter(calles_ejemplo$partido == "Tres de Febrero") %>% sample_n(500)
}


ejemplo <- normalizar_calles(sample_3f, "calle", debug = TRUE)

writexl::write_xlsx(ejemplo,"train_normalizacion.xlsx")
train <- readxl::read_excel("train_normalizacion.xlsx") %>% 
  mutate(is_true = ifelse(coincidencia == 0, T, F))


set.seed(123) # For reproducibility

# Create train/test split
train_index <- createDataPartition(train$is_true, p = 0.8, list = FALSE)
train_data <- train[train_index, ]
test_data  <- train[-train_index, ]
model <- randomForest(is_true ~ puntaje_mejor +
                        puntaje_distancia +
                        puntaje_tokens +
                        puntaje_mejor01 +
                        puntaje_mejor02 +
                        puntaje_mejor03 +
                        puntaje_mejor04 +
                        puntaje_mejor05 +
                        puntaje_mejor06 +
                        puntaje_mejor07 +
                        puntaje_mejor08 +
                        puntaje_mejor09 +
                        puntaje_mejor10 
                        , data = train_data)
print(model)
importance <- importance(model)
varImpPlot(model)  # Visualize feature importance


# Predict on test set
predictions <- predict(model, test_data, type = "class")

# Confusion matrix
confusionMatrix(predictions, test_data$is_true)



test_data$is_true <- as.factor(test_data$is_true)
na.omit(test_data$)

# Align predictions with true label levels
predictions <- factor(predictions, levels = levels(test_data$is_true))
predictions <- factor(ifelse(predictions > 0.5, "TRUE", "FALSE"), levels = levels(test_data$is_true))

print(head(predictions))
# Generate confusion matrix
confusionMatrix(predictions, test_data$is_true)

# ROC-AUC
roc_curve <- roc(test_data$is_true, as.numeric(predictions))
plot(roc_curve, main = "ROC Curve")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
Confusion Matrix: Provides accuracy
importance_values <- importance(model)
print(importance_values)
}