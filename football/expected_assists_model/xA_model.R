# gebrauchte Libraries
library(xgboost)
library(caret)
library(tidyverse)
library(kickR)

# Lade Daten für das Passspiel der Top 5 Ligen
df <- fbref_big5_player_stats(type = 'passing')
write.csv(df, 'data_20231024.csv') # gespeichert f. reproducibility

df$nation <- gsub(".*\\s", "", df$nation)
df$league <- gsub("^[a-z]+\\s", "", df$league)
df$position <- substr(df$position, 1, 2)

dd <- df

num_cols <- c('mins_per_90', 'pass_completion_percentage',
              'short_pass_completion_percentage',
              'medium_pass_completion_percentage',
              'long_pass_completion_percentage','xA','xAG','xag_performance')

# Vektor der Spaltennamen, die in Ganzzahlen umgewandelt werden sollen
columns <- c('birth_year', 'total_passes_completed', 'total_passes_attempted', 
             'total_passing_distance', 'total_progressive_distance', 'short_passes_completed',
             'short_passes_attempted', 'medium_passes_completed', 'medium_passes_attempted',
             'long_passes_completed', 'long_passes_attempted', 'assists', 'key_passes',
             'passes_into_final_third', 'passes_into_penalty_box',
             'crosses_into_penalty_box', 'progressive_passes')

for (i in num_cols) {
  dd[[i]] <- as.numeric(dd[[i]])
}

for (i in columns) {
  dd[[i]] <- as.integer(dd[[i]])
}

summary(dd)

# Nur Spieler, die > 360 Minuten gespielt haben
dd2 <- dd |>
  filter(mins_per_90 > 4)

# Neue Spalte 'name' erstellen
dd2$name <- sapply(strsplit(dd2$player, " "), function(names) {
  if (length(names) == 1) {
    # Ein Name, belasse ihn so
    return(names)
  } else if (length(names) == 2) {
    # Zwei Namen, extrahiere den Nachnamen
    return(names[2])
  } else {
    # Drei oder mehr Namen, extrahiere die letzten beiden Namen
    return(paste(names[length(names) - 1], names[length(names)], sep = " "))
  }
})

# Create the scatter plot
ggplot(dd2, aes(passes_into_penalty_box, passes_into_final_third)) +
  geom_point()

ggplot(dd2, aes(xA))+
  geom_histogram()


ggplot(dd2, aes(x = position, y = xA)) +
  geom_boxplot() +
  labs(title = "Distribution of xA by Positions", x = "Position", y = "xA")

ggplot(dd2, aes(x = league, y = xA)) +
  geom_boxplot() +
  labs(title = "Distribution of xA by League", x = "Position", y = "xA")

# Ausgewählte Spalten
selected_columns <- c(
  "xA",
  "xAG",
  "total_passes_completed",
  "total_passes_attempted",
  "pass_completion_percentage",
  "total_passing_distance",
  "total_progressive_distance",
  "short_passes_completed",
  "short_passes_attempted",
  "short_pass_completion_percentage",
  "medium_passes_completed",
  "medium_passes_attempted",
  "medium_pass_completion_percentage",
  "long_passes_completed",
  "long_passes_attempted",
  "long_pass_completion_percentage",
  "key_passes",
  "passes_into_final_third",
  "passes_into_penalty_box",
  "crosses_into_penalty_box",
  "progressive_passes"
)

## Neuen Datenrahmen mit ausgewählten Spalten erstellen
data <- dd2[selected_columns]

# Schritt 1: Aufteilung der Daten
set.seed(777)
indices <- sample(1:nrow(data), size = 0.7 * nrow(data))
train <- data[indices, ]
test <- data[-indices, ]

# Schritt 2: Modellauswahl und -training
model <- lm(xA ~ ., data = train)

summary(model)

# Schritt 3: Modellbewertung
predicted_xA <- predict(model, newdata = test)
mae <- mean(abs(predicted_xA - test$xA))
mse <- mean((predicted_xA - test$xA)^2)
rmse <- sqrt(mse)
rsquared <- 1 - (sum((predicted_xA - test$xA)^2) / sum((mean(test$xA) - test$xA)^2))

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R²):", rsquared, "\n")


# Nur statistisch signifikante Variablen in ein parsimoniales Modell aufnehmen
pars_model <- lm(xA ~ xAG + total_progressive_distance + 
                   long_pass_completion_percentage + key_passes + 
                   passes_into_penalty_box, data = train)

summary(pars_model)

# Trainieren des XGBoost-Modells
xgb_model <- xgboost(data = as.matrix(train[, -1]),
                     label = train$xA,
                     nrounds = 100,
                     objective = "reg:squarederror",
                     verbose = 1)

# Vorhersage auf den Testdaten
predicted_xA <- predict(xgb_model, as.matrix(test[, -1])) # xA ausschließen

# Bewertungsmetriken berechnen
mae <- mean(abs(predicted_xA - test$xA))
mse <- mean((predicted_xA - test$xA)^2)
rmse <- sqrt(mse)
rsquared <- 1 - (sum((predicted_xA - test$xA)^2) / sum((mean(test$xA) - test$xA)^2))

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R²):", rsquared, "\n")


feature_importance <- xgb.importance(model = xgb_model)

# Features nach Gewichtung in absteigender Reihenfolge sortieren
sorted_features <- feature_importance[order(-Gain), ]

# Nur progressive passes[7] bis zur Einbeziehung der Bedeutung
important_features <- sorted_features[1:7, ]

# Die Namen der wichtigen Features auflisten
important_feature_names <- important_features$Feature

# Trainings- und Testdaten mit nur den wichtigen Features aktualisieren
train_important <- train[, c("xA", important_feature_names)]
test_important <- test[, c("xA", important_feature_names)]

# Modell mit reduziertem Merkmalsatz erneut trainieren
new_model <- xgboost(data = as.matrix(train_important[, -1]),
                     label = train_important$xA,
                     nrounds = 100, # Bei Bedarf anpassen
                     objective = "reg:squarederror",
                     verbose = 1)

# Das neue Modell auf den Testdaten bewerten
predicted_xA_new <- predict(new_model, newdata = as.matrix(test_important[, -1]))
mae_new <- mean(abs(predicted_xA_new - test_important$xA))
mse_new <- mean((predicted_xA_new - test_important$xA)^2)
rmse_new <- sqrt(mse_new)
rsquared_new <- 1 - (sum((predicted_xA_new - test_important$xA)^2) / sum((mean(test_important$xA) - test_important$xA)^2))

cat("Mean Absolute Error (MAE) with reduced features:", mae_new, "\n")
cat("Mean Squared Error (MSE) with reduced features:", mse_new, "\n")
cat("Root Mean Squared Error (RMSE) with reduced features:", rmse_new, "\n")
cat("R-squared (R²) with reduced features:", rsquared_new, "\n")
# Geringfügiger Genauigkeitsverlust: Das erste Modell hat eine bessere Genauigkeit

# Kreuzvalidierung
# Definiere die Anzahl der Folds (z. B., 5-fold cross-validation)
num_folds <- 5

# Definiere die Steuerungsparameter für die Kreuzvalidierung
ctrl <- trainControl(
  method = "cv",
  number = num_folds,
  verboseIter = TRUE,
  summaryFunction = defaultSummary
)

# Definiere das Parametergitter für die Anpassung
tuneGrid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 1
)

# Trainiere das XGBoost-Modell mit k-fold Kreuzvalidierung
model_cv <- train(
  x = as.matrix(train_important[, -1]),
  y = train_important$xA,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = tuneGrid,
  verbose = FALSE
)

# Bewertung der Leistung des Modells auf den Testdaten
predicted_xA <- predict(model_cv, newdata = as.matrix(test_important[, -1]))
mae_cv <- mean(abs(predicted_xA - test_important$xA))

# Ausgabe des MAE der kreuzvalidierten Bewertung
cat("Cross-Validated Mean Absolute Error (MAE):", mae_cv, "\n")