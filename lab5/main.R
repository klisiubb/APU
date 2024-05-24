install.packages("tensorflow")
install.packages("keras")
library(tensorflow)
install_tensorflow()
library(keras)
install_keras()
mnist <- dataset_cifar10()
X_train <- mnist$train$x
X_test <- mnist$test$x
y_train <- mnist$train$y
y_test <- mnist$test$y
X_train <- array_reshape(X_train, c(nrow(X_train), 784))
X_train <- X_train / 255
X_test <- array_reshape(X_test, c(nrow(X_test), 784))
X_test <- X_test / 255
y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 10, activation = "softmax")
model <- keras::keras_model_sequential() %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")
summary(model)
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)
history <- model %>%
  fit(X_train, y_train, epochs = 50, batch_size = 128, validation_split = 0.15)
model %>%
  evaluate(X_test, y_test)
model %>%
  predict_classes(X_test)