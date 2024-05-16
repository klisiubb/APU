install.packages("neuralnet")
library(neuralnet)
#x ∈ [1; 10]
input <-  as.data.frame(runif(100, min=1, max=10))
#f(x) = x^2 + e^−x
output <- input^2 + exp(1)^(-input)
#Połączenie danych wejsciowych i wyjsciowych
trainingdata <- cbind(input,output)
colnames(trainingdata) <- c("Wejscie","Wyjscie")
#Trenowanie sieci neuronowej
net.sqrt <- neuralnet(Wyjscie~Wejscie,trainingdata, hidden=7, threshold=0.01, stepmax=1e7)
print(net.sqrt)
plot(net.sqrt, rep = "best")
#Prognozowanie z pomoca˛ sieci neuronowej
testdata <- as.data.frame(runif(300, min=1, max=10))
net.results <- compute(net.sqrt, testdata)
print(net.results$net.result)
cleanoutput <- cbind(testdata,log(testdata^2), as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Wejscie","Oczekiwane Wyjscie","Wyjscie sieci neuronowej")
print(cleanoutput)