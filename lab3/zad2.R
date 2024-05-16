setwd("C:/Users/klism/Documents/GitHub/APU/lab3/")
df <- read.csv("./lodowki.csv")
pojemnosc <- df[["pojemnosc"]]  
cena <- df[["cena"]]  

compare.trainingdata <- cbind(pojemnosc, cena)
scaled.pojemnosc <- as.data.frame(scale(pojemnosc))
trainingdata <- cbind(scaled.pojemnosc, cena)

colnames(trainingdata) <- c("Pojemnosc", "Cena")
#(error ≤ 100 z l)
net.price <- neuralnet(Cena~Pojemnosc,trainingdata, hidden<-c(7,1), threshold<-100, lifesign <- "full")
plot(net.price)

testdata <- data.frame(c(20,130))
scaled.testdata <- as.data.frame(scale(testdata))

net.results <- compute(net.price, scaled.testdata)
fixed_cena <- cbind(testdata, as.data.frame(net.results$net.result))
colnames(fixed_cena) <- c("Pojemnosc", "Cena")

print(fixed_cena)
