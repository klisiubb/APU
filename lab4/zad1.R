install.packages("C50")

library(MASS)

require(C50)

data("bacteria")

head(bacteria)

str(bacteria)

bacteria[,'ap'] <- factor(bacteria[,'ap'])

str(bacteria)

table(bacteria$ap)

m1 <- C5.0(bacteria[1:220,-6],bacteria[1:220,6])

summary(m1)

plot(m1)
