# library(devtools)
# devtools::install_github("Displayr/flipMultivariates")


# install.packages('rgdal', repos='http://r.findata.org')
library(flipMultivariates)

vehicles <- read.csv('data/vehicles.csv')
lda <- LDA(class ~ ., data = vehicles)

# lda

lda.2 <- LDA(class ~ COMPACTNESS + CIRCULARITY + DISTANCE.CIRCULARITY + RADIUS.RATIO,
             data = vehicles,
             output = "Scatterplot",
             prior = "Equal",
             subset = vehicles$ELONGATEDNESS < 50,
             weight = ifelse(vehicles$class == "saab", 2, 1))
print(lda.2)

LDA(class ~ COMPACTNESS + CIRCULARITY + DISTANCE.CIRCULARITY + RADIUS.RATIO,
    data = vehicles,
    output = "Prediction-Accuracy Table",
    prior = "Equal",
    subset = vehicles$ELONGATEDNESS < 50,
    weight = ifelse(vehicles$class == "saab", 2, 1))
