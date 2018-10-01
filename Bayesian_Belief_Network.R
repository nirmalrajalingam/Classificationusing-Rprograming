#Load the Library

library(factoextra)
library(bnlearn)
library(neuralnet)

#Load the dataset
data<- marks

head(data)
str(data)
View(data)
bn_df <- data.frame(data)
res <- hc(bn_df)
plot(res)
res$arcs <- res$arcs[-which((res$arcs[,'from'] == "Gender" & res$arcs[,'to'] == "Condition")),]
fittedbn <- bn.fit(res, data = bn_df)
print(fittedbn$STAT)

#constraint train 
constraint <- hc(data)
print(constraint)
plot(constraint)

#probability table
print(fittedbn <- bn.fit(constraint, data = data))

#prediction
print(cpquery(fittedbn, event = (STAT>=80), evidence = ( MECH>=60 & VECT>=60 & ALG>=60 & ANL>=60 ) ))

#BACKPROPAGATION
head(data)
nn <- neuralnet(STAT ~  MECH+VECT+ALG+ANL, data=data, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix

#To plot the backpropogation
  plot(nn)



