#### Packages
library(ggplot2)
library(GPBayes)
library(tidyverse)
####
a = 5
c = 0.25
kappa = 2.2
phi = (2*c)/(kappa^2*a)
outputdf = data.frame(TG_func=numeric(),
                      arctan=numeric(),
                      input=numeric(),
                      gaussian=numeric())
for (x in seq(-10,10,0.01)){
  value = (1.17)*(-log(HypergU(c+0.5,1.5-a,(x^2)/(2*phi))+1)+log(HypergU(c+0.5,1.5-a,0)+1))
  arctan_value = (2/pi)*atan(abs(x))
  gausian_value = 1 - exp(1)^(-x^2)
  outputdf = rbind(outputdf, data.frame(TG_func=value,gaussian = gausian_value, input=x, arctan=arctan_value))
  
}

ggplot(outputdf, aes(x=input)) +
  geom_line(aes(y=3*TG_func, color="Triple-Gamma"), size=1)+
  #geom_line(aes(y=arctan, color="Arctan"),size=1)+
  geom_line(aes(y=gaussian, color="Gaussian"),size=1)+
  scale_color_manual(name = "Pentalty Term", values = c("Gaussian"="green","Triple-Gamma" = "darkblue", "Arctan" = "red"))+
  labs(x="Beta Values", y="Penalty")+
  theme_minimal()


### Variations in A
c = 0.1
kappa = 2
range_x=20
intervals = 0.01
n = range_x/intervals
a_values = c(0.51,0.75,1,5)
matrix_output = matrix(nrow=n+1, ncol=length(a_values)+1)
matrix_output[,1] = seq(-10,10,0.01)
counter = 2

for (a in a_values){
  phi = (2*c)/(kappa^2*a)
  value = c()
  for (x in seq(-10,10,0.01)){
    value = c(value,-log(HypergU(c+0.5,1.5-a,(x^2)/(2*phi)))+log(HypergU(c+0.5,1.5-a,0)))
  }
  matrix_output[,counter] = value
  counter=counter+1
}

colnames(matrix_output) <- c("Input",paste0("a=",a_values))
matrix_output <- as.data.frame(matrix_output)

ggplot(matrix_output, aes(x=Input)) +
  geom_line(aes(y=`a=0.51`, color="a=0.51"),size=0.8)+
  geom_line(aes(y=`a=0.75`, color="a=0.75"),size=0.8)+
  geom_line(aes(y=`a=1`, color="a=1"),size=0.8)+
  geom_line(aes(y=`a=5`, color="a=5"),size=0.8)+
  scale_color_manual(name = "Pentalty Term", values = c("a=0.51"="green",
                                                        "a=0.75"="darkblue",
                                                        "a=1"="darkred",
                                                        "a=5"="darkorange"))+
  labs(x="Beta Values", y="Penalty")+
  theme_minimal()
  