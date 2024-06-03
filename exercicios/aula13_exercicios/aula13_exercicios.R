pacman::p_load(tidyverse,utils,RcppAlgos,gtools,matrixStats)



# Criando o dataframe a partir dos dados fornecidos
Xi = c(0.2000, 0.2400, 0.2150, 0.2100, 0.2500, 0.2300, 0.1950, 0.2050, 0.2100, 0.2200)
Yi = c(0.0105, 0.0150, 0.0125, 0.0110, 0.0155, 0.0135, 0.0095, 0.0105, 0.0115, 0.0125)


dados <- data.frame(Laranja = 1:10,Xi,Yi)

# Visualizando o dataframe
print(dados)

# encontrando N

muX = mean(Xi) #peso medio de uma laranja da nosso amostra de 10
tx = 900 #Peso total das laranjas 

N = tx/muX


# Calculando as novas variaveis 


dados = dados %>%
  mutate(Xi2 = Xi^2,
         Yi2 = Yi^2,
         XiYi = Xi*Yi)



Sxy = sum((Yi-mean(Yi))*((Xi-mean(Xi))))

S2x = sum((Xi-mean(Xi))^2)


B0 = Sxy/S2x
B0


yreg = mean(Yi) + B0*sum(Xi-mean(Xi)) #Media
yreg

Tyreg = N*yreg #Total
Tyreg




n = 10


B0 = (sum(Yi*Xi) - n*mean(Yi)*mean(Xi))/(sum(Xi^2)-N*mean(Xi)^2)

B0





































































