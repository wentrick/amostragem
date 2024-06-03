pacman::p_load(tidyverse,utils,RcppAlgos,gtools,matrixStats,dplyr,tidyr)

######## 7.1

#parametros populacionais

D = c(uc1,uc2,uc3)
N = length(D)

mean(D)
var(D) #S2
var(D)* (N - 1) / N #sigma2

#parametros dos conglomerados


# Criação do data frame inicial
df <- data.frame(
  conglo = c("C","C","C","D","D","D"),
  id =  c(1,2,3,1,2,3)
)

# Adicionando manualmente a coluna com vetores
df$new_column <- I(list(
  c(2),
  c(6,8,10),
  c(10,12),
  c(2,6,8),
  c(10,10),
  c(12)
))

# Usando dplyr para calcular a média dos vetores
df <- df %>%
  rowwise() %>%
  mutate(conglo = as.factor(conglo),
         b = length(new_column),
         mu = mean(new_column),
         sigma2 = var(new_column)*(b - 1) / b,
         t = sum(new_column)) %>%
  ungroup() %>%
  mutate(b_bar = ave(df$b,df$conglo,FUN = mean),
         y_barc = t/b_bar)

df[is.na(df)] <- 0

df2 <- as.data.frame(df) %>%
  group_by(conglo) %>%
  summarize(n = n(),
            sigma2_bar = mean(sigma2),
            t_bar = mean(t),
            y_bar = mean(y_barc),
            var_y = var(y_barc)*(n - 1) / n)
  
df2  

# Exibindo o data frame com a nova coluna de médias
print(df)


###################### 7.7


### A)
#parametros populacionais
D = c(c(0,1),c(1,2,2,3),c(3,3,4,4,5,5))
N = 12
A = 3
mean(D)
var(D) #S2
var(D)* (N - 1) / N #sigma2

B = c(2,4,6)
b_bar = mean(B)
mua = c(0.5,2,4)
sigma2a = c(0.25,0.50,0.67)

sigma2ect = 1/A * sum((B/mean(B)*mua - mean(D))^2) 

sigma2ec = 1/A * sum(B/mean(B)*(mua - mean(D))^2)

sigma2dc = 1/A * sum(B/mean(B)*sigma2a)

sigma2eq = 1/A * sum((B/mean(B))^2*(mua-mean(D))^2)

sigma2 = sigma2dc + sigma2ec

### B)

dados <- I(list(c(0,1),c(1,2,2,3),c(3,3,4,4,5,5)))

resultsT <- c()
resultsYc1 = c()
resultsYc1_quad = c()

amostra <- c(1,1,1, 2, 1, 3, 2, 1,2,2,2, 3, 3, 1, 3, 2,3,3)
t= 0

for (i in seq(1, length(amostra)/2)) {
  t_bar <- mean(c(sum(unlist(dados[amostra[i+t]])),sum(unlist(dados[amostra[i+t+1]]))))
  yc1 = t_bar/b_bar
  yc1_quad = yc1^2
  resultsT <- append(resultsT, t_bar)
  resultsYc1 = append(resultsYc1, yc1)
  resultsYc1_quad = append(resultsYc1_quad, yc1_quad)
  t=t+1
}

print(resultsT)
print(resultsYc1)
print(resultsYc1_quad)


#D - Renda Var e Esp

espD = sum(resultsYc1 * 1/9) #Esperanca
esp_quadD = sum((resultsYc1^2) * 1/9)

varD = esp_quadD - espD^2 #Variancia

######## c)
dados <- I(list(c(0,1),c(1,2,2,3),c(3,3,4,4,5,5)))
conglo23 = unlist(dados[2:3])

pares = as.data.frame(t(combn(conglo23,2)))

cor(pares$V1,pares$V2)

pc2 = (sigma2eq - (sigma2dc/(b_bar-1)))/(sigma2eq+sigma2dc)




#################################### 7.8


N = 2000
A = 200
B = 10
a = 20
b_bar = 200/20
Ta = c(5,3,2,9,3,1,6,10,4,4,2,3,6,1,1,7,0,7,2,1)

T_bar = sum(Ta)/a

var_T = (A^2/a) * (1/(a-1)) * sum((Ta-T_bar)^2) 

pc1 = T_bar/b_bar

var_pc1 = 1/(a*(a-1)*b_bar^2)* sum((Ta-pc1*B)^2)

s2ec = 1/((a-1)*b_bar^2) * sum((Ta-pc1*B)^2)
#s2dc = 1/ * sum(B/b_bar*) #falta o alpha como fazer?


rint = (s2ec - (s2dc/(b_bar-1)))/(s2ec+s2dc)




####################7.11

dados = D = c(c(0,1),c(1,2,2,3),c(3,3,4,4,5,5))

N = 12
A = 3
mean(D)
var(D) #S2
var(D)* (N - 1) / N #sigma2

















