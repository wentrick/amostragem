pacman::p_load(tidyverse,utils,RcppAlgos)


teste = combn(1:3,2,simplify = F)


comboGeneral(1:3,2,repetition = T)















renda = c(12,30,18)
ntrab = c(1,3,2)
results = c()
amostra = c(1,1, 1,2, 1,3, 2,1, 2,2, 2,3, 3,1, 3,2, 3,3)

for (i in c(1:(length(amostra)/2))) {
  r = (renda[i]+renda[i+1])/(ntrab[i]+ntrab[i+1])
  results = append(r)
}


renda <- c(12, 30, 18)
ntrab <- c(1, 3, 2)
results <- c()
amostra <- c(1, 1, 1, 2, 1, 3, 2, 1, 2, 2, 2, 3, 3, 1, 3, 2, 3, 3)

for (i in seq(1, length(amostra)/2)) {
  r <- (renda[amostra[i]] + renda[amostra[i+1]]) / (ntrab[amostra[i]] + ntrab[amostra[i+1]])
  results <- append(results, r)
}

print(results)
