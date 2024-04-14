pacman::p_load(tidyverse,utils,RcppAlgos,gtools,matrixStats)

N = 6
d = c(1,2,3,4,5,6)
D=c(1,4,5,5,6,6)

n = 2


dados = as.data.frame(permutations(N,n,d, repeats.allowed = FALSE)) %>%
  mutate(var1 = D[V1],
         var2 = D[V2])
  

dados = dados  %>%
  rowwise() %>%
  mutate(bar_y = mean(c(var1,var2)),
         s2 = var(c(var1,var2)))


dados = dados %>%
  mutate(p = 1/30,
         esp_bar_y = bar_y*p,
         esp_bar_y_quad = (bar_y^2)*p,
         esp_s2 = s2*p) %>%
  rowwise() %>%
  mutate(bar_yc = ifelse( 1 %in% c(V1,V2) & ! 6 %in% c(V1,V2), bar_y + 1,
                          ifelse(6 %in% c(V1,V2) & !1 %in% c(V1,V2), bar_y - 1, bar_y))) %>%
  ungroup() %>%
  mutate(esp_bar_yc = bar_yc*p,
         esp_bar_yc_quad = (bar_y^2)*p)
  


sigma2 = var(D)
mu = mean(D)
esp_bar_y = sum(dados$esp_bar_y)
esp_bar_y2 = sum(dados$esp_bar_y_quad)
esp_bar_yc = sum(dados$esp_bar_yc)
esp_bar_yc2 = sum(dados$esp_bar_yc_quad)
esp_s2 = sum(dados$esp_s2)

var_bar_y = esp_bar_y2 - esp_bar_y^2
var_bar_yc = esp_bar_yc2 - esp_bar_yc^2
