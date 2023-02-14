install.packages('Ecdat')

library(Ecdat)

data(Airq) # Carrega banco de dados do Ecdat

names(Airq)

# airq: índice de qualidade do ar (quanto menor, melhor)
# vala: valor das empresas nas cidades (milhares de dólares)
# rain: quantidade de chuvas (em polegadas)
# coas: posição costeira da cidade (sim ou não)
# dens: densidade populacional (milha²)
# medi: renda média per capita (dólares)

# Quais as variáveis afetam a qualidade do ar na cidade?

# Análise descritiva
summary(Airq)

plot(airq ~ vala, data = Airq)
plot(airq ~ coas, data = Airq)
plot(airq ~ rain, data = Airq)
plot(airq ~ dens, data = Airq)
plot(airq ~ medi, data = Airq)

# Modelo estatístico
# y ~ x -> y em função de x
# p_valor indica a sginificância do modelo ou da variável
# Se p_valor < significancia -> p_valor é considerado significativo

significancia <- 0.05


# Regressão linear
reg_lin_vala <- lm(data = Airq, airq ~ vala)

p_valor_vala <- summary(reg_lin_vala)$coefficients[,4][2]

if (p_valor_vala <= significancia) {
  cat('A variável VALA é significativa')
} else {
  cat('A variável VALA não é significativa')
}


reg_lin_coas <- lm(data = Airq, airq ~ coas)

p_valor_coas <- summary(reg_lin_coas)$coefficients[,4][2]

if (p_valor_coas <= significancia) {
  cat('A variável COAS é significativa')
} else {
  cat('A variável COAS não é significativa')
}


reg_lin_rain <- lm(data = Airq, airq ~ rain)

p_valor_rain <- summary(reg_lin_rain)$coefficients[,4][2]

if (p_valor_rain <= significancia) {
  cat('A variável RAIN é significativa')
} else {
  cat('A variável RAIN não é significativa')
}


reg_lin_dens <- lm(data = Airq, airq ~ dens)

p_valor_dens <- summary(reg_lin_dens)$coefficients[,4][2]

if (p_valor_dens <= significancia) {
  cat('A variável DENS é significativa')
} else {
  cat('A variável DENS não é significativa')
}


reg_lin_medi <- lm(data = Airq, airq ~ medi)

p_valor_medi <- summary(reg_lin_medi)$coefficients[,4][2]

if (p_valor_medi <= significancia) {
  cat('A variável MEDI é significativa')
} else {
  cat('A variável MEDI não é significativa')
}