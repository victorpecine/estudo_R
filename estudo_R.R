dados <- read.csv('estatistica-r-parte-1-arquivos-projeto-inicial/dados.csv')

head(dados)

dist_freq_quali <- cbind(freq = table(dados$Sexo), porcent = prop.table((table(dados$Sexo)) * 100))
dist_freq_quali

colnames(dist_freq_quali) <- c('frequencia', 'porcentagem')

rownames(dist_freq_quali) <- c('masculino', 'feminino')

dist_freq_quali

frequencia <- table(dados$Sexo, dados$Cor)
frequencia

rownames(frequencia) <- c('masculino', 'feminino')

colnames(frequencia) <- c('indigena', 'branca', 'preta', 'amarela', 'parda')

frequencia

dist_freq_quali_percent <- prop.table(frequencia) * 100

dist_freq_quali_percent <- round(dist_freq_quali_percent, digits=2)

dist_freq_quali_percent

renda_media <- tapply(dados$Renda, list(dados$Sexo, dados$Cor), mean)

rownames(renda_media) <- c('masculino', 'feminino')
colnames(renda_media) <- c('indigena', 'branca', 'preta', 'amarela', 'parda')

renda_media

max(dados$Renda)

classes <- c(0, 2900, 7100, 22000, 200000)
labels <- c('D', 'C', 'B', 'A')


frequencia <- table(
  cut(
    x = dados$Renda,
    breaks = classes,
    labels = labels,
    include.lowest = TRUE
    )
  )

frequencia

porcent_classe <- prop.table(frequencia) * 100
porcent_classe

dist_freq_quanti <- cbind('frequencia' = frequencia, 'porcentagem' = porcent_classe)

dist_freq_quanti <- dist_freq_quanti[order(row.names(dist_freq_quanti)),]

dist_freq_quanti


classes_altura <- c(min(dados$Altura), 1.65, 1.75, max(dados$Altura))

labels_altura <- c('1 - Baixa', '2 - Média', '3 - Alta')

frequencia <- table(
  cut(
    x = dados$Altura,
    breaks = classes_altura,
    labels = labels_altura,
    include.lowest = TRUE
    )
  )

percentual <- prop.table(frequencia) * 100

dist_freq_altura <- cbind('frequencia' = frequencia, 'porcentagem' = percentual)

dist_freq_altura[
  order(row.names(dist_freq_altura)),
]


# Definição do número de classes
# Regra de Sturges

n <- nrow(dados)

k <- 1 + (10 / 3) * log10(n)

k <- floor(k)
k

frequencia <- table(
  cut(
    x = dados$Renda,
    breaks = k,
    include.lowest = TRUE
    )
  )

frequencia <- cbind(frequencia)
frequencia

percentual <- prop.table(frequencia) * 100
percentual

dist_freq_amplitude_fixa <- cbind('frequencia' = frequencia, 'porcentagem' = porcent_classe)

dist_freq_amplitude_fixa <- dist_freq_amplitude_fixa[order(row.names(dist_freq_amplitude_fixa)),]

dist_freq_amplitude_fixa

options(repr.plot.width=18, repr.plot.height=8)

hist(dados$Altura)

hist(x=dados$Altura,
     breaks='sturges',
     col='lightgreen',
     main='Histograma das alturas',
     xlab='Altura',
     ylab='Frequência',
     ylim=c(0, 20000)
)


install.packages('ggplot2')
library(ggplot2)

ggplot(data = dados, aes(x = Altura)) +
  geom_histogram(binwidth = 0.02,
                 color = 'black',
                 alpha = 0.3) +
  ylab('Frequência') +
  xlab('Altura') +
  ggtitle('Histograma das alturas') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title.y = element_text(size = 12, vjust = +0.2),
        axis.title.x = element_text(size = 12, vjust = -0.2),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)
        )

ggplot(dados, aes(x = Altura, y = after_stat(density))) +
  geom_histogram(binwidth = 0.02,
                 color = 'black',
                 alpha = 0.8,
                 fill = 'lightgreen') +
  geom_density(color='black', linewidth = 1) +
  ylab('Frequência') +
  xlab('Altura') +
  ggtitle('Histograma das alturas') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title.y = element_text(size = 12, vjust = +0.2),
        axis.title.x = element_text(size = 12, vjust = -0.2),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)
  )


# QUartis
quantile(dados$Renda, c(0.25, 0.5, 0.75))

# Decis
decis <- c()
for (i in 1:9){
  decis <- c(decis, i / 10)
}
decis

quantile(dados$Renda, decis)

# Centis
centis <- c()
for (i in 1:99){
  centis <- c(centis, i / 100)
}
centis

quantile(dados$Renda, centis)


# Distribuição de frequência acumulada
ggplot(data = dados, aes(x = Idade)) +
  geom_histogram(
    aes(y = cumsum(..count..) / sum(..count..)),
    bins = 10,
  ) +
  geom_freqpoly(
    aes(y = cumsum(..count..) / sum (..count..)),
    color = 'lightblue',
    linewidth = 1
  )


# Boxplot
sexo <- c('masculino', 'feminino')

cor <- c('indigena', 'branca', 'preta', 'amarela', 'parda')

anos_de_estudo <- c('sem instrução/menos de 1 anos',
                    '1 ano',
                    '2 anos',
                    '3 anos',
                    '4 anos',
                    '5 anos',
                    '6 anos',
                    '7 anos',
                    '8 anos',
                    '9 anos',
                    '10 anos',
                    '11 anos',
                    '12 anos',
                    '13 anos',
                    '14 anos',
                    '15 anos ou mais',
                    'não determinado')

ggplot(dados, aes(x = '', y = Altura)) +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  geom_boxplot(fill = 'lightblue') +
  coord_flip() +
  ylab('Metros') +
  xlab('') +
  ggtitle('Boxplot alturas')


dados$Cat.Sexo <- factor(dados$Sexo) # Cria variável categórica
levels(dados$Cat.Sexo) <- sexo # Níveis (labels) das variáveis categóricas

head(dados)

ggplot(data = dados, aes(x = Cat.Sexo,
                         y = Altura,
                         group = Sexo)) +
  stat_boxplot(geom = 'errorbar', width = 0.4) +
  geom_boxplot(fill = c('lightblue', 'orange')) +
  coord_flip() +
  ylab('Metros') +
  xlab('Sexo') +
  ggtitle('Boxplot alturas x sexo')


dados$Cat.UF <- factor(dados$UF) 

ggplot( 
  data = dados[(dados$UF == 29 | dados$UF == 35) & dados$Renda < 10000, ],  
  aes(y = Renda, x = Cat.UF) 
) +  
  stat_boxplot(geom ='errorbar', width = 0.4) +  
  geom_boxplot(fill = c('#3274A1', "orange")) +  
  coord_flip() + 
  ylab("R$") +  
  xlab("UF") +  
  ggtitle('Renda (R$) - Bahia X São Paulo') + 
  theme( 
    plot.title = element_text(size = 14, hjust = 0.5), 
    axis.title.y = element_text(size = 12, vjust = +0.2), 
    axis.title.x = element_text(size = 12, vjust = -0.2), 
    axis.text.y = element_text(size = 10), 
    axis.text.x = element_text(size = 10) 
  )


# Tabela normal padronizada
Z <- seq(0, 3.99, by = 0.01)

probabilidade <- pnorm(Z)

tabela_norm_padron <- matrix(probabilidade, ncol = 10, byrow = TRUE)

colnames(tabela_norm_padron) <- format(seq(0.00, 0.09, by = 0.01))
rownames(tabela_norm_padron) <- format(seq(0.00, 3.9, by = 0.1), digits = 2, nsmall = 2)

tabela_norm_padron


# O faturamento diário de um motorista de aplicativo segue uma distribuição
# aproximadamente normal, com média R$ 300,00 e desvio padrão igual a R$ 50,00.
# Obtenha as probabilidades de que, em um dia aleatório, o motorista ganhe:
# Entre R$ 250,00 e R$ 350,00
# Entre R$ 400,00 e R$ 500,00

# 1. entre R$ 250,00 e R$ 350,00
media <- 300
desvio_padrao <- 50
Z_inferior = (250 - media) / desvio_padrao
Z_superior = (350 - media) / desvio_padrao

probabilidade <- pnorm(Z_superior) - pnorm(Z_inferior)
round(probabilidade, 4)

# 2. entre R$ 400,00 e R$ 500,00
media <- 300
desvio_padrao <- 50
Z_inferior = (400 - media) / desvio_padrao
Z_superior = (500 - media) / desvio_padrao

probabilidade <- pnorm(Z_superior) - pnorm(Z_inferior)
round(probabilidade, 4)



# Os pesos dos lutadores de uma academia de MMA, com 500 atletas, são normalmente
# distribuídos, com média igual a 80,5 kg e desvio padrão igual a 12,2 kg.
# Encontre o número de atletas dessa academia que se enquadram na categoria peso leve,
# que vai de 65,7 kg até 70,3 kg.

n <- 500
media <- 80.5
desvio_padrao <- 12.2
Z_inferior <- (65.7 - media) / desvio_padrao
Z_superior <- (70.3 - media) / desvio_padrao

probabilidade <- pnorm(Z_superior) - pnorm(Z_inferior)

n_atletas <- probabilidade * n
ceiling(n_atletas)



# O Inmetro verificou que as lâmpadas incandescentes da fabricante XPTO apresentam uma
# vida útil normalmente distribuída, com média igual a 720 dias e desvio padrão igual a
# 30 dias. Calcule a probabilidade de uma lâmpada escolhida ao acaso durar:
# Entre 650 e 750 dias
# Mais que 800 dias
# Menos que 700 dias

# 1.
media <- 720
desvio_padrao <- 30
Z_inferior <- (650 - media) / desvio_padrao
Z_superior <- (750- media) / desvio_padrao

probabilidade <- pnorm(Z_superior) - pnorm(Z_inferior)
round(probabilidade, 4)

# 2.
media <- 720
desvio_padrao <- 30
Z_superior <- (800- media) / desvio_padrao

probabilidade <- pnorm(Z_superior)
round(probabilidade, 4)

# 3.
media <- 720
desvio_padrao <- 30
Z_inferior <- (700 - media) / desvio_padrao

probabilidade <- pnorm(Z_inferior)
round(probabilidade, 4)



# Teste de normalidade
significancia <- 0.05

set.seed(2811)
amostra <- sample_n(dados, 5000)
resultado <- shapiro.test(amostra$Renda)
p_valor <- resultado$p.value

if (p_valor <= significancia){
  cat('Hipótese nula rejeitada\nA amostra não é proveniente de uma distribuição normal')
} else{
  cat('Hipótese nula aceita\nA amostra é proveniente de uma distribuição normal')
}



# Correlação

# Y = Gasto da família
# X = Renda da família
dataset <- data.frame(
  Y = c(3011, 1305, 1879, 2654, 2849, 1068, 2892, 2543, 3074, 849, 2184, 2943, 1357, 2755, 2163, 3099, 1600, 353, 1778, 740, 2129, 3302, 2412, 2683, 2515, 2395, 2292, 1000, 600, 1864, 3027, 1978, 2791, 1982, 900, 1964, 1247, 3067, 700, 1500, 3110, 2644, 1378, 2601, 501, 1292, 2125, 1431, 2260, 1770),
  X = c(9714, 3728, 6062, 8845, 8378, 3338, 8507, 7947, 9915, 1632, 6825, 8918, 4100, 9184, 6180, 9997, 4500, 1069, 5925, 2466, 6083, 9712, 7780, 8383, 7185, 7483, 7640, 2100, 2000, 6012, 8902, 5345, 8210, 5662, 2700, 6546, 2900, 9894, 1500, 5000, 8885, 8813, 3446, 7881, 1164, 3401, 6641, 3329, 6648, 4800)
)

head(dataset)

summary(dataset)

apply(dataset, 2, sd)

install.packages('ggplot2')
library(ggplot2)

stack(dataset) # Empilhamento das infos do dataset

# Boxplot
ggplot(stack(dataset), aes(x = ind, y = values)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  xlab("Variáveis") + 
  ylab("Reais (R$)") + 
  ggtitle('Box-plot') +
  theme(
    plot.title=element_text(size = 14, hjust = 0.5),
    axis.title.y=element_text(size = 12, vjust = +0.2),
    axis.title.x=element_text(size = 12, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

# Dispersão
ggplot(data = dataset, aes(x = X, y = Y)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_smooth(method = lm) +
  xlab("Renda das Famílias") + 
  ylab("Gasto das Famílias") + 
  ggtitle('Reta de Regressão - Gasto X Renda') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

cor(dataset) # Correlação

# Covariância
amostra <- data.frame(
  Idade = c(39, 29, 21, 49, 29, 34, 32, 32, 24, 53, 28, 28, 46, 58, 41, 43, 31, 55, 52, 54),
  Renda = c(1500, 1000, 3500, 1570, 600, 1200, 2000, 500, 1300, 600, 1500, 3000, 0, 550, 1500, 1600, 746, 1000, 0, 1400),
  Anos.de.Estudo = c(6, 7, 12, 13, 9, 12, 12, 6, 7, 5, 7, 16, 12, 3, 12, 9, 1, 6, 1, 6),
  Altura = c(1.6162, 1.7525, 1.6940, 1.8041, 1.7854, 1.7468, 1.6633, 1.6937, 1.6569, 1.6671, 1.6786, 1.6730, 1.7853, 1.6090, 1.7833, 1.6709, 1.6392, 1.6861, 1.7107, 1.7288)
)

head(amostra)

matrix_cov <- cov(amostra)
matrix_cov


# Regressão linear
regressao <- lm(formula = Y ~ X,
                data = dataset)
regressao

beta_1 <- regressao$coefficients[1]
beta_1

beta_2 <- regressao$coefficients[2]
beta_2

resumo <- summary(regressao)
resumo

# Previsões dentro da amosta
dataset['Y_pred'] <- regressao$fitted.values
dataset


pred = predict(regressao, data.frame(X = c(7510, 7550)))
pred


# Resíduos
dataset['residuos'] <- regressao$residuals
head(dataset)
