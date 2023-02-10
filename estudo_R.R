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
