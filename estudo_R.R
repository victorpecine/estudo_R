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
