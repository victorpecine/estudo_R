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


renda_media <- tapply((vector), index, function)