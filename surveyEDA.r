library(e1071)
library(utils)

idades = c(22,20,21,21,20,21,21,20,20,21,21,19,19,20,28,22,21,19,22,20,20,20,24,19,18,21,19,22,28,24,23,22,21,22,18,23,19,19,20,20)
periodo = c('2018.1', '2018.1', '2018.1', '2018.1', '2019.2', '2018.1', '2018.1', '2018.1', '2018.1', '2018.1', '2018.2', '2019.2', '2019.2', '2019.1', '2019.2', '2019.1', '2018.1', '2020.1', '2018.1', '2020.1', '2020.1', '2019.1', '2018.1', '2020.1', '2020.1', '2017.1', '2019.1', '2018.1', '2016.1 ou antes', '2018.1', '2017.1', '2016.2', '2020.1', '2019.1', '2020.1', '2016.2', '2020.1', '2020.1', '2020.2', '2018.2')
trabalha = c('Não', 'Sim', 'Não', 'Sim', 'Sim', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Sim', 'Sim', 'Não', 'Sim', 'Não', 'Sim', 'Não', 'Sim', 'Sim', 'Não', 'Sim', 'Sim', 'Não', 'Sim', 'Não', 'Sim', 'Não', 'Sim', 'Não', 'Sim', 'Sim', 'Sim', 'Não', 'Não', 'Sim', 'Sim', 'Não', 'Sim', 'Sim')
projeto = c('Sim', 'Não', 'Não', 'Não', 'Não', 'Não', 'Sim', 'Sim', 'Sim', 'Sim', 'Não', 'Sim', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Sim', 'Não', 'Não', 'Não', 'Não', 'Sim', 'Sim', 'Sim', 'Sim', 'Sim', 'Não', 'Sim', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não', 'Não')
utiliza = c(5,5,3,3,4,2,4,5,4,5,3,4,4,5,4,5,5,5,5,1,3,5,5,5,5,1,5,5,4,4,4,5,5,5,2,4,1,3,5,4)
aprender = c(4,3,2,5,1,3,3,2,3,4,3,3,4,4,5,4,5,4,4,5,4,5,4,5,5,1,3,5,3,5,4,4,5,4,4,2,4,4,4,5)
problema = c(3,1,1,1,1,1,2,1,2,2,2,1,1,3,3,1,4,2,1,3,1,2,4,1,2,1,2,1,4,4,3,2,4,1,1,1,2,2,2,1)
niveis = c(2,4,4,5,4,5,3,3,4,5,3,4,5,3,5,3,3,4,5,5,4,5,2,5,4,1,5,2,2,2,5,5,3,4,3,5,2,3,5,3)
documentacao = c(3,2,5,5,3,2,3,4,2,3,2,4,2,4,3,4,5,5,5,1,2,5,3,5,5,1,4,5,3,5,3,4,2,3,3,2,2,2,5,2)
contribui = c(1,2,1,1,1,1,2,1,2,1,1,2,1,1,3,1,4,1,2,1,1,1,1,4,1,1,2,1,1,1,2,1,4,1,1,1,1,1,1,5)
experiencia = c(5,5,4,5,3,4,4,5,4,4,4,4,4,5,4,4,2,5,5,3,4,5,4,5,5,1,5,4,4,4,4,5,4,4,3,4,4,4,4,5)


# data = cbind(idades, periodo, trabalha, projeto, utiliza, aprender, problema,niveis,documentacao,contribui,experiencia)
df <- data.frame(idades, periodo, trabalha, projeto, utiliza, aprender, problema,niveis,documentacao,contribui,experiencia)
write.csv(df, file ="survey.csv", row.names=FALSE)

transform(table(idades), FreqReal = Freq/40, FreqAcum = cumsum(Freq), FreqRelAcum = cumsum(Freq)/40)
transform(table(periodo), FreqReal = Freq/40, FreqAcum = cumsum(Freq), FreqRelAcum = cumsum(Freq)/40)

par(mfrow=c(2,3))
table(trabalha)
barplot(table(idades), main = "dist idades")
barplot(table(periodo), main = "dist periodo")
barplot(table(trabalha), main = "dist trabalha")
barplot(table(projeto), main = "dist projeto")
pie(table(periodo))
skewness(table(documentacao))  

cor(table(df$aprender), table(df$utiliza), method="pearson")

