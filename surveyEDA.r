library(e1071)
library(utils)
library(skimr)
# library(RCurl)

df <- read.csv("https://raw.githubusercontent.com/nathanzilgo/metodologia-survey/master/survey.csv")
# df <- data.frame(idades, periodo, trabalha, projeto, utiliza, aprender, problema,niveis,documentacao,contribui,experiencia)
# write.csv(df, file ="survey.csv", row.names=FALSE)

transform(table(df$idades), FreqReal = Freq/40, FreqAcum = cumsum(Freq), FreqRelAcum = cumsum(Freq)/40)
transform(table(df$periodo), FreqReal = Freq/40, FreqAcum = cumsum(Freq), FreqRelAcum = cumsum(Freq)/40)

print("Sumario do Survey:")
summary(df)

print("Mais informações com o pacote skimr:")
skim(df)

df %>%
  dplyr::group_by(df$trabalha)
  skim()
  
df %>%
  dplyr::group_by(df$projeto)
  skim()

par(mfrow=c(2,3))
table(df$trabalha)
barplot(table(df$idades), main = "dist idades")
barplot(table(df$periodo), main = "dist periodo")
barplot(table(df$trabalha), main = "dist trabalha")
barplot(table(df$projeto), main = "dist projeto")
pie(table(df$periodo), main = "dist periodo 2", radius = 1)

skewness(table(df$documentacao))  

cor(table(df$aprender), table(df$utiliza), method="pearson")
