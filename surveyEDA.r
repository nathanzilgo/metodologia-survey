# Universidade Federal de Campina Grande
# Aluno: Nathan Fernandes Pedroza
# Disciplina: Metodologia Científica
# Data: 18/08/2021
# Survey a respeito da eficácia da plataforma Stack Oveflow

# Hipótese a ser testada:
# Testar a qualidade e eficiência do Stack Overflow na aprendizagem e solução 
# de problemas para estudantes e profissionais de computação.

library(e1071)
library(utils)
library(skimr)
library(corrplot)

df <- read.csv("./survey.csv")

print("Sumario do Survey:")
summary(df)

print("Mais informações com o pacote skimr:")
skim(df)

print("Agrupando por alunos que trabalham ou não:")
df %>%
  dplyr::group_by(trabalha) %>%
  skim()
print("Com isso, é possível notar que a média da frequencia de utilização das pessoas
      que trabalham é de 4.14, enquanto que os que não estão, é de 3.89")
print("Pessoas que trabalham aprendem mais com o Stack Overflow (3.95) do que
      as que não trabalham (3.58)")

print("Agrupando por alunos que atuam em projetos academicos ou não:")    
df %>%
  dplyr::group_by(projeto) %>%
  skim()
print("Com isso, é possível notar que a média da frequencia de utilização das pessoas
      que estão em projeto é de 4.54, enquanto que os que não estão, é de 3.78")
print("Não é notável uma diferença substancial quanto ao aprendizado dos usuários do
      StOv que estão ou não em projetos")

par(mfrow=c(2,3.5))

barplot(table(df$idades), main = "dist idades")
barplot(table(df$periodo), main = "dist periodo")
barplot(table(df$aprender), main = "dist aprender")
barplot(table(df$experiencia), main = "dist experiencia")
barplot(table(df$utiliza), main = "dist utiliza")
barplot(table(df$experiencia), main = "dist experiencia")

pie(table(df$trabalha), main = "dist trabalha", radius = 1) 
pie(table(df$projeto), main = "dist projeto", radius = 1)

print("Com os gráficos e o summary, é possível notar que a maioria dos alunos no survey
      tem uma boa experiencia (media de 4.12), a plataforma os incentiva a aprender
      (media de 3.78) e a utilizam com uma ótima frequência (média de 4.03)")

# Medida de simetria para verificar se a média dos dados é menor que a mediana:
print("Skewness documentacao:")
skewness(table(df$documentacao))

print("Skewness experiencia:")
skewness(table(df$experiencia))

cor(table(df$aprender), table(df$utiliza), method="pearson")
cor(table(df$niveis), table(df$experiencia), method="pearson")
cor(table(df$aprender), table(df$documentacao), method="pearson")
