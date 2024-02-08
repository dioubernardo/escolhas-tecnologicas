library(ggplot2)
library(tidyverse)

theme = theme_bw() + theme(
  text = element_text(size=11, family="serif"),
  axis.text=element_text(size=11, family="serif"),
  axis.title=element_text(size=11, family="serif"),
  plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
)

setwd("~/Doutorado/Tese/aceitação tecnologia/acm/")

amostra <- read.csv("dados.csv")

amostra$Idade <- as.numeric(amostra$Idade)
idades <- data.frame(amostra$Sexo, amostra$Idade)
colnames(idades) <- c("Sexo", "Idade")
idades$Sexo <- factor(idades$Sexo, levels=c("Feminino", "Masculino"))

summary(filter(idades, Sexo == "Feminino"))
summary(filter(idades, Sexo == "Masculino"))
summary(idades)

idades = union_all(
  idades,
  data.frame(Sexo = 'Geral', Idade = amostra$Idade)
)

idades$Sexo <- factor(idades$Sexo, levels=c("Geral", "Masculino", "Feminino"))

ggplot(idades, aes(x=Sexo, y=Idade, fill=Sexo)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.1, alpha=0.5, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape=4, col = "red") +
  ylab("Idades")+
  coord_flip() +
  theme + 
  theme(legend.position="none") + 
  scale_fill_brewer(palette="Paired")
  
ggsave("graficos/sexo-idades.png", width=16, height = 5, units = "cm", dpi = 300)

#--------------------------------

amostra$TempoDocencia <- as.numeric(amostra$TempoDocencia)
tempos <- data.frame(amostra$Sexo, amostra$TempoDocencia)
colnames(tempos) <- c("Sexo", "TempoDocencia")
tempos$Sexo <- factor(tempos$Sexo, levels=c("Feminino", "Masculino"))

summary(filter(tempos, Sexo == "Feminino"))
summary(filter(tempos, Sexo == "Masculino"))
summary(tempos)

tempos = union_all(
  tempos,
  data.frame(Sexo = 'Geral', TempoDocencia = amostra$TempoDocencia)
)

tempos$Sexo <- factor(tempos$Sexo, levels=c("Geral", "Masculino", "Feminino"))

ggplot(tempos, aes(x=Sexo, y=TempoDocencia, fill=Sexo)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.1, alpha=0.5, width = 0.2) +
  stat_summary(fun = mean, geom = "point", shape=4, col = "red") +
  ylab("Anos de docência")+
  coord_flip() +
  theme + 
  theme(legend.position="none") +
  scale_fill_brewer(palette="Paired")

ggsave("graficos/sexo-tempo-de-docencia.png", width=16, height = 5, units = "cm", dpi = 300)

# ------------------------------

# teste de Normalidade
library("RVAideMemoire")
byf.shapiro(Idade ~ Sexo, amostra)
byf.shapiro(TempoDocencia ~ Sexo, amostra)

# se p-valor > 0,05 
#   não rejeitar hipotese nula
#   então dados com distribuição normal

# Nota: Nem Idade nem tempo de Docência tem distribuição normal

# ------------------------------

# no caso de dados não normais rodar testes não parametricos, 
# como Teste de Mann-Whitney
# H0 -> Os grupos NÃO são estatisticamente diferentes p-valor >  0,05
# H1 -> Os  grupos são estatisticamente diferentes    p-valor <= 0,05

library(stats)
wilcox.test(Idade ~ Sexo, amostra)
wilcox.test(TempoDocencia ~ Sexo, amostra)
