
library("FactoMineR")
library("factoextra")
library(ggplot2)
library(Factoshiny)
library(dplyr)
library(tidyr)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

theme = theme_bw() +
  theme(
    text = element_text(size=11, family="serif"),
    axis.text=element_text(size=11, family="serif"),
    axis.title=element_text(size=11, family="serif"),    
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

setwd("~/Doutorado/Tese/aceitação tecnologia/acm/")

respostas <- read.csv("acm.csv")

# -----------------------------------------------

respostas$Graduação <- as.factor(respostas$Graduação)
respostas$Pós.GraduaçãoShort <- sapply(respostas$Pós.Graduação, function(x) substrRight(x,3) )
respostas$Pós.GraduaçãoShort <- as.factor(respostas$Pós.GraduaçãoShort)


respostas %>%
  count(Graduação, Pós.GraduaçãoShort) %>%
  complete(Graduação = unique(Graduação), Pós.GraduaçãoShort = unique(Pós.GraduaçãoShort), 
           fill = list(n = 0)) %>%
  ggplot(aes(factor(Graduação), n, fill = factor(Pós.GraduaçãoShort))) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(aes(label=n), size=3, family="serif",
    position = position_dodge(width = 1),
    vjust = -0.3
  ) +
  labs(x = "Graduação", y="Ocorrências", fill="Pós-Graduação") +
  theme +
  theme(axis.text=element_text(size=10, family="serif")) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10)) +
  scale_fill_brewer(palette="Paired")

ggsave("graficos/formação.png", width=16, height = 5, units = "cm", dpi = 300)

respostas <- respostas[,-8]

# ---------------------------------------------------

usos = union_all(
  union_all(
    union_all (
      data.frame(categoria = 'Software R', uso = respostas$UsoR),
      data.frame(categoria = 'Software\nestatístico', uso = respostas$UsoSoftEstatistico)
    ),
    union_all(
      data.frame(categoria = 'Planilhas\neletrônica', uso = respostas$UsoSoftPlanilha),
      data.frame(categoria = 'Ling. de\nprogramação', uso = respostas$UsoLingProgramacao)
    ),
  ),
  data.frame(categoria = 'Software\neducacional', uso = respostas$UsoSoftEducacional)
)

usos$uso <- sapply(usos$uso, function(x) substrRight(x,3) )

ggplot(usos, aes(x = categoria, fill=uso)) +
  geom_bar(position = 'dodge') +
  geom_text(stat='count', aes(y=after_stat(count), label=after_stat(count)), size=3, family="serif",
            position = position_dodge(width = 1),
            vjust = -0.2
  ) +
  labs(x = "Categoria do software", y="Ocorrências", fill="Uso") +
  theme +
  theme(axis.text=element_text(size=10, family="serif")) +
  scale_y_continuous(limits = c(0, 248), breaks = seq(0, 250, by = 50)) +
  scale_fill_brewer(palette="Paired")


ggsave("graficos/softwares.png", width=16, height = 5, units = "cm", dpi = 300)


# ---------------------------------------------------

respostas$Pós.Graduação <- as.factor(respostas$Pós.Graduação)
respostas$UsoR <- as.factor(respostas$UsoR)
respostas$UsoSoftEstatistico <- as.factor(respostas$UsoSoftEstatistico)
respostas$UsoSoftPlanilha <- as.factor(respostas$UsoSoftPlanilha)
respostas$UsoLingProgramacao <- as.factor(respostas$UsoLingProgramacao)
respostas$UsoSoftEducacional <- as.factor(respostas$UsoSoftEducacional)

summary(respostas)

res.mca <- MCA(respostas, ncp=5, graph = FALSE)

# Rodar a interface shiny para gerar o auto relatorio
MCAshiny(res.mca)

# percentuais de inércia explicados por cada dimensão do MCA
fviz_screeplot(
  res.mca, 
  ylim = c(0, 17),
  addlabels = TRUE, barfill = "#a6cee3",
  ggtheme = theme, font.family = "serif",
  main = "", xlab = "Dimensões", ylab = "Percentual da Variância Explicada"
)

# individuos e variaveis
fviz_mca_biplot(res.mca, repel = TRUE, ggtheme = theme)

# variaveis
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE, ggtheme = theme, font.family = "serif")

# valores variaveis
fviz_mca_var(
  res.mca, repel = TRUE, 
  ggtheme = theme, font.family = "serif",
  col.var = "red",
  title = "",
  labelsize = 3
)
ggsave("graficos/mapa-correspondência.png", width=16, height = 9, units = "cm", dpi = 300)

var <- get_mca_var(res.mca)
round(var$contrib,2)
