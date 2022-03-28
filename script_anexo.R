# Script do artigo "LIDERANDO DO MEIO? DIMENSIONANDO A LIDERANÇA DAS POTÊNCIAS INTERMEDIÁRIAS NAS ORGANIZAÇÕES INTERNACIONAIS (1975-2017)"

library(tidyverse)
library(haven)
library(readr)

##### DONWLOAD E ARRUMANDO A BASE ####
# Fazendo o download das bases de dados
link <- "https://github.com/felipelirapaiva/PISI/blob/main/international_organizations.csv?raw=true"
download.file(link, "international_organizations.csv")
international_org <- read.csv("international_organizations.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

international_org <- international_org %>%
  mutate(Classificação = case_when(país == "Estados Unidos" ~ "1. Potência mundial",
                                   país == "França" ~ "1. Potência mundial",
                                   país == "Japão" ~ "1. Potência mundial",
                                   país == "Suécia" ~ "2. Potência média",
                                   país == "Egito" ~ "4. Potência subregional",
                                   país == "Brasil" ~ "3. Potência regional",
                                   país == "Nigéria" ~ "4. Potência subregional",
                                   país == "Índia" ~ "3. Potência regional",
                                   país == "Austrália" ~ "2. Potência média",
                                   país == "Senegal" ~ "5. Potência menor",
                                   país == "Canadá" ~ "1. Potência mundial",
                                   país == "Suíça" ~ "5. Potência menor",
                                   país == "Gana" ~ "5. Potência menor",
                                   país == "Reino Unido" ~ "1. Potência mundial",
                                   país == "Argélia" ~ "5. Potência menor",
                                   país == "Chile" ~ "5. Potência menor",
                                   país == "Dinamarca" ~ "2. Potência média",
                                   país == "Itália" ~ "1. Potência mundial",
                                   país == "Alemanha" ~ "1. Potência mundial",
                                   país == "Espanha" ~ "2. Potência média",
                                   país == "Filipinas" ~ "4. Potência subregional",
                                   país == "Líbano" ~ "5. Potência menor",
                                   país == "Portugal" ~ "5. Potência menor",
                                   país == "China" ~ "3. Potência regional",
                                   país == "Coreia do Sul" ~ "2. Potência média",
                                   país == "Arábia Saudita" ~ "3. Potência regional",
                                   país == "Argentina" ~ "3. Potência regional",
                                   país == "Paquistão" ~ "4. Potência subregional",
                                   país == "Quênia" ~ "5. Potência menor",
                                   país == "Bélgica" ~ "2. Potência média",
                                   país == "Fiji" ~ "5. Potência menor",
                                   país == "Áustria" ~ "5. Potência menor",
                                   país == "Finlândia" ~ "5. Potência menor",
                                   país == "Jordânia" ~ "5. Potência menor",
                                   país == "Nova Zelândia" ~ "5. Potência menor",
                                   país == "Sudão" ~ "5. Potência menor",
                                   país == "Tailândia" ~ "5. Potência menor",
                                   país == "Turquia" ~ "3. Potência regional",
                                   país == "África do Sul" ~ "3. Potência regional",
                                   país == "México" ~ "3. Potência regional",
                                   país == "Peru" ~ "5. Potência menor",
                                   país == "Sri Lanka" ~ "5. Potência menor",
                                   país == "Tanzânia" ~ "5. Potência menor",
                                   país == "Bulgária" ~ "5. Potência menor",
                                   país == "Grécia" ~ "5. Potência menor",
                                   país == "Holanda" ~ "2. Potência média",
                                   país == "Kuwait" ~ "5. Potência menor",
                                   país == "Mali" ~ "5. Potência menor",
                                   país == "Marrocos" ~ "5. Potência menor",
                                   país == "Serra Leoa" ~ "5. Potência menor",
                                   país == "Tunísia" ~ "5. Potência menor",
                                   país == "Irlanda" ~ "5. Potência menor",
                                   país == "Noruega" ~ "2. Potência média",
                                   país == "Rússia" ~ "3. Potência regional",
                                   país == "Camarões" ~ "5. Potência menor",
                                   país == "Equador" ~ "5. Potência menor",
                                   país == "Irã" ~ "4. Potência subregional",
                                   país == "El Salvador" ~ "5. Potência menor",
                                   país == "Etiópia" ~ "5. Potência menor",
                                   país == "Panamá" ~ "5. Potência menor",
                                   país == "Togo" ~ "5. Potência menor",
                                   país == "Uganda" ~ "5. Potência menor"))


#### APRESENTANDO OS RESULTADOS ####
# Criando um objeto para fazer o treemmap (figura 1)
fig1 <- international_org %>%
  group_by(ano, país, Classificação) %>%
  count()%>%
  filter(Classificação != "NA")
  


# Figura A1 - Distribuição de OIs lideradas por cada grupo e cada país
library(treemap)
library(RColorBrewer)
library(scales)
library(ggpubr)

treemap(fig1,
        index=c("Classificação","país"),
        vSize="n",
        type="index",
        palette = "Set1",
        title="",
        border.col=c("black","black"),
        border.lwds=c(1,1),
        fontsize.labels=c(10,9),
        fontface.labels=c(1,1),
        bg.labels=c("transparent"),
        overlap.labels=0,
        align.labels = list(
          c("left", "top"),
          c("center", "center")))


# Contagem para saber quantas OIs-ano cada grupo liderou
international_org %>%
  group_by(Classificação)%>%
  count()


## Tabela A1 – Concentração relativa de lideranças por grupo

# Um país pode liderar 0, 1, 2, ..., OIs. Para o cálculo do índice de concentração,
# consideraremos apenas liderando 1 ou mais OIs
# Mundiais (168); Intermediários (412); e Menores (329) [valores para o denominador]
# Para ver demais grupos, substituir: filter(Classificação == "NOME DO GRUPO")
tabela2 <- international_org %>%
  group_by(Classificação, ano, país) %>%
  count()%>%
  filter(Classificação == "1. Potência mundial")

# Número de OIs lideradas por ano por cada país em cada grupo
tabela2 <- international_org %>%
  group_by(Classificação, ano, país) %>%
  count()%>%
  rename(lideradas = n)

# Quantas vezes cada país do grupo liderou 1, 2, 3, 4 ou mais OIs.
tabela2 <- tabela2  %>%
  group_by(lideradas, Classificação)%>%
  count()


## Figura A2 - Número de OIs por classificação (1975-2017)

# Criando o objeto para a figura em 5 categorias
figs5 <-  international_org %>%
  group_by(ano, Classificação) %>% 
  count()%>%
  filter(Classificação != "NA") 

# Criando o objeto para a figura em 3 categorias
figs3  <- international_org %>%
  mutate(Classificação = case_when(Classificação == "1. Potência mundial" ~ "1. Potência mundial",
                                   Classificação == "2. Potência média" ~ "2. Potência intermediária",
                                   Classificação == "3. Potência regional" ~ "2. Potência intermediária",
                                   Classificação == "4. Potência subregional" ~ "2. Potência intermediária",
                                   Classificação == "5. Potência menor" ~ "3. Potência menor"))

figs3 <- figs3 %>%
  group_by(ano, Classificação) %>% 
  count()%>%
  filter(Classificação != "NA")

# Escolhendo o tema para as figuras
theme_set(theme_pubr())

# Figura 2 - Total de OIs lideradas por cada grupo em cada ano
# Primeiro, fazendo cada gráfico separadamente
fig2A <-  ggplot(figs5, aes(ano, n )) +
  geom_line(aes (group=Classificação, color=Classificação), lwd = 1.3) +
  scale_x_continuous(breaks=seq(1975,2015,5))+
  scale_y_continuous(breaks=seq(1,20,2))+
  labs(x = "", 
       y = "OIs")+
  geom_vline(xintercept = 1991, lwd = 1)+
  geom_vline(xintercept = 2001, lwd = 1)+
  geom_vline(xintercept = 2008, lwd = 1)+
  scale_colour_manual(values = c("#F73C3C", "#02A731","#1EE055","#8EEFAA","#F5F346"))

fig2B <-  ggplot(figs3, aes(ano, n )) +
  geom_line(aes (group=Classificação, color=Classificação), lwd = 1.3) +
  scale_colour_manual(values = c("#F73C3C","#0F6F2A","#F5F346"))+
  scale_x_continuous(breaks=seq(1975,2015,5))+
  scale_y_continuous(breaks = seq(1,17,2), limits = c(1,17))+
  labs(x = "", 
       y = "OIs")+
  geom_vline(xintercept = 1991, lwd = 1)+
  geom_vline(xintercept = 2001, lwd = 1)+
  geom_vline(xintercept = 2008, lwd = 1)+
  theme(legend.position = "none")

# Agora, juntando os dois gráficos para fazer a figura 2
figure <- ggarrange(fig2A, fig2B, ncol=1, nrow=2, common.legend = TRUE,
                    legend="bottom")
figure

