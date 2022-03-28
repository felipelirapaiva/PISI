# Script do artigo "LIDERANDO DO MEIO? DIMENSIONANDO A LIDERANÇA DAS POTÊNCIAS INTERMEDIÁRIAS NAS ORGANIZAÇÕES INTERNACIONAIS (1975-2017)"

library(tidyverse)
library(haven)
library(readr)

##### DONWLOAD E ARRUMANDO A BASE #####
# Fazendo o download das bases de dados
link <- "https://github.com/felipelirapaiva/PISI/blob/main/international_organizations.csv?raw=true"
download.file(link, "international_organizations.csv")
international_org <- read.csv("international_organizations.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

link <- "https://github.com/felipelirapaiva/PISI/blob/main/WPI.csv?raw=true"
download.file(link, "WPI.csv")
WPI <- read.csv("WPI.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

# Transformando o WPI num banco longo:
library(tidyr)
WPI <- WPI %>%
  pivot_longer(
    `1975`:`2017`,
    names_to = "ano")

# Transformando "ano" em variável numérica.
WPI$ano <- as.numeric(as.character(WPI$ano))

# Renomeando as variáveis
WPI <- WPI %>% 
  rename(poder = value,
         país = WPI)

# Filtrando os países que lidararam OI
WPI <- WPI %>%
  filter(país %in% c("Estados Unidos","França","Japão","Suécia",
                     "Egito","Brasil","Nigéria","Índia","Austrália","Senegal",
                     "Canadá","Suíça","Gana","Reino Unido", "Argélia","Chile",
                     "Dinamarca","Itália","Alemanha","Espanha","Filipinas",
                     "Líbano","Portugal","China","Coreia do Sul","Arábia Saudita",
                     "Argentina","Paquistão","Quênia","Bélgica","Fiji","Áustria",
                     "Finlândia","Jordânia","Nova Zelândia","Sudão","Tailândia",
                     "Turquia","África do Sul","México","Peru","Sri Lanka",
                     "Tanzânia","Bulgária","Grécia","Holanda","Kuwait","Mali",
                     "Marrocos","Serra Leoa","Tunísia","Irlanda","Noruega",
                     "Rússia","Camarões","Equador","Irã","El Salvador", "Togo",
                     "Uganda", "Panamá", "Etiópia"))

# Aplicando a classificação do WPI aos países:
# Mundial (>820), Intermediária (819-600) Menor (<599)
WPI <-  WPI %>%
  mutate(Classificação = case_when(poder < 600 ~ "3. Potência menor",
                                   poder >= 600 & poder < 820 ~ "2. Potência intermediária",
                                   poder >= 820 ~ "1. Potência mundial"))

##### 3. METODOLOGIA #####
## Quadro 2 - Classificação das potências que lideraram ao menos uma OI entre 1975-2017
quadro2 <- WPI %>%
  group_by(Classificação, país)%>%
  count()

## Figura 1 - Potências que mudaram de classificação (1975-2017)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

figura1 <- WPI %>%
  filter(país %in% c("Reino Unido", "Canadá", "China", "Coreia do Sul",
                     "Irã", "Irlanda", "Itália", "Kuwait", "Portugal",
                     "Rússia", "Turquia", "Chile", "Tailândia"))

ggplot(figura1, aes(ano, poder))+
  geom_line(aes (group=país, color=Classificação), lwd = 1)+
  scale_colour_manual(values = c("#F73C3C","#0F6F2A","#F5F346"))+
  facet_wrap(~país, ncol = 3)+
  theme_pubr()+
  scale_x_continuous(breaks=seq(1975,2017,10))+
  scale_y_continuous(breaks=c(600,820))+
  geom_vline(xintercept = 1991, lwd = 0.3, linetype=1)+
  geom_vline(xintercept = 2001, lwd = 0.3, linetype=1)+
  geom_vline(xintercept = 2008, lwd = 0.3, linetype=1)+
  geom_hline(yintercept = 820, lwd = 0.3, linetype=3)+
  geom_hline(yintercept = 600, lwd = 0.3, linetype=3)+
  xlab("Ano") + ylab("WPI")+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 13),
    axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

##### 4. RESULTADOS #####

## Tabela 1 - Número de organizações-ano que cada país liderou (1975-2017)
tabela1 <- international_org %>%
  group_by(país)%>%
  count()

## Mergindo os bancos de dados para os resultados posteriores
library(scales)
PISI <- left_join(international_org, WPI,
                  by = c("país" = "país",
                         "ano" = "ano"))

## Retirando os casos ausentes do WPI (Líbano 1976-1988)
PISI <- PISI %>%
  filter(Classificação != "NA")

## Figura 2 - Distribuição de OIs lideradas por cada grupo e cada país
library(treemap)

# Criando um objeto para o treemap
figura2 <- PISI %>%
  group_by(ano, país, Classificação) %>%
  count()%>%
  filter(Classificação != "NA")

# Criando o treemap
treemap(figura2,
        index=c("Classificação","país"),
        vSize="n",
        type="index",
        palette = c("#F73C3C","#0F6F2A","#F5F346"),
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

# Quantas OIs cada grupo liderou (mencionado em texto; não em tabela)
tot_presid_grupo <- PISI %>%
  group_by(Classificação) %>%
  count()%>%
  rename(lideradas = n)

## Tabela 2 - Concentração relativa de lideranças por grupo

# Um país pode liderar 0, 1, 2, ..., OIs. Para o cálculo do índice de concentração,
# consideraremos apenas liderando 1 ou mais OIs
# Mundiais (168); Intermediários (412); e Menores (329) [valores para o denominador]
# Para ver demais grupos, substituir: filter(Classificação == "NOME DO GRUPO")

tabela2 <- PISI %>%
  group_by(Classificação, ano, país) %>%
  count()%>%
  filter(Classificação == "1. Potência mundial")

# Número de OIs lideradas por ano por cada país em cada grupo
  tabela2 <- PISI %>%
    group_by(Classificação, ano, país) %>%
    count()%>%
    rename(lideradas = n)

# Quantas vezes cada país do grupo liderou 1, 2, 3, 4 ou mais OIs.
 tabela2 <- tabela2  %>%
    group_by(lideradas, Classificação)%>%
    count()

 
## Figura 3 - Número de OIs por classificação (1975-2017)
library(ggpubr)
 
 figuras2e3 <- PISI %>%
  group_by(Classificação, ano)%>%
  count()
 
 ggplot(figuras2e3, aes(ano, n)) +
   geom_line(aes (group=Classificação, color=Classificação), lwd = 1.3) +
   scale_colour_manual(values = c("#F73C3C","#0F6F2A","#F5F346"))+
   scale_x_continuous(breaks=seq(1975,2015,5))+
   scale_y_continuous(breaks = seq(1,16,2), limits = c(1,16))+
   labs(x = "Anos", 
        y = "OIs")+
   geom_vline(xintercept = 1991, lwd = 1)+
   geom_vline(xintercept = 2001, lwd = 1)+
   geom_vline(xintercept = 2008, lwd = 1)+
   theme_pubr()+
   theme(legend.position = "bottom")

 ## Figura 4 - porcentagens
 ggplot(figuras2e3, aes(fill=Classificação, y=n, x=ano))+
   geom_bar(position="fill", stat="identity")+
   scale_fill_manual(values = c("#F73C3C","#0F6F2A","#F5F346"))+
   theme_pubr()+
   scale_x_continuous(breaks=seq(1975,2017,5))+
   scale_y_continuous(labels = scales::percent)+
   labs(x = "Anos", 
        y = "Percentual")+
   geom_vline(xintercept = 1991, lwd = 1)+
   geom_vline(xintercept = 2001, lwd = 1)+
   geom_vline(xintercept = 2008, lwd = 1)+
   theme(legend.position = "bottom") 

### Figura 5 - Liderança de intermediários tradicionais e regionais (1975-2017)
 figuras5 <- PISI %>%
   filter(país %in% c("África do Sul", "Brasil", "Índia", "China", "Rússia",
                      "Turquia", "Noruega", "Suécia", "Holanda", "Canadá",
                      "Austrália", "Dinamarca", "Bélgica", "Coreia do Sul",
                      "México"))%>%
   filter(Classificação == "2. Potência intermediária")%>%
    mutate(Tipo = case_when(país == "África do Sul" ~ "Regional",
                           país == "Brasil" ~ "Regional",
                           país == "Índia" ~ "Regional",
                           país == "China" ~ "Regional",
                           país == "Rússia" ~ "Regional",
                           país == "Turquia" ~ "Regional",
                           país == "México" ~ "Regional",
                           país == "Noruega" ~ "Tradicional",
                           país == "Suécia" ~ "Tradicional",
                           país == "Holanda" ~ "Tradicional",
                           país == "Canadá" ~ "Tradicional",
                           país == "Austrália" ~ "Tradicional",
                           país == "Dinamarca" ~ "Tradicional",
                           país == "Bélgica" ~ "Tradicional",
                           país == "Coreia do Sul" ~ "Tradicional"))%>%
   group_by(Tipo, ano)%>%
   count()
                                    
 ggplot(figuras5, aes(ano, n)) +
   geom_line(aes (group=Tipo, color=Tipo), lwd = 1.3) +
   scale_x_continuous(breaks=seq(1975,2015,5))+
   scale_y_continuous(breaks = seq(1,7,1), limits = c(1,7))+
   labs(x = "Anos", 
        y = "OIs")+
   geom_vline(xintercept = 1991, lwd = 1)+
   geom_vline(xintercept = 2001, lwd = 1)+
   geom_vline(xintercept = 2008, lwd = 1)+
   theme_pubr()+
   theme(legend.position = "bottom")                               
 