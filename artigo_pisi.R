# Script versão 3 do artigo

library(tidyverse)
library(haven)
library(readr)

# Fazendo o download das bases de dados
link <- "https://github.com/felipelirapaiva/PISI/blob/main/international_organizations.csv?raw=true"
download.file(link, "international_organizations.csv")
international_org <- read.csv("international_organizations.csv", dec = ",", encoding = "UTF-8", check.names = FALSE)

# Tabela 1 - Número de organizações-ano que cada país liderou (1975-2016)
international_org  %>%
    group_by(país) %>%
    count()

# Classificando as potências e criando o objeto "fig" para fazer as próximas figuras.
library(ggplot2)
library(scales)

  international_org <- international_org %>%
    mutate(Classificação = case_when(país == "Estados Unidos" ~ "1. Potência mundial",
                                     país == "França" ~ "1. Potência mundial",
                                     país == "Japão" ~ "1. Potência mundial",
                                     país == "Suécia" ~ "2. Potência intermediária",
                                     país == "Egito" ~ "4. Potência subregional",
                                     país == "Brasil" ~ "3. Potência regional",
                                     país == "Nigéria" ~ "4. Potência subregional",
                                     país == "Índia" ~ "3. Potência regional",
                                     país == "Austrália" ~ "2. Potência intermediária",
                                     país == "Senegal" ~ "5. Potência menor",
                                     país == "Canadá" ~ "1. Potência mundial",
                                     país == "Suíça" ~ "5. Potência menor",
                                     país == "Gana" ~ "5. Potência menor",
                                     país == "Reino Unido" ~ "1. Potência mundial",
                                     país == "Argélia" ~ "5. Potência menor",
                                     país == "Chile" ~ "5. Potência menor",
                                     país == "Dinamarca" ~ "2. Potência intermediária",
                                     país == "Itália" ~ "1. Potência mundial",
                                     país == "Alemanha" ~ "1. Potência mundial",
                                     país == "Espanha" ~ "2. Potência intermediária",
                                     país == "Filipinas" ~ "4. Potência subregional",
                                     país == "Líbano" ~ "5. Potência menor",
                                     país == "Portugal" ~ "5. Potência menor",
                                     país == "China" ~ "3. Potência regional",
                                     país == "Coréia do Sul" ~ "2. Potência intermediária",
                                     país == "Arábia Saudita" ~ "3. Potência regional",
                                     país == "Argentina" ~ "3. Potência regional",
                                     país == "Paquistão" ~ "4. Potência subregional",
                                     país == "Quênia" ~ "5. Potência menor",
                                     país == "Bélgica" ~ "2. Potência intermediária",
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
                                     país == "Holanda" ~ "2. Potência intermediária",
                                     país == "Kuwait" ~ "5. Potência menor",
                                     país == "Mali" ~ "5. Potência menor",
                                     país == "Marrocos" ~ "5. Potência menor",
                                     país == "Serra Leoa" ~ "5. Potência menor",
                                     país == "Tunísia" ~ "5. Potência menor",
                                     país == "Irlanda" ~ "5. Potência menor",
                                     país == "Noruega" ~ "2. Potência intermediária",
                                     país == "Rússia" ~ "3. Potência regional",
                                     país == "Camarões" ~ "5. Potência menor",
                                     país == "Equador" ~ "5. Potência menor",
                                     país == "Irã" ~ "4. Potência subregional",
                                     país == "El Salvador" ~ "5. Potência menor"))
  
  
  fig <-  international_org %>%
    group_by(ano, Classificação) %>% 
    count()%>%
    filter(Classificação != "NA")

# Figura 1 - Total de OIs lideradas por cada grupo em cada ano
  ggplot(fig, aes(ano, n )) +
    geom_line(aes (group=Classificação, color=Classificação), lwd = 1.5) +
    theme_classic()+
    scale_x_continuous(breaks=seq(1975,2015,5))+
    scale_y_continuous(breaks=seq(2,19,2))+
    labs(title = "Figura 1",
         subtitle = "Total de OIs lideradas por cada grupo em cada ano",
         x = "", 
         y = "OIs",
         caption = "Fonte: Elaboração própria")+
    geom_vline(xintercept = 1991, lwd = 1)+
    geom_vline(xintercept = 2008, lwd = 1)

# Figura 2 - Porcentagem de OIs lideradas por cada grupo em cada ano 
  ggplot(fig, aes(fill=Classificação, y=n, x=ano)) + 
    geom_bar(position="fill", stat="identity") +
    theme_classic()+
    scale_x_continuous(breaks=seq(1975,2015,5))+
    scale_y_continuous(labels = scales::percent)+
    labs(title = "Figura 2",
         subtitle = "Porcentagem de OIs lideradas por grupos de potências",
         x = "", 
         y = "",
         caption = "Fonte: Elaboração própria")+
  geom_vline(xintercept = 1991, lwd = 1)+
    geom_vline(xintercept = 2008, lwd = 1)

# Adicionando algumas siglas para a confeccção do treemap (figura 3)
  fig3 <- international_org %>%
    group_by(ano, país, Classificação) %>%
    count()%>%
    filter(Classificação != "NA") %>%
    mutate(sigla = case_when(país == "Estados Unidos" ~ "Estados Unidos",
                             país == "França" ~ "França", país == "Japão" ~ "Japão",
                             país == "Suécia" ~ "Suécia", país == "Egito" ~ "Egito",
                             país == "Brasil" ~ "Brasil", país == "Nigéria" ~ "Nigéria",
                             país == "Índia" ~ "Índia", país == "Austrália" ~ "Austrália",
                             país == "Senegal" ~ "Senegal", país == "Canadá" ~ "Canadá",
                             país == "Suíça" ~ "Suíça", país == "Gana" ~ "Gana",
                             país == "Reino Unido" ~ "Reino Unido", país == "Argélia" ~ "Argélia",
                             país == "Chile" ~ "Chile", país == "Dinamarca" ~ "Dinamarca",
                             país == "Itália" ~ "Itália", país == "Alemanha" ~ "Alemanha",
                             país == "Espanha" ~ "Espanha", país == "Filipinas" ~ "Filipinas",
                             país == "Líbano" ~ "Líbano", país == "Portugal" ~ "Portugal",
                             país == "China" ~ "China", país == "Coréia do Sul" ~ "Coréia do Sul",
                             país == "Arábia Saudita" ~ "SA", país == "Argentina" ~ "Argentina",
                             país == "Paquistão" ~ "Paquistão", país == "Quênia" ~ "Quênia",
                             país == "Bélgica" ~ "Bélgica", país == "Fiji" ~ "Fiji",
                             país == "Áustria" ~ "Áustria", país == "Finlândia" ~ "Finlândia",
                             país == "Jordânia" ~ "Jor- dânia", país == "Nova Zelândia" ~ "Nova Zelândia",
                             país == "Sudão" ~ "Sudão", país == "Tailândia" ~ "Tailândia",
                             país == "Turquia" ~ "Turquia", país == "África do Sul" ~ "Á. do Sul",
                             país == "México" ~ "México", país == "Peru" ~ "Peru",
                             país == "Sri Lanka" ~ "Sri Lanka", país == "Tanzânia" ~ "Tanzânia",
                             país == "Bulgária" ~ "Bulgária", país == "Grécia" ~ "Grécia",
                             país == "Holanda" ~ "Holanda", país == "Kuwait" ~ "Kuwait",
                             país == "Mali" ~ "Mali", país == "Marrocos" ~ "Mar- rocos",
                             país == "Serra Leoa" ~ "Serra Leoa", país == "Tunísia" ~ "Tunísia",
                             país == "Irlanda" ~ "Irlanda", país == "Noruega" ~ "Noruega",
                             país == "Rússia" ~ "Rús- sia", país == "Camarões" ~ "Ca- ma- rões",
                             país == "Equador" ~ "Equador", país == "Irã" ~ "Irã",
                             país == "El Salvador" ~ "El Salvador"))
  
# Figura 3 - Distribuição de OIs lideradas por cada grupo e cada país
library(treemap)
library(RColorBrewer)

  treemap(fig3,
          index=c("Classificação","sigla"),
          vSize="n",
          type="index",
          palette = "Set1",
          title="Figura 3 - Distribuição de OIs lideradas por cada grupo e cada país",
          
          border.col=c("black","black"),
          border.lwds=c(1,1),
          
          fontsize.labels=c(10,9),
          fontface.labels=c(1,1),
          bg.labels=c("transparent"),
          overlap.labels=0,
          align.labels = list(
            c("left", "top"),
            c("center", "center")))