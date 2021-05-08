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

# Classificando as potências.
library(ggplot2)
library(ggpubr)
library(scales)

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
                                     país == "Coréia do Sul" ~ "2. Potência média",
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
                                     país == "El Salvador" ~ "5. Potência menor"))

# Contagem para saber quantas OIs-ano cada grupo liderou
international_org %>%
    group_by(Classificação)%>%
    count()

# Contagem para saber quantas OIs-anos cada país liderou
international_org %>%
  group_by(país)%>%
  count()

# Criando um objeto para fazer o treemmap (figura 1)
fig1 <- international_org %>%
    group_by(ano, país, Classificação) %>%
    count()%>%
    filter(Classificação != "NA")

# Figura 1 - Distribuição de OIs lideradas por cada grupo e cada país
library(treemap)
library(RColorBrewer)

  treemap(fig1,
          index=c("Classificação","país"),
          vSize="n",
          type="index",
          palette = c("#F73C3C", "#02A731","#1EE055","#8EEFAA","#F5F346"),
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
    geom_vline(xintercept = 2008, lwd = 1)+
  theme(legend.position = "none")

# Agora, juntando os dois gráficos para fazer a figura 2
figure <- ggarrange(fig2A, fig2B, ncol=1, nrow=2, common.legend = TRUE,
                    legend="bottom")
figure

# Figura 3 - Porcentagem de OIs lideradas por cada grupo em cada ano 
  ggplot(figs5, aes(fill=Classificação, y=n, x=ano)) + 
    geom_bar(position="fill", stat="identity") +
    theme_classic()+
    scale_x_continuous(breaks=seq(1975,2015,5))+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "", 
         y = "OIs")+
    geom_vline(xintercept = 1991, lwd = 1)+
    geom_vline(xintercept = 2008, lwd = 1)+
    scale_fill_manual(values = c("#F73C3C", "#02A731","#1EE055","#8EEFAA","#F5F346"))
  
#####################################
# Coisas citadas textualmente no artigo
  
# Destrinchando os países por classifcação.
# Opções para colocar entre parênteses:
# "1. Potência mundial"; "2. Potência média"; "3. Potência regional";
# "4. Potência subregional"; "5. Potência menor".
  porpaises <- international_org %>%
    filter(Classificação == "4. Potência subregional")
  
  porpaises <-  porpaises %>%
    group_by(ano, país) %>% 
    count()

# Figura 
  ggplot(porpaises, aes(ano, n )) +
    geom_line(aes (group=país, color=país), lwd = 1.5) +
    theme_classic()+
    scale_x_continuous(breaks=seq(1975,2015,5))+
    scale_y_continuous(breaks=seq(2,19,2))+
    labs(title = "Figura X",
         subtitle = "Total de OIs lideradas por cada país em cada ano",
         x = "", 
         y = "OIs",
         caption = "Fonte: Elaborado pelos autores")+
    geom_vline(xintercept = 1991, lwd = 1)+
    geom_vline(xintercept = 2008, lwd = 1)
  