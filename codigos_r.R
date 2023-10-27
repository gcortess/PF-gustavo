library(readr)
vendas <- read_csv("bancos/vendas.csv")
View(vendas)
devolução <- read_csv("bancos/devolução_atualizado.csv")
View(devolução)
library(tidyverse)
library(lubridates)

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}


# descobrindo quais os valores das variaveis

### product name
vendas$`Product Name` <- as.factor(vendas$`Product Name`)
levels(vendas$`Product Name`)
vendas$`Product Name` <- as.character(vendas$`Product Name`)
### category
vendas$Category <- as.factor(vendas$Category)
levels(vendas$Category)
vendas$Category <- as.character(vendas$Category)
### color
vendas$Color <- as.factor(vendas$Color)
levels(vendas$Color)
vendas$Color <- as.character(vendas$Color)
### size
vendas$Size <- as.factor(vendas$Size)
levels(vendas$Size)
vendas$Size <- as.character(vendas$Size)

# trocando os valores para o português

### Product Name
vendas$`Product Name`[vendas$`Product Name` == "Dress"] <- "Vestido"
vendas$`Product Name`[vendas$`Product Name` == "Shoes"] <- "Tênis"
vendas$`Product Name`[vendas$`Product Name` == "Sweater"] <- "Sueter"
vendas$`Product Name`[vendas$`Product Name` == "T-shirt"] <- "Camiseta"
vendas$`Product Name`[vendas$`Product Name` == "Jeans"] <- "Calças"
vendas$`Product Name` <- as.factor(vendas$`Product Name`)

### Category
vendas$Category[vendas$Category == "Kids' Fashion"] <- "Moda Infantil"
vendas$Category[vendas$Category == "Men's Fashion"] <- "Moda Masculina"
vendas$Category[vendas$Category == "Women's Fashion"] <- "Moda Feminina"
vendas$Category <- as.factor(vendas$Category)

### Color
vendas$Color[vendas$Color == "Black"] <- "Preto"
vendas$Color[vendas$Color == "Blue"] <- "Azul"
vendas$Color[vendas$Color == "Green"] <- "Verde"
vendas$Color[vendas$Color == "Red"] <- "Vermelho"
vendas$Color[vendas$Color == "White"] <- "Branco"
vendas$Color[vendas$Color == "Yellow"] <- "Amarelo"
vendas$Color <- as.factor(vendas$Color)

### Size
vendas$Size[vendas$Size == "L"] <- "G"
vendas$Size[vendas$Size == "S"] <- "P"
vendas$Size[vendas$Size == "XL"] <- "GG"
vendas$Size <- as.factor(vendas$Size)

### Motivo devolução (trocando os NA's para não devolvidos)

vendas <- vendas %>%
  mutate(`Motivo devolução` = ifelse(is.na(`Motivo devolução`), "Não devolvido", `Motivo devolução`))

### Data

vendas$`Data Venda` <- mdy(vendas$`Data Venda`)
vendas$Mês <- month(vendas$`Data Venda`)
vendas$Mês <- as.character(vendas$Mês)

 vendas$Mês[vendas$Mês == "1"] <- "Janeiro"
 vendas$Mês[vendas$Mês == "2"] <- "Fevereiro"
 vendas$Mês[vendas$Mês == "3"] <- "Março"
 vendas$Mês[vendas$Mês == "4"] <- "Abril"
 vendas$Mês[vendas$Mês == "5"] <- "Maio"
 vendas$Mês[vendas$Mês == "6"] <- "Junho"
 vendas$Mês[vendas$Mês == "7"] <- "Julho"
 vendas$Mês[vendas$Mês == "8"] <- "Agosto"
 vendas$Mês[vendas$Mês == "9"] <- "Setembro"
 vendas$Mês[vendas$Mês == "10"] <- "Outubro"
 vendas$Mês[vendas$Mês == "11"] <- "Novembro"
 vendas$Mês[vendas$Mês == "12"] <- "Dezembro"

# Faturamento anual por categoria

vendaspc <- vendas %>% 
  filter(Mês != " ") %>% 
  filter(`Motivo devolução` == "Não devolvido") %>% 
  filter(Price != " ") %>% 
  filter(Category != " ") %>%  
  group_by(Category, Mês) %>%
  summarise(faturamento = sum(Price))

#### GRAFICO DO FATURAMENTO ANUAL POR CATEGORIA

mes_ordenado <- factor(vendaspc$Mês, levels = c("Janeiro", "Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"))

ggplot(vendaspc, aes(x = mes_ordenado, y = faturamento, group = Category, colour = Category)) +
  geom_line(size = 1) + geom_point(size = 2) +
  scale_colour_manual(name = "Categoria", values = c("#A11D21", "#003366", "#CC9900")) +
  labs(x = "Mês", y = "Faturamento (em milhares de reais)") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    axis.text.y = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  ylim(0, 3000) +
  theme(legend.position = "top")


kruskal.test(Category ~ faturamento, data = vendaspc)

# VARIAÇÃO DE PREÇO POR MARCA

vendas$Brand <- as.factor(vendas$Brand)
vendaspm <- vendas %>% 
  filter(Brand == "Adidas" | Brand == "Gucci" | Brand == "H&M" | Brand == "Nike" | Brand == "Zara")
#### GRAFICO DE BOXPLOT PREÇO/MARCA

ggplot(vendaspm, aes(x=Price, y=Brand)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun ="mean", geom="point", shape=23, size=3, fill="white")+
  coord_flip() +
  labs(x="PREÇO", y="MARCA")+
  theme_estat()

#### teste de correlaçao de kruskal wallis

kruskal.test(Price ~ Brand, data = vendas)

# RELAÇÃO ENTRE CATEGORIA E MARCA
vendascm <- vendas %>% 
  filter(Category == "Moda Masculina" | Category == "Moda Feminina") %>% 
  filter(Brand != " ") %>%  
  group_by(Category, Brand) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100, 2))


#### GRAFICO DE COLUNAS CATEGORIA/MARCA

names(vendascm)[names(vendascm) == "Category"] <- "Categoria"

meanTLE <- c(76, 73, 63, 74, 66, 71, 60, 62, 73, 76)

ggplot(vendascm) +
  aes(x = Brand, y = freq,
      fill = Categoria) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  labs(x = "MARCA", y = "FREQUÊNCIA ABSOLUTA")+
  geom_text(
    aes(label = meanTLE),
    vjust = -0.5,
    colour = "black", 
    position = position_dodge(width=0.9),
    fontface = "bold",
    size=3,
    angle = 0,
    hjust = 0.5) + 
  ylim(0, 80) +
  scale_fill_manual(values = c("#A11D21","#003366")) +
  theme_bw()

#### grafico dos produtos não devolvidos

vendascmsd <- vendas %>% 
  filter(Category == "Moda Masculina" | Category == "Moda Feminina") %>% 
  filter(Brand != " ") %>%
  filter(`Motivo devolução` == "Não devolvido") %>% 
  group_by(Category, Brand) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100, 2))

names(vendascmsd)[names(vendascmsd) == "Category"] <- "Categoria"

meanTLEsd <- c(50, 50, 41, 45, 40, 45, 42, 44, 40, 49)

ggplot(vendascmsd) +
  aes(x = Brand, y = freq,
      fill = Categoria) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  labs(x = "MARCA", y = "FREQUÊNCIA ABSOLUTA")+
  geom_text(
    aes(label = meanTLEsd),
    vjust = -0.5,
    colour = "black", 
    position = position_dodge(width=0.9),
    fontface = "bold",
    size=3,
    angle = 0,
    hjust = 0.5) + 
  ylim(0, 80) +
  scale_fill_manual(values = c("#A11D21","#003366")) +
  theme_bw()

# RELAÇÃO ENTRE PREÇO E AVALIAÇÃO

modelo_regressao <- lm(Rating ~ Price, data = vendas)
ggplot(vendas, aes(x = Price, y = Rating)) +
  geom_point(colour = "#A11D21", size = 2) +
  geom_abline(intercept = coef(modelo_regressao)[1], slope = coef(modelo_regressao)[2], color = "Black") +
  labs(x = "PREÇO", y = "AVALIAÇÃO") +
  theme_bw() +
  theme(
    axis.title.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(colour = "black", size = 12),
    axis.text = element_text(colour = "black", size = 9.5),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black")
  )

#### teste de correlação de pearson

cor.test(vendas$Price, vendas$Rating, method = "pearson")

# FREQUÊNCIA DE CADA TIPO DE DEVOLUÇÃO POR MARCA

vendasmd <- vendas %>% 
  filter(Brand != " ") %>% 
  filter(`Motivo devolução` != "Não devolvido") %>%  
  group_by(`Motivo devolução`, Brand) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = round((freq/sum(freq))*100, 2))

meanTMD <- c(20, 24,19,39,34, 28,26,22,28,20,30,20,27,29,20)

ggplot(vendasmd) +
  aes(x = Brand, y = freq,
      fill = `Motivo devolução`) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  labs(x = "MARCA", y = "FREQUÊNCIA ABSOLUTA")+
  geom_text(
    aes(label = meanTMD),
    vjust = -0.5,
    colour = "black", 
    position = position_dodge(width=1),
    fontface = "bold",
    size=3,
    angle = 0,
    hjust = 0.5) + 
  ylim(0, 40) +
  scale_fill_manual(values = c("#A11D21","#003366", "#CC9900")) +
  theme_bw()

kruskal.test(Brand ~ `Motivo devolução`,data = vendas)
