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

### REMOVENDO OS DADOS ANTIGOS DA DEVOLUÇÃO, COLOCANDO A NOVA, REMOVENDO OS NA's E TIRANDO OS ITENS DUPLICADOS
vendas <- vendas %>% 
  distinct(`Product ID`, .keep_all = TRUE)
vendas <- vendas[, !names(vendas) %in% "Motivo devolução"]
vendas <- vendas %>%
  left_join(devolução, by = "Unique ID")
vendas$`Motivo devolução` <- ifelse(is.na(vendas$`Motivo devolução`), "Não devolvido", vendas$`Motivo devolução`)

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
  filter(Price != " ") %>% 
  filter(Category != " ") %>%  
  group_by(Category, Mês) %>%
  summarise(faturamento = sum(Price))


#### GRAFICO DO FATURAMENTO ANUAL POR CATEGORIA

mes_ordenado <- factor(vendas_pc$Mês, levels = c("Janeiro", "Fevereiro","Março","Abril","Maio","Junho","Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"))

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
  ylim(0,4000) +
  theme(legend.position = "top")

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
  labs(x="Preço em reais", y="Marca")+
  theme_estat()

vendas_teses <- filter(vendas, Brand == "Zara")
mean(vendas_teses$Rating, na.rm = T)


# RELAÇÃO ENTRE CATEGORIA E COR

vendascm <- vendas %>% 
  filter(Category == "Moda Masculina" | Category == "Moda Feminina") %>% 
  filter(Color != " ") %>%  
  group_by(Category, Color) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq))
    )

names(vendascm)[names(vendascm) == "Category"] <- "Categoria"
names(vendascm)[names(vendascm) == "Color"] <- "Cor"


ggplot(vendascm, aes(x = fct_reorder(Cor, freq, .desc=F), y = freq,
                     fill = Categoria)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  labs(x = "Cor", y = "Frequência absoluta e relativa") +
  geom_text(
    aes(label = paste(freq, " (", freq_relativa, ")", sep = "")),
    vjust = 0.4,
    colour = "black", 
    position = position_dodge(width=1),
    fontface = "bold",
    size=3,
    angle = 0,
    hjust = -0.1) + 
  ylim(0, 80) +
  theme_estat() +
  coord_flip() +
  theme(legend.position = "top")

vendascmt <- vendas %>% 
  select(Category, Color) %>% 
  filter(Category != "Moda Infantil") %>% 
  filter(Category != " ") %>% 
  filter(Color != " ") 

tabela <- table(vendascmt$Category, vendascmt$Color)
view(tabela)
chisq.test(tabela)
# RELAÇÃO ENTRE PREÇO E AVALIAÇÃO

ggplot(vendas, aes(x = Price, y = Rating)) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Preço", y = "Avaliação") +
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
  mutate(
    freq_relativa = scales::percent((freq / sum(freq)))
  )
  
ggplot(vendasmd) +
  aes(x = fct_reorder(Brand, freq, .desc = F), y = freq,
      fill = `Motivo devolução`) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    aes(label = paste(freq, " (", freq_relativa, ")", sep = "")),
    vjust = 0.4,
    colour = "black", 
    position = position_dodge(width=1),
    fontface = "bold",
    size=3,
    angle = 0,
    hjust = -0.1) + 
  labs(x = "Marca", y = "Frequência absoluta e relativa") +
  ylim(0, 40) +
  theme_estat() +
  coord_flip()


