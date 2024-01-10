### Packages and data import
library(pacman)
pacman:::p_load("tidyverse", "BSDA", "EnvStats", "DescTools",
                "gridExtra", "knitr", "ggrepel", "readxl", "pspearman", "rio")


setwd(choose.dir())

amostra_397 <- read_excel("clientes_2019.xlsx", 
                          col_types = c("text", "text", "numeric", 
                                        "text", "text", "text", "text", "numeric", 
                                        "text", "numeric", "text", "text", 
                                        "text", "text"))

#Sample 347

amostra_397_sem_NA <- amostra_397[!is.na(amostra_397[,1]),]

amostra_347 <- amostra_397_sem_NA[sample(394,347),]

#Saving file

write_rds(amostra_347, "amostra347.xls")

amostra_347 <- read_rds("amostra_347.xls")


### Question 2
# Plot
amostra_347$Sexo<- parse_character(amostra_347$Sexo)


amostra_347 %>% 
  group_by(Sexo) %>% 
  summarise(prop = n()/347) %>% 
  mutate(Sexo = fct_recode(Sexo,
                           "Masculino" = "2",
                           "Feminino" = "1")) %>% 
  ggplot(aes(x = Sexo, y = prop, fill = Sexo)) +
  geom_col() +
  theme_minimal()+
  scale_fill_brewer(palette = 6, type = "qual")+
  theme(legend.position = ("none"), axis.title=element_text(size=15),
        plot.title = element_text(hjust=0.5), axis.text = element_text(size = 13))+
  labs(title = "Percentuais de clientes do sexo feminino e masculino", x = "Sexo", y = "Proporção")

# Proportion estimation
kable(BinomCI(sum(amostra_347$Sexo == 1), 347))
kable(BinomCI(sum(amostra_347$Sexo == 2), 347))


### Question 3
# Plot
amostra_347$Residência<- as.character(amostra_347$Residência)

x <- amostra_347 %>% 
  group_by(Residência) %>% 
  summarise(Proporção = n()/347) %>% 
  mutate(Residência = fct_recode(Residência,
                                 "Centro" = "1",
                                 "Oeste" = "2","Leste" = "3", "Sul" = "4"))

ggplot(x, aes(x = reorder(Residência, -Proporção, FUN = median), y = Proporção, fill = Residência)) +
  geom_col() +
  theme_minimal()+
  theme(legend.position = ("none"), axis.title=element_text(size=15), plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 13))+
  labs(title = "Proporções de clientes por locais de Residência", x = "Local de Residência", y = "Proporção")

# Table
x <- amostra_347 %>% 
  group_by(Residência) %>% 
  summarise("Freq. Observada" = n()) %>% 
  mutate("Freq. Esperada" = 347*0.25) %>% 
  mutate(Residência = fct_recode(Residência,
                                 "Centro" = "1",
                                 "Oeste" = "2","Leste" = "3", "Sul" = "4"))

kable(x, caption = "Tabela da Freq. de clientes por região")

# Proportion estimation
x <- amostra_347 %>% 
  group_by(Residência) %>% 
  mutate(Moradores = n()) %>% 
  select(Moradores, Residência)


x <- unique(x)

chisq.test(x$Moradores)

### Question 4
# Plot
amostra_347$Loja<- as.character(amostra_347$Loja)

amostra_347 <- amostra_347 %>% 
  mutate(Residência = fct_recode(Residência,
                                 "Centro" = "1",
                                 "Oeste" = "2","Leste" = "3", "Sul" = "4"),
         Sexo = fct_recode(Sexo, "Feminino" = "1", "Masculino" = "2"))

ggplot(amostra_347)+
  geom_bar(aes(Loja, fill= Sexo), position = "dodge")+
  scale_fill_brewer(palette = 6, type = "qual")+
  theme_minimal()+
  theme(axis.title=element_text(size=15), plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 13))+
  labs(title = "Número de clientes por tipo de loja", x = "Tipo de loja", y = "Frequência")

# Chisq test
chisq.test(amostra_347$Sexo, amostra_347$Loja)

### Question 5
# Plot
amostra_347$Qualidade<- as.character(amostra_347$Qualidade)

ggplot(amostra_347)+
  geom_boxplot(aes(Qualidade, Idade, fill = Qualidade))+
  scale_fill_brewer(palette = "Reds", type = "seq")+
  theme_minimal()+
  theme(legend.position = ("none"), axis.title=element_text(size=15),
        plot.title = element_text(hjust=0.5), axis.text = element_text(size = 13))+
  labs(title = "Boxplot da opinião de qualidade relativa ao preço por idade",
       x = "Opinião da qualidade", y = "Idade")

# Spearman test
x <- order(amostra_347$Idade)
y <- ordered(amostra_347$Qualidade)

spearman.test(amostra_347$Idade, amostra_347$Qualidade)

### Question 6
# Plots
x <- ggplot(amostra_347)+
  geom_histogram(aes(Valor), binwidth = 5, color = "white", fill = "black")+
  scale_fill_brewer(palette = 6, type = "seq")+
  theme_minimal()+
  theme(axis.title=element_text(size=15), plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 13))+
  labs(title = "Histograma do valor da última compra em intervalos de 5 em 5 reais",
       x = "Valor da última compra", y = "Frequência")

y <- ggplot(amostra_347)+
  geom_histogram(aes(Renda), binwidth = 1, color = "white", fill = "black")+
  scale_fill_brewer(palette = 6, type = "seq")+
  theme_minimal()+
  theme( axis.title=element_text(size=15), plot.title = element_text(hjust=0.5),
         axis.text = element_text(size = 13))+
  labs(title = "Histograma da renda por unidade de salário mínimo",
       x = "Renda", y = "Frequência")

grid.arrange(x,y, nrow = 2)

# Normality test
shapiro.test(amostra_347$Valor)

### Question 7
# Plot
ggplot(amostra_347, aes(Renda, Valor))+
  geom_point()+
  geom_smooth(method = "lm", se=F, color = "red")+
  theme_minimal()+
  theme(axis.title=element_text(size=15), plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 13))+
  labs(title = "Gráfico de Disperção entre Renda e Valor da última Compra",
       x = "Renda", y = "Valor da última compra")

# Corr test
cor.test(amostra_347$Renda,amostra_347$Valor,method='spearman')

### Question 8
# Plot
ggplot(amostra_347, aes(x = "", y = Valor))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = ("none"), axis.title=element_text(size=15),
        plot.title = element_text(hjust=0.5), axis.text = element_text(size = 13))+
  labs(title = "Boxplot dos valores da última compra",
       x = "", y = "Valor da Última Compra")

# Wilcox test
wilcox.test(amostra_347$Valor,mu = 180,paired=F)

### Question 9
# Plot
ggplot(amostra_347)+
  geom_boxplot(aes(Sexo, Valor, fill = Sexo))+
  scale_fill_brewer(palette = 6, type = "qual")+
  theme_minimal()+
  theme(legend.position = ("none"), axis.title=element_text(size=15),
        plot.title = element_text(hjust=0.5), axis.text = element_text(size = 13))+
  labs(title = "Boxplot do valor da última compra em relação ao sexo",
       x = "Sexo", y = "Valor da última compra")

# Kruskal test
kruskal.test(amostra_347$Sexo ~ amostra_347$Valor, data = amostra_347)

### Question 10
# Plot
ggplot(amostra_347)+
  geom_boxplot(aes(reorder(Residência, -Valor, FUN = median), Valor, fill = Residência))+
  theme_minimal()+
  theme(legend.position = ("none"), axis.title=element_text(size=15),
        plot.title = element_text(hjust=0.5), axis.text = element_text(size = 13))+
  labs(title = "Boxplot do valor da última compra em relação à Residência",
       x = "Local de Residência", y = "Valor da última compra")

# Kruskal test
kruskal.test(amostra_347$Residência ~ amostra_347$Valor, data = amostra_347)