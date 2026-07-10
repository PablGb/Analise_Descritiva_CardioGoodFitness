library(readr)
library(dplyr)

dados <- read_csv("CardioGoodFitness.csv")

#Informações preliminares da base de dados (180 linhas 9 colunas/variáveis):

#Product: Os 3 tipos de esteira - TM195, TM498, ou TM798.
#Age: Anos completos.
#Gender: Gênero - Masculino e Feminino.
#Education: Anos estudados.
#MaritalStatus: Estado civil - Solteiro ou casado.
#Usage: Número médio de vezes que o clientes planeja usar a esteira por semana.
#Fitness: Autoavaliação da aptidão física em uma escala de 1 a 5, onde 1 é má forma e 5 é excelente.
#Income: Renda familiar anual.
#Miles: Número médio de milhas que o cliente espera caminhar/correr a cada semana.

### Visualização Geral dos Dados ###

#Informações sintetizadas da base de dados
summary(dados)

### Análise Multivariada dos Dados ###

# Calcular a matriz de correlação apenas para colunas numéricas
cor_matrix <- cor(dados[sapply(dados, is.numeric)], use = "complete.obs")



# Exibir a matriz de correlação
print(cor_matrix)


# Instalar o pacote (se necessário)
install.packages("corrplot")
# Carregar o pacote
library(corrplot)

#Mapa de calor
corrplot(cor_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(50),
         tl.col = "black",        # Cor dos rótulos das variáveis
         addCoef.col = "black",  # Adicionar valores das correlações
         number.cex = 0.7,       # Tamanho dos números
)
title("Mapa de Calor da Matriz de Correlação", line = 1.0)  

# Exibir a matriz de correlação
print(cor_matrix)

### Análise Univariada dos Dados ###

#Análise da frequência absoluta e relativa do tipo de esteira:
tab_freq_Product <- dados %>%
  count(Product) %>% # Conta a frequência absoluta
  mutate(freq_rel_Product = (n / sum(n))*100) # Calcula a frequência relativa

#Visualização da tabela de frequências do tipo de esteira
tab_freq_Product

#Gráfico de Barras do tipo de esteira 
barplot(table(dados$Product),
        main = "Distribuição de frequência por tipo de produto de esteira",
        ylab = "No. dos de esteiras",
        xlab = "Tipos de esteiras")

#Gráfico de setores do tipo de esteira 
pie(table(dados$Product),
    main = 'Gráfico de setores das frequências dos tipos de produtos de esteira',
    radius = 1.0
)



#Boxplot de Milhas que o cliente espera percorrer a cada semana por cada Tipo de Esteira
boxplot(dados$Miles~dados$Product,
        main = "Boxplot de milhas que o cliente espera percorrer na semana 
        por cada Tipo de Esteira",
        ylab = "Milhas",
        xlab = "Tipo de Esteira", col = c("grey"))

#Análise da frequência absoluta e relativa do sexo dos clientes:
tab_freq_Gender <- dados %>%
  count(Gender) %>% # Conta a frequência absoluta
  mutate(freq_rel_Gender = (n / sum(n))*100) # Calcula a frequência relativa

tab_freq_Gender

#Gráfico de setores referente ao gênero dos clientes
pie(table(dados$Gender),
    main = 'Gráfico de setores referente ao gênero dos clientes',
    radius = 2.0
)


#Análise da frequência absoluta e relativa dos anos de estudo do cliente:
tab_freq_Education <- dados %>%
  count(Education) %>% # Conta a frequência absoluta
  mutate(freq_rel_Education = (n / sum(n))*100) # Calcula a frequência relativa

tab_freq_Education

#Milhas que o cliente espera percorrer a cada semana em cada Tipo de Esteira
boxplot(dados$Age~dados$Gender,
        main = "Milhas que o cliente espera percorrer na semana 
        em cada Tipo de Esteira",
        ylab = "Idade",
        xlab = "Gênero", col = c("blue"))


#Análise da frequência absoluta e relativa do estado civil dos clientes:
tab_freq_MaritalStatus <- dados %>%
  count(MaritalStatus) %>% # Conta a frequência absoluta
  mutate(freq_rel_MaritalStatus = (n / sum(n))*100) # Calcula a frequência relativa

tab_freq_MaritalStatus

#Gráfico de setores do estado civil dos clientes
pie(table(dados$MaritalStatus),
    main = 'Gráfico de setores do estado civil dos clientes',
    radius = 1.0
)

#library(dplyr)

#Análise da frequência absoluta e relativa dos dias que os clientes pretendem usar as esteiras:
freq_abs_Usage <- table(dados$Usage)
freq_abs_Usage
round(((freq_abs_Usage)/180)*100,2)

#Análise da frequência absoluta e relativa dos dias que os clientes pretendem usar as esteiras:
tab_freq_Usage <- dados %>%
  count(Usage) %>% # Conta a frequência absoluta
  mutate(freq_rel_Usage = (n / sum(n))*100) # Calcula a frequência relativa

tab_freq_Usage

#Análise da frequência absoluta e relativa da autoavaliação física dos clientes:
tab_freq_Fitness <- dados %>%
  count(Fitness) %>% # Conta a frequência absoluta
  mutate(freq_rel_Fitness = (n / sum(n))*100) # Calcula a frequência relativa

tab_freq_Fitness


#Informações sintetizadas da base de dados
summary(dados)


#Função para calcular a moda das variáveis da base de dados:
calcular_moda <- function(x) {
  valores_unicos <- unique(x) #agrupa todos os valores aparecem
  frequencia <- tabulate(match(x, valores_unicos)) #Identifica a frequencia dos valores que aparecem
  valores_unicos[which.max(frequencia)] #identifica o indice do numero que mais aparece e retorna seu valor
}
#Observação sobre a função calcular_moda: Ela não necessariamente identifica o valor que mais aparece, podem ocorrer empates!


#Calculando as modas das variáveis
moda_Product <- calcular_moda(dados$Product)
moda_Age <- calcular_moda(dados$Age) 
moda_Gender <- calcular_moda(dados$Gender)
moda_Education <- calcular_moda(dados$Education)
moda_MaritalStauts <- calcular_moda(dados$MaritalStatus)
moda_Usage <- calcular_moda(dados$Usage)
moda_Fitness <- calcular_moda(dados$Fitness)
moda_Income <- calcular_moda(dados$Income)
moda_Miles <- calcular_moda(dados$Miles)

#Dataframe das modas
moda <- data.frame(
  Colunas = c('Product', 'Age', 'Gender', 'Education', 'MaritalStatus', 
              'Usage', 'Fitness', 'Income', 'Miles'),
  Modas = c('TM195', 25, 'Male', 16, 'Partnered', 3, 3, 45480, 85)
)

### Análise Bivariada dos Dados ###

#Tabela de produto por número de dias que o cliente planeja utilizar a esteira
table(dados$Product,dados$Usage)

#Tabela de produto por autoavaliação de aptidão física do cliente
table(dados$Product,dados$Fitness)


### Análise Multivariada dos Dados ###

#Boxplot da distribuição de Idade
boxplot(dados$Age,
        main = "Boxplot das Idades",
        ylab = "Anos de idade",
        xlab = "Idade", col = c("red"))


#Boxplot da distribuição Education
boxplot(dados$Education,
        main = "Boxplot da distribuição da variável Education",
        ylab = "Anos de Estudo",
        xlab = "Education", col = c("blue"))

#Boxplot da distribuição Usage
boxplot(dados$Usage,
        main = "Boxplot da distribuição da variável Usage",
        ylab = "Dias médios da semana",
        xlab = "Usage", col = c("yellow"))

#Boxplot da distribuição Fitness
boxplot(dados$Fitness,
        main = "Boxplot da distribuição da variável Fitness",
        ylab = "Autoavaliação",
        xlab = "Fitness", col = c("pink"))

#Boxplot da distribuição Income
boxplot(dados$Income,
        main = "Boxplot da distribuição da variável Income",
        ylab = "Renda Familiar",
        xlab = "Income", col = c("tomato"))
options(scipen = 999)

#Boxplot da distribuição Miles
boxplot(dados$Miles,
        main = "Boxplot da distribuição da variável Miles",
        ylab = "Milhas",
        xlab = "Miles", col = c("lightcoral"))

# Calculando a frequência por faixa de idade
dados$Faixa_Idade <- cut(dados$Age, 
                         breaks = c(18, 22, 26, 30, 34, 38, 42, 46, 50), 
                         labels = c("18-21", "22-25", "26-29", "30-33", "34-37", "38-41", "42-45", "46-50"),
                         right = TRUE)  


frequencia_faixa <- table(dados$Faixa_Idade)
print(frequencia_faixa)
barplot(frequencia_faixa,
        main = "Distribuição de frequência por Idade de cliente",
        ylab = "No. de clientes",
        xlab = "Idades",
        col = "green4")
)

#teste Anova
anova_resultado <- aov(Income ~ Product, data = dados)
summary(anova_resultado)

# Grafico boxplot relacionado a anova
dados$Product <- factor(dados$Product, levels = unique(dados$Product))
plot(dados$Product, dados$Income, 
     main = "Gráfico da Renda Familiar por Esteira",  
     xlab = "Esteira",                               
     ylab = "Renda Anual",                           
     col = c("tan", "olivedrab", "darkslateblue"),  
     pch = 19,                                       
     xaxt = "n")   


axis(1, at = 1:length(levels(dados$Product)), labels = levels(dados$Product))
#Histograma renda anual
options(scipen = 999)
hist(dados$Income, 
     main = "Distribuição de Renda", 
     xlab = "Renda Anual", 
     ylab = "Frequência", 
     col = "tomato", 
     breaks = 25,)


#Histograma distancia em milhas
hist(dados$Miles, 
     main = "Distribuição de Distância Percorrida Em Milhas", 
     xlab = "Distância (Miles)", 
     ylab = "Frequência", 
     col = "lightcoral", 
     breaks = 25, 
     freq = FALSE)
lines(density(dados$Miles), col = "black", lwd = 2)

#Histograma distancia em Km
dados$Kilometros <- dados$Miles * 1.60934
hist(dados$Kilometros, 
     main = "Distribuição de Distância Percorrida Em Kilometeros", 
     xlab = "Distância (Km)", 
     ylab = "Frequência", 
     col = "lightseagreen", 
     breaks = 25, 
     freq = FALSE)
lines(density(dados$Kilometros), col = "black", lwd = 2)


#Grafico estado civil e genero
crosstab <- table(dados$Gender, dados$MaritalStatus)
print(crosstab)
barplot(crosstab, 
        beside = TRUE, 
        main = "Distribuição de Consumidores por estado civil e gênero", 
        xlab = "Estado Civil", 
        ylab = "Frequência", 
        col = c("lightcoral","lightblue"),  
        legend.text = rownames(crosstab),     
        args.legend = list(title = "Gênero", x = "topright")  
)
#Grafico faixa etaria por tipo de esteira
dados$AgeGroup <- cut(dados$Age, breaks = c(18, 25, 35, 45,50), 
                      labels = c("18-25", "26-35", "36-45", "46-50"))
table(dados$AgeGroup, dados$Product)
barplot(table(dados$AgeGroup, dados$Product), 
        beside = TRUE,  
        col = c("lightblue", "lightcoral", "lightgreen", "lightyellow"),  
        main = "Distribuição de Compras por Faixa Etária e Produto",  
        xlab = "Faixa Etária",  
        ylab = "Número de Compras",  
        legend.text = levels(dados$AgeGroup),  
        args.legend = list(title = "Faixa Etária", x = "topright"))

