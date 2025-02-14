library(readr)

setwd('C:/Users/arlyl/OneDrive/Pasta One Drive geral/Documentos/Dados_em_R')

getwd()
list.files()
DadosGás = read_csv('baseEstudoRca2004-01.csv')
library(readr)

# Ler o arquivo com ponto-e-vírgula como delimitador
DadosGás <- read_delim('baseEstudoRca2004-01.csv', delim = ";")
library(dplyr)

DadosGás <- DadosGás %>%
  mutate(across(where(is.character), ~ trimws(.))) # Remove espaços extras de colunas de texto

# Verificar valores NA
colSums(is.na(DadosGás))

# Substituir NA por um valor padrão (opcional)
DadosGás[is.na(DadosGás)] <- "Desconhecido"

library(knitr)

# Criar uma tabela bonita
kable(head(DadosGás), caption = "Tabela de Dados de Gás") # Exibe as primeiras linhas

# Renomear colunas
names(DadosGás) <- c("Regiao", "Estado", "Cidade", "Empresa", "CNPJ", "Endereco", "Produto", "Data", "Valor1", "Valor2", "Unidade", "Tipo")
library(dplyr)

DadosGás <- DadosGás %>%
  mutate(across(where(is.character), ~ trimws(.))) # Remove espaços extras de colunas de texto

library(dplyr)

DadosGás <- DadosGás %>%
  mutate(across(where(is.character), ~ trimws(.))) # Remove espaços extras de colunas de texto

library(knitr)

# Criar uma tabela bonita
kable(head(DadosGás), caption = "Tabela de Dados de Gás") # Exibe as primeiras linhas

install.packages("knitr")

library(knitr)

library(knitr)

# Criar uma tabela bonita
kable(head(DadosGás), caption = "Tabela de Dados de Gás") # Exibe as primeiras linhas

install.packages("dplyr")

library(dplyr)

DadosGás <- DadosGás %>%
  mutate(across(where(is.character), ~ trimws(.))) # Remove espaços extras de colunas de texto

# Substituir "Desconhecido" por NA em colunas numéricas
DadosGás <- DadosGás %>%
  mutate(across(where(is.numeric), ~ na_if(., "Desconhecido")))

# Excluir linhas com NA
DadosGás <- na.omit(DadosGás)

# Ou imputar valores ausentes (ex.: substituir NA pela média)
library(dplyr)
DadosGás <- DadosGás %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
# Substituir "Desconhecido" por NA em colunas numéricas
DadosGás <- DadosGás %>%
  mutate(across(where(is.numeric), ~ na_if(., "Desconhecido")))

# Excluir linhas com NA
DadosGás <- na.omit(DadosGás)

# Ou imputar valores ausentes (ex.: substituir NA pela média)
library(dplyr)
DadosGás <- DadosGás %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Substituir "Desconhecido" por NA em colunas numéricas
DadosGás <- DadosGás %>%
  mutate(across(where(is.numeric), ~ na_if(., "Desconhecido")))

# Excluir linhas com NA
DadosGás <- na.omit(DadosGás)

# Ou imputar valores ausentes (ex.: substituir NA pela média)
library(dplyr)
DadosGás <- DadosGás %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Verificar os nomes das colunas
names(DadosGás)


# Verificar os nomes das colunas
names(DadosGás)


# Estrutura geral
str(DadosGás)

# Visualização compacta
library(dplyr)
glimpse(DadosGás)


library(readr)

# Ler o arquivo com ponto-e-vírgula como delimitador
DadosGás <- read_delim("baseEstudoRca2004-01.csv", delim = ";")


# Verificar nomes das colunas
names(DadosGás)

# Verificar estrutura
str(DadosGás)

# Visualizar os nomes das colunas
names(DadosGás)

# Estrutura geral (tipos de dados)
str(DadosGás)

# Resumo estatístico básico
summary(DadosGás)

library(dplyr)

# Resumo estatístico para colunas numéricas
DadosGás %>%
  summarise(
    Media_Valor1 = mean(Valor1, na.rm = TRUE),
    Mediana_Valor1 = median(Valor1, na.rm = TRUE),
    DesvioPadrao_Valor1 = sd(Valor1, na.rm = TRUE),
    Min_Valor1 = min(Valor1, na.rm = TRUE),
    Max_Valor1 = max(Valor1, na.rm = TRUE),
    
    Media_Valor2 = mean(Valor2, na.rm = TRUE),
    Mediana_Valor2 = median(Valor2, na.rm = TRUE),
    DesvioPadrao_Valor2 = sd(Valor2, na.rm = TRUE),
    Min_Valor2 = min(Valor2, na.rm = TRUE),
    Max_Valor2 = max(Valor2, na.rm = TRUE)
  )

# Contagem de entradas por Região
table(DadosGás$Regiao)

# Contagem de entradas por Estado
table(DadosGás$Estado)

# Contagem de entradas por Cidade
table(DadosGás$Cidade)

# Verificar os nomes das colunas
names(DadosGás)

# Verificar os nomes das colunas
names(DadosGás)

library(dplyr)

# Converter colunas para numérico
DadosGás <- DadosGás %>%
  mutate(
    `Valor de Venda` = as.numeric(gsub("[^0-9\\.]", "", `Valor de Venda`)), # Remove caracteres não numéricos
    `Valor de Compra` = as.numeric(gsub("[^0-9\\.]", "", `Valor de Compra`))
  )

# Resumo estatístico para colunas numéricas
resumo_numericas <- DadosGás %>%
  summarise(
    Media_Valor_Venda = mean(`Valor de Venda`, na.rm = TRUE),
    Mediana_Valor_Venda = median(`Valor de Venda`, na.rm = TRUE),
    DesvioPadrao_Valor_Venda = sd(`Valor de Venda`, na.rm = TRUE),
    Min_Valor_Venda = min(`Valor de Venda`, na.rm = TRUE),
    Max_Valor_Venda = max(`Valor de Venda`, na.rm = TRUE),
    
    Media_Valor_Compra = mean(`Valor de Compra`, na.rm = TRUE),
    Mediana_Valor_Compra = median(`Valor de Compra`, na.rm = TRUE),
    DesvioPadrao_Valor_Compra = sd(`Valor de Compra`, na.rm = TRUE),
    Min_Valor_Compra = min(`Valor de Compra`, na.rm = TRUE),
    Max_Valor_Compra = max(`Valor de Compra`, na.rm = TRUE)
  )

# Exibir o resumo
print(resumo_numericas)

# Contagem de entradas por Região
table(DadosGás$`Regiao - Sigla`)

# Contagem de entradas por Estado
table(DadosGás$`Estado - Sigla`)

# Contagem de produtos
table(DadosGás$Produto)

library(ggplot2)

ggplot(DadosGás, aes(x = `Regiao - Sigla`)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Contagem de Entradas por Região", x = "Região", y = "Contagem")

ggplot(DadosGás, aes(x = `Valor de Venda`)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Distribuição de Valor de Venda", x = "Valor de Venda", y = "Frequência")

ggplot(DadosGás, aes(y = `Valor de Compra`)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot de Valor de Compra", y = "Valor de Compra")
# Verificar se o pacote ggplot2 está instalado
"ggplot2" %in% installed.packages()

# Instalar o pacote ggplot2
install.packages("ggplot2")

# Carregar o pacote ggplot2
library(ggplot2)


# Carregar o pacote ggplot2
library(ggplot2)

# Gráfico 1: Contagem de Entradas por Região
ggplot(DadosGás, aes(x = `Regiao - Sigla`)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Contagem de Entradas por Região", x = "Região", y = "Contagem")

ggplot(DadosGás, aes(x = `Regiao - Sigla`)) +
  geom_bar(fill = "green") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Contagem de Entradas por Região", x = "Região", y = "Contagem")

# Contagem de entradas por região
table(DadosGás$`Regiao - Sigla`)

# Resumo estatístico
summary(DadosGás$`Regiao - Sigla`)

# Verificar valores ausentes
sum(is.na(DadosGás$`Regiao - Sigla`))

# Verificar categorias únicas
unique(DadosGás$`Regiao - Sigla`)

ggplot(DadosGás, aes(x = `Regiao - Sigla`)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Contagem de Entradas por Região", x = "Região", y = "Contagem") +
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, by = 50000))

DadosGás$`Regiao - Sigla` <- as.factor(DadosGás$`Regiao - Sigla`)

table(DadosGás$`Regiao - Sigla`)

summary(DadosGás$`Regiao - Sigla`)

ggplot(DadosGás, aes(x = `Regiao - Sigla`)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Contagem de Entradas por Região", x = "Região", y = "Contagem") +
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, by = 50000))

table(DadosGás$`Regiao - Sigla`)

ggsave("grafico_regiao.png", width = 10, height = 6, units = "in")

prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Total de linhas no conjunto de dados
nrow(DadosGás)

# Soma das contagens por região
sum(table(DadosGás$`Regiao - Sigla`))

# Total de linhas no conjunto de dados
nrow(DadosGás)

# Soma das contagens por região
sum(table(DadosGás$`Regiao - Sigla`))

# Total de linhas no conjunto de dados
nrow(DadosGás)

# Soma das contagens por região
sum(table(DadosGás$`Regiao - Sigla`))

prop.table(table(DadosGás$`Regiao - Sigla`)) * 100


# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Converter para data frame
percentuais_df <- as.data.frame(percentuais)
colnames(percentuais_df) <- c("Regiao", "Percentual") # Renomear colunas

# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Converter para data frame
percentuais_df <- as.data.frame(percentuais)
colnames(percentuais_df) <- c("Regiao", "Percentual") # Renomear colunas

# Criar o gráfico
ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "blue") + # Barras com base nos valores
  geom_text(aes(label = round(Percentual, 2)), vjust = -0.5, size = 4) + # Adicionar rótulos
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotacionar rótulos do eixo X
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) # Escala do eixo Y

# Verificar se o objeto percentuais_df existe
exists("percentuais_df")

# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Converter para data frame
percentuais_df <- as.data.frame(percentuais)

# Renomear colunas
colnames(percentuais_df) <- c("Regiao", "Percentual")

# Exibir o data frame
print(percentuais_df)

# Criar o gráfico
ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "blue") + # Barras com base nos valores
  geom_text(aes(label = round(Percentual, 2)), vjust = -0.5, size = 4) + # Adicionar rótulos
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotacionar rótulos do eixo X
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) # Escala do eixo Y

# Verificar se a coluna existe
"Regiao - Sigla" %in% names(DadosGás)

# Contagem de entradas por região
table(DadosGás$`Regiao - Sigla`)

# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100



# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Exibir os percentuais
print(percentuais)


# Converter para data frame
percentuais_df <- as.data.frame(percentuais)

# Renomear colunas
colnames(percentuais_df) <- c("Regiao", "Percentual")

# Exibir o data frame
print(percentuais_df)

# Criar o gráfico
ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "blue") + # Barras com base nos valores
  geom_text(aes(label = round(Percentual, 2)), vjust = -0.5, size = 4) + # Adicionar rótulos
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotacionar rótulos do eixo X
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) # Escala do eixo Y

# Verificar se a coluna existe
"Regiao - Sigla" %in% names(DadosGás)

# Contagem de entradas por região
table(DadosGás$`Regiao - Sigla`)

# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Exibir os percentuais
print(percentuais)

# Calcular os percentuais
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100

# Exibir os percentuais
print(percentuais)

# Converter para data frame
percentuais_df <- as.data.frame(percentuais)

# Renomear colunas
colnames(percentuais_df) <- c("Regiao", "Percentual")

# Exibir o data frame
print(percentuais_df)

# Criar o data frame manualmente
percentuais_df <- data.frame(
  Regiao = c("CO", "N", "NE", "S", "SE"),
  Percentual = c(7.459214, 4.797340, 16.183653, 16.686972, 54.872820)
)

# Exibir o data frame
print(percentuais_df)

# Criar o gráfico
ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "blue") + # Barras com base nos valores
  geom_text(aes(label = round(Percentual, 2)), vjust = -0.5, size = 4) + # Adicionar rótulos
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotacionar rótulos do eixo X
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) # Escala do eixo Y

# Forçar o cálculo dos percentuais manualmente
contagens <- table(DadosGás$`Regiao - Sigla`) # Contagem absoluta
percentuais <- (contagens / sum(contagens)) * 100 # Cálculo manual dos percentuais

# Exibir os percentuais
print(percentuais)


# Verificar se a coluna existe
"Regiao - Sigla" %in% names(DadosGás)

# Contagem de entradas por região
table(DadosGás$`Regiao - Sigla`)

# Contagem absoluta
contagens <- table(DadosGás$`Regiao - Sigla`)

# Exibir as contagens absolutas
print(contagens)

# Total de entradas
total <- sum(contagens)

# Exibir o total
print(total)

# Total de entradas
total <- sum(contagens)

# Exibir o total
print(total)

# Calcular os percentuais
percentuais <- (contagens / total) * 100

# Exibir os percentuais
print(percentuais)

# Criar o data frame manualmente
percentuais_df <- data.frame(
  Regiao = c("CO", "N", "NE", "S", "SE"),
  Percentual = c(7.459214, 4.797340, 16.183653, 16.686972, 54.872820)
)

# Exibir o data frame
print(percentuais_df)


# Criar o data frame manualmente
percentuais_df <- data.frame(
  Regiao = c("CO", "N", "NE", "S", "SE"),
  Percentual = c(7.459214, 4.797340, 16.183653, 16.686972, 54.872820)
)

# Exibir o data frame
print(percentuais_df)

# Criar o data frame manualmente.
percentuais_df <- data.frame(
  Regiao = c("CO", "N", "NE", "S", "SE"),
  Percentual = c(7.459214, 4.797340, 16.183653, 16.686972, 54.872820)
)

# Exibir o data frame
print(percentuais_df)

# Criar o gráfico
ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "orange") + # Barras com base nos valores
  geom_text(aes(label = round(Percentual, 2)), vjust = -0.5, size = 4) + # Adicionar rótulos
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotacionar rótulos do eixo X
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) # Escala do eixo Y

# Criar o gráfico
ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "orange") + # Barras com base nos valores
  geom_text(aes(label = paste0(round(Percentual, 2), "%")), vjust = -0.5, size = 4) + # Adicionar rótulos com %
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + # Rótulos do eixo X na horizontal
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10)) # Escala do eixo Y

ggsave("grafico_percentual_regiao.png", width = 10, height = 6, units = "in")

ggplot(DadosGás, aes(x = `Regiao - Sigla`, y = `Valor de Venda`)) +
  geom_boxplot(fill = "orange") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Distribuição de Valor de Venda por Região", x = "Região", y = "Valor de Venda")

ggsave("grafico_percentual_regiao.png", width = 10, height = 6, units = "in")