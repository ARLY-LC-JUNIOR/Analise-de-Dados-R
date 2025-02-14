# ==============================
# 1. Configuração Inicial
# ==============================

# Carregar bibliotecas necessárias
library(readr)  # Para leitura de arquivos CSV
library(dplyr)  # Para manipulação de dados
library(ggplot2)  # Para criação de gráficos
library(knitr)  # Para criar tabelas bonitas

# Definir o diretório de trabalho
setwd('C:/Users/arlyl/OneDrive/Pasta One Drive geral/Documentos/Dados_em_R')
getwd()  # Verificar o diretório atual
list.files()  # Listar arquivos no diretório

# Ler o arquivo CSV com ponto-e-vírgula como delimitador
DadosGás <- read_delim("baseEstudoRca2004-01.csv", delim = ";")

# Observação: O uso de `read_delim` é uma boa prática quando o delimitador não é padrão (vírgula).
# Certifique-se de que o arquivo existe no diretório especificado para evitar erros.

# ==============================
# 2. Limpeza e Pré-processamento dos Dados
# ==============================

# Remover espaços extras de colunas de texto
DadosGás <- DadosGás %>%
  mutate(across(where(is.character), ~ trimws(.)))

# Observação: A função `trimws()` é útil para limpar espaços desnecessários.
# Repetir essa etapa várias vezes no código original foi redundante, mas não prejudicial.

# Substituir NA por "Desconhecido" (opcional)
DadosGás[is.na(DadosGás)] <- "Desconhecido"

# Observação: Substituir NA por "Desconhecido" pode ser útil em algumas análises,
# mas cuidado ao aplicar isso a colunas numéricas, pois pode causar problemas.

# Excluir linhas com NA ou imputar valores ausentes
DadosGás <- na.omit(DadosGás)  # Excluir linhas com NA
# Ou imputar valores ausentes pela média (exemplo abaixo):
# DadosGás <- DadosGás %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Observação: Escolher entre excluir ou imputar depende do contexto.
# Excluir linhas pode reduzir o tamanho do dataset, enquanto imputar pode introduzir viés.

# Renomear colunas para facilitar a manipulação
names(DadosGás) <- c("Regiao", "Estado", "Cidade", "Empresa", "CNPJ", "Endereco", 
                     "Produto", "Data", "Valor1", "Valor2", "Unidade", "Tipo")

# Observação: Renomear colunas é uma boa prática para facilitar a leitura e manipulação.
# Certifique-se de que os nomes sejam descritivos e consistentes.

# Converter colunas para numérico, removendo caracteres não numéricos
DadosGás <- DadosGás %>%
  mutate(
    `Valor de Venda` = as.numeric(gsub("[^0-9\\.]", "", `Valor de Venda`)),
    `Valor de Compra` = as.numeric(gsub("[^0-9\\.]", "", `Valor de Compra`))
  )

# Observação: A conversão para numérico é essencial para análises quantitativas.
# O uso de `gsub()` para remover caracteres não numéricos é uma solução eficaz.

# ==============================
# 3. Análise Descritiva
# ==============================

# Resumo estatístico básico
summary(DadosGás)

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
print(resumo_numericas)

# Observação: O resumo estatístico é fundamental para entender a distribuição dos dados.
# A repetição de cálculos semelhantes no código original foi desnecessária, mas não prejudicial.

# Contagem de entradas por categoria
table(DadosGás$`Regiao - Sigla`)
table(DadosGás$`Estado - Sigla`)
table(DadosGás$Produto)

# Observação: A função `table()` é útil para análises categóricas.
# Certifique-se de que as colunas existam antes de usá-las para evitar erros.

# ==============================
# 4. Visualizações
# ==============================

# Gráfico de barras para contagem de entradas por região
ggplot(DadosGás, aes(x = `Regiao - Sigla`)) +
  geom_bar(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Contagem de Entradas por Região", x = "Região", y = "Contagem") +
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, by = 50000))

# Observação: O gráfico de barras é uma boa escolha para visualizar contagens categóricas.
# A rotação dos rótulos do eixo X melhora a legibilidade.

# Histograma para distribuição de Valor de Venda
ggplot(DadosGás, aes(x = `Valor de Venda`)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Distribuição de Valor de Venda", x = "Valor de Venda", y = "Frequência")

# Observação: O histograma é útil para entender a distribuição de variáveis numéricas.
# A escolha do `binwidth` deve ser ajustada conforme os dados.

# Boxplot para Valor de Compra
ggplot(DadosGás, aes(y = `Valor de Compra`)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot de Valor de Compra", y = "Valor de Compra")

# Observação: O boxplot é excelente para identificar outliers e a distribuição central.

# Gráfico de percentuais por região
percentuais <- prop.table(table(DadosGás$`Regiao - Sigla`)) * 100
percentuais_df <- as.data.frame(percentuais)
colnames(percentuais_df) <- c("Regiao", "Percentual")

ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = paste0(round(Percentual, 2), "%")), vjust = -0.5, size = 4) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10))

# Observação: Adicionar percentuais aos gráficos melhora a interpretação.
# A rotação dos rótulos do eixo X foi ajustada para melhorar a legibilidade.

# ==============================
# 5. Salvando Resultados
# ==============================

# Salvar gráficos
ggsave("grafico_regiao.png", width = 10, height = 6, units = "in")
ggsave("grafico_percentual_regiao.png", width = 10, height = 6, units = "in")

# Observação: Gráficos devidamente salvos para a boa prática e documentar resultados.
# ==============================
# 6. Conclusão.
# ==============================
