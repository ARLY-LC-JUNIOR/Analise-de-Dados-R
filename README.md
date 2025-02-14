# Análise de Dados Gasolina no Brasil

Este repositório contém uma análise exploratória de dados sobre preços de combustíveis no Brasil, com foco em distribuição regional, estatísticas descritivas e visualizações impactantes. O objetivo é fornecer insights claros sobre os padrões de preços de gasolina por região, estado e cidade.

## 🌟 Por Que Este Projeto?

A análise de preços de combustíveis é essencial para entender as disparidades regionais e identificar tendências econômicas. Este projeto demonstra:

- **Habilidades Técnicas**: Manipulação de dados, limpeza, análise estatística e visualização usando R.
- **Pensamento Crítico**: Identificação de padrões, outliers e insights acionáveis.
- **Comunicação de Dados**: Apresentação clara de resultados por meio de gráficos e tabelas.

## 🛠️ Ferramentas Utilizadas

- **Linguagem de Programação**: R
- **Bibliotecas Principais**:
  - `readr`: Leitura de dados.
  - `dplyr`: Manipulação e transformação de dados.
  - `ggplot2`: Criação de visualizações.
  - `knitr`: Geração de tabelas formatadas.
- **Ambiente de Desenvolvimento**: RStudio e VS Code.
- **Controle de Versão**: Git e GitHub.

## 📂 Estrutura do Projeto

```
O repositório está organizado da seguinte forma:
Analise_dados_gasolina.R/
├── data/ # Dados brutos utilizados no projeto
├── baseEstudoRca2004-01.csv
├── scripts/ # Scripts R organizados por etapas
├── 01_data_cleaning.R # Limpeza dos dados
├── 02_analysis.R # Análises descritivas
├── 03_visualizations.R # Criação de gráficos
├── output/ # Resultados gerados (gráficos, tabelas, etc.)
├── grafico_regiao.png
├── grafico_percentual_regiao.png
├── .gitignore # Arquivos ignorados pelo Git
├── README.md # Documentação do projet
└──
```

# 📊 Resultados Principais

## 1. Distribuição de Preços por Região  
**Gráfico de Barras**  

**Descrição:**  
Este gráfico mostra a contagem de entradas por região, destacando a predominância da região Sudeste (SE) nos dados.  

**Interpretação:**  
A região Sudeste possui mais postos de gasolina registrados, refletindo sua maior densidade populacional e atividade econômica.  


**Interpretação:**  
Essa distribuição reflete as diferenças regionais no consumo de combustíveis e na infraestrutura de abastecimento.

## 📈 Insights Obtidos
Disparidade Regional: Os preços de gasolina variam significativamente entre regiões, com a região Sudeste apresentando maior concentração de dados.
Outliers: Algumas cidades apresentam valores extremos de preços, sugerindo possíveis irregularidades ou diferenças locais.
Tendências Econômicas: As regiões com maior densidade populacional tendem a ter preços mais competitivos.  

**Código para Gerar o Gráfico:**  

```r
  ggplot(percentuais_df, aes(x = Regiao, y = Percentual)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = paste0(round(Percentual, 2), "%")), vjust = -0.5, size = 4) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = "Distribuição Percentual por Região", x = "Região", y = "Percentual (%)")

