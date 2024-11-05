
# Carregar pacotes necessários
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(psych)
library(GGally)
library(car)
library(ppcor)
library(reshape2)

# Carregar e Preparar Dados

# Carregar os dados do arquivo Excel
file_path <- "../dados/hsb2n.xlsx" # Substitua pelo caminho correto
dados <- read_excel(file_path)

# Renomear as colunas
colnames(dados) <- c("id", "genero", "raca", "clasocial", "tipescola", "programa",
                     "ler", "escrever", "matematica", "ciencias", "estsociais")

# Análise Univariada

# Gráficos de barras para variáveis categóricas
categoricas <- c("genero", "raca", "clasocial", "tipescola", "programa")
for (var in categoricas) {
    print(
      ggplot(dados, aes_string(x = var)) +
        geom_bar(fill = "skyblue") +
        labs(title = paste("Distribuição de", var), x = var, y = "Contagem") +
        theme_minimal()
    )
}
  
# Histogramas para variáveis numéricas
numericas <- c("ler", "escrever", "matematica", "ciencias", "estsociais")
for (var in numericas) {
    print(
      ggplot(dados, aes_string(x = var)) +
        geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
        labs(title = paste("Histograma de", var), x = var, y = "Frequência") +
        theme_minimal()
    )
}

# Análise Bivariada
# Gráficos de dispersão para variáveis numéricas
ggpairs(dados[, numericas],
        title = "Gráficos de Dispersão para Variáveis Numéricas",
        diag = list(continuous = "densityDiag"),
        axisLabels = "show")

# Análise de Correlação
# Matriz de correlação com significância
cor_matrix <- cor(dados[numericas], use = "complete.obs")
cor_test <- psych::corr.test(dados[numericas])
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", 
         p.mat = cor_test$p, sig.level = 0.05, insig = "blank")

# Modelagem Estatística
# Regressão Múltipla
modelo_multiplo <- lm(escrever ~ ler + matematica + ciencias + estsociais, data = dados)
summary(modelo_multiplo)

# ANOVA para verificar diferenças de desempenho entre grupos de 'programa'
anova_programa <- aov(escrever ~ programa, data = dados)
summary(anova_programa)

# Para analisar graficamente o ajuste e a qualidade do modelo de regressão múltipla em R,
# podemos utilizar gráficos de diagnóstico, como:

# 1.Gráficos de Resíduos: Avaliar suposições de linearidade, homocedasticidade e normalidade.
# 2.Gráficos de Componentes e Resíduos: Visualizar o impacto de cada variável preditora.
# 3. Gráficos de Valores Previstos vs. Observados: Comparar valores reais e ajustados.

# Aqui está o código para cada gráfico usando a regressão múltipla aplicada ao modelo modelo_multiplo.

# Código para Gráficos de Diagnóstico da Regressão Múltipla

# Ajuste do modelo de regressão múltipla

modelo_multiplo <- lm(escrever ~ ler + matematica + ciencias + estsociais, data = dados)

# 1. Gráficos de Diagnóstico Básico (4 em 1)
par(mfrow = c(2, 2))  # Ajusta layout para 4 gráficos em 1
plot(modelo_multiplo)

# Descrição dos gráficos:
# - Resíduos vs Ajustados: Avalia a homocedasticidade (distribuição uniforme dos resíduos).
# - QQ Plot: Avalia a normalidade dos resíduos.
# - Scale-Location: Avalia a homocedasticidade.
# - Resíduos vs Leverage: Identifica outliers com alta influência.

# 2. Gráfico de Componentes e Resíduos para cada variável preditora
crPlots(modelo_multiplo)  # Requer o pacote 'car'

# 3. Gráfico de Previsão vs Observado
# Comparando valores reais de 'escrever' com valores previstos pelo modelo
dados$previsao <- predict(modelo_multiplo)

ggplot(dados, aes(x = escrever, y = previsao)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Valores Observados vs Previstos",
       x = "Valores Observados (escrever)",
       y = "Valores Previstos") +
  theme_minimal()

# 4. Gráfico de Resíduos Padronizados
dados$residuos_padronizados <- rstandard(modelo_multiplo)

ggplot(dados, aes(x = previsao, y = residuos_padronizados)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Resíduos Padronizados vs Valores Previstos",
       x = "Valores Previstos",
       y = "Resíduos Padronizados") +
  theme_minimal()

# Descrição dos Gráficos

# Gráficos de Diagnóstico (4 em 1): Esses gráficos ajudam a verificar:
# - Resíduos vs Ajustados: Detecta problemas de homocedasticidade.
# - QQ Plot: Verifica a normalidade dos resíduos.
# - Scale-Location: Confirma se a variabilidade dos resíduos é constante.
# - Resíduos vs Leverage: Identifica pontos influentes (outliers).

# Gráficos de Componentes e Resíduos (crPlots):
# - Mostrma o impacto individual de cada variável preditora sobre a resposta, 
#   ajustando o efeito das demais variáveis.

# Valores Observados vs. Previstos:
# - Ajuda a verificar a precisão das previsões. 
#   O ideal é que os pontos estejam próximos da linha vermelha (linha 1:1).

# Resíduos Padronizados vs. Valores Previstos:
# - Verifica a homocedasticidade e identifica possíveis padrões nos resíduos. 
#   A ausência de padrão indica um bom ajuste do modelo.

# Esses gráficos fornecem uma visão completa sobre a qualidade do ajuste do modelo 
# de regressão múltipla e ajudam a identificar quaisquer violações das suposições do modelo.


# Conclusão
# Os resultados destacam a importância das variáveis ler e programa para o desempenho acadêmico 
# em escrita, entre outros insights significativos.
