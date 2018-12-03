library(tidyverse)
library(plotly)
library(reshape2)
rm(list = ls())
# TABELA PRICE ----
funcaoRetornaJurosPrice <- function(i, n, pv = 100) {
  pmt <- pv * ((1 + i) ^ n * i ) * ((1 + i) ^ n - 1) ^ -1 
  retorno <- n * pmt - pv
return(retorno)
}

# MATRIZ POSSIBILIDADES RETORNO ----
pv <- 100
juros <- seq(0.04, 0.12, by = 0.01)
parcelas <- seq(1, 15, by = 1)
df <- expand.grid(pv, juros, parcelas)
colnames(df) <- c('pv', 'juros', "parcelas")
  # RETORNO
df$retorno <- apply(df, 1, function (x) {
  funcaoRetornaJurosPrice(pv = x[1], i = x[2], n = x[3])
})

df <- within(df, {
  retornoMensal <- retorno / parcelas
  jurosFator <- as.factor(juros)
  lRetornoMensal <- log(retorno / parcelas)
})

# PLOTAR ----
ggplot(df, aes(y = retornoMensal, x = parcelas, colour = jurosFator)) +
  geom_line()


# DETERMINAR CURVA DE PONTOS DE INFLEXÃO ----
# criar equação do retorno mensal em função de parcelas e juros, plotar o comportamento dos pontos de inflexão
funcaoRetornoMensal <- function(i, n, pv = 100) {
  return( ((pv * ((1 + i) ^ n * i ) * ((1 + i) ^ n - 1) ^ -1) * n - pv ) / n )
}

funcaoSubstJurosParcelas <- function(io, no, iDi = 0.01) {
  ponto <- funcaoRetornoMensal(i = io, n = no)
  noAdj <- no + 1
  incrm <- funcaoRetornoMensal(i = io - iDi, n = no) - ponto
  while ( round(incrm, 1) != 0 ) {
    incrm <- funcaoRetornoMensal(i = io - iDi, n = noAdj) - ponto
    noAdj <- noAdj + 1
  }
  resultado <- data.frame(
    Juros = io, Parcelas = no, IncrmtoParcelas = noAdj
  )
return(resultado)
}

dfPlot <- expand.grid(pv, juros, parcelas)
colnames(dfPlot) <- c('pv', 'juros', "parcelas")
dfPlot <- dfPlot[dfPlot$parcelas > 2, c(2, 3)]
dfPlot <- split(dfPlot, f = list(dfPlot$juros, dfPlot$parcelas))

dfPlot <- lapply(dfPlot, function(x) {
  data.frame(
    funcaoSubstJurosParcelas(io = x$juros, no = x$parcelas),
    JurosMensal = funcaoRetornoMensal(i = x$juros, n = x$parcelas)
  )
}) %>%
  do.call(rbind, .)


# SCATTER 3D  ----
  # VALORES PARA PLANO
planoScatter <- dfPlot
planoScatter <- acast(planoScatter, Parcelas ~ Juros, value.var = "IncrmtoParcelas")
  # PLOT 
scatter3D <- plot_ly(dfPlot,
                             x = ~Juros, 
                             y = ~Parcelas, 
                             z = ~IncrmtoParcelas, 
                             type = "scatter3d", mode = "markers", alpha = 0.5, 
                             hoverinfo = 'text', 
                             text = ~paste('</br> Juros Mensal: ', round(JurosMensal, 3),
                                           '</br> Número Parcelas: ', round(Parcelas, 0),
                                           '</br> Juros: ', round(Juros, 3),
                                           '</br> Incremento Parcelas: ', round(IncrmtoParcelas, 3))
) %>% 
  layout(
    title = "Incremento N para 'i - 0.01'",
    scene = list(
      xaxis = list(title = "Juros"),
      yaxis = list(title = "Parcelas "),
      zaxis = list(title = "Parcelas a mais para compensar redução juros")
    )) 
  # ADICIONAR PLANO AO GRÁFICO
scatter3D <- add_trace(p = scatter3D,
                               z = planoScatter,
                               x = eixoX,
                               y = eixoY, type = 'surface', alpha = 0.2
)

scatter3D

# APENAS PLANO
plot_ly(z = ~planoScatter) %>%
  add_surface()
