# CÁLCULO DA PRÉVIA (COM APLICAÇÃO DE PROPHET)
require(magrittr)
require(dplyr)
require(tidyr)
require(prophet)
require(lubridate)
rm(list = ls())
tempo <- proc.time()
# STRINGS
dataCorteInc <- as.Date("2016-01-01")
dataCorteFim <- Sys.Date()
dataFinalPrevisao <- dmy(paste(days_in_month(month(Sys.Date())), month(dataCorteFim), year(dataCorteFim), sep = "-"))
passosAdiante <- as.numeric(dataFinalPrevisao - dataCorteFim)
# BASES ----
setwd("")
baseR <- read.table("baseR.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
feriados <- read.table("Feriados.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

  # AJUSTE DATA ----
baseR$Data <- as.Date(baseR$Data, format = "%d/%m/%Y")
colnames(baseR)[which(colnames(baseR) == "Descrição.Grupo.Prestador.Pagamento")] <- "Prestador.Pagamento"
baseR <- baseR[baseR$Linha.de.Atendimento != "-", ]
baseR <- baseR[baseR$Data >= dataCorteInc & baseR$Data <= dataCorteFim,]
rho <- 1
  # COMPILAR POR DIA ----
baseR <- baseR %>%
  dplyr::group_by(Data, Prestador.Pagamento, Linha.de.Atendimento) %>%
  dplyr::summarise(Custo = sum(Custo.Total)) %>%
  dplyr::arrange(Data) %>%
  # tidyr::spread(Linha.de.Atendimento, Custo) %>%
  as.data.frame

  # GERAR LISTAS COM POSSIBILIDADES ÚNICAS ----
    # MESCLAR DATAS POSSÍVEIS (PARA INCLUIR ONDE CUSTO É ZERO)
datasReal <- data.frame(Data = seq(dataCorteInc, dataCorteFim, by = 'd'))
combUnicas <- unique(
  data.frame(
    baseR$Prestador.Pagamento, 
    baseR$Linha.de.Atendimento, 
    stringsAsFactors = FALSE
  )
)

baseLista <- vector("list", nrow(combUnicas))
for (w in seq_len(nrow(combUnicas))) {
  baseLista[[w]] <- data.frame(
    Prestador.Pagamento = combUnicas[w, 1],
    Linha.de.Atendimento = combUnicas[w, 2],
    baseR %>%
      filter(
        Prestador.Pagamento == combUnicas[w, 1],
        Linha.de.Atendimento == combUnicas[w, 2]
      ) %>%
      select(Data, Custo) %>%
      as.data.frame %>% 
      merge(datasReal, ., all.x = TRUE),
    stringsAsFactors = FALSE
  )
}

# MODELAGEM ----
  # GERAR VETOR DE FERIADOS ----
feriados$Feriados <- as.Date(feriados$Feriados, format = "%d/%m/%Y")
colnames(feriados) <- c("holiday", "ds")

  # GERAR BASE NO FORMATO PARA PREVISÕES ----
baseModelo <- lapply(baseLista, function(x) 
  data.frame(
    ds = x$Data,
    y = x$Custo
  )
)
  # SUBSTITUIR NA POR 0
for (w in seq_along(baseModelo)) {
  baseModelo[[w]]$y[is.na(baseModelo[[w]]$y)] <- 0
}

  # MODELAR ----
listaModelos <- lapply(baseModelo, function(x) {
  prophet(
    df = x,
    growth = "linear",
    n.changepoints = 10,
    yearly.seasonality = TRUE,
    weekly.seasonality = TRUE,
    daily.seasonality = TRUE,
    holidays = feriados
  )
})

  # PREVER ----
listaPrevisao <- lapply(listaModelos, function(w) {
  make_future_dataframe(w, periods = passosAdiante) %>%
    predict(w, .)
})

  # VERIFICAR ERRO MÉDIO PARA O PERÍODO A PREVER ----
erroMedio <- mapply(function(x, y) 
  y %>%
    select(ds, yhat) %>%
    merge(., x) %>%
    mutate(
      Ano = year(ds),
      Mes = month(ds),
      Dia = day(ds)
    ) %>%
    # FILTRO PERIODOS REF ERRO CORRIGIR
    filter(
      Ano == year(dataFinalPrevisao) &
      Mes == month(dataFinalPrevisao) #&
      #Dia == day(dataFinalPrevisao)
    ) %>%
    group_by(Ano) %>%
    summarise(
      ErroMedio = (sum(yhat) - sum(y + 1)) / sum(y + 1),
      ErroP45 = quantile((yhat - y + 1) / (y + 1), 0.45)
    ),
  x = baseModelo, y = listaPrevisao, SIMPLIFY = FALSE
)


  # GERAR DATA.FRAME COM PREVISÕES ----
intervaloPrevisao <- seq(nrow(listaPrevisao[[1]]) - passosAdiante + 1, nrow(listaPrevisao[[1]]))
previsaoFinal <- vector("list", length(listaPrevisao))
for (z in seq_along(previsaoFinal)) {
  previsaoFinal[[z]] <- data.frame(
    Data = seq.Date(dataCorteFim + 1, dataFinalPrevisao, by = 1),
    Prestador.Pagamento = combUnicas[z, 1],
    Linha.de.Atendimento = combUnicas[z, 2],
    Custo = listaPrevisao[[z]]$yhat[intervaloPrevisao] * (1 - as.numeric(erroMedio[[z]][2]) * -1), # 2 é média e 3 é percentil
    stringsAsFactors = FALSE
  )
}

previsaoFinal <- do.call(rbind, previsaoFinal)
previsaoFinal$Custo <- previsaoFinal$Custo * rho
write.table(previsaoFinal, "Resultado.csv", sep = ";", row.names = FALSE)


# GERAR DATA.FRAME REALIZADO MÊS ----
realizadoMes <- baseR %>%
  mutate(
    Ano = year(Data),
    Mes = month(Data)
  ) %>%
  filter(
    (Ano == year(dataFinalPrevisao - 32) & Mes == month(dataFinalPrevisao - 32)) |
    (Ano == year(dataFinalPrevisao) & Mes == month(dataFinalPrevisao))
  ) %>%
  dplyr::group_by(Data, Prestador.Pagamento, Linha.de.Atendimento) %>%
  dplyr::summarise(Custo = sum(Custo)) %>%
  dplyr::arrange(Data) %>%
  # tidyr::spread(Linha.de.Atendimento, Custo) %>%
  as.data.frame

write.table(realizadoMes, "Realizado.csv", sep = ";", row.names = FALSE)

proc.time() - tempo


# EXEMPLO DE MODELAGEM ----
teste <- baseModelo[[1]]
teste$y[is.na(teste$y)] <- 0
modelo <- prophet(
  df = teste,
  growth = "linear",
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  holidays = feriados
)

futuro <- make_future_dataframe(modelo, periods = 9)
previsao <- predict(modelo, futuro)
plot(modelo, previsao)
