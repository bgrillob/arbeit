# IMPORTAR MODELOS DO SCRIPT ANTERIOR 
load("modelos.RData")
# BASES: GUIAS A DEDUZIR (CUSTO JÁ RECONHECIDO) ----
baseCustoReconhecido <- readr::read_csv2("Base_Custos_Mensal_18meses.csv", guess_max = 20000)
colnames(baseCustoReconhecido) <- make.names(colnames(baseCustoReconhecido))
# CUSTO POR GUIA JÁ RECONHECIDA ----
baseCustoReconhecido <- baseCustoReconhecido %>%
  dplyr::group_by(Chave.Atendimento, Chave.Guia.AT) %>%
  dplyr::summarise(
    CustoReconhecido = sum(Valor.Real.Pago)
  )
# BASES: AUTORIZAÇÕES A ESTIMAR (ÚLTIMOS 6 MESES) ----
baseGuiasAutorizadas <- readr::read_csv2("Base_Controle_Autorizacoes_Diario.csv")
colnames(baseGuiasAutorizadas) <- make.names(colnames(baseGuiasAutorizadas))
# FILTRAR PROCEDIMENTOS
baseGuiasAutorizadas <- baseGuiasAutorizadas %>%
  arrange(Codigo.Movimento...AT) %>%
  mutate(
    Data.de.Emissao.da.Guia = as.Date(Data.de.Emissao.da.Guia, format = "%d/%m/%Y")
  ) %>%
  filter(Procedimento.Insumo == "PROCEDIMENTO")

# CLASSIFICAR PROCEDIMENTOS
classificacaoGuia <- merge(baseGuiasAutorizadas, 
                           {baseCustoHist %>% 
                               select(Chave.Atendimento, Chave.Guia.AT)
                           }) %>%
  group_by(Chave.Atendimento) %>%
  summarise(
    Chave.Guia.AT = min(Chave.Guia.AT[Data.de.Emissao.da.Guia == min(Data.de.Emissao.da.Guia)])
  ) %>%
  merge(., {
    baseGuiasAutorizadas %>%
      select(Chave.Guia.AT, Codigo.Movimento...AT)
  }) %>%
  arrange(Codigo.Movimento...AT) %>%
  group_by(
    Chave.Atendimento
  ) %>%
  summarise(
    Classificador = paste(Codigo.Movimento...AT, collapse = "_"),
    GuiasAt = paste(Chave.Guia.AT, collapse = "_")
  )


# BASES: IMPORTAR REFERÊNCIA DOS PRESTADORES ----
classificacaoPrestador <- read.table("baseRefPrestador.csv", 
                                     header = TRUE, sep = ";", stringsAsFactors = FALSE)

# BASES: MESCLAR BASES E SEPARAR POR CLASSIFICAÇÃO ----
baseR <- merge(baseCustoHist, classificacaoGuia) %>%
  merge(., classificacaoPrestador, all.x = TRUE) %>%
  mutate(
    ClassificacaoPrestador = replace_na(ClassificacaoPrestador, "Sem Classificacao")
  )

baseLista <- split(baseR, f = baseR$Classificador)

# BASES: AJUSTAR FORMATO DA BASE PARA MATRIZ RETANGULAR (REGRESSÃO) ----
funcaoAjustarVariaveis <- function(df) {
  df <- df %>% 
    filter(
      Valor.Real.Pago > 3
    ) %>%
    within(., {
      Ano <- readr::parse_number(Período)
      Data <- as.Date(paste(Ano, 12, 31, sep = "-"))
      Idade <- as.numeric(difftime(Data, lubridate::dmy_hms(Data.Nascimento.BENEF), units = "days") / 365)
    }) %>%
    group_by(
      Classificador, Chave.Atendimento, Produto, Idade, Sexo.Beneficiario, ClassificacaoPrestador
    ) %>%
    summarise(
      Custo = sum(Valor.Real.Pago)
    )
  return(df)
}

cl <- makeCluster(detectCores())
clusterExport(cl, "funcaoAjustarVariaveis")
baseLista <- parLapply(cl, baseLista, function(x) funcaoAjustarVariaveis(df = x))
stopCluster(cl)


# APLICAR MODELO----
procedimentos <- names(baseLista)
for(w in procedimentos) {
  if (!listaModelos[[w]]$modeloLog) {
    baseLista[[w]]$Previsto <- predict(listaModelos[[w]]$modeloFinal, baseLista[[w]])
  } else {
    baseLista[[w]]$Previsto <- exp(predict(listaModelos[[w]]$modeloFinal, baseLista[[w]])) * exp(var(residuals(listaModelos[[w]]$modeloFinal))/2)
  }
}
# SUMARIZAR 
baseAutorizada <- do.call(rbind, baseLista) %>%
  merge(., baseCustoReconhecido, all.x = TRUE) %>%
  mutate(
    CustoNaoReconhecido = ifelse(Previsto - CustoReconhecido < 0, 0, Previsto - CustoReconhecido)
  ) %>%
  group_by(Produto, Idade, Sexo.Beneficiario) %>%
  summarise(
    CustoNaoReconhecido = sum(CustoNaoReconhecido)
  )

# SAÍDA
write.xlsx(baseAutorizada, file = "IBNR.xlsx", sheetName = "Custo Não Reconhecido")

 
