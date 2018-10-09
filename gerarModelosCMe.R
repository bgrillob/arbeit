# PREVISÃO CUSTO PENDENTE - AUTORIZAÇÕES
  # SEPARAR POR STATUS:
    # (I) PENDENTE
    # (II) AUTORIZADA 
    # (III) DIGITADA
  # FILTRAR POR SITUAÇÃO:
    # (I) SEM CONTA
    # (II) HM
  # ESTIMAR CUSTO MÉDIO POR COMBINAÇÃO DE PROCEDIMENTOS
    # INFOS:
      # QUANTIDADE GUIAS 
      # MÉDIA
      # DESVIO PADRÃO
  # FORMATO DE APRESENTAÇÃO:
    # CUSTO TOTAL PENDENTE
    # SEGREGAR POR:
      # (I) COM OCORRÊNCIA HISTÓRICA
      # (II) SEM OCORRÊNCIA HISTÓRICA
library(tidyverse)
library(parallel)
library(caret)
setwd("/root/Desktop/zBruno_Grillo/Previsão Custo Autorizado/")
# BASES ----
  # BASES: CUSTO HISTÓRICO (REGREDIR) ----
baseCustoHist <- readr::read_csv2("/root/Desktop/QlikView/Custos/Base_Custos_Mensal_18meses.csv")
colnames(baseCustoHist) <- make.names(colnames(baseCustoHist))

  # BASES: AUTORIZAÇÃO HISTÓRICA (REGREDIR) ----
baseAutorizadaHist <- readr::read_csv2("/root/Desktop/QlikView/Custos/Base_Controle_Autorizacoes_Mensal.csv")
colnames(baseAutorizadaHist) <- make.names(colnames(baseAutorizadaHist))
baseAutorizadaHist <- baseAutorizadaHist %>%
  arrange(Codigo.Movimento...AT) %>%
  mutate(
    Data.de.Emissao.da.Guia = as.Date(Data.de.Emissao.da.Guia, format = "%d/%m/%Y")
  ) %>%
  filter(Procedimento.Insumo == "PROCEDIMENTO")

  # BASES: CLASSIFICAÇÃO DE CADA GUIA AT ----
classificacaoGuia <- merge(baseAutorizadaHist, 
                           {baseCustoHist %>% 
                               select(Chave.Atendimento, Chave.Guia.AT)
                           }) %>%
  group_by(Chave.Atendimento) %>%
  summarise(
    Chave.Guia.AT = min(Chave.Guia.AT[Data.de.Emissao.da.Guia == min(Data.de.Emissao.da.Guia)])
  ) %>%
  merge(., {
    baseAutorizadaHist %>%
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

  # BASES: FREQ OCORRENCIA (FILTRAR APENAS COM MAIS QUE N OCORRÊNCIAS) ----
freqOCorrencia <- classificacaoGuia %>%
  group_by(Classificador) %>%
  summarise(
    Freq = n_distinct(Chave.Atendimento)
  ) %>%
  arrange(-Freq) %>%
  filter(
    Freq >= 20
  )

  # BASES: FILTRAR CLASSIFIC ----
classificacaoGuia <- classificacaoGuia %>%
  filter(
    Classificador %in% freqOCorrencia$Classificador
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

  # BASES: AJUSTAR FORMATO DA BASE PARA MATRIZ RETANGULAR (REGRESSÃO)
funcaoAjustarVariaveis <- function(df) {
  library(tidyverse)
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


  # BASES: REMOVER BASES ORIGINAIS ----
rm(list = c("baseAutorizadaHist", "baseCustoHist"))


# MODELAGEM ----
  # MODELAGEM: FUNÇÃO MODELO ----
df <- baseLista[[w]]
funcaoModelo <- function(df, nTam = 0.8, nMax = 2500) { # RETORNA MODELO E ESTATÍSTICAS DOS RESÍDUOS
  library(tidyverse)
  library(caret)
  # FUNÇAO MODELO: AJUSTE BASE PARA MODELO ----
  baseModelo <- df %>% 
    as.data.frame %>%
    select(-c(Classificador, Chave.Atendimento)) %>%
    mutate(
      Produto = as.factor(Produto),
      Sexo.Beneficiario = as.factor(Sexo.Beneficiario),
      ClassificacaoPrestador = as.factor(ClassificacaoPrestador),
      logCusto = log(Custo)
    )
  
    # DETERMINAR VARIAVEIS A EMPREGAR ----
  variaveisUsar <- baseModelo %>%
    select(-c(Custo, logCusto)) %>%
    lapply(., nlevels) %>%
    unlist %>%
    {names(.)[. != 1]} # pegar apenas explicativas 
  
    # FUNÇAO MODELO: AMOSTRAGEM ----
  nLin <- nrow(baseModelo)
  nAmostra <- ifelse(nLin * 0.8 > nMax, nMax, nLin * 0.8)
  refTreino <- sample(seq(nLin), size = nAmostra)
  baseModeloTreino <- baseModelo[refTreino, ]
  baseModeloTeste <- baseModelo[-refTreino, ]
  
    # FUNÇAO MODELO: CONTROLES ----
  controle <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 2
  )
    # FUNÇAO MODELO: MODELOS (LOG E NIVEL) ----
  modeloRF <- train(as.formula(paste("Custo ~", paste(variaveisUsar, collapse = " + "))),
                    data = baseModeloTreino,
                    method = "rf",
                    metric = "RMSE"
  )
  modeloRFlog <- train(as.formula(paste("logCusto ~", paste(variaveisUsar, collapse = " + "))),
                    data = baseModeloTreino,
                    method = "rf",
                    metric = "RMSE"
  )
  # FUNÇAO MODELO: ESCOLHA MÉTODO VENCEDOR ----
    # RESIDUOS
  comparacaoResiduos <- data.frame(
    baseModeloTeste,
    yHatNivel = predict(modeloRF, baseModeloTeste),
    yHatLog = exp(predict(modeloRFlog, baseModeloTeste)) * exp(var(residuals(modeloRFlog))/2)
  ) %>%
    mutate(
      uHatNivel = yHatNivel - Custo,
      uHatLog = yHatLog - Custo
    ) 
    # COMPARAÇAO
  erroMedioAbs <- data.frame(
    Nivel = mean(abs(comparacaoResiduos$uHatNivel)), 
    Log = mean(abs(comparacaoResiduos$uHatLog))
  )
  # FUNÇAO MODELO: RESULTADO ----
  if (erroMedioAbs$Log < erroMedioAbs$Nivel) {
    resultado <- list(
      modeloFinal = modeloRF,
      modeloLog = TRUE,
      dfAnaliseResidual = {
        comparacaoResiduos %>%
          select(-c(yHatLog, uHatLog))  
      }
    )
  } else {
    resultado <- list(
      modeloFinal = modeloRFlog,
      modeloLog = FALSE,
      dfAnaliseResidual = {
        comparacaoResiduos %>%
          select(-c(yHatNivel, uHatNivel))  
      }
    )
  }
return(resultado)
}
cl <- makeCluster(detectCores())
clusterExport(cl, "funcaoModelo")
listaModelos <- parLapply(cl, baseLista, function(x) funcaoModelo(df = x))
stopCluster(cl)

save(listaModelos, "modelos.RData")
