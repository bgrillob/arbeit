options(java.parameters = "- Xmx2048m")

library(tidyverse)
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(xlsx)

rm(list = ls())
setwd("/root/Desktop/zBruno_Grillo/Internações/")
dir_ref <- "Bases Ref/"
dir_result <- "Resultados/"
# BASES ----
tempo <- proc.time()
nBaseR <- c("/root/Desktop/QlikView/Custos/Base_Custos_Quinzenal_anosAnteriorAtual.csv")
baseR <- vector("list", length(nBaseR))
tempo_dcr <- proc.time()
for (arquivos in seq_along(nBaseR)) {
  baseR[[arquivos]] <- read.table(nBaseR[[arquivos]], 
    header = TRUE, sep = ";", stringsAsFactors = FALSE, dec = ",", encoding = "UTF-8-BOM", quote = "")
}
baseR <- do.call(rbind, baseR)

  # AJUSTE NOMES
colnames(baseR)[colnames(baseR) == "Descrição.Principio.Ativo"] <- "Descricao.Principio.Ativo" # TIRAR CARACTERES ESPECIAIS PARA MERGE
colnames(baseR)[colnames(baseR) == "Descrição.Grupo.Prestador.Pagamento"] <- "Descricao.Grupo.Prestador.Pagamento"
colnames(baseR)[colnames(baseR) == "Nível.2"] <- "Nivel.2"
colnames(baseR)[colnames(baseR) == "Quantidade"] <- "Quantidade.Movimentos"
colnames(baseR)[colnames(baseR) == "Valor.Real.Pago"] <- "Custo.Total"
  # AJUSTES PRELIMINARES
baseR <- within(baseR, {
  Chave.Guia.Principal <- as.character(Chave.Guia.Principal)
  Codigo.Movimento <- as.character(Codigo.Movimento)
  Custo.Total <- abs(Custo.Total)
  Quantidade.Movimentos <- abs(Quantidade.Movimentos)
  Nome.Abrev..Prestador.Principal <- gsub("\\/", " ", Nome.Abrev..Prestador.Principal)
})

  # SUBSTITUIR CÓDIGOS DE-PARA ----
tempo <- proc.time()
nomes_arquivos <- c("depara1.csv", "depara2.csv", "depara3.csv", "depara4.csv", "depara5.csv", "depara6.csv")
deparas <- vector("list", length(nomes_arquivos))
for (w in seq_along(nomes_arquivos)) {
  deparas[[w]] <- read.table(paste0(dir_ref, nomes_arquivos[[w]]), header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM", quote = "")
}
deparas <- do.call(rbind, deparas)
codigos <- data.frame(
  Codigo.Movimento = apply(deparas[, 1:2], 1, function(x) paste(x[1], sprintf("%08d", x[2]), sep = "")),
  Novo = apply(deparas[, 3:4], 1, function(y) paste(y[1], sprintf("%08d", y[2]), sep = "")),
  stringsAsFactors = FALSE
)

baseR <- merge(baseR, codigos, all.x = TRUE) %>%
  within(., {
    Codigo.Movimento[!is.na(Novo)] <- Novo[!is.na(Novo)]
  })

  # AGRUPAR POR CODIGO MOVIMENTO (AGRUPA CÓDIGOS ALTERADOS)
variaveisAgrupar <- c(
  "Ano", "Chave.Beneficiario", "Sexo.Beneficiario", "Data.Nascimento.BENEF",
  "Produto", "Chave.Guia.Principal", "Nome.Abrev..Prestador.Principal", 
  "Nivel.2", "Codigo.Movimento", "Descricao.Procedimento.Insumo", "Descricao.Principio.Ativo",
  "Classificacao.Despesa.Insumo"
)
baseR <- baseR %>%
  group_by_at(vars(one_of(variaveisAgrupar))) %>%
  summarise(
    Custo.Total = sum(Custo.Total), 
    Quantidade.Movimentos = sum(Quantidade.Movimentos)) %>%
  as.data.frame

paste("Tempo de-para: ", floor((proc.time() - tempo)[3]), "s", collapse = "")


  # ALTERAR DATA NASCIMENTO PARA IDADE ----
tempo <- proc.time()
baseR <- within(baseR, {
  Idade <- as.numeric(difftime(Sys.Date(), as.Date(Data.Nascimento.BENEF, "%d/%m/%Y"), "days")) / 365
  FaixaEtaria <- cut(Idade, 
    breaks = c(0, 4, 9, 19, 29, 39, 49, 59, 69, 950), 
    labels = c("00-04", "05-09", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"))
})
paste("Tempo ajuste idade: ", floor((proc.time() - tempo)[3]), "s", collapse = "")


# ANÁLISE EXPLORATÓRIA ----
  # MAIORES VARIAÇÕES ----
sumarioMaioresVariacoes <- baseR %>%
  group_by(Nome.Abrev..Prestador.Principal, Ano) %>%
  summarise(
    Custo = sum(Custo.Total)
  ) %>%
  mutate(
    Ano = paste0("a", Ano)
  ) %>%
  spread(Ano, Custo) %>%
  na.omit %>%
  mutate(
    Variacao = a2018 - a2017,
    VariacaoPerc = a2018 / a2017 - 1
  ) %>%
  arrange(-Variacao) %>%
  filter(Variacao > 700000) %>%
  as.data.frame

write.xlsx(sumarioMaioresVariacoes, "Maiores Variações.xlsx", row.names = FALSE, append = TRUE)


# FUNÇÃO CRIAR CESTA E COMPARAR ----
funcaoSinteseInternacao <- function(baseTeste, benef_t0 = 530085, benef_t1 = 523094, 
  t0 = 2017, t1 = 2018) {
  
  options(scipen = 9999, digits = 4)
  # NOMES
  refNomes <- unique(data.frame(
    Codigo.Movimento = baseTeste$Codigo.Movimento,
    Descricao.Procedimento.Insumo = baseTeste$Descricao.Procedimento.Insumo,
    Descricao.Principio.Ativo = baseTeste$Descricao.Principio.Ativo,
    Classificacao.Despesa.Insumo = baseTeste$Classificacao.Despesa.Insumo,
    stringsAsFactors = FALSE
  ))
  # BENEFICIARIOS
  baseBenef <- data.frame(Ano = c(t0, t1), Beneficiarios = c(benef_t0, benef_t1))
  # SÍNTESE INTERNAÇÃO ----
  sinteseInternacao <- baseTeste %>%
    group_by(Ano) %>%
    summarise(
      Custo = sum(Custo.Total),
      Guias = n_distinct(Chave.Guia.Principal),
      Usuarios = n_distinct(Chave.Beneficiario),
      Diarias = sum(Quantidade.Movimentos[Classificacao.Despesa.Insumo %in% 
        c("Acomodação", "Acomodação Trat Intensivo", "Acomodação Psiquiátrica")]
      )
    ) %>% 
    merge(baseBenef) %>%
    mutate(
      Propensao = Usuarios / Beneficiarios,
      TempoPermanencia = Diarias / Guias,
      CustoDiaria = Custo / Diarias
    ) %>% 
    merge(., {
      baseTeste %>% # BASE COM MEDIANA CESTA
        group_by(Ano, Chave.Guia.Principal) %>%
        summarise(
          Qtde = n_distinct(Codigo.Movimento),
          Custo = sum(Custo.Total)
        ) %>%
        group_by(
          Ano
        ) %>%
        summarise(
          MediaCestaQtd = mean(Qtde),
          Q1CestaQtd = quantile(Qtde, probs = 0.25),
          MedianaCestaQtd = quantile(Qtde, probs = 0.5),
          Q3CestaQtd = quantile(Qtde, probs = 0.75),
          MediaCestaCusto = mean(Custo),
          Q1CestaCusto = quantile(Custo, probs = 0.25),
          MedianaCestaCusto = quantile(Custo, probs = 0.5),
          Q3CestaCusto = quantile(Custo, probs = 0.75)
        )
    }) %>%
    merge(., {
      baseTeste %>% # BASE COM TAMANHO CESTA
        group_by(Ano) %>%
        summarise(TamanhoCesta = n_distinct(Codigo.Movimento))
      }
    ) %>%
    merge(., {
      baseTeste %>% # PROPORÇÃO DE UTI
        filter(
          Classificacao.Despesa.Insumo %in% c("Acomodação", "Acomodação Trat Intensivo")
        ) %>%
        group_by(Ano) %>%
        summarise(
          PropUTI = 
            sum(Quantidade.Movimentos[Classificacao.Despesa.Insumo == "Acomodação Trat Intensivo"]) / 
            sum(Quantidade.Movimentos),
          DiariasUTI = sum(Quantidade.Movimentos[Classificacao.Despesa.Insumo == "Acomodação Trat Intensivo"])
        )
    })
      # TRANSPOR E ORGANIZAR DISPOSIÇÃO
  nomesLin <- colnames(sinteseInternacao)
  sinteseInternacao <- split(sinteseInternacao, f = sinteseInternacao$Ano)
  sinteseT0 <- data.frame(sinteseInternacao[paste0(t0)]) %>% as.numeric
  sinteseT1 <- data.frame(sinteseInternacao[paste0(t1)]) %>% as.numeric
  varNominal <- sinteseT1 - sinteseT0 %>% as.numeric
  varPerc <- sinteseT1 / sinteseT0 - 1 %>% as.numeric
  
  sinteseInternacao <- data.frame(
    Descricao = nomesLin, t0 = sinteseT0, t1 = sinteseT1,
    VarNominal = varNominal, VarPerc = varPerc, stringsAsFactors = F
  )
  
  # EFEITOS ----
  matrizComp <- data.frame(
    Codigo.Movimento = unique(baseTeste$Codigo.Movimento), 
    stringsAsFactors = FALSE
    ) %>% 
    merge(., {
      baseTeste %>%
        filter(Ano == t0) %>%
        group_by(Codigo.Movimento) %>%
        summarise(
          CustoT0 = sum(Custo.Total),
          QuantidadeT0 = sum(Quantidade.Movimentos), 
          GuiasQueContemT0 = n_distinct(Chave.Guia.Principal)
        ) %>%
        mutate(
          PrecoT0 = CustoT0 / (QuantidadeT0 + 0.05),
          QtdePorGuiaContemT0 = QuantidadeT0 / GuiasQueContemT0,
          CustoGuiasContemT0 = CustoT0 / GuiasQueContemT0
        ) %>%
        select(
          -GuiasQueContemT0
        )
    }, all.x = TRUE) %>%
    merge(., {
      baseTeste %>%
        filter(Ano == t1) %>%
        group_by(Codigo.Movimento) %>%
        summarise(
          CustoT1 = sum(Custo.Total),
          QuantidadeT1 = sum(Quantidade.Movimentos), 
          GuiasQueContemT1 = n_distinct(Chave.Guia.Principal)
        ) %>%
        mutate(
          PrecoT1 = CustoT1 / (QuantidadeT1 + 0.05),
          QtdePorGuiaContemT1 = QuantidadeT1 / GuiasQueContemT1,
          CustoGuiasContemT1 = CustoT1 / GuiasQueContemT1
        ) %>%
        select(
          -GuiasQueContemT1
        )
    }, all.x = TRUE) %>%
    replace(is.na(.), 0) %>%
    mutate(
      VariacaoCusto = CustoT1 - CustoT0,
      EfeitoPreco = PrecoT1 * QuantidadeT0 - CustoT0,
      EfeitoQtde = PrecoT0 * QuantidadeT1 - CustoT0
    ) %>%
    merge(., refNomes)
  
    # EFEITOS: PRINCIPAIS EFEITO PREÇO ----
  sinteseTopPreco <- matrizComp %>%
    arrange(-EfeitoPreco) %>%
    select(
      Descricao.Procedimento.Insumo, Codigo.Movimento, 
      CustoT0, PrecoT0, QuantidadeT0,
      CustoT1, PrecoT1, QuantidadeT1, 
      EfeitoPreco
    ) %>%
    filter(
      QuantidadeT1 > 3 &
      QuantidadeT0 > 3
    ) %>%
    mutate(VarPreco = PrecoT1 / PrecoT0 - 1) 
  
    # EFEITOS: PRINCIPAIS EFEITO QUANTIDADE ----
  sinteseTopQtde <- matrizComp %>%
    arrange(-EfeitoQtde) %>%
    select(
      Descricao.Procedimento.Insumo, Codigo.Movimento, 
      CustoT0, PrecoT0, QuantidadeT0,
      CustoT1, PrecoT1, QuantidadeT1, 
      EfeitoQtde
    ) %>%
    filter(
      QuantidadeT1 > 3 &
      QuantidadeT0 > 3
    ) 
  
  # ITENS MAIS RECORRENTES ----
  incidenciaCesta <- baseTeste %>%
    group_by(Codigo.Movimento, Descricao.Procedimento.Insumo, Ano) %>%
    summarise(
      GuiasAparece = n_distinct(Chave.Guia.Principal)
    ) %>%
    merge(., {
      baseTeste %>%
        group_by(Ano) %>%
        summarise(
          GuiasTotal = n_distinct(Chave.Guia.Principal)
        )
    }) %>%
    mutate(
      Incidencia = GuiasAparece / GuiasTotal
    ) %>%
    arrange(-Incidencia)
    # INCIDÊNCIA POR ANO
  incidenciaT0 <- incidenciaCesta %>%
    filter(Ano == t0)
  incidenciaT1 <- incidenciaCesta %>%
    filter(Ano == t1)
  
    # ITENS MAIS RECORRENTES: VARIAÇÃO INCIDÊNCIA ----
  varIncidencia <- merge(incidenciaT0, incidenciaT1, 
    by = c("Codigo.Movimento", "Descricao.Procedimento.Insumo"), all = T) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    select(
      Codigo.Movimento, Descricao.Procedimento.Insumo, Incidencia.x, Incidencia.y
    ) %>%
    within(
      VarIncidencia <- Incidencia.y - Incidencia.x
    ) %>%
    arrange(-VarIncidencia) %>%
    set_colnames(c(
      "Codigo.Movimento", "Descricao.Procedimento.Insumo",
      paste0("Incidencia", t0), paste0("Incidencia", t1), "VarIncidencia"
    ))
  
  # MUDANÇAS ITENS ----
  codT0 <- unique(baseTeste$Codigo.Movimento[baseTeste$Ano == t0])
  codT1 <- unique(baseTeste$Codigo.Movimento[baseTeste$Ano == t1])
  codNovos <- codT1[!(codT1 %in% codT0)]
  codAntigos <- codT0[!(codT0 %in% codT1)]
    # MUDANÇAS ITENS: ITENS EM T1 QUE NÃO ESTAVAM EM T0 ----
  novosItens <- matrizComp %>% 
    filter(
      Codigo.Movimento %in% codNovos
    ) %>%
    select(
      Codigo.Movimento, Descricao.Procedimento.Insumo, Descricao.Principio.Ativo,
      CustoT1, QuantidadeT1
    ) %>%
    arrange(-CustoT1) 
    # MUDANÇAS ITENS: ITENS EM T0 QUE NÃO ESTÃO EM T1 ----
  antigosItens <- matrizComp %>% 
    filter(
      Codigo.Movimento %in% codAntigos
    ) %>%
    select(
      Codigo.Movimento, Descricao.Procedimento.Insumo, Descricao.Principio.Ativo,
      CustoT0, QuantidadeT0
    ) %>%
    arrange(-CustoT0) 
  
  # PRINCÍPIO ATIVO ----
    # PRINCÍPIO ATIVO: MAIORES VARIAÇÕES ----
  varPrincipioAtivo <- baseTeste %>%
    filter(
      Classificacao.Despesa.Insumo %in% c("SP/SADT", "Material de consumo", 
        "Medicamento", "OPME", "Kit Insumo", "Medicamento Quimioterápico",
        "Acomodação Trat Intensivo", "Acomodação", "Acomodação Acompanhante")
    ) %>%
    group_by(Ano, Descricao.Principio.Ativo) %>%
    summarise(
      Custo = sum(Custo.Total)
    ) %>%
    within(., {
      Ano <- paste0("c", Ano)
    }) %>%
    spread(Ano, value = Custo) %>%
    replace(is.na(.), 0) %>%
    mutate(
      Variacao = get(paste0("c", t1)) - get(paste0("c", t0))
    ) %>%
    arrange(-Variacao) %>%
    filter(Descricao.Principio.Ativo != "-")
  
    # FILTRAR TOP-N PRINCIPIOS ATIVO
  topPrincAtiv <- matrizComp %>%
    filter(Descricao.Principio.Ativo %in% varPrincipioAtivo$Descricao.Principio.Ativo) %>%
    merge(., {
      varPrincipioAtivo %>% select(Descricao.Principio.Ativo, Variacao)
    }) %>%
    arrange(-Variacao)
  
    # PRINCÍPIO ATIVO: MUDANÇA INCIDÊNCIA ----
  incidenciPrincAt <- baseTeste %>%
    group_by(Descricao.Principio.Ativo, Ano) %>%
    summarise(
      GuiasAparece = n_distinct(Chave.Guia.Principal),
      Custo = sum(Custo.Total)
    ) %>%
    mutate(
      CustoPorGuiaAparece = Custo / GuiasAparece
    ) %>%
    merge(., {
      baseTeste %>%
        group_by(Ano) %>%
        summarise(
          GuiasTotal = n_distinct(Chave.Guia.Principal)
        )
    }) %>%
    mutate(
      Incidencia = GuiasAparece / GuiasTotal
    ) %>%
    arrange(-Incidencia)
  # INCIDÊNCIA POR ANO
  incidenciaPrincAtT0 <- incidenciPrincAt %>%
    filter(Ano == t0)
  incidenciaPrincAtT1 <- incidenciPrincAt %>%
    filter(Ano == t1)
  
  varIncidenciaPrincAt <- merge(incidenciaPrincAtT0, incidenciaPrincAtT1, 
    by = c("Descricao.Principio.Ativo"), all = T) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    select(
      Descricao.Principio.Ativo, Incidencia.x, Incidencia.y,
      CustoPorGuiaAparece.x, CustoPorGuiaAparece.y
    ) %>%
    within(., {
      VarIncidencia <- Incidencia.y - Incidencia.x
      VarCustoGuiaAparece <- CustoPorGuiaAparece.y - CustoPorGuiaAparece.x
    }) %>%
    arrange(-VarIncidencia) %>%
    set_colnames(c(
      "Descricao.Principio.Ativo",
      paste0("Incidencia", t0), paste0("Incidencia", t1), 
      paste0("CustoPorGuiaAparece", t0), paste0("CustoPorGuiaAparece", t1), 
      "VarCustoGuiaAparece", "VarIncidencia"
    ))
  
  # DIÁRIAS ----
  baseDiarias <- baseTeste %>%
    filter(
      Classificacao.Despesa.Insumo %in% c("Acomodação", "Acomodação Trat Intensivo")
    ) %>%
    group_by(Ano, Chave.Guia.Principal) %>%
    summarise(Diarias = sum(Quantidade.Movimentos)) %>%
    within(., {
      Ano <- as.factor(Ano)
    })
      # GRÁFICO: HISTOGRAMA DIÁRIAS ----
  histogramaDiarias <- ggplot(baseDiarias, aes(x = Diarias, fill = Ano)) +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") + 
    ggtitle("Distribuição Diarias") + xlab("Diarias") + ylab("Guias") 
      
      # TABELA: SUMÁRIO DIÁRIAS ----
  sumarioDiarias <- baseDiarias %>%
    group_by(Ano) %>%
    summarise(
      Q1 = quantile(Diarias, probs = 0.1),
      Mediana = quantile(Diarias, probs = 0.5),
      Q3 = quantile(Diarias, probs = 0.75)
    ) %>%
    as.data.frame
  
  # GRÁFICOS: HISTOGRAMA MOVIMENTOS ÚNICOS ÚNICAS ----
  baseMovimentos <- baseTeste %>%
    group_by(Ano, Chave.Guia.Principal) %>%
    summarise(CodUnicos = n_distinct(Codigo.Movimento)) %>%
    within(., {
      Ano <- as.factor(Ano)
      CodTrunc <- ifelse(CodUnicos > 100, 100, CodUnicos)
    })
  
  histogramaMovimentos <- ggplot(baseMovimentos, aes(x = CodTrunc, fill = Ano)) +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") + 
    ggtitle("Distribuição Códigos Movimento") + xlab("Códigos Mov Únicos") + ylab("Guias") 
  
  # GRÁFICOS: HISTOGRAMA CUSTO POR GUIA ----
  baseCustoGuia <- baseTeste %>%
    group_by(Ano, Chave.Guia.Principal) %>%
    summarise(Custo = sum(Custo.Total)) %>%
    within(., {
      Ano <- as.factor(Ano)
      logCusto <- log(Custo + 1)
    })
  
  maxLog <- ceiling(max(baseCustoGuia$logCusto))
  eixoX <- seq(0, maxLog, by = 0.5)
  nomesEixoX <- paste(eixoX, "\n R$", round(exp(eixoX)))
  
  histogramaCustoGuia <- ggplot(baseCustoGuia, aes(x = logCusto, fill = Ano)) +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") + 
    ggtitle("Distribuição Custo Guias") + xlab("Custo Guia (log)") + ylab("Guias") +
    scale_x_continuous(breaks = eixoX, labels = nomesEixoX) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  
  # RESULTADOS!! ----
  resultado <- list(
    sintIntern = sinteseInternacao,
    sintTopPreco = sinteseTopPreco,
    sintTopQtde = sinteseTopQtde,
    incidenciaT0 = incidenciaT0,
    incidenciaT1 = incidenciaT1,
    variacaoIncidencia = varIncidencia,
    novosItens = novosItens,
    antigosItens = antigosItens,
    sintTopPrinc = topPrincAtiv,
    varIncidPrincAt = varIncidenciaPrincAt,
    sintDiarias = sumarioDiarias,
    histDiarias = histogramaDiarias,
    histMov = histogramaMovimentos,
    histCusto = histogramaCustoGuia
  )
return(resultado)
}


# PRESTADORES ÚNICOS
prestadores <- unique(
  {sumarioMaioresVariacoes %>%
    filter(Nome.Abrev..Prestador.Principal != "CUIDAR +") %>%
    select(Nome.Abrev..Prestador.Principal) %>%
    pull
    })
escala <- 2.3
fatorImagem <- 1


# FUNÇÃO EXPORTAR ----
funcaoImprimirSaida <- function(baseUsar, diretorio, nomeArquivo) { # DIRETÓRIO APENAS NOME FINAL, USARÁ WD + DIR_RESULT + 
  saidaR <- funcaoSinteseInternacao(
    baseTeste = baseUsar
  )
  diretorio <- paste0(dir_result, diretorio)
  if (!dir.exists(diretorio)) {
    dir.create(diretorio)
  }
  caminhoPasta <- paste0(diretorio, "\\")
  # IMPRIMIR IMAGENS ANTES
    # HIST DIÁRIAS
  print(saidaR$histDiarias)
  ggsave(paste0(caminhoPasta, "Histograma Diarias.png"), width = 16 * escala, height = 9 * escala, units = "cm")
  dev.off()
    # HIST CUSTO
  print(saidaR$histCusto)
  ggsave(paste0(caminhoPasta, "Histograma Custo.png"), width = 16 * escala, height = 9 * escala, units = "cm")
  dev.off()
    # HIST MOVIMENTOS
  print(saidaR$histMov)
  ggsave(paste0(caminhoPasta, "Histograma Movimentos.png"), width = 16 * escala, height = 9 * escala, units = "cm")
  dev.off()
  
  wb <- createWorkbook(type = "xlsx")
  # TABELA: SINTESE GERAL
  addDataFrame(
    saidaR$sintIntern, sheet = createSheet(wb, sheetName = "SinteseInternacao"), 
    row.names = FALSE
  )
  # TABELA: EFEITO PREÇO
  addDataFrame(
    saidaR$sintTopPreco, sheet = createSheet(wb, sheetName = "EfeitoPreço"), 
    row.names = FALSE
  )
  # TABELA: EFEITO QTDE
  addDataFrame(
    saidaR$sintTopQtde, sheet = createSheet(wb, sheetName = "EfeitoQtde"), 
    row.names = FALSE
  )
  # TABELA: PRINCIPIO ATIVO
  addDataFrame(
    saidaR$sintTopPrinc, sheet = createSheet(wb, sheetName = "PrincipioAtivo"), 
    row.names = FALSE
  )
  # TABELA e GRÁFICO: DIÁRIAS
  abaAtual <- createSheet(wb, sheetName = "Diarias")
  addDataFrame(
    saidaR$sintDiarias, sheet = abaAtual, 
    row.names = FALSE
  )
  addPicture(paste0(caminhoPasta, "Histograma Diarias.png"), sheet = abaAtual,
             startRow = 1, startColumn = ncol(saidaR$sintDiarias) + 2, scale = fatorImagem
  )
  # TABELA: INCIDÊNCIA 
  abaAtual <- createSheet(wb, sheetName = "IncidenciaMov")
  addDataFrame(saidaR$incidenciaT0, abaAtual, row.names = FALSE)
  addDataFrame(saidaR$incidenciaT1, abaAtual, startColumn = ncol(saidaR$incidenciaT0) + 2, row.names = FALSE)
  addDataFrame(saidaR$variacaoIncidencia, abaAtual, row.names = FALSE,
               startColumn = 2 * (ncol(saidaR$incidenciaT0) + 2)
  )
  
  # TABELA: INCIDENCIA PRINC ATIVO 
  addDataFrame(
    saidaR$varIncidPrincAt, sheet = createSheet(wb, sheetName = "VarIncidenciaPrincAtivo"),
    row.names = FALSE
  )
  
  # TABELA: NOVOS ITENS
  addDataFrame(
    saidaR$novosItens, sheet = createSheet(wb, sheetName = "MovimentosNovos"), 
    row.names = FALSE
  )
  # TABELA: ANTIGOS ITENS
  addDataFrame(
    saidaR$antigosItens, sheet = createSheet(wb, sheetName = "MovimentosAbandonados"), 
    row.names = FALSE
  ) 
  # HIST: CUSTO GUIA
  addPicture(paste0(caminhoPasta, "Histograma Custo.png"), 
             sheet = createSheet(wb, sheetName = "HistCusto"),
             startRow = 1, scale = fatorImagem
  )
  # HIST: MOVIMENTOS UNICOS GUIA
  addPicture(
    paste0(caminhoPasta, "Histograma Movimentos.png"), 
    sheet = createSheet(wb, sheetName = "HistMov"), scale = fatorImagem, startRow = 1, startColumn = 1
  )
  saveWorkbook(wb, paste0(caminhoPasta, "\\Desdobramento - ", nomeArquivo, ".xlsx"))
}


# GERAR SAÍDAS ----
  # TODOS JUNTOS 
funcaoImprimirSaida(baseUsar = baseR, diretorio = "GERAL", nomeArquivo = "Todos prestadores")
  # HOSPITAIS (SEM NÍVEL 2)
for (w in prestadores) {
  funcaoImprimirSaida(baseUsar = {baseR %>% filter(Nome.Abrev..Prestador.Principal == w)}, 
    diretorio = w, nomeArquivo = paste0(w, " Geral"))
}
  # HOSPITAIS POR NÍVEL 2 
niveis <- c("CLINICA", "CIRURGICA")
for (w in prestadores) {
  for (y in niveis) {
    funcaoImprimirSaida(baseUsar = {baseR %>% filter(Nome.Abrev..Prestador.Principal == w & Nivel.2 == y)}, 
      diretorio = w, nomeArquivo = paste0(w, " ", y))
  }
}

