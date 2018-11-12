library(tidyverse)
library(highcharter)
# CRIAR BASE ----
linhas <- 1200000
ufs <- c('AC',	'AL',	'AP',	'AM',	'BA',	'CE',	'DF',	'ES',	'GO',	'MA',	'MT',	'MS',	'MG',	'PA',	'PB',	'PR',	'PE',	'PI',	'RJ',	'RN',	'RS',	'RO',	'RR',	'SC',	'SP',	'SE',	'TO')
set.seed(15081991)

df <- data.frame(
  CAMPAIGN_SK = sample(c("7B", "8B"), size = linhas, replace = TRUE),
  COMMUNICATION_OCCURRENCE_NO = sample(seq(1, 10), size = linhas, replace = TRUE),
  CELL_PACKAGE_SK = sample(paste("CL", seq(10)), size = linhas, replace = TRUE),
  CONTROL_GROUP_TYPE_CD = sample(c(TRUE, FALSE), size = linhas, replace = TRUE, prob = c(0.1, 0.9)),
  MARKETING_CELL_NM = sample(paste("MKT", seq(3)), size = linhas, replace = TRUE),
  CD_CLNT = paste("C", seq(linhas)),
  UF = sample(ufs, size = linhas, replace = TRUE),
  IDADE = rnorm(linhas, mean = 40, sd = 7),
  SEXO = sample(c("F", "M"), size = linhas, replace = TRUE),
  CLASSE_SOCIAL = sample(c("A", "B1", "B2", "C1", "C2", "D/E"), size = linhas, replace = TRUE),
  COMMUNICATION_NM = sample(paste("CMN", seq(10)), size = linhas, replace = TRUE),
  INTERAGIU = sample(c(TRUE, FALSE), size = linhas, replace = TRUE, prob = c(0.12, 0.88)), 
  QTDE_COMPRAS = sample(c(1, 0), size = linhas, replace = TRUE, prob = c(0.025, 0.975)),
  VALOR_COMPRAS = 0,
  stringsAsFactors = FALSE
)

set.seed(15081991)
posCompras <- df$QTDE_COMPRAS > 0
df$QTDE_COMPRAS[posCompras] <- sample(seq(1:4), size = sum(posCompras), replace = TRUE, prob = c(0.95, 0.035, 0.01, 0.005))
df$VALOR_COMPRAS[posCompras] <- rnorm(sum(posCompras), mean = 215, sd = 15)

df$INTERAGIU[df$CONTROL_GROUP_TYPE_CD == TRUE] <- 0

# BASE ----
set.seed(15081991)
df <- within(df, {
  FAIXA_ETARIA <- cut(IDADE, breaks = c(0, 25, 35, 45, 55, 900), labels = c("18-25", "26-35", "36-45", "46-55", "56-99"))
  IS_CONTROLE <- CONTROL_GROUP_TYPE_CD
  NOME_CELULA <- ifelse(IS_CONTROLE, paste(MARKETING_CELL_NM, "_Control", sep = ""), MARKETING_CELL_NM)
  NOME_GRUPO <- MARKETING_CELL_NM
})

# FUNÇÃO CAMPANHA ----
x <- df %>% filter(CAMPAIGN_SK == "7B")

funcaoResultCamp <- function(x) { # GERA UMA LISTA COM COMPRIMENTO DE 'N_OCORRÊNCIAS' 
  # FUNÇÃO CAMPANHA: SUMARIO ----
  apurarResultado <- function(base, agrupamento) {
      # TABELA
    porVar <- base %>%
      group_by_("CAMPAIGN_SK", "COMMUNICATION_OCCURRENCE_NO", agrupamento, "IS_CONTROLE") %>%
      summarise(
        Comunicados = n_distinct(CD_CLNT),
        Interacao = sum(INTERAGIU),
        ClientesCompraram = n_distinct(CD_CLNT[QTDE_COMPRAS > 0]),
        QtdeCompras = sum(QTDE_COMPRAS),
        ValorCompras = sum(VALOR_COMPRAS)
      ) %>%
      mutate(
        PercInteracao = Interacao / Comunicados,
        RetornoBruto = ifelse(IS_CONTROLE, ClientesCompraram / Comunicados, ClientesCompraram / Interacao),
        CompraMedia = ValorCompras / ClientesCompraram,
        TicketMedio = ValorCompras / QtdeCompras,
        ComprasPorCliente = QtdeCompras / ClientesCompraram,
        GastoTotal = Comunicados * 0.02 # inserir parâmetro de custo
      ) 
    
    porVar <- merge(porVar, {
      porVar %>%
        filter(IS_CONTROLE) %>%
        select_(agrupamento, "RetornoBruto", "CompraMedia", "TicketMedio", "ComprasPorCliente") %>%
        rename(
          RetornoBrutoGC = RetornoBruto, CompraMediaGC = CompraMedia, TicketMedioGC = TicketMedio, ComprasPorClienteGC = ComprasPorCliente
        )
    }, all.x = T) %>%
      mutate(
        RetornoIncremental = RetornoBruto - RetornoBrutoGC,
        VendaIncremental = RetornoIncremental * Interacao * CompraMedia,
        MargemBruta = VendaIncremental * 0.15, # inserir parâmetro de margem
        VendaSobreReal = VendaIncremental / GastoTotal,
        ROI = MargemBruta / GastoTotal,
        CompraMediaIncremental = CompraMedia - CompraMediaGC,
        TicketMedioIncremental = TicketMedio - TicketMedioGC,
        ComprasPorClienteIncremental = ComprasPorCliente - ComprasPorClienteGC
      ) %>%
      filter(!IS_CONTROLE)
    
    # # BOX PLOT VALOR_COMPRAS CLIENTES QUE INTERAGIRAM
    # dfPlot <- base %>%
    #   filter(QTDE_COMPRAS > 0)
    # limite <- quantile(dfPlot$VALOR_COMPRAS, 0.75) + 1.5 * (quantile(dfPlot$VALOR_COMPRAS, 0.75) - quantile(dfPlot$VALOR_COMPRAS, 0.25))
    # boxPlot <- ggplot(dfPlot, aes(y = VALOR_COMPRAS, colour = IS_CONTROLE)) +
    #   geom_boxplot() +
    #   ylim(0, limite) +
    #   facet_wrap(~get(agrupamento))
    # 
    # resultado <- list(
    #   df = porVar,
    #   boxPlot = dfPlot
    # )
  return(porVar)
  }
  # APLICAR FUNÇÃO RESULTADOS ----
  resultado <- split(x, f = x$COMMUNICATION_OCCURRENCE_NO) %>%
    lapply(., function(y) {
      list(
        porGrupoMarketing = apurarResultado(base = y, agrupamento = "MARKETING_CELL_NM"),
        porUF = apurarResultado(base = y, agrupamento = "UF"),
        porSexo = apurarResultado(base = y, agrupamento = "SEXO"),
        porClasseSocial = apurarResultado(base = y, agrupamento = "CLASSE_SOCIAL"),
        porFaixaEtaria = apurarResultado(base = y, agrupamento = "FAIXA_ETARIA")
      )
    })
  
  # RESULTADO CAMPANHA
return(resultado)
}

# APLICAR FUNÇÃO CAMPANHA ----
baseCampanhas <- split(df, f = df$CAMPAIGN_SK) %>%
  lapply(., function(x) {
    funcaoResultCamp(x = x)
}) 



# TESTE MAPA
dfUF <- baseCampanhas$`7B`$`1`$porUF
hcmap("countries/br/br-all", data = dfUF, value = "ROI",
  joinBy = c("hc-a2", "UF"), name = "", download_map_data = TRUE,
  dataLabels = list(enabled = TRUE, format = "{point.name}"),
  tooltip = list(valueDecimals = 1, valuePrefix = "", valueSuffix = "%")) 
