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
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------


library(tidyverse)
library(highcharter)
library(data.table)

rm(list = ls())
setwd('')
# BASE ----
arquivosCamp <- list.files(pattern = ".rds")
nomesLista <- gsub(".rds", "", arquivosCamp)
baseCampanhas <- vector("list", length(arquivosCamp))

for (w in seq_along(arquivosCamp)) {
  
  df <- readRDS(arquivosCamp[w])
  df <- within(df, {
    FAIXA_ETARIA <- cut(IDADE, breaks = c(0, 25, 35, 45, 55, 900), labels = c("18-25", "26-35", "36-45", "46-55", "56-99"))
    IS_CONTROLE <- grepl("_Control", MARKETING_CELL_NM)
    CELULA_MARKETING <- gsub("_Control", "", MARKETING_CELL_NM)
  }) %>%
    replace(is.na(.), 0)
  
  
  funcaoResultCamp <- function(x, ctFxEMM = 1, ctVrEMM = 0.0009, ctVrSMS = 0.05,
                               mrgmProd = 0.05, 
                               mrgmCartao = 0.05 
                      ) { # GERA UMA LISTA COM COMPRIMENTO DE 'N_OCORRÊNCIAS' 
    # TABELA RESULTADO PADRÃO ----
    funcaoResultadoPadrao <- function (y) {
      dfResultado <- y %>%
        group_by(CHANNEL, MARKETING_CELL_NM, CELULA_MARKETING, IS_CONTROLE) %>%
        summarise(
          Comunicados = n_distinct(CD_CLNT),
          Interacao = sum(INTERAGIU[INTERAGIU != 0]),
          ClientesCompras = n_distinct(CD_CLNT[QTD_COMPRAS > 0]),
          QtdeContratos = sum(QTD_COMPRAS),
          ValorGasto = sum(VALOR_COMPRAS)
        ) %>% 
        mutate(
          PercInteracao = ifelse(!IS_CONTROLE, {Interacao / Comunicados}, {0}),
          RetornoBruto = ifelse(!IS_CONTROLE, {ClientesCompras / Interacao}, {ClientesCompras / Comunicados}),
          CompraMedia = ValorGasto / ClientesCompras,
          TicketMedio = ValorGasto / QtdeContratos,
          FreqMedia = QtdeContratos / ClientesCompras
        ) %>% as.data.frame # PRECISA QUEBRA PARA USAR MERGE
      
      dfResultado <- dfResultado %>%
        merge(., {dfResultado %>% 
            filter(IS_CONTROLE == TRUE) %>% 
            select(CELULA_MARKETING, RetornoBruto) %>%
            rename(RetornoBrutoGC = RetornoBruto)
        }, all.x = TRUE) %>%
        mutate(
          RetornoIncrementalPP = RetornoBruto - RetornoBrutoGC,
          VendaIncremental = RetornoIncrementalPP * CompraMedia * Interacao,
          CustoTotal = ifelse(grepl("EMM", CHANNEL), 
                              {ctFxEMM + ctVrEMM * Comunicados}, 
                              {ctVrSMS * Comunicados}
          )
        ) %>%
        mutate(
          MargemProduto = VendaIncremental * mrgmProd,
          MargemTransacao = VendaIncremental * mrgmCartao,
          VendaRealInvestido = VendaIncremental / CustoTotal,
          MargemTotal = MargemProduto + MargemTransacao,
          ROI = MargemTotal / CustoTotal,
          RetornoIncrementalPP = ifelse(RetornoIncrementalPP < 0, 0, RetornoIncrementalPP)
        ) %>%
        select(
          CHANNEL, MARKETING_CELL_NM, Comunicados, Interacao, PercInteracao, ClientesCompras,
          RetornoBruto, QtdeContratos, ValorGasto, CompraMedia, TicketMedio, FreqMedia,
          RetornoIncrementalPP, VendaIncremental, CustoTotal, VendaRealInvestido, MargemTotal, 
          MargemProduto, MargemTransacao, ROI
        )
      return(dfResultado)
    }

    # FUNÇÃO CAMPANHA: SUMARIO ----
    apurarResultado <- function(base, agrupamento) {
      # TABELA
      porVar <- base %>%
        group_by_("CAMPAIGN_SK", "COMMUNICATION_OCCURRENCE_NO", agrupamento, "IS_CONTROLE") %>%
        summarise(
          Comunicados = n_distinct(CD_CLNT),
          Interacao = sum(INTERAGIU),
          ClientesCompraram = n_distinct(CD_CLNT[QTD_COMPRAS > 0]),
          QtdeCompras = sum(QTD_COMPRAS),
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
          RetornoIncremental = ifelse(RetornoBruto - RetornoBrutoGC < 0, 0, RetornoBruto - RetornoBrutoGC),
          VendaIncremental = RetornoIncremental * Interacao * CompraMedia,
          MargemBruta = VendaIncremental * 0.15, # inserir parâmetro de margem
          VendaSobreReal = VendaIncremental / GastoTotal,
          ROI = MargemBruta / GastoTotal,
          CompraMediaIncremental = CompraMedia - CompraMediaGC,
          TicketMedioIncremental = TicketMedio - TicketMedioGC,
          ComprasPorClienteIncremental = ComprasPorCliente - ComprasPorClienteGC
        ) %>%
        filter(!IS_CONTROLE) %>%
        replace(is.na(.), 0) 
      
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
          resultPadrao = funcaoResultadoPadrao(y = y), 
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
  
  baseCampanhas[[w]] <- funcaoResultCamp(df)
  
  # APLICAR FUNÇÃO CAMPANHA ----
  # baseCampanhas <- split(df, f = df$CAMPAIGN_SK) %>%
  #   lapply(., function(x) {
  #     funcaoResultCamp(x = x)
  #   }) 

}

names(baseCampanhas) <- nomesLista

save(baseCampanhas, file = "Input App.RData")


library(shiny)
library(tidyverse)
library(plotly)
library(highcharter)
library(DT)
# CARREGAR DADOS ----
load("C:\\Users\\00690\\Desktop\\Campanhas\\Projeto Dashboard\\Input App.RData")

# UI ----
app <- shinyApp(
ui = fluidPage(
  
  # TÍTULO DO APP ----
  titlePanel("Resultado Campanhas"),
  
  # DEFINIÇÕES BARRA LATERAL ----
  sidebarLayout(
    
    # INPUT ----
    sidebarPanel(
      # INPUT: CAMPANHA
      selectInput("campanha", "Campanha:",
                  c("618 - Saque Rápido Substituição Listagem" = "618",
                    "622 - Saque Rápido Mensal" = "622",
                    "667 - Saque Rápido Janeiro" = "667",
                    "740 - MC Limite Saque" = "740",
                    "745 - Elasticidade Março" = "745",
                    "756 - Elasticidade MC" = "756",
                    "1254 - Elasticidade Agosto" = "1254"
                  )
      ),
      # INPUT: OCORRÊNCIA
      numericInput("ocorrencia", "Ocorrência da Campanha:", value = 1, min = 1, max = 2),
      
      # INPUT: VARIÁVEL AGRUPAR ----
      selectInput("agrupamento", "Visualizar resultado por:",
                  c("Célula de Marketing" = "porGrupoMarketing",
                    "Classe Social" = "porClasseSocial",
                    "Estado" = "porUF",
                    "Faixa Etária" = "porFaixaEtaria",
                    "Sexo" = "porSexo"
                  )),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # INPUT: VISUALIZAR GRÁFICO POR ----
      selectInput("varPlot", "Variável Gráfico:",
                  c(
                    "ROI" = "ROI",
                    "Margem Bruta (R$)" = "MargemBruta",
                    "Venda sobre Real Gasto (R$)" = "VendaSobreReal",
                    "Venda Incremental (R$)" = "VendaIncremental",
                    "Retorno Incremental (p.p.)" = "RetornoIncremental",
                    "Compra Média Incremental (R$)" = "CompraMediaIncremental",
                    "Ticket Médio Incremental (R$)" = "TicketMedioIncremental",
                    "Compras por Cliente Incremental (#)" = "ComprasPorClienteIncremental",
                    
                    "Comunicados (#)" = "Comunicados",
                    "Interacao (#)" = "Interacao",
                    "Retorno Bruto (%)" = "RetornoBruto",
                    "Clientes com Compras (#)" = "ClientesCompraram",
                    "Quantidade de Contratos (#)" = "QtdeCompras",
                    "Valor Total em Compras (R$)" = "ValorCompras",
                    
                    "Compra Média (R$)" = "CompraMedia",
                    "Ticket Médio (R$)" = "TicketMedio",
                    "Compras por Cliente (#)" = "ComprasPorCliente"
                  )
      )
    ),
    
    # PAINEL OUTPUT ----
    mainPanel(
      
      # OUTPUT: ABAS DA RESPOSTA ----
      tabsetPanel(type = "tabs",
                  tabPanel("Resultado", dataTableOutput("dfPadrao")),
                  tabPanel("Resultado por Agrupamento", dataTableOutput("table")),
                  tabPanel("Gráfico", plotlyOutput("plot")),
                  tabPanel("Mapa (apenas para UF)", highchartOutput("mapa")),
                  tabPanel("Série Histórica", plotlyOutput("ts"))
      )
      
    )
  )
)
,
# SERVER ----
server = function(input, output) {
  # REACTIVE PARA TABELA PADRÃO ----
  dfPadrao <- reactive({
    baseCampanhas[[input$campanha]][[input$ocorrencia]][["resultPadrao"]] %>%
      rename_(
        "Canal" = "CHANNEL", "Nome Grupo Marketing" = "MARKETING_CELL_NM" , "Comunicados (#)" = "Comunicados",
        "Interação (#)" = "Interacao", "Interação (%)" = "PercInteracao",  
        "Clientes com Compras (#)" = "ClientesCompras", "Retorno Bruto (%)" = "RetornoBruto",
        "Quantidade de Contratos (#)" = "QtdeContratos", "Gasto Total (R$)" = "ValorGasto",
        "Compra Média (R$)" = "CompraMedia", "Ticket Médio (R$)" = "TicketMedio",
        "Frequência Média (#)" = "FreqMedia", "Retorno Incremental (p.p.)" = "RetornoIncrementalPP",
        "Venda Incremental (R$)" = "VendaIncremental", "Custo Total (R$)" = "CustoTotal",        
        "Venda por Real Investido (R$)" = "VendaRealInvestido", "Margem Total (R$)" = "MargemTotal",
        "Margem Produto (R$)" = "MargemProduto", "Margem Transação (R$)" = "MargemTransacao",
        "ROI" = "ROI"
      )
  })
  # REACTIVE PARA TABELA ----
  baseR <- reactive({
    df <- baseCampanhas[[input$campanha]][[input$ocorrencia]][[input$agrupamento]] %>%
      select(
        -IS_CONTROLE, -RetornoBrutoGC, -CompraMediaGC, -TicketMedioGC, -ComprasPorClienteGC
      ) %>%
      rename_(
        "ROI" = "ROI", "Margem Bruta (R$)" = "MargemBruta",  "Venda sobre Real Gasto (R$)" = "VendaSobreReal",
        "Venda Incremental (R$)" = "VendaIncremental", "Retorno Incremental (p.p.)" = "RetornoIncremental",
        "Compra Média Incremental (R$)" = "CompraMediaIncremental", "Ticket Médio Incremental (R$)" = "TicketMedioIncremental",
        "Compras por Cliente Incremental (#)" = "ComprasPorClienteIncremental", "Comunicados (#)" = "Comunicados", "Interacao (#)" = "Interacao",
        "Retorno Bruto (%)" = "RetornoBruto", "Clientes com Compras (#)" = "ClientesCompraram", "Quantidade de Contratos (#)" = "QtdeCompras",
        "Valor Total em Compras (R$)" = "ValorCompras", "Compra Média (R$)" = "CompraMedia", "Ticket Médio (R$)" = "TicketMedio",
        "Compras por Cliente (#)" = "ComprasPorCliente", "Gasto Total (R$)" = "GastoTotal", "Campanha" = "CAMPAIGN_SK", 
        "Ocorrência" = "COMMUNICATION_OCCURRENCE_NO", "Interação (%)" = "PercInteracao"
      )
    # as_tibble(cbind(
    #   Métrica = names(df), 
    #   t(df)
    # ))
  })
  
  # REACTIVE PARA GRAFICO TS ----
  graficoTS <- reactive({
    listaOcorrencias <- baseCampanhas[[input$campanha]]
    dfTS <- lapply(listaOcorrencias, function (x) {
      x[[input$agrupamento]]
    }) %>%
      do.call(rbind, .) 
    dfTS <- dfTS[, c(3, which(colnames(dfTS) == "COMMUNICATION_OCCURRENCE_NO"), which(colnames(dfTS) == input$varPlot))]
    colnames(dfTS) <- c(".FL", ".X", ".Y")
    
    pltTS <- ggplot(dfTS, aes(x = .X, y = .Y, colour = .FL)) +
      geom_line() +
      scale_x_discrete() +
      theme_bw()
    ggplotly(pltTS)
  })
  
  # REACTIVE PARA GRÁFICO ----
  grafico <- reactive({
    df <- baseCampanhas[[input$campanha]][[input$ocorrencia]][[input$agrupamento]] 
    df <- df[, c(3, which(colnames(df) == input$varPlot))]
    colnames(df) <- c(".X", ".Y")
    
    plt <- ggplot(df, aes(x = .X, y = .Y, fill = .X)) +
      geom_bar(stat = "identity") +
      theme_bw()
    ggplotly(plt)
  })
  
  # REACTIVE PARA MAPA ----
  mapa <- reactive({
    df <- baseCampanhas[[input$campanha]][[input$ocorrencia]][[input$agrupamento]] 
    df <- df[, c(3, which(colnames(df) == input$varPlot))]
    colnames(df) <- c(".X", ".Y")
    
    hcmap("countries/br/br-all", data = df, value = ".Y",
          joinBy = c("hc-a2", ".X"), name = "", download_map_data = TRUE,
          dataLabels = list(enabled = TRUE, format = "{point.name}"),
          tooltip = list(valueDecimals = 1, valuePrefix = "", valueSuffix = ""))
  })
  
  # GERAR SAÍDAS APP ----
    # GERAR SAÍDAS APP: TABELA PADRÃO ----
  output$dfPadrao <- renderDataTable({
    datatable(dfPadrao(), extensions = 'FixedColumns',
              options = list(
                dom = 't',
                scrollX = TRUE,
                fixedColumns = TRUE
              )) %>%
      formatCurrency(c(
        "Gasto Total (R$)", "Compra Média (R$)", "Ticket Médio (R$)", "Venda Incremental (R$)",
        "Custo Total (R$)", "Venda por Real Investido (R$)", "Margem Total (R$)",
        "Margem Produto (R$)", "Margem Transação (R$)"
      )) %>% 
      formatPercentage(c(
        "Interação (%)", "Retorno Incremental (p.p.)", "Retorno Bruto (%)"
        ), 
        2) %>%
      formatRound(c(
        'Comunicados (#)', "Interação (#)", "Clientes com Compras (#)", "Quantidade de Contratos (#)",
        "Frequência Média (#)"), digits = 0
      ) %>%
      formatRound(c('ROI'), digits = 2)
  })
    # GERAR SAÍDAS APP: GRÁFICO ----
  output$plot <- renderPlotly({
    grafico()
  })
  
    # GERAR SAÍDAS APP: MAPA ----
  output$mapa <- renderHighchart({
    mapa()
  })
    # GERAR SAÍDAS APP: SÉRIE HISTÓRICA ----
  output$ts <- renderPlotly({
    graficoTS()
  })
  
    # GERAR SAÍDAS APP: TABELA ----
  output$table <- renderDataTable({
    datatable(baseR(), extensions = 'FixedColumns',
              options = list(
                dom = 't',
                scrollX = TRUE,
                fixedColumns = TRUE
              )
    ) %>%
      formatCurrency(c(
        "Margem Bruta (R$)", "Venda sobre Real Gasto (R$)", "Venda Incremental (R$)",
        "Compra Média Incremental (R$)", "Ticket Médio Incremental (R$)", "Ticket Médio (R$)",
        "Valor Total em Compras (R$)", "Compra Média (R$)", "Gasto Total (R$)"
      )) %>% 
      formatPercentage(c(
        "Retorno Incremental (p.p.)", "Retorno Bruto (%)"
      ), 
      2) %>%
      formatRound(c(
        "Compras por Cliente Incremental (#)", "Comunicados (#)", "Interacao (#)",
        "Clientes com Compras (#)", "Quantidade de Contratos (#)"
        ), digits = 0
      ) %>%
      formatRound(c('ROI'), digits = 2)
  })
  
}
)
# APLICAÇÃO ----
#shinyApp(ui = ui, server = server)
runApp(app, host = "10.251.51.69", quiet = TRUE, launch.browser = TRUE)
