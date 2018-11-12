#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(highcharter)

# CARREGAR DADOS ----
baseCampanhas <- baseCampanhas

# UI ----
ui <- fluidPage(
  
  # TÍTULO DO APP ----
  titlePanel("Resultado Campanhas"),
  
  # DEFINIÇÕES BARRA LATERAL ----
  sidebarLayout(
    
    # INPUT ----
    sidebarPanel(
      # INPUT: CAMPANHA
      selectInput("campanha", "Campanha:",
                  c("734 - Campanha Inativos" = "7B",
                    "736 - Campanha Churn" = "8B")
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
                  tabPanel("Tabela Resultado", tableOutput("table")),
                  tabPanel("Gráfico", plotlyOutput("plot")),
                  tabPanel("Mapa (apenas para UF)", highchartOutput("mapa")),
                  tabPanel("Série Histórica", plotlyOutput("ts"))

      )
      
    )
  )
)
# SERVER ----
server <- function(input, output) {
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
        "Ocorrência" = "COMMUNICATION_OCCURRENCE_NO"
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
  output$table <- renderTable({
    baseR()
  })
  
}

# APLICAÇÃO ----
shinyApp(ui = ui, server = server)
