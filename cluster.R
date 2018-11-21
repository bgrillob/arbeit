#####################################################
########### FUNÇÃO CALCULAR CLUSTERS ################
#####################################################

# CLUSTER: FUNÇÃO COMPARAR CLUSTERS ----
funcaoCalcularClusters <- function(x, idClust, kTest = 3:12, seed = 15081991) {
  # CLUSTERS CANDIDATOS ----
  
  # CLUSTER: K-MEANS ----
  largSilKM <- map_dbl(kTest,  function(k){
    set.seed(seed)
    modelo <- kmeans(x = x, centers = k)
    silhueta <- silhouette(modelo$cluster, dist(x))
    mean(silhueta[, 3])
  }) %>%
    round(., 4)
  set.seed(seed)
  modeloKM <- kmeans(x = x, centers = kTest[which.max(largSilKM)])
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE COMPLETE ----
  largSilHCC <- map_dbl(kTest,  function(k){
    silhuetaCompl <- x %>%
      dist(., method = 'euclidean') %>% # MATRIZ DISSIMILARIDADE
      hclust(., method = 'complete') %>%
      cutree(., k = k) %>%
      silhouette(., dist(x, method = 'euclidean'))
    mean(silhuetaCompl[, 3])
  }) %>%
    round(., 4)
  
  modeloHCC <- x %>%
    dist(method = 'euclidean') %>%
    hclust(method = 'complete') %>%
    cutree(k = kTest[which.max(largSilHCC)])
  
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE CENTROIDE ----
  largSilHCW <- map_dbl(kTest,  function(k){
    silhuetaCompl <- x %>%
      dist(., method = 'euclidean') %>% # MATRIZ DISSIMILARIDADE
      hclust(., method = 'ward.D2') %>%
      cutree(., k = k) %>%
      silhouette(., dist(x, method = 'euclidean'))
    mean(silhuetaCompl[, 3])
  }) %>%
    round(., 4)
  
  modeloHCW <- x %>%
    dist(method = 'euclidean') %>%
    hclust(method = 'ward.D2') %>%
    cutree(k = kTest[which.max(largSilHCW)])
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE AVERAGE ----
  largSilHCA <- map_dbl(kTest,  function(k){
    silhuetaCompl <- x %>%
      dist(., method = 'euclidean') %>% # MATRIZ DISSIMILARIDADE
      hclust(., method = 'average') %>%
      cutree(., k = k) %>%
      silhouette(., dist(x, method = 'euclidean'))
    mean(silhuetaCompl[, 3])
  }) %>%
    round(., 4)
  
  modeloHCA <- x %>%
    dist(method = 'euclidean') %>%
    hclust(method = 'average') %>%
    cutree(k = kTest[which.max(largSilHCA)])
  
  # CLUSTER: HIERARCHICAL CLUSTER: LINKAGE CORRELATION-BASED (NÃO APLICÁVEL DATASET) ----
  # baseClusterT <- t(baseCluster)
  # largSilHCCorrW <- map_dbl(kTest,  function(k){
  #   silhueta <- baseClusterT %>%
  #     t %>%
  #     cor(., method = 'pearson') %>%
  #     as.dist(1 - .) %>%
  #     hclust(., method = 'ward.D2') %>%
  #     cutree(., k = k) %>%
  #     silhouette(., dist(baseCluster, method = 'euclidean')) 
  #   mean(silhueta[, 3])
  # })
  # 
  # modeloHCCorrW <- baseCluster %>%
  #   dist(method = 'euclidean') %>%
  #   hclust(method = 'complete') %>%
  #   cutree(k = kTest[which.max(largSilHCCorrW)])
  
  
  # TABELA COM SILHUETAS ----
  dfSumario <- data.frame(
    Iteracao = idClust,
    Cluster = c("k-Means", "HC-Complete", "HC-Centroide", "HC-Average"),
    SilhuetaMax = c(max(largSilKM), max(largSilHCC), max(largSilHCW), max(largSilHCA)),
    NumClusters = c(
      kTest[which.max(largSilKM)], kTest[which.max(largSilHCC)],
      kTest[which.max(largSilHCW)], kTest[which.max(largSilHCA)]
    ),
    nVariaveis = ncol(x)
  )
  
  # TABELA COM CLUSTERS ----
  dfClusters <- rbind(
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCW), Modelo = "K-means", Cluster = modeloKM$cluster),
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCC), Modelo = "HC-Complete", Cluster = modeloHCC),
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCW), Modelo = "HC-Centroide", Cluster = modeloHCW),
    data.frame(
      Iteracao = idClust, largSil = max(largSilHCA), Modelo = "HC-Average", Cluster = modeloHCA)
  )
  
  # GRÁFICO: SILHUETA EM FUNÇÃO DE K ----
  basePlotSilhueta <- rbind(
    data.frame(k = kTest, largSil = largSilKM, Modelo = 'K-Means'),
    data.frame(k = kTest, largSil = largSilHCC, Modelo = 'HC-Complete'),
    data.frame(k = kTest, largSil = largSilHCW, Modelo = 'HC-Centroide'),
    data.frame(k = kTest, largSil = largSilHCA, Modelo = 'HC-Average')
  )
  
  graficoSilhueta <- ggplot(basePlotSilhueta, aes(x = k, y = largSil, colour = Modelo)) +
    geom_line() +
    scale_x_continuous(breaks = kTest) + 
    labs(
      x = "K", y = "Largura Média Silhueta", 
      title = 'Largura Média da Silhueta em função do número de Clusters'
    ) + 
    theme(text = element_text(size = 20))
  
  
  # DENDOGRAMAS (DEIXAR DE FORA) ----
  #  dendogramaHCA <- x %>%
  #    dist(method = 'euclidean') %>%
  #    hclust(method = 'average') %>% 
  #    as.dendrogram %>% 
  #    color_branches(., k = kTest[which.max(largSilHCA)]) %>%
  #    plot
  
  # RESULTADO ----
  resultado <- list(
    Sumario = dfSumario, Clusters = dfClusters, grafSilhueta = graficoSilhueta
  )
  return(resultado)
}

######################################################
############# FUNÇÃO ESCOLHER CLUSTERS ###############
######################################################
# varFixas sempre entram no cluster (inserir vetor com nome variáveis)
# varCombn são testadas (inserir vetor com nome variáveis)
# percPior é o limite percentual em que é aceitável trazer um cluster que não tem máxima silhueta
# ex: percPior 0.03, traz-se os clusters que tem silhueta 97% ou mais em relação à máxima
# intK é o intervalo do número de clusters a testar
funcaoEscolherClusters <- function(df, varFixasNm, varCombnNm, percPior = 0.03, intK = 3:12) {
  varFixas <- which(colnames(df) %in% varFixasNm)
  varCombn <- which(colnames(df) %in% varCombnNm)
  # GERAR COMBINAÇÕES A TESTAR ----
  vetor <- varCombn
  combVariaveis <- lapply(seq_along(vetor), function (x) combinat::combn(vetor, x, simplify = FALSE)) %>%
    unlist(., recursive = FALSE) %>%
    lapply(., function (x) {c(varFixas, x)}) %>%
    append(., list(varFixas))
  
  # APLICAR COMBINAÇÕES ----
  retornoClusters <- lapply(combVariaveis, function(x) 
    funcaoCalcularClusters(x = df[, x], kTest = intK, idClust = paste0(x, collapse = "|"), seed = seed)
  )
  # ESCOLHER MELHORES ----
  comparacaoClusters <- lapply(retornoClusters, function(x) {x$Sumario}) %>%
    do.call(rbind, .)
  # CLUSTERS QUE IRÃO PARA O RELATÓRIO (OS ESCOLHIDOS) ----
  clustersParaRelatorio <- comparacaoClusters %>%
    arrange(-SilhuetaMax) %>%
    filter(
      SilhuetaMax > (max(SilhuetaMax) * (1 - percPior) ) # CRITÉRIO POR PROXIMIDADE DO MELHOR CLUSTER
    ) %>%
    mutate(Escolhido = "SIM")
  # CATEGORIZAÇÃO DE CADA OBSERVAÇÃO PELOS CLUSTERS ESCOLHIDOS ----
  resultadosClusters <- lapply(retornoClusters, function(x) {x$Clusters}) %>%
    do.call(rbind, .) %>%
    filter(
      largSil %in% unique(clustersParaRelatorio$SilhuetaMax) &
        Iteracao %in% unique(clustersParaRelatorio$Iteracao) &
        Modelo %in% unique(clustersParaRelatorio$Cluster)
    )
  resultadosClusters <- split(resultadosClusters, f = list(resultadosClusters$Iteracao, resultadosClusters$largSil, resultadosClusters$Modelo))
  posUsar <- unlist(lapply(resultadosClusters, nrow)) > 0
  resultadosClusters <- resultadosClusters[posUsar]
  # SCATTER 3D CLUSTERS CANDIDATOS ----
  # MODELO PARA PLANO ----
  lmPlano <- train(
    SilhuetaMax ~ NumClusters + nVariaveis,
    data = comparacaoClusters,
    method = "rf",
    metric = "RMSE",
    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 2),
    preProcess = c('center', 'scale')
  )
  
  # VALORES PARA PLANO
  eixoX <- seq(min(comparacaoClusters$NumClusters), max(comparacaoClusters$NumClusters), by = 1)
  eixoY <- seq(min(comparacaoClusters$nVariaveis), max(comparacaoClusters$nVariaveis), by = 1)
  planoScatter <- expand.grid(NumClusters = eixoX, nVariaveis = eixoY, KEEP.OUT.ATTRS = F)
  planoScatter$SilhuetaMax <- predict(lmPlano, newdata = planoScatter)
  planoScatter <- acast(planoScatter, nVariaveis ~ NumClusters, value.var = "SilhuetaMax")
  # PLOT 
  comparacaoClusters <- merge(comparacaoClusters, clustersParaRelatorio, all.x = TRUE) %>%
    replace(is.na(.), "NAO") %>%
    mutate(Escolhido = as.factor(Escolhido))
  
  scatterSilhueta3D <- plot_ly(comparacaoClusters,
                               x = ~NumClusters, 
                               y = ~nVariaveis, 
                               z = ~SilhuetaMax, 
                               type = "scatter3d", mode = "markers", color = ~Cluster, 
                               symbol = ~Escolhido, symbols = c('circle', 'diamond'),
                               hoverinfo = 'text', 
                               text = ~paste('</br> SilhuetaMax: ', round(SilhuetaMax, 3),
                                             '</br> Número Variáveis: ', round(nVariaveis, 3),
                                             '</br> Número Clusters: ', round(NumClusters, 3),
                                             '</br> Método: ', Cluster,
                                             '</br> Variáveis Usadas: ', Iteracao)
  ) %>%
    layout(
      title = "MaxSilhueta em função de k-clusters e n-variáveis",
      scene = list(
        xaxis = list(title = "Número Clusters"),
        yaxis = list(title = "Número Variáveis"),
        zaxis = list(title = "Silhueta Max")
      )) 
  # ADICIONAR PLANO AO GRÁFICO
  scatterSilhueta3D <- add_trace(p = scatterSilhueta3D, 
                                 z = planoScatter,
                                 x = eixoX,
                                 y = eixoY, type = 'surface', alpha = 0.2
  )
  
  # TRAZER RESULTADO ----
  resultado <- list(
    dfSumarioClusters = clustersParaRelatorio,
    dfClusterObservacoes = resultadosClusters,
    scatterSilhueta3D = scatterSilhueta3D,
    dfUtilizado = df
  )
  return(resultado)
}

##################################################
############ APLICAR FUNÇÕES: ####################
####### TRAZER OBJETO QUE SERÁ USADO NO APP ######
##################################################

# https://stats.stackexchange.com/questions/195446/choosing-the-right-linkage-method-for-hierarchical-clustering
# OBS: NECESSÁRIO EVOLUIR PARA UM CRITÉRIO QUE PENALIZE QUANTIDADE DE CLUSTERS

library(plotly)
library(tidyverse)
library(cluster)
library(purrr)
library(gridExtra)
library(GGally)
library(ggfortify)
library(dendextend)
library(combinat)
library(caret)
library(reshape2)
rm(list = ls())
setwd('DIRETORIO')
# FUNÇÕES PRE-DEFINIDAS ----
source('R Script - Cluster - Funcao Calcular Clusters.R')
source('R Script - Cluster - Funcao Escolher Cluster.R')

# STRINGS ----
seed <- 15081991
# BASE ----
baseR <- read.table("baseClusterV2.csv", header = T, sep = ";", stringsAsFactors = FALSE)
baseR <- baseR %>%
  replace(is.na(.), 0) %>%
  mutate(
    # TRANSFORMAÇÃO EXPONENCIAL 
    LOG_RECEITA_COMPRA_ON_COM_JUROS = log(RECEITA_COMPRA_ON_COM_JUROS + 1),
    LOG_VALOR_COMPRA_OFF = log(VALOR_COMPRA_OFF + 1),
    LOG_VALOR_COMPRA_ON = log(VALOR_COMPRA_ON + 1),
    LOG_RECEITA_COMPRA_OFF = log(RECEITA_COMPRA_OFF + 1),
    LOG_RECEITA_COMPRA_OFF_COM_JUROS = log(RECEITA_COMPRA_OFF_COM_JUROS + 1),
    LOG_RECEITA_SQRAP = log(RECEITA_SQRAP + 1),
    LOG_RECEITA_SAQUE_CARTAO = log(RECEITA_SAQUE_CARTAO + 1),
    LOG_VALOR_JUROS_OFF = log(VALOR_JUROS_OFF + 1),
    LOG_VALOR_TARIFAS_OFF = log(VALOR_TARIFAS_OFF + 1),
    LOG_VALOR_REFINANCIAMENTO_OFF = log(VALOR_REFINANCIAMENTO_OFF + 1),
    LOG_RECEITA_TOTAL = log(RECEITA_TOTAL + 1),
    # PROPORCOES
    PROP_INADIMP = (VALOR_REFINANCIAMENTO_OFF + VALOR_JUROS_OFF) / (RECEITA_TOTAL + 1)
  ) %>%
  replace(is.na(.), 0) 

baseR <- within(baseR, {
  TipoUso = ifelse(
    VALOR_COMPRA_ON > 0 & VALOR_COMPRA_OFF == 0, "Apenas On",
    ifelse(
      VALOR_COMPRA_ON == 0 & VALOR_COMPRA_OFF > 0, "Apenas Off",
      "On e Off")
  )
})

# BASE: AMOSTRA ----
set.seed(seed)
obsAmostra <- sample(seq(nrow(baseR)), size = 5000)
baseCluster <- baseR[obsAmostra, ]

funcaoRangeScale <- function(x) {
  resultado <- (x-min(x))/(max(x)-min(x))
return(resultado)
}


# BASE CLUSTER ----
fixas <- c("LOG_RECEITA_COMPRA_ON_COM_JUROS", "LOG_RECEITA_COMPRA_OFF", "LOG_RECEITA_COMPRA_OFF_COM_JUROS", "LOG_RECEITA_SQRAP")
variaveis <- c("LOG_RECEITA_SAQUE_CARTAO", "LOG_VALOR_JUROS_OFF" , "LOG_VALOR_TARIFAS_OFF", "LOG_VALOR_REFINANCIAMENTO_OFF")

  # BASE CLUSTER: AJUSTAR ESCALA DAS VARIÁVEIS
variaveisTransformar <- c(fixas, variaveis)
for (w in seq_along(variaveisTransformar)) {
  refCol <- which(colnames(baseCluster) == variaveisTransformar[w])
  baseCluster[, refCol] <- funcaoRangeScale(x = baseCluster[, refCol])
}

# RESULTADOS CLUSTERS ----
tempo <- proc.time()
clusters <- funcaoEscolherClusters(df = baseCluster, percPior = 0.05, intK = 4:9,
  varFixasNm = fixas, varCombnNm = variaveis)
proc.time() - tempo

save(clusters, file = "clusterApp.Rdata")
                          
###################################################################
################## APP VISUALIZAÇÃO CLUSTERS ######################
###################################################################


library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)

options(encoding = "UTF-8")
# LISTA CLUSTERS 
load("DIRETÓRIO\\clusterApp.Rdata")

# CRIAR INPUTS ----
  # CRIAR INPUTS: CLUSTERS DISPONÍVEIS ----
clustersDisponiveis <- names(clusters$dfClusterObservacoes)
names(clustersDisponiveis) <- clustersDisponiveis

  # CRIAR INPUTS: VARIÁVEIS ----
variaveisGraf <- colnames(clusters$dfUtilizado[, -1])
names(variaveisGraf) <- variaveisGraf

# UI ----
app <- shinyApp(
  ui = fluidPage(
    
    # TÍTULO DO APP ----
    titlePanel("Cluster"),
    
    # DEFINIÇÕES BARRA LATERAL ----
    sidebarLayout(
      
      # INPUT ----
      sidebarPanel(
        # INPUT: ID CLUSTER
        selectInput("cluster", "Cluster:",
                    clustersDisponiveis
        ),
        br(), # br() element to introduce extra vertical spacing
        # INPUT: VARIÁVEIS GRÁFICO ----
          # X
        selectInput("varPlotX", "Eixo X:",
                    variaveisGraf
        ),
          # Y
        selectInput("varPlotY", "Eixo Y:",
                    variaveisGraf
        ),
          # Z
        selectInput("varPlotZ", "Eixo Z:",
                    variaveisGraf
        ),
        br(),  # br() element to introduce extra vertical spacing
          # BOX PLOT
        selectInput("varPlotBox", "Variável Box Plot:",
                    variaveisGraf
        )
      
      ),
      
      # PAINEL OUTPUT ----
      mainPanel(
        
        # OUTPUT: ABAS DA RESPOSTA ----
        tabsetPanel(type = "tabs",
                    tabPanel("Sumário Candidatos", dataTableOutput("dfSumario")),
                    tabPanel("Tabela Paramétrica", dataTableOutput("dfParm")),
                    tabPanel("Tabela Não-Paramétrica", dataTableOutput("dfNaoParm")),
                    tabPanel("Gráfico Clusters", plotlyOutput("plotCluster")),
                    tabPanel("Gráfico Box Plot", plotlyOutput("plotBox")),
                    tabPanel("Gráfico Silhueta", plotlyOutput("plotSil"))
        )
        
      )
    )
  )
  ,
  # SERVER ----
  server = function(input, output) {
    # REACTIVE: 
      # REACTIVE: DF PARAMÉTRICA ----
    dfParm <- reactive({
      baseCluster <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      
      baseCluster %>%
        group_by(
          Cluster
        ) %>%
        summarise(
          TamGrupo = length(Cluster),
          Gasto = sum(VALOR_COMPRA_ON + VALOR_COMPRA_OFF + VALOR_SQRAP),
          Receita = sum(RECEITA_TOTAL),
          GastoMedio = mean(VALOR_COMPRA_ON + VALOR_COMPRA_OFF + VALOR_SQRAP),
          ReceitaMedia = mean(RECEITA_TOTAL),
          # COMPRAS ON
          GastoMedioOn = mean(VALOR_COMPRA_ON),
          ReceitaMediaOn = mean(RECEITA_COMPRA_ON_COM_JUROS),
          # COMPRAS OFF
          GastoMedioOff = mean(VALOR_COMPRA_OFF),
          ReceitaMediaOff = mean(RECEITA_COMPRA_OFF_COM_JUROS + RECEITA_COMPRA_OFF),
          # SAQUE RÁPIDO
          GastoMedioSqRap = mean(VALOR_SQRAP),
          ReceitaMediaSqRap = mean(RECEITA_SQRAP),
          # DEMAIS
          ReceitaMediaTarifas = mean(VALOR_TARIFAS_OFF),
          ReceitaMediaJurosAtraso = mean(VALOR_JUROS_OFF),
          PropMulher = mean(SEXO == 'F'),
          PropApenasOn = mean(TipoUso == "Apenas On"),
          PropApenasOff = mean(TipoUso == "Apenas Off"),
          MediaIdade = mean(IDADE),
          PropTarget = mean(CLASSE_SOCIAL %in% c("C1", "B2", "B1", "A"))
          #PropFatura = sum(VALOR_COMPRA_CARNE) / (sum(VALOR_COMPRA_FATURA) + sum(VALOR_COMPRA_CARNE))
        ) %>%
        mutate(
          PropGrupo = TamGrupo / sum(TamGrupo),
          Margem = Receita / Gasto,
          MargemOn = ReceitaMediaOn / GastoMedioOn,
          MargemOff = ReceitaMediaOff / GastoMedioOff,
          MargemSqRap = ReceitaMediaSqRap / GastoMedioSqRap,
          PropRecOn = ReceitaMediaOn / ReceitaMedia,
          PropRecOff = ReceitaMediaOff / ReceitaMedia,
          PropRecSqRap = ReceitaMediaSqRap / ReceitaMedia,
          PropReceitaTarifas = ReceitaMediaTarifas / ReceitaMedia,
          PropReceitaJurosAtraso = ReceitaMediaJurosAtraso / ReceitaMedia
        ) 
    })
    
    
      # REACTIVE: DF NÃO-PARAMÉTRICA ----
    dfNaoParm <- reactive({
      baseCluster <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      
      baseCluster %>%
        group_by(Cluster) %>%
        select(-SEXO, -CLASSE_SOCIAL, -TipoUso) %>%
        summarise_all(
          funs(
            Média = mean,
            Mediana = median, 
            Q1 = quantile(., probs = 0.25), 
            Q3 = quantile(., probs = 0.75))
        ) %>%
        merge(., {
          baseCluster %>%
            group_by(Cluster) %>%
            summarise(
              zTamCluster = length(Cluster),
              zPropCluster = length(Cluster) / length(obsAmostra)
            )
        }) %>%
        t %>%
        data.frame(
          Metrica = row.names(.), ., stringsAsFactors = FALSE
        ) %>%
        arrange(desc(Metrica)) %>%
        as.data.frame
      
    })
    
      # REACTIVE: SCATTER 3D ----
    plotCluster <- reactive({
      dfPlot <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      dfPlot <- data.frame(
        .X = dfPlot[, which(colnames(dfPlot) == input$varPlotX)],
        .Y = dfPlot[, which(colnames(dfPlot) == input$varPlotY)],
        .Z = dfPlot[, which(colnames(dfPlot) == input$varPlotZ)],
        .C = dfPlot[, which(colnames(dfPlot) == "Cluster")],
        dfPlot[, which(colnames(dfPlot) %in% c(
          "RECEITA_TOTAL", "VALOR_COMPRA_ON", "VALOR_COMPRA_OFF", "RECEITA_COMPRA_OFF",
          "RECEITA_COMPRA_ON_COM_JUROS", "RECEITA_SQRAP", "RECEITA_COMPRA_OFF_COM_JUROS",
          "RECEITA_SAQUE_CARTAO", "VALOR_JUROS_OFF"
        ))]
      )
      
      plot_ly(dfPlot,
              x = ~.X, 
              y = ~.Y, 
              z = ~.Z, 
              type = "scatter3d", mode = "markers", color = ~.C,
              hoverinfo = 'text', 
              text = ~paste(
                            '</br> Cluster: ', .C,
                            '</br> Receita Total: ', round(RECEITA_TOTAL, 2),
                            '</br> Valor Compras On: ', round(VALOR_COMPRA_ON, 2),
                            '</br> Valor Compras Off: ', round(VALOR_COMPRA_OFF, 2),
                            '</br> #Receita Compras Off: ', round(RECEITA_COMPRA_OFF, 2),
                            '</br> #Receita Compras On 0+8: ', round(RECEITA_COMPRA_ON_COM_JUROS, 2),
                            '</br> #Receita Saque Rápido: ', round(RECEITA_SQRAP, 2),
                            '</br> #Receita Compras Off Parcelada: ', round(RECEITA_COMPRA_OFF_COM_JUROS, 2)) 
      ) %>%
        layout(
          title = "Cluster escolhido por max(Silhueta(k))",
          scene = list(
            xaxis = list(title = input$varPlotX),
            yaxis = list(title = input$varPlotY),
            zaxis = list(title = input$varPlotZ)
          )) 
      
    })
    
      # REACTIVE: BOX-PLOT ----
    plotBox <- reactive({
      dfPlotBox <- data.frame(
        clusters[["dfUtilizado"]],
        Cluster = clusters[["dfClusterObservacoes"]][[input$cluster]]$Cluster,
        stringsAsFactors = FALSE
      )
      dfPlotBox <- data.frame(
        .C = as.factor(dfPlotBox[, which(colnames(dfPlotBox) == "Cluster")]),
        .Y = dfPlotBox[, which(colnames(dfPlotBox) == input$varPlotBox)]
      )
      ggplotBox <- ggplot(dfPlotBox, 
        aes_string(x = ".C", y = ".Y", fill = ".C")) + 
        geom_boxplot()
      ggplotly(ggplotBox)
    })
    
    # NÃO-REACTIVE ----
      # NÃO-REACTIVE: SUMÁRIO CLUSTERS ---- 
    dfSumario <- clusters[["dfSumarioClusters"]]
    
      # NÃO-REACTIVE: SILHUETA ----
    plotSilhueta <- clusters[["scatterSilhueta3D"]]
    
    # SAÍDAS APP ----
      # SAÍDAS APP: NR TABELA SUMARIO ----
    output$dfSumario <- renderDataTable({
      datatable(dfSumario,extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
    })
      # SAÍDAS APP: NR GRAFICO SILHUETA ----
    output$plotSil <- renderPlotly({
      plotSilhueta
    })
    
      # SAÍDAS APP: R TABELA PARAMÉTRICA ----
    output$dfParm <- renderDataTable({
      datatable(dfParm(), extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                )) %>%
        formatCurrency(c(
          "Gasto", "Receita", "GastoMedio", "ReceitaMedia", "GastoMedioOn",
          "ReceitaMediaOn", "GastoMedioOff", "ReceitaMediaOff", "GastoMedioSqRap",
          "ReceitaMediaSqRap", "ReceitaMediaTarifas", "ReceitaMediaJurosAtraso"
        ), digits = 2) %>%
        formatPercentage(c(
          "PropApenasOn", "PropApenasOff", "PropTarget", "PropGrupo", "Margem",
          "MargemOn", "MargemOff", "MargemSqRap", "PropRecOn", "PropRecOff", 
          "PropRecSqRap","PropReceitaTarifas", "PropReceitaJurosAtraso"
        ), digits = 2) %>%
        formatRound(
          c("MediaIdade"), digits = 2
        )
    })
      # SAÍDAS APP: R TABELA NÃO-PARAMPETRICA (NÃO FORMATADA) ----
    output$dfNaoParm <- renderDataTable({
      datatable(dfNaoParm(), extensions = 'FixedColumns',
                options = list(
                  dom = 't',
                  scrollX = TRUE,
                  fixedColumns = TRUE
                ))
    })
    
      # SAÍDAS APP: R GRÁFICO CLUSTER ----
    output$plotCluster <- renderPlotly({
      plotCluster()
    })
      # SAÍDAS APP: R GRÁFICO BOX-PLOT ----
    output$plotBox <- renderPlotly({
      plotBox()
    })
    
  }
)
# APLICAÇÃO ----
#shinyApp(ui = ui, server = server)
runApp(app, host = "IPv4", quiet = TRUE, launch.browser = TRUE)
                          
                          
                          
