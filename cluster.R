# https://stats.stackexchange.com/questions/195446/choosing-the-right-linkage-method-for-hierarchical-clustering
# 
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

# STRINGS ----
intTestar <- 3:9
seed <- 15081991
criterioCandidatos <- 0.97 # PEGA OS CLUSTERS ATÉ 3% PIOR QUE MELHOR CLUSTER
                           # OBS: NECESSÁRIO EVOLUIR PARA UM CRITÉRIO QUE PENALIZE QUANTIDADE DE CLUSTERS
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

  # BASE: AMOSTRA ----
set.seed(seed)
obsAmostra <- sample(seq(nrow(baseR)), size = 2000)
baseCluster <- baseR %>%
  select(
    #LOG_RECEITA_TOTAL, # LOG_VALOR_COMPRA_OFF, #LOG_VALOR_COMPRA_ON, 
    LOG_RECEITA_COMPRA_ON_COM_JUROS,
    LOG_RECEITA_COMPRA_OFF,
    LOG_RECEITA_COMPRA_OFF_COM_JUROS,
    LOG_RECEITA_SQRAP,
    LOG_RECEITA_SAQUE_CARTAO,
    LOG_VALOR_TARIFAS_OFF,
    LOG_VALOR_REFINANCIAMENTO_OFF,
    LOG_VALOR_JUROS_OFF
  ) %>%
  lapply(., function (x) {(x-min(x))/(max(x)-min(x))}) %>% do.call(cbind, .) %>% # RANGE SCALING
  #scale(., center = TRUE, scale = TRUE) %>% # MEAN-SD SCALING
  {.[obsAmostra, ]}

baseClusterNivel <- baseR %>%
  select(
    -contains('LOG'), -CD_CLNT
    #RECEITA_TOTAL, VALOR_COMPRA_OFF, VALOR_COMPRA_ON, RECEITA_SQRAP, 
    #RECEITA_COMPRA_ON_COM_JUROS, RECEITA_COMPRA_OFF_COM_JUROS
  ) %>%
  {.[obsAmostra, ]}


# CLUSTER ----
  # CLUSTER: FUNÇÃO COMPARAR CLUSTERS ----
funcaoDeterminarClusters <- function(x, idClust, kTest = intTestar, seed = seed) {
  # CLUSTERS CANDIDATOS ----
  
  # CLUSTER: K-MEANS ----
  largSilKM <- map_dbl(kTest,  function(k){
    set.seed(seed)
    modelo <- kmeans(x = x, centers = k)
    silhueta <- silhouette(modelo$cluster, dist(x))
    mean(silhueta[, 3])
  })
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
  })
  
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
  })
  
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
  })
  
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

  # CLUSTER: GERAR COMBINAÇÕES DE BASE
vetor <- 5:ncol(baseCluster)
combVariaveis <- lapply(seq_along(vetor), function (x) combinat::combn(vetor, x, simplify = FALSE)) %>%
  unlist(., recursive = FALSE) %>%
  lapply(., function (x) {c(seq(4), x)}) %>%
  append(., list(1:4))


  # CLUSTER: APLICAR FUNÇÃO ----
tempo <- proc.time()
retornoClusters <- lapply(combVariaveis, function(x) 
  funcaoDeterminarClusters(x = baseCluster[, x], idClust = paste0(x, collapse = "|"), seed = seed)
)
proc.time() - tempo

  # CLUSTER: ESCOLHER OS N MELHORES ----
comparacaoClusters <- lapply(retornoClusters, function(x) {x$Sumario}) %>%
  do.call(rbind, .)
    # CLUSTER: ESCOLHER OS N MELHORES: ESCOLHA ----
clustersParaRelatorio <- comparacaoClusters %>%
  arrange(-SilhuetaMax) %>%
  filter(
    SilhuetaMax > (max(SilhuetaMax) * criterioCandidatos) # CRITÉRIO POR PROXIMIDADE DO MELHOR CLUSTER
  ) %>%
  mutate(Escolhido = "SIM")

resultadosClusters <- lapply(retornoClusters, function(x) {x$Clusters}) %>%
  do.call(rbind, .) %>%
  filter(
    largSil %in% unique(clustersParaRelatorio$SilhuetaMax) &
    Iteracao %in% unique(clustersParaRelatorio$Iteracao) &
    Modelo %in% unique(clustersParaRelatorio$Cluster)
  )


    # CLUSTER: ESCOLHER OS N MELHORES: SCATTER 3D COM PLANO ----
      # MODELO PARA PREVISAO
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
  y = eixoY, type = 'surface'
)


# FUNÇÃO GERAR RESULTADOS CADA CLUSTER ----
gerarRelatorio <- function(x, y, xAux = baseClusterNivel) {
  # FUNÇÃO: AJUSTAR BASE ----
  baseScatter <- data.frame(
    x, xAux, Cluster = as.factor(y)
  )
    # FUNÇÃO: AJUSTAR BASE: INCLUIR VARIÁVEIS SCATTER 3D
  varPlot <- baseScatter %>%
    select(RECEITA_TOTAL, VALOR_COMPRA_ON, VALOR_COMPRA_OFF) %>%
    mutate(
      RECEITA_TOTAL = log(RECEITA_TOTAL + 1),
      VALOR_COMPRA_ON = log(VALOR_COMPRA_ON + 1),
      VALOR_COMPRA_OFF = log(VALOR_COMPRA_OFF + 1)
    ) %>%
    lapply(., function(x) {(x-min(x))/(max(x)-min(x))}) %>%
    do.call(cbind, .)
  colnames(varPlot) <- c('LOG_RECEITA_TOTAL', 'LOG_VALOR_COMPRA_ON', 'LOG_VALOR_COMPRA_OFF')
  
  baseScatter <- data.frame(baseScatter, varPlot, stringsAsFactors = FALSE)
  # FUNÇÃO: SCATTER 3D ----
  scatter3D <- plot_ly(baseScatter,
    x = ~LOG_VALOR_COMPRA_OFF, 
    y = ~LOG_VALOR_COMPRA_ON, 
    z = ~LOG_RECEITA_TOTAL, 
    type = "scatter3d", mode = "markers", color = ~Cluster,
    hoverinfo = 'text', 
    text = ~paste('</br> Receita Total: ', round(LOG_RECEITA_TOTAL, 3),
                  '</br> Valor Compras On: ', round(LOG_VALOR_COMPRA_ON, 3),
                  '</br> Valor Compras Off: ', round(LOG_VALOR_COMPRA_OFF, 3),
                  '</br> #Receita Compras Off: ', round(LOG_RECEITA_COMPRA_OFF, 3),
                  '</br> #Receita Compras On 0+8: ', round(LOG_RECEITA_COMPRA_ON_COM_JUROS, 3),
                  '</br> #Receita Saque Rápido: ', round(LOG_RECEITA_SQRAP, 3),
                  '</br> #Receita Compras Off Parcelada: ', round(LOG_RECEITA_COMPRA_OFF_COM_JUROS, 3)) 
  ) %>%
    layout(
      title = "Cluster escolhido por max(Silhueta(k))",
      scene = list(
        xaxis = list(title = "Valor Compras Off"),
        yaxis = list(title = "Valor Compras On"),
        zaxis = list(title = "Receita Total")
      )) 
  
  # FUNÇÃO: TABELA DESCRIÇÃO NÃO-PARAMÉTRICA ----
  sumarioCluster <- baseScatter %>%
    group_by(Cluster) %>%
    select(-SEXO, -CLASSE_SOCIAL) %>%
    summarise_all(
      funs(
        Média = mean,
        Mediana = median, 
        Q1 = quantile(., probs = 0.25), 
        Q3 = quantile(., probs = 0.75))
    ) %>%
    merge(., {
      baseScatter %>%
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
  
  # FUNÇÃO: TABELA DESCRIÇÃO PARAMÉTRICA ----
  dfPorCluster <- baseScatter %>%
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
      PropMulher = mean(SEXO == 'F')
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
  
  # FUNÇÃO: GRÁFICO DESCRIÇÃO PARAMÉTRICA ----
  
  
  # FUNÇÃO: BOX PLOT NIVEL E LOG ----
    # LOG ----
  vetorVariaveisLog <- c(
    "LOG_RECEITA_SQRAP", "LOG_RECEITA_COMPRA_ON_COM_JUROS", 
    "LOG_RECEITA_COMPRA_OFF_COM_JUROS", "LOG_RECEITA_COMPRA_OFF",
    "LOG_RECEITA_TOTAL", "LOG_VALOR_COMPRA_ON"
  )
  vetorNomesLog = c(
    "# Rg Scl - Log Receita Saque Rapido", "# Rg Scl - Log Receita 0 + 8", 
    "# Rg Scl - Log Receita Off Parcelada Juros" , "# Rg Scl - Log Receita Compra Off",
    "Rg Scl - Log Receita Total", "Rg Scl - Log Valor Vendas On"
    )
  
  listaGraficosLog <- vector("list", length(vetorNomesLog))
  for (w in seq_along(vetorNomesLog)) {
    listaGraficosLog[[w]] <- ggplot(baseScatter, 
      aes_string(x = "Cluster", y = vetorVariaveisLog[w], fill = "Cluster")) + 
      geom_boxplot() + 
      labs(y = vetorNomesLog[w])
  }
  
  grafBoxPlotLog <- grid.arrange(
    listaGraficosLog[[1]], listaGraficosLog[[2]], 
    listaGraficosLog[[3]], listaGraficosLog[[4]], 
    listaGraficosLog[[5]], listaGraficosLog[[6]],
    ncol = 3
  )
  
    # NÍVEL ----
  vetorVariaveisNivel <- c(
    "RECEITA_SQRAP", "RECEITA_COMPRA_ON_COM_JUROS", "RECEITA_COMPRA_OFF_COM_JUROS",
    "RECEITA_COMPRA_OFF", "RECEITA_TOTAL", "VALOR_COMPRA_ON"
    )
  vetorNomesNivel <- c(
    "Receita Saque Rapido (R$)", "Receitas Compras Parceladas (On)", 
    "Receitas Compras Parceladas Juros (Off)", "Receita Compras OFF (R$)", 
    "Receita Total (R$)", "Valor Compras ON (R$)"
  )
  
  listaGraficosNivel <- vector("list", length(vetorNomesNivel))
  for (w in seq_along(vetorNomesNivel)) {
    listaGraficosNivel[[w]] <- ggplot(baseScatter, 
      aes_string(x = "Cluster", y = vetorVariaveisNivel[w], fill = "Cluster")) + 
      geom_boxplot() + 
      labs(y = vetorNomesNivel[w])
  }
  
  grafBoxPlotNivel <- grid.arrange(
    listaGraficosNivel[[1]], listaGraficosNivel[[2]], 
    listaGraficosNivel[[3]], listaGraficosNivel[[4]], 
    listaGraficosNivel[[5]], listaGraficosNivel[[6]],
    ncol = 3
  )
  
  # FUNÇÃO: GGPAIRS ----
  posNivel <- which(colnames(baseScatter) %in% vetorVariaveisNivel)
  posLog <- which(colnames(baseScatter) %in% vetorVariaveisLog)
  
  grafDensHistNivel <- ggpairs(baseScatter[, posNivel], title = "Variáveis Nível")
  grafDensHistLog <- ggpairs(baseScatter[, posLog], title = "Variáveis Log")
  grafDensHistNivelCluster <- ggscatmat(baseScatter, columns = posNivel, color = "Cluster")
  grafDensHistLogCluster <- ggscatmat(baseScatter, columns = posLog, color = "Cluster")
  
  # RESULTADO FUNÇÃO ----
  resultado <- list(
    gScatter3D = scatter3D, dfSumarioClusterNP = sumarioCluster, 
    dfSumarioClusterParametrico = dfPorCluster, gBoxPlotLog = grafBoxPlotLog,
    gBoxPlotNivel = grafBoxPlotNivel, gDensScatterNivel = grafDensHistNivel, 
    gDensScatterLog = grafDensHistLog, gDensScatterNivelCl = grafDensHistNivelCluster,
    gDensScatterLogCl = grafDensHistLogCluster
  )
return(resultado)
}


# RESULTADOS CLUSTERS ----
  # RESULTADOS CLUSTERS: GERAR RELATÓRIOS ----
resultadosClusters <- split(resultadosClusters, f = resultadosClusters$largSil)
clusterRelatorioFinal <- lapply(resultadosClusters, function(x) {
  gerarRelatorio(
    x = baseCluster[, {
      x$Iteracao[1] %>%  # COLUNAS DE REFERÊNCIA
        as.character %>% strsplit(., split = "\\|") %>% unlist %>% as.numeric
      }],
    y = x$Cluster
  )
})
  # RESULTADOS CLUSTERS: NOMEAR LISTA RELATÓRIOS ----
    # ORDEM: MÉTODO, VARIÁVEIS, N CLUSTERS
names(clusterRelatorioFinal) <- data.frame(
  SilhuetaMax = names(resultadosClusters) %>% as.numeric %>% round(., digits = 6)
) %>%
  merge(., {clustersParaRelatorio %>%
      select(SilhuetaMax, Cluster, Iteracao, NumClusters, nVariaveis) %>%
      mutate(SilhuetaMax = round(SilhuetaMax, digits = 6))
  }, all.x = TRUE) %>%
  select(-SilhuetaMax, -nVariaveis) %>%
  apply(., 1, function(y) {paste0(y, collapse = "<>")})

  # RESULTADOS CLUSTER: FINAL ----
clusterRelatorioFinal




# TABELAS DESCRITIVAS ----
  # TABELA DESCRITIVA: PROPORÇÕES ON, OFF, ONOFF ----
baseR <- within(baseR, {
  TipoUso = ifelse(
    VALOR_COMPRA_ON > 0 & VALOR_COMPRA_OFF == 0, "Apenas On",
    ifelse(
      VALOR_COMPRA_ON == 0 & VALOR_COMPRA_OFF > 0, "Apenas Off",
      "On e Off")
  )
})

dfPorUso <- baseR %>%
  group_by(
    TipoUso
  ) %>%
  summarise(
    TamGrupo = n_distinct(CD_CLNT),
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
    PropMulher = mean(SEXO == 'F')
    #PropFatura = sum(VALOR_COMPRA_CARNE) / (sum(VALOR_COMPRA_FATURA) + sum(VALOR_COMPRA_CARNE))
  ) %>%
  mutate(
    PropGrupo = TamGrupo / sum(TamGrupo),
    Margem = Receita / Gasto,
    MargemOn = ReceitaMediaOn / GastoMedioOn,
    MargemOff = ReceitaMediaOff / ReceitaMedia,
    MargemSqRap = ReceitaMediaSqRap / GastoMedioSqRap,
    PropRecOn = ReceitaMediaOn / ReceitaMedia,
    PropRecOff = ReceitaMediaOff / ReceitaMedia,
    PropRecSqRap = ReceitaMediaSqRap / ReceitaMedia,
    PropReceitaTarifas = ReceitaMediaTarifas / ReceitaMedia,
    PropReceitaJuros = ReceitaMediaJurosAtraso / ReceitaMedia
  )


  # TABELA DESCRITIVA: CLUSTERS GERADOS ----


# ANÁLISE EXPLORATÓRIA: PRÉ CLUSTER ----
df <- baseR %>%
  select(
    LOG_RECEITA_COMPRA_ON_COM_JUROS,
    LOG_RECEITA_COMPRA_OFF,
    LOG_RECEITA_COMPRA_OFF_COM_JUROS,
    LOG_RECEITA_SQRAP,
    LOG_RECEITA_SAQUE_CARTAO,
    LOG_VALOR_TARIFAS_OFF,
    LOG_VALOR_REFINANCIAMENTO_OFF,
    LOG_VALOR_JUROS_OFF
    #PROP_INADIMP
  ) %>%
  lapply(., function (x) {(x-min(x))/(max(x)-min(x))}) %>% do.call(cbind, .) %>%
  {.[obsAmostra, ]}

prcomp(df) %>% summary
autoplot(prcomp(df), x = 1, y = 2, data = df, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
autoplot(prcomp(df), x = 3, y = 4, data = df, #colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



# GRÁFICO DISTRIBUIÇÃO SAQUE RÁPIDO [QUANDO EFETUA]
ggplot(baseScatter[baseScatter$LOG_RECEITA_SQRAP > 0.1,], aes(x = LOG_RECEITA_SQRAP)) + geom_histogram()
