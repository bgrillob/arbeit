library(httr)
library(magrittr)
library(tidyverse)
library(rvest)
require(xml2)
setwd("/media/crikket/DATABASE/The Functional Method 2.0/")
rm(list = ls())
# DIRETÓRIO PODE SER UMA URL
diretorios <- list.dirs()
diretorios <- diretorios[grepl("Week", diretorios)]

funcaoBaixarVideos <- function(diretorio) {
  # arquivos com links
  arquivos <- list.files(diretorio)
  arquivos <- arquivos[grepl("html", arquivos) & !grepl("widget", arquivos) & !grepl("iframe", arquivos)]
  
  # pegar links wistia (com máscara)
  linksPre <- vector("list", length(arquivos))
  for (w in seq_along(arquivos)) {
    texto <- paste(readLines(paste(diretorio, arquivos[w], sep = "/")), collapse="\n")
    texto <- strsplit(texto, split = " ") %>%
      unlist
    texto <- texto[grepl("wistia.net", texto) & grepl("url", texto)] 
    linksPre[[w]] <- substring(texto, first = str_locate(texto, "http")[1], last = nchar(texto))
    rm(texto)
  }
  linksPre <- unlist(linksPre)
  
    # procurar links diretos com '.bin' (vídeos de 540p)
  linksVideo <- NULL
  for (w in seq_along(linksPre)) {
    texto <- readLines(linksPre[w])
    texto <- texto[grepl(".bin", texto)]
    texto <- strsplit(texto, split = "type") %>% unlist
    texto <- texto[grepl("960", texto) & grepl("540p", texto)]
    texto <- texto[1]
    texto <- substring(texto, str_locate(texto, "http")[1], last = str_locate(texto, ".bin")[2])
    linksVideo[w] <- texto
    rm(texto)
  }
  # baixar videos
  semana <- substring(diretorio, first = 12, last = 12)
  dia <- substring(diretorio, first = 20, last = 20)
  for (w in seq_along(linksVideo)) {
    nomeArquivo <- paste("w0", semana, "d0", dia, " - Video 0", w, ".mp4", sep = "")
    download.file(linksVideo[w], destfile = nomeArquivo)
  }
}


for (w in seq_along(diretorios)) {
  funcaoBaixarVideos(diretorio = diretorios[w])
}
