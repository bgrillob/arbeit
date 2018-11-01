#IMPORTANTE - verificar tempo para cálculo
tempo <- proc.time()
função()
print(proc.time() - tempo)


## PROBLEMA 1 - SOMA MULTIPLOS DE 3 e 5 MENOR DO QUE 1000 ----
total <- 0	
for (i in 1:999) {
  if (i %% 3 == 0 | i %% 5 == 0) {
    total <- total + i
  }
}

## PROBLEMA 2 - SOMA SEQUÊNCIA FIBONACCI MENOR DO QUE 4kk e PARES ----
fibonacci <- function(repetições){
  fn <- NULL
  fn[1] <- 1
  fn[2] <- 1
  for (i in 3:repetições) {
    fn[i] <- fn[i - 1] + fn[i - 2]
  }
  return(fn)
}

serie <- fibonacci(100)
format(sum(serie[serie %% 2 == 0 & serie < 4000000]), scientific = FALSE)

## PROBLEMA 3 - ACHAR O MAIOR NÚMERO PRIMO DA FATORIZAÇÃO DE 600851475143 ----

retorna_primos <- function(número){
    é.primo <- function(num){ 
        resultado <- 0
        if (num == 2) {
            resultado <- TRUE
        }
        else if (any(num %% 2:floor(num/2) == 0)) {
            resultado <- FALSE
        }
            else {
                resultado <- TRUE
            }
    return(resultado)
  }
  
  lista.primos <- NULL 
  máximo.primo <- 10000 
  for (i in 1:máximo.primo) { 
    if (é.primo(i) & número %% i == 0) {
      lista.primos <- c(lista.primos, i)
    }
  }
  while (prod(lista.primos) != número) { 
    máximo.primo <- máximo.primo + 10000 
    for (i in 1:máximo.primo) {
      if (é.primo(i) & número %% i == 0) {
        lista.primos <- c(lista.primos, i)
      }
    }
  }
  return(lista.primos)
}

max(retorna_primos(600851475143))

## PROBLEMA 4 - ACHAR O MAIOR PALINDROMO GERADO PELO PRODUTO DE DOIS NÚMEROS COM TRÊS DÍGITOS ---- 

palindromo <- function(menor, maior){
  distancia <- maior - menor
  vetor1 <- rep(seq(menor, maior, 1), times = distancia)
  vetor2 <- rep(seq(menor, maior, 1), each = distancia)
  produto <- vetor1 * vetor2
  
  gerar_inverso <- function(número){
    spliter <- as.character(unlist(strsplit(as.character(número), split = "")))
    n <- length(spliter)
    novo <- NULL
    for (i in 1:n) {
      novo[i] <- spliter[n - i + 1]
    }
    novo <- as.numeric(paste(novo, collapse = ''))
    return(novo)
  }
  
  invertido <- NULL
  for (i in 1:length(produto)) {
    invertido[i] <- gerar_inverso(produto[i])
  }
  
  resultado <- cbind(vetor1, vetor2, produto, invertido, produto - invertido)
  resultado <- as.data.frame(resultado)
  resultado <- resultado[resultado$V5 == 0,]
  return(resultado)
}

## PROBLEMA 5 - menor valor divisível (isto é, sem resto) por 1 a 20; importante notar que sempre precisa terminar em 0 ---- 

menor_múltiplo <- function(divisor_máximo){ #maior valor da sequência
  divisor_mínimo <- 1
  máximo_temporário <- 10000
  é.divisível <- function(número, mínimo, máximo){
    resultado <- NULL
    if ( all(número %% mínimo:máximo == 0)) {
      resultado <- TRUE
    }
    else {resultado <- FALSE}
    return(resultado)
  }
  espaçamento <- NULL
  if (divisor_máximo > 9) {
    espaçamento <- 10
  }
  else {espaçamento <- 1}
  
  
  lista <- NULL
  
  for (i in seq(divisor_máximo, máximo_temporário, espaçamento)) {
    if (é.divisível(i, divisor_mínimo, divisor_máximo)) {
      lista <- c(lista, i)}
  }
  
  while (length(lista) == 0) {
    máximo_anterior <- máximo_temporário #setting a previous to avoid repeating numbers since máximo_divisor
    máximo_temporário <- máximo_temporário + 100000
    for (i in seq(máximo_anterior, máximo_temporário, espaçamento)) {
      if (é.divisível(i, divisor_mínimo, divisor_máximo)) {
        lista <- i
      }
    }
  }
  return(min(lista))
}

menor_múltiplo(20)

## PROBLEMA 6 - diferança da soma dos quadrados e quadrado da soma ---- 

diferença <- function(fim, início = 1){
  série <- seq(início, fim, by = 1)
  soma_quadrados <- sum(série ^ 2)
  quadrado_soma <- sum(série) ^ 2
  diferença <- quadrado_soma - soma_quadrados
  return(diferença)
}

## PROBLEMA 7 - enésimo primo ---- 
enésimo_primo <- function(n){
  é.primo <- function(num){ #FUNCTION TO ASSESS WHETHER IT'S A PRIME NUMBER OR NOT
    resultado <- 0
    if (num == 2) {
      resultado <- TRUE
    }
    else if (any(num %% 2:(num - 1) == 0)) {
      resultado <- FALSE
    }
    else {
      resultado <- TRUE
    }
    return(resultado)
  }
  
  lista_primos <- NULL
  número_máximo <- 95000
  
  while (length(lista_primos) < n) {
    lista_primos <- NULL
    número_máximo <- número_máximo + 10000
    for (i in 2:número_máximo) {
      if (é.primo(i)) {
        lista_primos <- c(lista_primos, i)
      }
    }
  }
  return(lista_primos[i])
}

## PROBLEMA 8 - maior produto entre a série ---- 

maior_produto <- function(número, n_adjacentes = 13){
  lista_dígitos <- as.character(unlist(strsplit(as.character(número), split = "")))
  lista_produto <- NULL
  for (i in 1:(length(lista_dígitos) - n_adjacentes + 1)) {
    lista_produto[i] <- prod(as.numeric(lista_dígitos[i:(i + n_adjacentes - 1)]))
  }
  return(max(lista_produto))
}

## PROBLEMA 9 - tripla pitagórica (a² + b² = c²) cuja soma seja igual à 1000, tal que a < b < c ---- 
#http://stackoverflow.com/questions/2817848/find-pythagorean-triplet-for-which-a-b-c-1000
soma_tripla <- function(valor_soma){
  x_a <- 0
  x_b <- 0
  x_c <- 0
  valores <- NULL
  for (x_a in 1:(valor_soma/2)) {
    for (x_b in 1:(valor_soma/2)) {
      x_c <- (x_a ^ 2 + x_b ^ 2) ^ 0.5
      soma <- x_a + x_b + x_c
      if (soma == valor_soma) {
        valores <- c(x_a, x_b, x_c)
      }
    }
  }
  return(valores)
}
soma_tripla(1000)
prod(soma_tripla(1000))

## PROBLEMA 10 - soma dos primos abaixo de 2 milhões ---- 

soma_primos <- function(n){
  é.primo <- function(num){ 
    resultado <- 0
    if (num == 2) {
      resultado <- TRUE
    }
    else if (any(num %% 2:(num - 1) == 0)) {
      resultado <- FALSE
    }
    else {
      resultado <- TRUE
    }
    return(resultado)
  }
  
  lista_primos <- NULL
  número_máximo <- n	
  
  for (i in 2:número_máximo) {
    if (é.primo(i)) {
      lista_primos <- c(lista_primos, i)
    }
  }
            
  somatorio <- sum(as.numeric(lista_primos[lista_primos < n])) #.Machine$integer.max é 2bilhões 2,147,483,647
  return(somatorio)
}

teste <- NULL
for (i in seq(10000, 150000, 10000)) {
  inicio <- proc.time()
  soma_primos(i)
  teste <- rbind(teste, c(i, proc.time() - inicio))
}
    ## ALTERNATIVA MAIS EFICIENTE
soma_primos <- function(maximo){ #alternativa mais eficiente
  lista <- 1:maximo
  for (i in 2:max(lista)) {
    if (i != 0 & 2*i < maximo) {
      for (j in seq(2*i, maximo, by = i)) {
        lista[j] = 0
      }
    }
  }
  soma <- sum(lista) - 1
  return(soma)
}

## PROBLEMA 11 - maior produto de 4 termos adjacentes de uma matriz ---- 
maior_produto <- function(matriz, n_adjacentes){
  n_linhas <- dim(matriz)[1]
  n_colunas <- dim(matriz)[2]
  
  horizontal <- NULL
  for (i in 1:n_linhas) {
    for (j in 1:(n_colunas - n_adjacentes + 1)) {
      horizontal <- c(horizontal, 
                      prod(matriz[i, j:(j + n_adjacentes - 1)])
      ) 
    }
  }
  vertical <- NULL
  for (i in 1:(n_linhas  - n_adjacentes + 1)) {
    for (j in 1:n_colunas) {
      vertical <- c(vertical,
                    prod(matriz[i:(i + n_adjacentes - 1),j])
      )
    }	
  }		
  
  subconjunto <- NULL
  diagonal_esquerda <- NULL  #esquerda para direita decrescente
  diagonal_direita <- NULL  #esquerda para direita crescente
  
  for (i in 1:(n_linhas  - n_adjacentes + 1)) {
    for (j in 1:(n_colunas - n_adjacentes + 1)) {
      subconjunto <- matriz[i:(i + n_adjacentes - 1), j:(j + n_adjacentes - 1)]
      diagonal_esquerda <- c(diagonal_esquerda,
                             prod(subconjunto[row(subconjunto) - col(subconjunto) == 0])
      )
      diagonal_direita <- c(diagonal_direita,
                            prod(subconjunto[row(subconjunto) + col(subconjunto) == (dim(subconjunto)[1] + 1)])
      )
    }
  }
  
  final <- c(horizontal, vertical, diagonal_esquerda, diagonal_direita)
  return(max(final))
}

## PROBLEMA 12 - primeiro valor triangular a ter 500 divisores (inclusive 1 e próprio valor) - triangular(12375) ---- 
triangular_divisores <- function(n_divisores){
  
  triangular <- function(número){
    resultado <- sum(as.numeric(1:número))
    return(resultado)
  }
  
  qtde_divisores <- function(número){
    resultado <- sum(as.numeric(número %% 1:ceiling(número/2) == 0)) + 1 #adicionar 1 devido à remoção do número/número
    return(resultado)
  }
  
  valores <- NULL #lista com números que tem mais do que n_divisores 
  divisores <- 0 #string para parar o loop while
  repets_inferior <- 1
  repets_superior <- n_divisores * 5 #intervalo em que deve ser rodado
  
  incremento <- NULL
  if (n_divisores < 100) {
    incremento <- 50000
  }
  else {incremento <- 200000}
  
  
  for (i in repets_inferior:repets_superior) {
    if (qtde_divisores(triangular(i)) >= n_divisores) {
      valores <- c(valores, triangular(i))
      divisores <- max(valores)
    }
  }
  
  while (divisores < n_divisores) {
    repets_inferior <- repets_superior
    repets_superior <- repets_superior + incremento
    for (i in repets_inferior:repets_superior) {
      if (qtde_divisores(triangular(i)) >= n_divisores) {
        valores <- c(valores, triangular(i))
        divisores <- max(valores)
      }
    }
  }
  return(valores)
}		

## PROBLEMA 13 - primeiros dez dígitos da soma de 100 valores com 50 números ---- 

quadro <- read.table("X:/numero.csv", header = FALSE, sep = ";", numerals = "no.loss")
vetor <- as.character(levels(quadro[1,1]))
as.character(sum(as.numeric(vetor)))


## PROBLEMA 14 - sequencia collatz mais longa

collatz <- function(número_inicial){
  lista <- número_inicial
  while (lista[length(lista)] != 1) {
    comprimento <- length(lista)
    if (lista[comprimento] %% 2 == 1) {
      lista[comprimento + 1] <- (lista[comprimento] * 3) + 1
    }
    else{
      lista[comprimento + 1] <- lista[comprimento] / 2
    }
  }
  return(lista)
}

lista <- NULL
inicio <- 800000
fim <- 1000000
for (i in inicio:fim) {
  lista[i] <- length(collatz(i))
}
which(lista == max(lista))

## PROBLEMA 15 - caminhos num grid simétrico ---- 

caminhos_grid <- function(n_linhas = 2){
  n_movimentos <- 2 * n_linhas #quantidade de deslocamentos até chegar no ponto final
  mov_unidir <- n_linhas #máximo de movimentos unidirecionais
  resultado <- factorial(n_movimentos) / factorial(mov_unidir) ** 2
  return(resultado)
}

## PROBLEMA 16 - power digit sum 2^1000 | PROBLEMA: necessary to call C routine, too large ---- 
library(gmp)
soma_digitos <- function(expoente, base = 2){
  options(scipen = 9999)
  numero <- as.bigz(base ** expoente)
  spliter <- as.character(unlist(strsplit(as.character(numero), split = "")))
  resultado <- sum(as.integer(spliter))
  options(scipen = 0)
  return(resultado)
}

## PROBLEMA 17 - NÚMERO DE LETRAS PALAVRAS (limite de números 4 dígitos) ---- 
contagem_letras_soma <- function(inicio = 1, fim = 1000){
  
  criar_palavra <- function(numero){
    #lista para evitar problema do dez ao dezenove
    lista_dec <- c('one', 'two', 'three', 'four', 'five', 'six', 'seven',	'eight', 'nine', 'ten',
                   'eleven', 'twelve','thirteen', 'fourteen','fifteen','sixteen','seventeen','eighteen','nineteen','twenty',
                   'twenty-one','twenty-two','twenty-three','twenty-four','twenty-five','twenty-six','twenty-seven','twenty-eight',
                   'twenty-nine','thirty','thirty-one','thirty-two','thirty-three','thirty-four','thirty-five','thirty-six',
                   'thirty-seven','thirty-eight','thirty-nine','forty','forty-one','forty-two','forty-three','forty-four','forty-five',
                   'forty-six','forty-seven','forty-eight','forty-nine','fifty','fifty-one','fifty-two','fifty-three','fifty-four',
                   'fifty-five','fifty-six','fifty-seven','fifty-eight','fifty-nine','sixty','sixty-one','sixty-two','sixty-three',
                   'sixty-four','sixty-five','sixty-six','sixty-seven','sixty-eight','sixty-nine','seventy','seventy-one',
                   'seventy-two','seventy-three','seventy-four','seventy-five','seventy-six','seventy-seven','seventy-eight','seventy-nine',
                   'eighty','eighty-one','eighty-two','eighty-three','eighty-four','eighty-five','eighty-six','eighty-seven',
                   'eighty-eight','eighty-nine','ninety','ninety-one','ninety-two','ninety-three','ninety-four','ninety-five',
                   'ninety-six','ninety-seven','ninety-eight','ninety-nine'
    )
    
    numero <- sprintf("%05d", numero)
    ref_dez <- substr(numero, start = 4, stop = 5)
    dezena <- lista_dec[as.numeric(ref_dez)]
    
    ref_cent <- substr(numero, start = 3, stop = 3)
    conector_centena <- ifelse((ref_cent != "0" & ref_dez != "00"), "and", "")
    centena <- paste(lista_dec[as.numeric(ref_cent)], 
                     ifelse(ref_cent != 0, "hundred", ""),
                     sep = " "
    )
    
    ref_mil <- substr(numero, start = 1, stop = 2)
    conector_milhar <- ifelse((ref_mil != "00" & ref_cent != "0"), "and", "")
    milhar <- paste(lista_dec[as.numeric(ref_mil)], 
                    ifelse(as.numeric(ref_mil) != 0, "thousand", ""),
                    sep = " "
    )
    resultado <- paste(milhar, conector_milhar, centena, conector_centena, dezena, sep = " ")
    return(resultado)
  }
  
  lista <- NULL
  for (i in inicio:fim) {
    palavra <- criar_palavra(i)
    letras <- unlist(strsplit(palavra, ""))
    lista[i] <- length(letras[letras != " " & letras != "-"])
  }
  
  total <- sum(lista, na.rm = TRUE)
  return(total)
}

## PROBLEMA 18 - CAMINHO DE MAIOR SOMA (somente adjacentes) ---- 
la <- as.matrix(read.table("X:/trng - piramd.txt", sep = "\t", header = FALSE))
caminho_maximo <- function(matriz){
  n_linhas <- dim(matriz)[1] #quanto precisa descer
  matriz[2, 1:2] <- matriz[2, 1:2] + matriz[1,1] #gerando acumulados segunda linha
  for (i in 3:n_linhas) {
    matriz[i, 1] <- matriz[i, 1] + matriz[(i - 1), 1]
    matriz[i, i] <- matriz[i, i] + matriz[(i - 1), (i - 1)]
    for (j in 2:(i - 1)) {
      matriz[i, j] <- matriz[i, j] + max(matriz[(i - 1), (j - 1):j])
    }
  }
  vetor <- max(matriz[n_linhas,])
  return(vetor)
}

## PROBLEMA 19 - CONTAGEM DE DOMINGOS NO PRIMEIRO DIA DO MÊS NO PERÍODO DE 01/01/1901 a 31/12/2000 ---- 

contagem_dias <- function(inicio, fim, dia_ref = "01", dia_semana = "domingo"){
  lista <- seq(inicio, fim, by = 1)
  if (inherits(lista, "Date")) {
    resultado <- sum(as.logical(lista[weekdays(lista) == dia_semana & format(lista, "%d") == dia_ref]))
  }
  else{
    stop("início e fim precisam ser da classe Date")
  }
  return(resultado)
}

## PROBLEMA 20 - FACTORIAL DIGIT SUM #numeros grandes em R, sempre com gmp ---- 
library(gmp)
soma_digitos_fatorial <- function(numero){
  options(scipen = 9999)
  fatorial <- factorialZ(numero)
  spliter <- as.character(unlist(strsplit(as.character(fatorial), split = "")))
  resultado <- sum(as.integer(spliter))
  options(scipen = 0)
  return(resultado)
}

## PROBLEMA 21 - NUMEROS AMIGOS abaixo de 10000 ---- 
soma_amigos <- function(superior = 10000, inferior = 1){
  divisores_exatos <- function(numero){
    lista_num <- 1:(numero/2)
    divisores <- numero %% lista_num == 00
    resultado <- lista_num[divisores == TRUE]
    return(resultado)
  }
  
  vetor <- NULL
  for (i in inferior:superior) {
    n1_amigo <- sum(divisores_exatos(i))
    n2_amigo <- sum(divisores_exatos(n1_amigo))
    if (n2_amigo == i & i != n1_amigo) {
      vetor[i] <- i
    }
  }
  resultado <- vetor[is.na(vetor) == FALSE] #sum(vetor, na.rm = TRUE)
  return(resultado)
}

## PROBLEMA 22 - SOMA LETRAS RANK (problemas na leitura base dados) ---- 
dados <- read.table("https://projecteuler.net/project/resources/p022_names.txt", header = FALSE, sep = ",")
dados <- t(as.matrix(dados))
row.names(dados) <- NULL

soma_rank <- function(dados){ #precisa ser matriz
  dados <- sort(dados) #ordenar
  n <- length(dados)
  letras <- toupper(letters) #vetor letras maiúsculas
  transformar_e_somar <- function(nome){
    lista_digitos <- as.character(unlist(strsplit(as.character(nome), split = "")))
    for (i in 1:length(lista_digitos)) {
      lista_digitos[i] <- which(letras == lista_digitos[i])
    }
    resultado <- sum(as.numeric(lista_digitos))
    return(resultado)
  }
  vetor <- NULL
  for (i in 1:n) {
    vetor[i] <- as.numeric(transformar_e_somar(dados[i]) * i)
  }
  resultado <- sum(vetor)
  return(resultado)
}

## PROBLEMA 23 - soma números não abundantes ---- 
problema_23 <- function(maximo_incerto = 28123){
  divisores_exatos <- function(numero){
    lista_num <- 1:(numero/2)
    divisores <- numero %% lista_num == 00
    resultado <- lista_num[divisores == TRUE]
    return(resultado)
  }
  #todos números maiores que 28123 podem ser expressos como a soma de dois abundantes
  lista_abundantes <- NULL
  for (i in 2:maximo_incerto) {
    if (i < sum(divisores_exatos(i))) {
      lista_abundantes[i] <- i
    }
  }
  
  lista_abundantes <- lista_abundantes[is.na(lista_abundantes) == FALSE]
  n <- length(lista_abundantes)
  lista_numeros <- 1:(maximo_incerto - 1)
  
  for (i in 1:n) {
    reduzir_loop <- i
    for (j in reduzir_loop:n) {
      if (sum(lista_abundantes[i], lista_abundantes[j]) < maximo_incerto) {
        lista_numeros[sum(lista_abundantes[i], lista_abundantes[j])] <- 0
      }
    }
  }
  resultado <- sum(lista_numeros, na.rm = TRUE)
  return(resultado)
}

## PROBLEMA 24 - 1,000,000 combinação - 0 1 2 3 4 5 6 7 8 9; infos: (i) enésimo dígito; (ii)  2783915460 ---- 

enésima_permutação <- function(digitos = seq(0, 9, 1), n = 1000000){
  índices <- digitos
  permuta <- NULL
  comp <- length(digitos)
  anterior <- 0
  for (i in 1:(comp - 1)) {
    comp_atual <- length(digitos) #length do vetor continuamente alterada
    n_índice <- max(which(índices * factorial(comp_atual - 1) + anterior < n)) #qual índice
    anterior <- índices[n_índice] * factorial(comp_atual - 1) + anterior #valor de referência posição anterior (acumulado)
    permuta[i] <- digitos[n_índice]
    digitos <- digitos[-n_índice] #valores a serem inseridos
    índices <- seq(0, length(digitos) - 1, 1) #índice crescente 
  }
  permuta <- c(permuta, digitos) #adiciona último dígito
  return(permuta)
}       

## PROBLEMA 25 - PRIMEIRO NÚMERO FIBONACCI COM 1000 DÍGITOS ---- 
library(gmp) #require gmp
primeiro_fibonacci <- function(n_digitos = 1000){
  i <- 5
  comprimento <- length(as.character(unlist(strsplit(as.character(fibnum(i)), split = ""))))
  while (comprimento < n_digitos) {
    i <- i + 1
    comprimento <- length(as.character(unlist(strsplit(as.character(fibnum(i)), split = ""))))
  }
  return(i)
}

## PROBLEMA 26 - CICLO MAIS LONGO DE REPETIÇÕES 1/N (a concluir) ---- 
sequencia_ciclica <- function(numero){
    x <- 1 # numerador 1
    resto <- c(x)
    repeat {
        x <- (x * 10) %% numero  # achar o resto
        if (x == 0) { # se o resto for 0, ciclo acaba
            return(0)
        }
        if ((x %in% resto)) { # o resto é igual a um dos elementos anteriores, encontrou-se ciclo
            break
        }
        else {
            resto <- c(resto, x) # se o número não apareceu até agora, adiciona-se à lista
        }
    }
    comp_resto <- length(resto) # comprimento ciclo
    excedente <- which(resto == x) # excluir parte que não repete
    resultado <- comp_resto - excedente + 1
return(resto)
}

sequencia_mais_longa <- function(numero) {
    comp_maximo <- 0
    numero_gerador <- 0
    for (i in 1:numero) {
        comp_i <- sequencia_ciclica(i)
        if (comp_i > comp_maximo) {
            comp_maximo <- comp_i
            numero_gerador <- i
        }
    }
return(numero_gerador)
}
 

## PROBLEMA 27 - COEFICIENTES EQUAÇÃO EULER MAIOR SEQUENCIA PRIMOS ---- 
        # ABORADGEM: CRIAR LISTA PRIMOS
        # CRIAR FÓRMULA PARA A EQUAÇÃO
        # ITERAÇÃO (EXCLUIR SE PRIMEIRO NÃO FOR PRIMO) PARA VER SE GERA CONSECUTIVAMENTE
    
    # RETORNA VETOR RESULTADOS FUNÇAO QUADRADA EULER
euler_quadrada <- function(alpha, beta, n_gerar) {
    vetor <- seq(1, n_gerar, by = 1)
    resultado <- vetor ^ 2 + alpha * vetor + beta
return(resultado)
}
    # CRIAR LISTA PRIMOS - MÁXIMO = N_PRIMOS; 10k = 1230; 20k 2263; 100k = 9593; 2kk = 148934 
criar_lista_primos <- function(maximo, n_primos = NULL){
    lista <- 1:maximo
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    lista <- lista[lista != 0]
    if (is.null(n_primos)) {
        n_primos <-    length(lista)
    }
    return(lista[1:n_primos])
}

    # ITERAÇÃO ENTRE FÓRMULAS
lista_primos <- criar_lista_primos(maximo = 200000)
tempo <- proc.time()
alpha_maximo <- 1000
beta_maximo <- 1000
resultado <- matrix(0, nrow = 1, ncol = 3)
for (a in -alpha_maximo:alpha_maximo) {
    for (b in -beta_maximo:beta_maximo) {
        numeros_gerados <- euler_quadrada(alpha = a, beta = b, n_gerar = 1000)
        primo_logico <- numeros_gerados %in% lista_primos
        comp_sequencia <- which(primo_logico == FALSE)[1] - 1
        if (comp_sequencia > 5) {
            resultado <- rbind(resultado, c(a, b, comp_sequencia))
        }
    }
}
proc.time() - tempo

linha_ref <- which.max(resultado[, 3])
alpha_final <- resultado[linha_ref, 1]
beta_final <- resultado[linha_ref, 2]
alpha_final
beta_final
# RESULTADO = - 59231 (-61 * 971)

## PROBLEMA 28 - SOMA DIAGONAL DA ESPIRAL (EM FORMA DE MATRIX) POR SEQUENCIA COM 1 AO CENTRO ---- 
    # COMPREENSÃO DO PROBLEMA
        # NÚMERO ELEMENTOS NA MATRIZ EM FUNÇÃO DO ENÉSIMO ESPIRAL (1º espiral c/ 9 elementos) = (2x + 1)^2 ou 4x²+4x+1
        # QTDE NOVOS ELEMENTOS É A PRIMEIRA DIFERENÇA DA FUNÇÃO ACIMA: 8x + 4
            # SERÁ MÚLTIPLO DE 4, E HAVERÁ 4 REPETIÇÕES DE MOVIMENTOS

valores_49 <- c(
    43, 44, 45, 46, 47, 48, 49,
    42, 21, 22, 23, 24, 25, 26,
    41, 20, 07, 08, 09, 10, 27,
    40, 19, 06, 01, 02, 11, 28,
    39, 18, 05, 04, 03, 12, 29,
    38, 17, 16, 15, 14, 13, 30,
    37, 36, 35, 34, 33, 32, 31
)
valores_81 <- c(
    73, 74, 75, 76, 77, 78, 79, 80, 81,
    72, 43, 44, 45, 46, 47, 48, 49, 50,
    71, 42, 21, 22, 23, 24, 25, 26, 51,
    70, 41, 20, 07, 08, 09, 10, 27, 52,
    69, 40, 19, 06, 01, 02, 11, 28, 53, 
    68, 39, 18, 05, 04, 03, 12, 29, 54,
    67, 38, 17, 16, 15, 14, 13, 30, 55,
    66, 37, 36, 35, 34, 33, 32, 31, 56,
    65, 64, 63, 62, 61, 60, 59, 58, 57
)
valores <- valores_81
dimensoes <- sqrt(length(valores_81))
matriz <- matrix(valores, nrow = dimensoes, ncol = dimensoes, byrow = TRUE)
posicoes <- matrix(0, ncol = 2, nrow = length(valores))
for (i in seq_along(valores)) {
    posicoes[i, ] <- which(matriz == i, arr.ind = TRUE)
}
dif_posicoes <- diff(posicoes)


    # ELABORAÇÃO SOLUÇÃO
        # ABORDAGEM MATEMÁTICA:
            # VER VALORES PRESENTES DE CADA DIAGONAL 4X (PARTINDO DO CENTRO)
            # ESTIMAR FUNÇÕES GERADORAS
            # ORIGEM PARA DIREITA SUPERIOR: (9, 25, 49, ...) = 4x² + 4x + 1
            # ORIGEM PARA DIREITA INFERIOR: (3, 13, 31, ...) = 4x² - 2x + 1
            # ORIGEM PARA ESQUERDA SUPERIOR: (7, 21, 43, ...) = 4x² + 2x + 1
            # ORIGEM PARA ESQUERDA INFERIOR: (5, 17, 37, ...) = 4x² + 0x + 1
            # GERAR 500 VALORES PARA CADA, SOMAR E + 2 (ORIGEM APARECE 2x NAS DIAGONAIS)
x <- seq(1, 500, by = 1)
y1 <- 4 * x ^ 2 + 4 * x + 1
y2 <- 4 * x ^ 2 - 2 * x + 1
y3 <- 4 * x ^ 2 + 0 * x + 1
y4 <- 4 * x ^ 2 + 2 * x + 1
sum(y1, y2, y3, y4) + 1
        # ABORDAGEM CONSTRUÇÃO ESPIRAL:
            # INICIAR COM MATRIZ 3X3 E APLICAR A LÓGICA INFRACITADA
            # GERAR QUANTIDADE DE NOVOS ELEMENTOS (MÚLTIPLO DE 4)
            # DIVIDIR NOVOS ELEMENTOS EM 4 PARTES, PARA GERAR PRIMEIRAS DIFERENÇAS
            # PRIMEIRAS DIFERENÇAS PRA COLUNA E LINHA
            # APLICAR AS DIFERENÇAS NUM LOOP CONTROLANDO A POSIÇÃO ADICIONADA ANTERIOR
            # GERAR MATRIZ
            # SOMA DIAGONAL (COL - LIN = 0) E DIAGONAL INVERSA [COL - LIN = MAX(LINHA) + 1] - POIS A MATRIZ SERÁ SIMÉTRICA

gerar_espiral <- function(dimensoes) {
    if (dimensoes %% 2 == 0) {
        stop("Precisa ser ímpar!")
    }
    valores_inicias <- 
    matriz <- matrix(0, nrow = dimensoes, ncol = dimensoes)
    posicao_inicial <- ceiling(dimensoes / 2)
    matriz[posicao_inicial, posicao_inicial] <- 1
    
}

colunas <- seq(3, 19, by = 2)
linhas <- seq(1, 17, by = 2)
qtde_novos_numeros <- 2 * colunas + 2 * linhas

## PROBLEMA 29 - QUANTIDADE VALORES UNICOS COMBINAÇÕES a^b e b^a ---- 
    # ABORDAGEM 1
require(gmp)
comb_potencias <- function(a_max, b_max) {
    a <- as.bigz(2:a_max)
    b <- as.bigz(2:b_max)
    resultado <- list()
    for (i in seq_along(a)) {
        resultado[[i]] <- as.character(a[i] ^ b)
    }
    resultado <- unlist(resultado)
    resultado <- unique(resultado)
return(resultado)
}
tempo <- proc.time()
length(comb_potencias(a_max = 100, b_max = 100))
proc.time() - tempo

    # ABORDAGEM 2 (FUNÇÃO OUTER)
length(unique(sort(c(outer(2:100,2:100,"^")))))

    # ABORDAGEM 3 (DO.CALL + GMP)
vetor <- as.bigz(2:100)
valores <- do.call("c", sapply(vetor, function(x) x ^ vetor))
length(unique(valores))

## PROBLEMA 30 - SOMA NÚMEROS QUE PODEM SER ESCRITOS COMO SOMA DA QUINTA POTÊNCIA DE SEUS DÍGITOS ---- 
potencia_digitos_numero <- function(n_potencia, intervalo = 1500:10000) {
    comp_digitos <- length(unlist(strsplit(as.character(max(intervalo)), "")))
    lista_split <- strsplit(as.character(intervalo), "")
    soma_digitos_potencia <- unlist(
        lapply(lista_split, function(x) sum(as.numeric(x) ^ n_potencia))
    )
    matriz_comparacao <- cbind(intervalo, soma_digitos_potencia, intervalo - soma_digitos_potencia)
    valores <- matriz_comparacao[which(matriz_comparacao[, 3] == 0), 1]
return(valores)
}

tempo <- proc.time()
sum(potencia_digitos_numero(n_potencia = 5, intervalo = 200:1000000))
proc.time() - tempo

## PROBLEMA 31 - NUMERO DE COMBINAÇÕES MOEDAS PARA FORMAR £2 (1p, 2p, 5p, 10p, 20p, 50p, £1, £2) ----

    # ABORDAGEM 1 - RECURSIVA
moedas <- c(200, 100, 50, 20, 10, 5, 2, 1)
n_maneiras <- function(valor_alvo, moedas_disponiveis) {
        # SE HOUVER APENAS UMA MOEDA DISPONÍVEL, HÁ APENAS UMA FORMA
    if (moedas_disponiveis == 1) {
        return(1)
    }
        # AJUSTAR RESULTADO = 0 PARA COMEÇAR RECURSÃO
    resultado <- 0
        # LOOP NAS MOEDAS DISPONÍVEIS
    for (i in seq(1, moedas_disponiveis)) {
        # REDUZIR ENÉSIMA MOEDA DO VALOR_ALVO
        restante <- valor_alvo - moedas[i]
        # SE RESTANTE FOR 0, HÁ APENAS MAIS UMA MANEIRA DE FAZER
        if (restante == 0) {
            resultado <- resultado + 1
        }
        # SE O RESTANTE FOR MAIOR DO QUE 0, PODE HAVER MAIS FORMAS, DAÍ COMEÇA A RECURSÃO
        # RECURSÃO VAI REDUZINDO O VALOR E VENDO POSSIBILIDADE PARA CADA BLOCO RESTANTE
        if (restante > 0) {
            resultado <- resultado + n_maneiras(restante, i)
        }
    }
return(resultado)
}

n_maneiras(200, length(moedas))

    # ABORDAGEM 2 - RECURSIVA PROJECT EULER FORUM
troco <- function(n, x){
    moedas <- c(200, 100, 50, 20, 10, 5, 2, 1)
    if (x == 0) {
        return(1)
    }
    if (n == 8) {
        return(1)
    }
    a <- x %/% (moedas[n]) # QUANTAS VEZES moedas[n] CABE EM x
    c <- 0
    for (i in 0:a) {
        c <- c + troco(n + 1, x - i * moedas[n])
    }
return(c)
}

## PROBLEMA 32 - ACHAR SOMA DO PRODUTO DOS NUMEROS PANDIGITAIS DE 1 A 9 ----
    # MONTAR POSSIBILIDADES ÚNICAS CUJA QUANTIDADE DE DÍGITOS DOS MULTIPLICADORES E PRODUTOS SEJA IGUAL À N_DIGITOS
tempo <- proc.time()
    # ELABORAR MATRIZ DE POSSIBILIDADES - 2 E 3 DÍGITOS; 1 E 4 DÍGITOS
combinacoes_possiveis <- rbind(
    cbind(
        rep(seq(2,9), each = length(seq(1000, 9999))),
        rep(seq(1000, 9999), times = length(seq(2, 9)))
    ),
    cbind(
        rep(seq(10, 99), each = length(seq(100, 999))),
        rep(seq(100, 999), times = length(seq(10, 99)))
    )
)
    # GERAR OS PRODUTOS E ELIMINAR PRODUTOS COM MAIS DE 5 DÍGITOS
combinacoes_possiveis <- cbind(combinacoes_possiveis, combinacoes_possiveis[, 1] * combinacoes_possiveis[, 2])
combinacoes_possiveis <- combinacoes_possiveis[combinacoes_possiveis[,3] <= 9999 ,]
    # FUNÇÃO PARA TESTAR SE É PANDIGITAL com 9 DÍGITOS (EXCLUSIVAMENTE 9 DÍGITOS)
é_pandigital <- function(n) {
    n <- as.numeric(unlist(strsplit(as.character(n), split = "")))
    n_digitos <- 9 #length(n)
    possiveis <- seq(1, n_digitos, by = 1)
    freq_digitos <- NULL
    for (i in seq_along(possiveis)) {
        freq_digitos[i] <- length(which(n == possiveis[i]))
    }
    resultado <- all(freq_digitos == 1)
    return(resultado)
}
    # APLICAR FUNÇÃO
posicoes_pandigitais <- apply(combinacoes_possiveis, 1, function(x) é_pandigital(c(x)))
posicoes_pandigitais <- which(posicoes_pandigitais == TRUE)

    # TRAZER RESULTADOS
combinacoes <- combinacoes_possiveis[posicoes_pandigitais, ]
resultado_32 <- sum(unique(combinacoes[, 3]))
proc.time() - tempo

## PROBLEMA 33 - FRAÇÕES COM CANCELAMENTO DE DÍGITOS IDÊNTICOS e RESULTADOS IDÊNTICOS ----
    # NUMERADOR E DENOMINADOR PRECISAM CONTER DOIS DÍGITOS
    # RAZÃO INFERIOR A 1 E EXCLUI-SE AS FRAÇÕES TRIVIAIS

    # GERAR VALORES POSSÍVEIS PARA AS FRAÇÕES
valores_numerador <- NULL
valores_denominador <- NULL
for (i in 10:99) {
    for (j in seq(i + 1, 99)) {
        posicao_i <- length(valores_numerador) + 1
        posicao_j <- length(valores_denominador) + 1
        valores_numerador[posicao_i] <- i
        valores_denominador[posicao_j] <- j
    }
}
    # TRANSFORMAR EM MATRIZ
matriz_combinacoes <- cbind(valores_numerador, valores_denominador)

    # FUNÇÃO PARA TESTAR CONDIÇÃO e TRAZER A FRAÇÃO GERADORA
cancelamento <- function(num, den){
    # ELIMINAR AS TRIVIAIS
    if (num %% 10 == 0 & den %% 10 == 0) {
        return("Trivial")
    }
    if (num %% 11 == 0 & den %% 11 == 0) {
        return("Trivial")
    }
    # SE NÃO FOR TRIVIAL, FAZER:
    else {
        resultado_fracao <- num / den
        # GERAR POSSÍVEIS VALORES ÚNICOS
        den_possiveis <- as.numeric(unlist(strsplit(as.character(den), split = "")))
        num_possiveis <- as.numeric(unlist(strsplit(as.character(num), split = "")))
        # DIGITOS EM COMUM (MÁXIMO = 2)
        digitos_comum <- unique(den_possiveis[which(den_possiveis %in% num_possiveis)])
        if (length(digitos_comum) == 0) {
            return(NA)
        }
        else {
            # GERAR RESULTADOS POSSÍVEIS
            resultados_possiveis <- NULL # onde irão os valores para comparar
            valores_fracao <- matrix(0, nrow = 4, ncol = 2) # matriz para armazenar valores de fração
            indice_pos <- 0 # índice para alterar posição e evitar necessidade c ou cbind
            for (i in seq_along(digitos_comum)) {
                indice_pos <- indice_pos + 1
                num_parcial <- ifelse(length(which(num_possiveis == digitos_comum[i])) == 0,
                    num_possiveis[1],
                    num_possiveis[which(num_possiveis != digitos_comum[i])]
                )
                den_parcial <- ifelse(length(which(den_possiveis == digitos_comum[i])) == 0,
                    den_possiveis[1],
                    den_possiveis[which(den_possiveis != digitos_comum[i])]
                )
                resultados_possiveis[indice_pos] <- num_parcial / den_parcial
                valores_fracao[indice_pos, ] <- c(num_parcial, den_parcial)
            }
            # VERIFICAR EXISTÊNCIA IDÊNTICOS
            if (any(resultados_possiveis %in% resultado_fracao)) {
                resultado <- paste(valores_fracao[which(resultados_possiveis %in% resultado_fracao == TRUE),],
                    collapse = "/"
                )
            }
            else {
                resultado <- NA
            }
            
        }
    }
    return(resultado)
} 

    # APLICAR FUNÇÃO NAS POSSIBILIDADES E DESCOBRIR QUAIS GERAM ESSE RESULTADO
resultados <- NULL
for (i in 1:dim(matriz_combinacoes)[1]) {
    resultados[i] <- cancelamento(matriz_combinacoes[i, 1], matriz_combinacoes[i, 2])
}
resultados <- cbind(matriz_combinacoes, resultados)
resultados <- resultados[which(is.na(resultados[, 3]) == FALSE), ]
resultados <- resultados[resultados[, 3] != "Trivial", ]

    # ACHAR O VALOR DO DENOMINADOR DO PRODUTO DAS QUATRO FRAÇÕES COM DENOMINADOR DADO NO MENOR TERMO COMUM
num_final <- prod(as.numeric(resultados[, 1]))
den_final <- prod(as.numeric(resultados[, 2]))

## PROBLEMA 34 - SOMA FATORIAL DOS DIGITOS  IGUAL AO NÚMERO ---- 
fatorial_digitos_numero <- function(intervalo = 10:100000) {
    comp_digitos <- length(unlist(strsplit(as.character(max(intervalo)), "")))
    lista_split <- strsplit(as.character(intervalo), "")
    soma_digitos_fatorial <- unlist(
        lapply(lista_split, function(x) sum(factorial(as.numeric(x))))
    )
    matriz_comparacao <- cbind(intervalo, soma_digitos_fatorial, intervalo - soma_digitos_fatorial)
    valores <- matriz_comparacao[which(matriz_comparacao[, 3] == 0), 1]
    return(valores)
}

## PROBLEMA 35 - PRIMOS CIRCULARES ABAIXO DE 1 MILHÃO ----
    # FUNÇÃO PARA CRIAR LISTA DE PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
return(lista[lista != 0])
}


    # FUNÇÃO PARA GERAR COMBINAÇÕES POSSÍVEIS (INCLUSIVE O PRÓPRIO NÚMERO)
combinacoes <- function(numero) {
    digitos <- as.numeric(unlist(strsplit(as.character(numero), split = "")))
    comp_numero <- length(digitos)
    lista_circulares <- NULL
    disp_atual <- digitos
    for (i in seq(1, comp_numero)) {
        disp_atual <- c(disp_atual[-1], disp_atual[1])
        lista_circulares[i] <- as.numeric(paste(disp_atual, collapse = ""))
    }
return(lista_circulares)
}
    # FUNÇÃO PARA VERIFICAR A CONDIÇÃO (ABORDAGEM FUNCIONAL)
é_primo_circular <- function(numeros) {
    é_primo <- function(num){
        resultado <- 0
        if (num == 2) {
            resultado <- TRUE
        }
        else if (any(num %% 2:floor(num/2) == 0)) {
            resultado <- FALSE
        }
        else {
            resultado <- TRUE
        }
        return(resultado)
    }
    resultados <- NULL
    for (i in seq_along(numeros)) {
        resultados[i] <- é_primo(numeros[i])
    }
return(all(resultados))
}

    # APLICAR À LISTA (FORMA FUNCIONAL)
lista <- lista_primos(1000000)
tempo <- proc.time()
for (i in seq_along(lista)) {
    if (!é_primo_circular(combinacoes(lista[i]))) {
        lista[i] <- 0
    }
}
lista <- lista[lista != 0]
proc.time() - tempo

    # APLICAR À LISTA (FORMA MEMÓRIA) - MUITO MAIS EFICIENTE
lista <- lista_primos(1000000)
tempo <- proc.time()
primos_circulares <- NULL
for (i in seq_along(lista)) {
    if (all(combinacoes(lista[i]) %in% lista)) {
        primos_circulares <- c(primos_circulares, lista[i])
    }
}
proc.time() - tempo
length(primos_circulares)

## PROBLEMA 36 - PALÍNDROMO DE DUAS FORMAS: BINÁRIO (BASE 2) E NÚMERO (BASE 10) ---- 
    # FUNÇÃO PARA GERAR O INVERSO DO NÚMERO
gerar_inverso <- function(numero){
    spliter <- as.character(unlist(strsplit(as.character(numero), split = "")))
    novo <- paste(rev(spliter), collapse = '') #as.numeric(paste(novo, collapse = ''))
return(novo)
}
    # FUNÇÃO PARA VERIFICAR SE É PALÍNDROMO
é_palíndromo <- function(numero) {
    num_inverso <- gerar_inverso(numero)
    resultado <- num_inverso == as.character(numero)
return(resultado)
}

    # FUNÇÃO PARA GERAR NÚMERO EM BASE 2 SEM LEADING ZEROS (E SEM OS TRAILING ZEROS)
gerar_base2 <- function(numero) {
    n_32bits <- as.numeric(intToBits(numero)) 
    pos_final <- max(which(n_32bits == 1))
    resultado <- paste(n_32bits[1:pos_final], collapse = "")
return(resultado)
}

    # APLICAR
tempo <- proc.time()
lista <- NULL
pos_ref <- 0
for (i in 1:999999) {
    if (é_palíndromo(i)) {
        if (é_palíndromo(gerar_base2(i))) {
            pos_ref <- pos_ref + 1
            lista[pos_ref] <- i
        }
    }
}
proc.time() - tempo

## PROBLEMA 37 - PRIMOS TRUNCÁVEIS (REMOVER DÍGITOS DA ESQUERDA E MANTÉM PRIMO) ---- 
    # FUNÇÃO PARA GERAR NÚMERO DE PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    return(lista[lista != 0])
}

    # GERAR COMBINAÇÕES (INCLUSIVE PRÓPRIO NÚMERO)
combinacoes_truncamento <- function(numero) {
    digitos <- as.numeric(unlist(strsplit(as.character(numero), split = "")))
    comp_numero <- length(digitos) - 1
    lista_dir_esq <- NULL
    lista_esq_dir <- numero
    for (i in seq(1, comp_numero)) {
        lista_esq_dir[i + 1] <- as.numeric(paste(digitos[-(1:i)], collapse = ""))
        lista_dir_esq[i] <- as.numeric(paste(rev(rev(digitos)[-(1:i)]), collapse = ""))
    }
    lista_gerados <- c(lista_esq_dir, lista_dir_esq)
    return(lista_gerados)
}

# APLICAR À LISTA (FORMA MEMÓRIA - MAIS EFICIENTE)
lista <- lista_primos(10000)
tempo <- proc.time()
primos_truncaveis <- NULL
for (i in seq_along(lista)) {
    if (all(combinacoes_truncamento(lista[i]) %in% lista)) {
        primos_truncaveis <- c(primos_truncaveis, lista[i])
    }
}
proc.time() - tempo
sum(primos_truncaveis)

## PROBLEMA 38 - MAIOR PANDIGITAL FORMADO POR TABUADA DE NÚMEROS (TABUADA ATÉ VALOR VARIÁVEL) ---- 
    # FUNÇÃO VERIFICADORA PANDIGITAL
é_pandigital <- function(n) {
    n <- as.numeric(unlist(strsplit(as.character(n), split = "")))
    n_digitos <- length(n)
    possiveis <- seq(1, n_digitos, by = 1)
    freq_digitos <- NULL
    for (i in seq_along(possiveis)) {
        freq_digitos[i] <- length(which(n == possiveis[i]))
    }
    resultado <- all(freq_digitos == 1)
    return(resultado)
}

    # FUNÇÃO GERADORA DE PRODUTO 9 DÍGITOS
euler_38 <- function(x) {
    resultado <- NULL
    multiplicador <- 1
    while (length(resultado) < 9) {
        resultado <- c(resultado,
            unlist(strsplit(as.character(x * multiplicador), split = ""))
        )
        multiplicador <- multiplicador + 1
    }
    if (length(resultado) == 9) {
        numero_final <- as.numeric(paste(resultado, collapse = ""))
        if (é_pandigital(numero_final)) {
            return(numero_final)
        }
        else {
            return(NA)
        }
    }
    else {
        return(NA)
    }
}

    # CRIAR LISTA
tempo <- proc.time()
lista_38 <- NULL
for (i in seq_len(10000)[-1]) {
    lista_38[i] <- euler_38(i)
}
max(lista_38, na.rm = TRUE)
proc.time() - tempo

## PROBLEMA 39 - PERÍMETRO DE TRIANGULO (TRIÂNGULO RETÂNGULO) QUE TEM MAIOR QUANTIDADE COMBINAÇÕES (P < 1001) ---- 

    # FUNÇÃO QUE GERA PERIMETRO DADO CATETOS E RETORNA (I) PERÍMETRO E (II) HIPOTENUSA
gerar_perimetro <- function(CaAd, CaOp) {
    HI <- sqrt(CaAd ^ 2 + CaOp ^ 2)
    perimetro <- sum(HI, CaAd, CaOp)
    resultado <- c(perimetro, HI)
return(resultado)
} 

    # FUNÇÃO PARA GERAR VALORES POSSÍVEIS DE CATETOS E RETORNAR QUANTIDADE DE COMBINAÇÕES 
lados_possíveis <- function(perimetro) {
    limite <- ceiling(perimetro * 0.5) # NO CASO MAIS EXTREMO, APROX 50% DO PERÍMETRO É DADO PELO CATETO OPOSTO E ADJACENTE
    inicio <- ceiling(limite / 3) # IGNORA VALORES MUITO BAIXOS
    resultado <- matrix(0, nrow = 0, ncol = 3)
    for (CA in inicio:limite) { 
        for (CO in CA:limite) {
            perimetro_hipotenusa <- gerar_perimetro(CaAd = CA, CaOp = CO)
            if (perimetro_hipotenusa[1] == perimetro) {
                resultado <- rbind(resultado, c(CA, CO, perimetro_hipotenusa[2]))
            }
        }
        
    }
    #resultado <- dim(resultado)[1] # IGNORAR SE QUISER OS CASOS EM QUE OCORRE
return(resultado)
}

    # APLICAR FUNÇÕES
valores <- seq_len(1000)
tempo <- proc.time()
resultados <- sapply(valores, lados_possíveis)
proc.time() - tempo
which.max(resultados)



    # ABORDAGEM 2 (FÓRMUAL EUCLIDIANA)

RightTri <- function(num){
    h <- 0
    for (i in 1:(num/2)) {
        b <- (2 * i * num - num ^ 2) / (2 * i - 2 * num)
        if ( b %% 1 == 0 & b > i) {
            h <- h + 1
        }
    }  
return(h)
}
tempo <- proc.time()
zz <- sapply(2:10000, RightTri)      
match(max(zz),zz) + 1
proc.time() - tempo

for (i in 1:(num/2)) {
    b[i] <- (2 * i * num - num ^ 2) / (2 * i - 2 * num)
    la[i] <- b[i] %% 1 == 0 & b[i] > i
}

require(numbers)
comb_possiveis <- function(perimetro) {
    resultado <- 0
    for (m in seq(2, ceiling(sqrt(perimetro / 2)))) {
        for (n in 1:m) {
            if (GCD(m, n) == 1 & as.logical((m - n) %% 2)) {
                for (k in seq(2 * m * (m + n), perimetro, by = 2 * m * (m + n))) {
                    resultado <- c(resultado, k)
                }
            }
        }
    }
return(resultado)
}

# TESTE
matriz <- matrix(0, nrow = 1, ncol = 3)
perimetro <- 60
for (m in seq(2, ceiling(sqrt(perimetro / 2)))) {
    for (n in 1:m) {
        matriz <- rbind(matriz, c(m, n, as.logical(GCD(m, n) == 1 & as.logical((m - n) %% 2))))
    }
}
matriz <- matriz[matriz[,3] == 1, ]    
lista <- list()
for (i in seq_len(dim(matriz)[1])) {
    lista[[i]] <- seq(2 * matriz[i, 1] * (matriz[i, 1] + matriz[i, 2]), perimetro, by = 2 * matriz[i, 1] * (matriz[i, 1] + matriz[i, 2]))
}

## PROBLEMA 40 - PRODUTO DOS ENÉSIMOS DÍGITOS DA CONSTANTE CHAMPERNOWNE ----
tempo <- proc.time()
numeros <- unlist(strsplit(paste(seq_len(200000), collapse = ""), split = ""))
ref_result <- c(1, 10, 100, 1000, 10000, 100000, 1000000)
prod(as.numeric(numeros[ref_result]))
proc.time() - tempo

## PROBLEMA 41 - MAIOR PRIMO PANDIGITAL ATÉ N (SOLUÇÃO EFICIENTE NÃO CONCLUÍDA) ----
# ABORDAGEM 1 - GERAR PRIMOS E TESTAR PANDIG
    # LISTA DE PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    return(lista[lista != 0])
}
tempo <- proc.time()
lista <- lista_primos(2000000)
proc.time() - tempo

    # TESTAR CONDIÇÃO PANDIGITAL
é_pandigital <- function(n) {
    n <- as.numeric(unlist(strsplit(as.character(n), split = "")))
    n_digitos <- length(n)
    possiveis <- seq(1, n_digitos, by = 1)
    freq_digitos <- NULL
    for (i in seq_along(possiveis)) {
        freq_digitos[i] <- length(which(n == possiveis[i]))
    }
    resultado <- all(freq_digitos == 1)
return(resultado)
}

# ABORDAGEM 2 - GERAR PANDIGITAIS E TESTAR PRIMOS (FALTA MONTAR COMBINATÓRIO)
    # GERAR COMBINAÇÕES
combinacoes <- function(x) {
    n <- length(x)
    resultado <- vector("list", factorial(n)) # PRODUZ UMA LISTA COM GAMMA(N+1) ELEMENTOS
    p <- ip <- seqn <- seq_len(n)
    m <- n + 1
    d <- c(0, rep(-1, n - 1))
    p <- c(m, p, m)
    i <- 1
    use <- -c(1, n + 2)
    while (m != 1) {
        resultado[[i]] <- x[p[use]]
        i <- i + 1
        chk <- p[ip + d + 1] > seqn
        m <- max(seqn[!chk])
        if (m < n) 
            d[(m + 1):n] <- -d[(m + 1):n]
        index1 <- ip[m] + 1
        index2 <- p[index1] <- p[index1 + d[m]]
        p[index1 + d[m]] <- m
        tmp <- ip[index2]
        ip[index2] <- ip[m]
        ip[m] <- tmp
    }
return(resultado)
}

# chk = change_key
# i = índice de posição

## PROBLEMA 42 - QTDE PALAVRAS TRIÂNGULO ---- 
    # GERAR NÚMEROS TRIANGULOS
num_triangulos <- function(x, lista = TRUE){
    if (!lista) {
        return(0.5 * x * (x + 1))
    }
    else {
        x <- seq_len(x)
        return(0.5 * x * (x + 1))
    }
}
lista_triang <- num_triangulos(50)

    # SOMA DAS LETRAS
soma_letras <- function(nome){
    nome <- toupper(nome)
    lista_digitos <- as.character(unlist(strsplit(as.character(nome), split = "")))
    for (i in 1:length(lista_digitos)) {
        lista_digitos[i] <- which(LETTERS == lista_digitos[i])
    }
    resultado <- sum(as.numeric(lista_digitos))
return(resultado)
}

#resultado <- sum(which(LETTERS %in% lista_digitos)): PROBLEMA POIS CONSIDERA CARACTERES DUPLICADOS APENAS UMA VEZ

    # IMPORTAR TEXTO
texto <- read.table("C:/Users/bruno.bermudez/Desktop/Bruno Grillo/Linguagens de Programação/R/R - Scripts/text.txt", header = FALSE, sep = ",")
texto <- t(as.matrix(texto))
row.names(texto) <- NULL

    # APLICAR FUNÇÕES
lista_somas <- apply(texto, 1, soma_letras)
resultado <- sum(lista_somas %in% lista_triang)

## PROBLEMA 43 - PROPRIEDADE DIVISIBILIDADE COMBINAÇÃO DÍGITOS NÚMEROS PANDIGITAIS 0 A 9 ----
    # GERAR PANDIGITAIS
combinacoes <- function(x) {
    n <- length(x)
    resultado <- vector("list", factorial(n)) # PRODUZ UMA LISTA COM GAMMA(N+1) ELEMENTOS
    p <- ip <- seqn <- seq_len(n)
    m <- n + 1
    d <- c(0, rep(-1, n - 1))
    p <- c(m, p, m)
    i <- 1
    use <- -c(1, n + 2)
    while (m != 1) {
        resultado[[i]] <- x[p[use]]
        i <- i + 1
        chk <- p[ip + d + 1] > seqn
        m <- max(seqn[!chk])
        if (m < n) 
            d[(m + 1):n] <- -d[(m + 1):n]
        index1 <- ip[m] + 1
        index2 <- p[index1] <- p[index1 + d[m]]
        p[index1 + d[m]] <- m
        tmp <- ip[index2]
        ip[index2] <- ip[m]
        ip[m] <- tmp
    }
    return(resultado)
}

lista_pandigitais <- combinacoes(0:9)

    # FUNÇÃO VERIFICADORA (CONSIDERA SAÍDA COMBINACOES; MAIS ADHOC POSSÍVEL)
teste_sub_divisibilidade <- function(x) {
    n_bloco <- 3
    pos_inicial <- 2
    primos <- c(2, 3, 5, 7, 11, 13, 17)
    numeros_gerados <- NULL
    e_divisivel <- NULL
    for (i in seq_along(primos) - 1) {
        numeros_gerados[i + 1] <- as.numeric(
            paste(x[(pos_inicial + i):(pos_inicial + n_bloco - 1 + i)], collapse = "")
        )
        e_divisivel[i + 1] <- numeros_gerados[i + 1] %% primos[i + 1] == 0
    }
return(all(e_divisivel))
}

    # APLICAÇÃO FUNÇÃO
tempo <- proc.time()
resultado <- unlist(lapply(lista_pandigitais, teste_sub_divisibilidade))
proc.time() - tempo

    # ÍNDICES PARA SOMAR
indices <- which(resultado == TRUE)
numeros_resultado <- NULL
for (k in seq_along(indices)) {
    numeros_resultado[k] <- as.numeric(paste(lista_pandigitais[[indices[k]]], collapse = ""))
}
sum(numeros_resultado)

## PROBLEMA 44 - PAR NÚMEROS PENTAGONOS CUJA SOMA E DIFERENÇA SÃO NÚMEROS PENTÁGONOS COM MENOR DIFERENÇA ----
    # NÚMEROS PENTAGONOS
num_pentagonos <- function(x, lista = TRUE){
    if (!lista) {
        return(x * (3 * x - 1) / 2)
    }
    else {
        x <- seq_len(x)
        return(x * (3 * x - 1) / 2)
    }
}
lista_pent <- num_pentagonos(2400)


    # TESTAR CONDIÇÃO
tempo <- proc.time()
resultado <- list()
posicao <- 1
for (i in seq_along(lista_pent)) {
    for (j in i:length(lista_pent)) { 
        if (sum(lista_pent[i], lista_pent[j]) %in% lista_pent & abs(lista_pent[i] - lista_pent[j]) %in% lista_pent) {
            resultado[[posicao]] <- c(lista_pent[i], lista_pent[j], abs(lista_pent[j] - lista_pent[i]))
            posicao <- posicao + 1
        }
    }
}
proc.time() - tempo

## PROBLEMA 45 - NÚMERO QUE É TRIÂNGULO, PENTÁGONO E HEXÁGONO ----
    # NÚMERO TRIÂNGULO
num_triangulos <- function(x, lista = TRUE){
    if (!lista) {
        return(0.5 * x * (x + 1))
    }
    else {
        x <- seq_len(x)
        return(0.5 * x * (x + 1))
    }
}

    # NÚMERO PENTÁGONO
num_pentagonos <- function(x, lista = TRUE){
    if (!lista) {
        return(x * (3 * x - 1) / 2)
    }
    else {
        x <- seq_len(x)
        return(x * (3 * x - 1) / 2)
    }
}

    # NÚMERO HEXÁGONO
num_hexagonos <- function(x, lista = TRUE){
    if (!lista) {
        return(x * (2 * x - 1))
    }
    else {
        x <- seq_len(x)
        return(x * (2 * x - 1))
    }
}

    # GERAR LISTAS E TESTAR
tempo <- proc.time()
lista_pent <- num_pentagonos(70000)
lista_hex <- num_hexagonos(50000)
lista_hex[which(lista_hex %in% lista_pent)] # IGNORA-SE O TRIANG, POIS TODO HEXA É TRIANGULO
proc.time() - tempo

######## PROBLEMA 46 (NÃO RESOLVIDO) - TESTE CONJUNTURA GOLDBACH (MENOR ÍMPAR COMPOSTO QUE NÃO PODE SER ESCRITO COMO SOMA DE PRIMO E DOBRO DE UM NÚMERO AO QUADRADO) ----

## PROBLEMA 47 - PRIMEIROS QUATRO NÚMEROS CONSECUTIVOS COM QUATRO FATORES PRIMOS DISTINTOS ----
    # LISTA PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    return(lista[lista != 0])
}
primos <- lista_primos(2000000)


    # FUNÇÃO FATORIZAÇÃO
fatorizacao <- function(x) {
    x_parcial <- x
    # GERAR FATORES
    lista_fatores <- NULL
    k <- 1
    while (x_parcial != 1) {
        if (x_parcial %% primos[k] == 0) {
            lista_fatores[length(lista_fatores) + 1] <- primos[k]
            x_parcial <- x_parcial / primos[k]
            k <- 1
        }
        else {
            k <- k + 1
        }
    }
return(lista_fatores)
}

    # FUNÇÃO QUE TESTA N-CONSECUTIVOS PARA M-PRIMOS DISTINTOS
euler_47 <- function(x, n_consec = 4, m_primos = 4) {
    lista_numeros <- matrix(seq(x, x + n_consec - 1), nrow = n_consec, ncol = 1)
    fatores_unicos <- apply(lista_numeros, 1, function(k) length(unique(fatorizacao(k))))
return(all(fatores_unicos >= m_primos))
}

    # APLICAR FUNÇÃO
resultado <- NULL
parcial <- FALSE
j <- 644
tempo <- proc.time()
while (parcial == FALSE) {
    parcial <- euler_47(j, n_consec = 4, m_primos = 4)
    j <- j + 1
}
proc.time() - tempo

## PROBLEMA 48 - 10 ÚLTIMOS DÍGITOS SOMA SÉRIE SELF-POWER ATÉ 1000 ----
require(gmp)
euler_48 <- function(x) {
    return(sum.bigz(pow.bigz(seq_len(x), seq_len(x))))
}

## PROBLEMA 49 - PROGRESSÃO ARITMÉTICA COM RESULTADOS PRIMOS E PERMUTAÇÃO DOS DEMAIS (CADA NÚMERO COM 4 DÍGITOS) ----
    # ETAPAS COM MENOR NÚMERO DE CÁLCULOS: 
        # LISTA PRIMOS
        # VALOR RAÍZ (A PARTIR DOS PRIMOS POSSÍVEIS) (EXCLUI MAIORES QUE 7779, POIS PROGRESSÃO É MÚLTIPLA DE 1110)
        # PROGRESSÃO (MÚLTIPLOS DE 1110); 
        # VERIFICAR CONDIÇÃO PRIMOS E PERMUTAÇÕES
tempo <- proc.time()
    # GERAR PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    return(lista[lista != 0])
}
primos <- lista_primos(9999) # TODOS PRIMOS COM 4 DÍGITOS (PARA CONFIRMAR CONDIÇÃO DE PRIMO)
primos <- primos[primos > 999]

    # PROGRESSÕES
gerar_progressoes <- function(x_inicial, dist, termos = 3) {
    return(x_inicial + ((seq_len(termos) - 1) * dist))
}
    
    # GERAR LISTA COM SÉRIES
    tempo_parcial <- proc.time()
ref <- 0
progr_possiveis <- seq_len(9) * 1110
lista_series <- list()
for (i in seq_along(primos)) {
    for (j in seq_along(progr_possiveis)) {
        ref <- ref + 1
        lista_series[[ref]] <- gerar_progressoes(x_inicial = primos[i], dist = progr_possiveis[j])
    }
}
    proc.time() - tempo_parcial
    
    # VERIFICAR PRIMOS
    tempo_parcial <- proc.time()
indices <- which(unlist(lapply(lista_series, function(x) all(x %in% primos))) == TRUE)
lista_series <- lista_series[indices]
    proc.time() - tempo_parcial

    # FUNÇÃO CRIA COMBINAÇÕES
# GERAR COMBINAÇÕES
permutacoes <- function(x) { # SAI RESULTADO NUMÉRICO (NÃO VETOR COM DÍGITOS)
    n <- length(x)
    resultado <- vector("list", factorial(n)) # PRODUZ UMA LISTA COM GAMMA(N+1) ELEMENTOS
    p <- ip <- seqn <- seq_len(n)
    m <- n + 1
    d <- c(0, rep(-1, n - 1))
    p <- c(m, p, m)
    i <- 1
    use <- -c(1, n + 2)
    while (m != 1) {
        resultado[[i]] <- as.numeric(paste(x[p[use]], collapse = ""))
        i <- i + 1
        chk <- p[ip + d + 1] > seqn
        m <- max(seqn[!chk])
        if (m < n) 
            d[(m + 1):n] <- -d[(m + 1):n]
        index1 <- ip[m] + 1
        index2 <- p[index1] <- p[index1 + d[m]]
        p[index1 + d[m]] <- m
        tmp <- ip[index2]
        ip[index2] <- ip[m]
        ip[m] <- tmp
    }
    return(resultado)
}

    # VERIFICAR SE É PERMUTAÇÃO
é_permuta <- function(x) {
    digitos <- as.numeric(unlist(strsplit(as.character(x[1]), split = "")))
    combinacoes_possiveis <- unlist(permutacoes(digitos))
return(all(x %in% combinacoes_possiveis))
}

    # APLICAR FUNÇÃO
    tempo_parcial <- proc.time()
pos_resultado <- which(unlist(lapply(lista_series, é_permuta)) == TRUE)
resultado <- lista_series[pos_resultado]
    proc.time() - tempo_parcial
proc.time() - tempo
resultado

## PROBLEMA 50 - PRIMO ABAIXO DE 1.000.000 QUE É A SOMA DO MAIOR NÚMERO DE PRIMOS CONSECUTIVOS ----
    # GERAR LISTA PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    return(lista[lista != 0])
}
primos <- lista_primos(1000000)

    # VETOR SOMA ACUMULADA (VER QUAL A POSIÇÃO DO MAIOR QUE PERTENCE AOS PRIMOS)
tempo <- proc.time()
resultado <- list()
ref <- 0
for (i in seq_along(primos)) {
    for (j in i:length(primos)) {
        if (sum(primos[i:j]) %in% primos) {
            ref <- ref + 1
            resultado[[ref]] <- matrix(c(sum(primos[i:j]), j - i), nrow = 1, ncol = 2)
       } 
    }
}
resultado <- do.call(rbind, resultado)
resultado[which.max(resultado[, 2]),]
proc.time() - tempo

## PROBLEMA 51 - PRIMEIRO PRIMO QUE 8 COMBINAÇÕES (SUBSTITUINDO PARTES DO NÚMERO PELOS MESMOS DÍGITOS) GEREM PRIMOS ----
    # GERAR LISTA PRIMOS
lista_primos <- function(maximo){ 
    lista <- 1:maximo
    lista[1] <- 0
    for (i in 2:max(lista)) {
        if (i != 0 & 2*i < maximo) {
            for (j in seq(2*i, maximo, by = i)) {
                lista[j] = 0
            }
        }
    }
    return(lista[lista != 0])
}
tempo <- proc.time()
primos <- lista_primos(999999)
proc.time() - tempo

    # FUNÇÃO QUE GERA COMBINAÇÕES COMUTATIVAS
comb_comutat <- function(x, elemnt) {
    e <- 0
    h <- elemnt
    posicoes <- seq_len(elemnt) # POSICOES A SELECIONAR DO VETOR X
    comp_x <- length(x)
    n_comb <- factorial(comp_x) / prod(factorial(comp_x - elemnt), factorial(elemnt))
    resultado <- vector("list", n_comb)
    resultado[[1]] <- x[posicoes] 
    
    i <- 1
    nmmp1 <- comp_x - elemnt + 1 # ÚLTIMO DÍGITO (POSIÇÃO) A INICIAR COMBINAÇÃO
    mp1 <- elemnt + 1
    
    while (posicoes[1] != nmmp1) {
        i <- i + 1
        if (e < comp_x - h) {
            h <- 1
            e <- posicoes[elemnt]
            j <- 1
        }
        else {
            h <- h + 1
            e <- posicoes[mp1 - h]
            j <- seq_len(h)
        }
        posicoes[elemnt - h + j] <- e + j
        resultado[[i]] <- x[posicoes]
    }
return(resultado)
}

    # FUNÇÃO VERIFICAR QUANTOS PRIMOS OBTÊM-SE AO SUBSTITUIR AS POSICOES (DETERMINA POSICOES A SUBSTITUIR)
subst_testar <- function(x, pos_subst, leading_zero = TRUE){ # x é número splitado
    n_primos <- 0
    if (!leading_zero && 1 %in% pos_subst) {
        return(0)
    }
    else {
        for (k in 0:9) {
            x[pos_subst] <- k
            if (as.numeric(paste(x, collapse = "")) %in% primos) {
                n_primos <- n_primos + 1
            }
        }
    }
return(n_primos)
}

    # FUNÇAO QUE RETORNA MÁXIMO
euler_51 <- function(x) {
    x_sep <- as.numeric(unlist(strsplit(as.character(x), split = "")))
    comp_x <- length(x_sep)
    splitters <- seq_len(comp_x - 1)[-1] # 2:3
    resultado <- NULL
    for (i in seq_along(splitters)) {
        posicoes <- comb_comutat(seq_len(comp_x), splitters[i])
        resultado <- c(resultado, unlist(lapply(posicoes, function(w) subst_testar(x_sep, w, TRUE))))
    }
return(max(resultado, na.rm = TRUE))
}

    # APLICAR
tempo <- proc.time()
max_primos <- 0
z <- which(primos == 120383) + 1 # ENUNCIADO 56003 COMO PRIMEIRO PRIMO 7
while (max_primos != 8) {
    max_primos <- euler_51(primos[z])
    resultado <- primos[z]
    z <- z + 1
}
resultado
proc.time() - tempo


# OBSERVAÇÕES:
    # CONSIDERANDO LEADING ZEROS, O MENOR É 100109 (CONSIDERANDO QUE 000109 FAZ PARTE)
    # 120383 TAMBÉM GERA
    # RESPOSTA É 121313

## PROBLEMA 52 - MÚLTIPLOS PERMUTADOS (PRIMEIRO NÚMERO QUE X, 2X, 3X, 4X, 5X E 6X TENHAM OS MESMOS DÍGITOS) ----
  # ABORDAGEM: NÚMEROS PRECISAM TER MESMA QUANTIDADE DE DÍGITOS, ENTÃO 
  # TESTA-SE A CONDIÇÃO APENAS ATÉ 17***, CASO CONTRÁRIO EXCEDE A QUANTIDADE DE DÍGITOS
  # CONTROLE DE ESTRUTURA ENVOLVE TODAS ETAPAS (VAI TESTANDO CONDIÇÕES DADO QUE O NÚMERO TENHA N DIGITOS)

# GERAR POTENCIAIS CANDIDATOS
gerar_candidatos52 <- function(n_digitos, qtde_mult = 6) { # QTDE MULTIPLICACOES
  x_inc <- 10 ^ (n_digitos - 1) + 1
  x_fnl <- ceiling((10 ^ n_digitos - 1) / qtde_mult)
  resultado <- seq(x_inc, x_fnl)
return(resultado)
}

# GERAR COMBINAÇÕES
permutacoes <- function(x) { # SAI RESULTADO NUMÉRICO (NÃO VETOR COM DÍGITOS)
  n <- length(x)
  resultado <- vector("list", factorial(n)) # PRODUZ UMA LISTA COM GAMMA(N+1) ELEMENTOS
  p <- ip <- seqn <- seq_len(n)
  m <- n + 1
  d <- c(0, rep(-1, n - 1))
  p <- c(m, p, m)
  i <- 1
  use <- -c(1, n + 2)
  while (m != 1) {
    resultado[[i]] <- as.numeric(paste(x[p[use]], collapse = ""))
    i <- i + 1
    chk <- p[ip + d + 1] > seqn
    m <- max(seqn[!chk])
    if (m < n) 
      d[(m + 1):n] <- -d[(m + 1):n]
    index1 <- ip[m] + 1
    index2 <- p[index1] <- p[index1 + d[m]]
    p[index1 + d[m]] <- m
    tmp <- ip[index2]
    ip[index2] <- ip[m]
    ip[m] <- tmp
  }
  return(resultado)
}

# FUNÇÃO TESTE CONDIÇÃO
mult_perm <- function(x, qtde_mult = 6) {
  x_func <- unlist(strsplit(as.character(x), split = ""))
  possibilidades <- unlist(permutacoes(x_func))
  x <- x * seq_len(qtde_mult)
  resultado <- all(x %in% possibilidades)
return(resultado)
}

# APLICAR FUNÇÕES
tempo <- proc.time()
resultado <- matrix(0, nrow = 0, ncol = 2)
digitos <- 5
ref <- 1
while (!any(resultado[, 2])) {
  digitos <- digitos + 1
  psblds <- gerar_candidatos52(digitos)
  for (k in seq_along(psblds)) {
    if (mult_perm(psblds[k])) {
      resultado <- rbind(resultado, c(psblds[k], TRUE))
      break
    }
  }
}
proc.time() - tempo

## PROBLEMA 53 - POSSIBILIDADES COMBINATÓRIAS COM MAIS DE 1 MILHÃO DE COMBINAÇÕES ----
n_limite <- 100
limite <- 1000000
resultado <- NULL
posResultado <- 0

for (w in seq_len(n_limite)) {
  for (y in seq(0, w)) {
    if ((factorial(w) / (factorial(y) * factorial(w - y))) >= limite ) {
      posResultado <- posResultado + 1
      resultado[posResultado] <- paste0("n=", w, ", r=", y)
    }
  }
}

## PROBLEMA 54 - VITÓRIAS DE POKER JOGADOR 1 ----
  # IMPORTAR DADOS
dados <- read.table("https://projecteuler.net/project/resources/p054_poker.txt", 
  sep = " ", 
  stringsAsFactors = FALSE)
  # SEPARAR MÃOS
jogadorUm <- dados[, 1:5]
jogadorDois <- dados[, 6:10]

  # FUNÇÃO MÃO (PRECISA SER COMPRIMENTO 5)
mao <- function(cartas) {
  # REFERÊNCIAS ----
  jogo <- c(
    "High Card", "One Pair", "Two Pairs", "Three of a Kind", 
    "Straight", "Flush", "Full House", "Four of a Kind", 
    "Straight Flush", "Royal Flush")
    # VALORES CARTAS
  valAtual <- c("T", "J", "Q", "K", "A")
  valDesej <- seq(10, 14)
  # AJUSTAR PARA VALORES 
  listaCartas <- strsplit(as.character(cartas), split = "")
    # VALORES
  valores <- unlist(lapply(listaCartas, function(x) x[1]))
  for (w in seq_along(valAtual)) {
    valores[valores == valAtual[w]] <- valDesej[w]
  }
  valores <- as.numeric(valores)
  valores <- valores[order(valores)]
    # NAIPES
  naipes <- unlist(lapply(listaCartas, function(x) x[2]))
  
  # TESTAR MÃOS ----
    # É FLUSH (p1) ----
  isFlush <- length(unique(naipes)) == 1
  tiebreakFlush <- max(valores)
    # É SEQUÊNCIA (p1) ----
  valoresTemp <- valores
  valoresTemp[valoresTemp == 14] <- 1
  isStraight <- any(all(diff(valores) == 1), all(diff(valoresTemp) == 1))
  tiebreakStraight <- max(valores)
    # É ROYAL STRAIGHT FLUSH ----
  isRoyal <- isStraight & isFlush & max(valores) == 14
  if (isRoyal) {
    resultado <- list(
      Resultado = which(jogo == "Royal Flush"),
      CriterioUm = 0, CriterioDois = 0, CriterioTres = 0, CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É STRAIGHT FLUSH ----
  isStraightFlush <- isFlush & isStraight & max(valores) <= 13
  if (isStraightFlush) {
    resultado <- list(
      Resultado = which(jogo == "Straight Flush"),
      CriterioUm = 0, CriterioDois = 0, CriterioTres = 0, CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É QUADRA ----
  isFour <- any(xtabs(~ valores) == 4)
  if (isFour) {
    cardFour <- as.numeric(names(which(xtabs(~ valores) == 4)))
    tiebreakFour <- max(valores[valores != cardFour])
    resultado <- list(
      Resultado = which(jogo == "Four of a Kind"),
      CriterioUm = cardFour, CriterioDois = tiebreakFour, CriterioTres = 0, CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É FULL HOUSE ----
  isFullHouse <- sum(xtabs(~ valores) == 2) == 1 & any(xtabs(~ valores) == 3)
  if (isFullHouse) {
    ThreeFullHouse <- as.numeric(names(which(xtabs(~ valores) == 3)))
    PairFullHouse <- as.numeric(names(which(xtabs(~ valores) == 2)))
    resultado <- list(
      Resultado = which(jogo == "Full House"),
      CriterioUm = ThreeFullHouse, CriterioDois = PairFullHouse, CriterioTres = 0, CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É FLUSH ----
  if (isFlush) {
    resultado <- list(
      Resultado = which(jogo == "Flush"),
      CriterioUm = max(valores), CriterioDois = max(valores[-which.max(valores)]),
      CriterioTres = 0, CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É STRAIGHT ----
  if (isStraight) {
    Resultado <- list(
      Resultado = which(jogo == "Straight"),
      CriterioUm = max(valores), CriterioDois = max(valores[-which.max(valores)]),
      CriterioTres = 0, CriterioQuatro = 0, CriterioCinco = 0
    )
    return(Resultado)
  }
    # É TRINCA ----
  isThree <- any(xtabs(~ valores) == 3)
  if (isThree) {
    cardThree <- as.numeric(names(which(xtabs(~ valores) == 3)))
    tiebreakThree <- max(valores[valores != cardThree])
    tiebreakTwoThree <- min(valores[valores != cardThree])
    resultado <- list(
      Resultado = which(jogo == "Three of a Kind"),
      CriterioUm = cardThree, CriterioDois = tiebreakThree, CriterioTres = tiebreakTwoThree,
      CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É DOIS PARES ----
  isDoublePair <- sum(xtabs(~ valores) == 2) == 2
  if (isDoublePair) {
    firstPair <- max(as.numeric(names(which(xtabs(~ valores) == 2))))
    secondPair <- min(as.numeric(names(which(xtabs(~ valores) == 2))))
    tiebreakTwoPairs <- max(valores[!(valores %in% c(firstPair, secondPair))])
    resultado <- list(
      Resultado = which(jogo == "Two Pairs"),
      CriterioUm = firstPair, CriterioDois = secondPair, CriterioTres = tiebreakTwoPairs,
      CriterioQuatro = 0, CriterioCinco = 0
    )
    return(resultado)
  }
    # É UM PAR ----
  isPair <- sum(xtabs(~ valores) == 2) == 1
  if (isPair) {
    firstPair <- max(as.numeric(names(which(xtabs(~ valores) == 2))))
    valOrdSemPar <- valores[valores != firstPair]
    tiebreakPair <- valOrdSemPar[3]
    tiebreak2Pair <- valOrdSemPar[2]
    tiebreak3Pair <- valOrdSemPar[1]
    resultado <- list(
      Resultado = which(jogo == "One Pair"),
      CriterioUm = firstPair, CriterioDois = tiebreakPair, CriterioTres = tiebreak2Pair,
      CriterioQuatro = tiebreak3Pair, CriterioCinco = 0
    )
    return(resultado)
  }
    # CARTA MAIS ALTA
  valoresOrdenados <- valores[order(valores)]
  resultado <- list(
    Resultado = which(jogo == "High Card"),
    CriterioUm = valoresOrdenados[5], CriterioDois = valoresOrdenados[4], CriterioTres = valoresOrdenados[3],
    CriterioQuatro = valoresOrdenados[2], CriterioCinco = valoresOrdenados[1]
  )
return(resultado)
}

  # FUNÇÃO COMPARAÇÃO
comparacao <- function(jogoUm, jogoDois) {
  tabelaComparativa <- data.frame(
    jogoUm = unlist(jogoUm), 
    jogoDois = unlist(jogoDois))
  
  tabelaComparativa <- within(tabelaComparativa, {
    Resultado <- apply(tabelaComparativa, 1, function(x) x[1] - x[2])
    Vencedor <- rep(0, nrow(tabelaComparativa))
    Vencedor[Resultado > 0] <- "J1"
    Vencedor[Resultado < 0] <- "J2"
  })
  resultado <- tabelaComparativa$Vencedor
  resultado <- resultado[resultado != 0]
return(resultado[1])
}

  # APLICAÇÃO
resultado <- NULL
for (w in seq_len(nrow(jogadorUm))) {
  resultado[w] <- comparacao(jogoUm = mao(jogadorUm[w, ]), jogoDois = mao(jogadorDois[w, ]))
}
sum(resultado == "J1")

## PROBLEMA 55 - NÚMERO LYCHREL ----

## PROBLEMA 67 - CAMINHO MAIOR SOMA 100 linhas ----
caminho_maximo <- function(matriz){
    n_linhas <- dim(matriz)[1] #quanto precisa descer
    matriz[2, 1:2] <- matriz[2, 1:2] + matriz[1,1] #gerando acumulados segunda linha
    for (i in 3:n_linhas) {
        matriz[i, 1] <- matriz[i, 1] + matriz[(i - 1), 1]
        matriz[i, i] <- matriz[i, i] + matriz[(i - 1), (i - 1)]
        for (j in 2:(i - 1)) {
            matriz[i, j] <- matriz[i, j] + max(matriz[(i - 1), (j - 1):j])
        }
    }
    vetor <- max(matriz[n_linhas,])
return(vetor)
}
