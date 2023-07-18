#################################################################
## AO PREENCHER ESSE CABEÇALHO COM O MEU NOME E O MEU NÚMERO USP,
## DECLARO QUE SOU O ÚNICO AUTOR E RESPONSÁVEL POR ESSE PROGRAMA.
## TODAS AS PARTES ORIGINAIS DESSE EXERCÍCIO PROGRAMA (EP) FORAM
## DESENVOLVIDAS E IMPLEMENTADAS POR MIM SEGUINDO AS INSTRUÇÕES
## DESSE EP E QUE PORTANTO N~AO CONSTITUEM DESONESTIDADE ACADÊMICA
## OU PLÁGIO.
## DECLARO TAMBÉM QUE SOU RESPONSÁVEL POR TODAS AS CÓPIAS
## DESSE PROGRAMA E QUE EU NÃO DISTRIBUI OU FACILITEI A
## SUA DISTRIBUIÇÃO. ESTOU CIENTE QUE OS CASOS DE PLÁGIO E
## DESONESTIDADE ACADÊMICA SER~AO TRATADOS SEGUNDO OS CRIT´ERIOS
## DIVULGADOS NA PÁGINA DA DISCIPLINA.
## ENTENDO QUE EPS SEM ESTE CABEÇALHO NÃO SERÃO CORRIGIDOS E,
## AINDA ASSIM, PODERÃO SER PUNIDOS POR DESONESTIDADE ACADÊMICA.
## Nome :Jonatas Michel Cardoso Vieira
## NUSP : 9913690
## Turma: 21
## Prof.: Roberto Hirata Jr.
## Referências: Com exceção das rotinas fornecidas no enunciado
## e em sala de aula, caso você tenha utilizado alguma referência,
## liste-as abaixo para que o seu programa não seja considerado
## plágio ou irregular.
## Exemplo:
## - A contrução dos elementos dos gráficos foram baseados em:
##https://2engenheiros.com/2017/02/03/graficos-de-barra-no-r/
##http://www.leg.ufpr.br/~walmes/ensino/dsbd/7-vis-graphics.html
#################################################################
## [Seu programa]

criaVetorAno <- function(D){
  dataSeparada <- character()
  vetorAno <- character()
  for(i in 1:nrow(D)){
    dataSplit <- strsplit(D[i,"date"],split = "-")
    dataSeparada <- unlist(list(dataSeparada,dataSplit))
  }
  #Colocando apenas o ano (AAAA) no vetorAno
  for (i in seq(from = 1, to = length(dataSeparada), by = 3)) {
    vetorAno <- c(vetorAno, dataSeparada[i])
  }
  return(vetorAno)
}

BigMacUSPrice <- function(D,A){
  for(i in 1:nrow(D)){
    if(D[i,"isoA3"]=="USA"){
      vetorAno <- criaVetorAno(D)
      if(vetorAno[i] == A){
        precoBigMacEUA <- D[i,"localPrice"]
      }
    }
  }
  return(precoBigMacEUA)
}

mediaColunaPais <- function(D,C,P){
  #Verificando se a classe da coluna C é numérica
  if(class(D[ , C]) == "numeric"){
    soma <- as.numeric(0)
    nAmostras <- as.numeric(0)
    for(i in 1:nrow(D)){
      if(D[i,"isoA3"] == P){
        soma <- soma + D[i,C]
        nAmostras <- nAmostras + 1
      }
    }
    media <- soma/nAmostras
  }else{
    media <- NULL
  }
  return(media)
}

varColunaPais <- function(D,C,P){
  if(class(D[ , C]) == "numeric"){
    soma <- as.numeric(0)
    nAmostras <- as.numeric(0)
    mediaAmostral <- mediaColunaPais(D,C,P)
    for(i in 1:nrow(D)){
      if(D[i,"isoA3"] == P){
        soma <- soma + (D[i,C] - mediaAmostral)^2
        nAmostras <- nAmostras + 1
      }
    }
    variancia <- soma/(nAmostras-1)
  }else{
    variancia <- NULL
  }
  return(variancia)
}

criaVetorBMI <- function(D){
  precoEUA <- numeric()
  #acessando cada valor do Big Mac para um ano entre 2000 e 2022
  for(ano in 2000:2022){
    precoEUA <- c(precoEUA, BigMacUSPrice(D, ano))
  }
  anoAnalisado <- 2000
  idPrecoEUA <- 1
  contador <- 1
  anoPreco <- criaVetorAno(D)
  indiceBigMac <- numeric()
  
  for(i in 1:length(anoPreco)){
    #compara se o ano de um dado é o mesmo do analisado
    if(anoPreco[contador] == (anoAnalisado) && anoAnalisado <= "2022"){
      indice <- D[contador,"localPrice"]/precoEUA[idPrecoEUA]
      indiceBigMac <- c(indiceBigMac, indice)
      contador <- contador+1}else{
        #caso o ano do dado for diferente, calcula levando em conta o valor
        #do Big Mac do proximo ano
        anoAnalisado <- anoAnalisado+1
        idPrecoEUA <-idPrecoEUA+1
        indice <- D[contador,"localPrice"]/precoEUA[idPrecoEUA]
        indiceBigMac <- c(indiceBigMac, indice)
        contador <- contador+1
      }
  }
  return(indiceBigMac)
}
#Reaproveitar o código do calculo da média do IBM para um vetor de paises
indiceBigMacMedioPais <- function(paises,coluna,dataframe){
  indiceBigMacMedio <- numeric()
  for(i in 1:length(paises)){
    indiceBigMacMedio <- c(indiceBigMacMedio, mediaColunaPais(dataframe,coluna,paises[i]))
    indiceBigMacMedio <-round(indiceBigMacMedio, 3)
  }
  return(indiceBigMacMedio)
}

#Reaproveitar o código do calculo da variância do IBM para um vetor de paises
varianciaPaises <- function(paises,coluna,dataframe){
  varPaises <- numeric()
  for(i in 1:length(paises)){
    varPaises <- c(varPaises, varColunaPais(dataframe,coluna,paises[i]))
    varPaises <-round(varPaises, 3)
  }
  return(varPaises)
}

##---------------------PROGRAMA----------------------##
cat("Digite o caminho da base se dados: ")
caminhoCSV <- readline()
#past concatena a entrada do usuário com as aspas para leitura do arquivo
BigMacData <- read.csv(paste(caminhoCSV, sep = ""), header = TRUE)

#Adiciona 2 colunas de dados ao dataframe
BigMacData <- cbind(BigMacData, Ano = criaVetorAno(BigMacData))
BigMacData <- cbind(BigMacData, indiceBM = criaVetorBMI(BigMacData))

#pega uma unica ocorrencia do país no dataframe
Paises <- unique(sort(BigMacData[,"isoA3"]))

#vetor criado de acordo os  países e regiões que são classificados como "economias avançadas"(segundo o FMI)
paisesEconomiaAvancada <- c("AUS", "AUT", "BEL", "CAN", "KOR", "DNK", "SVK", "SVN", "ESP", "USA", "EST", "FIN", "FRA", "GRC", "HKG", "IRL", "ISR", "ITA", "JPN", "LVA", "LTU", "NZL", "NOR", "NLD", "PRT", "GBR", "CZE", "SGP", "SWE", "CHE", "TWN")

#vetor com os demais países
outrosPaises <- setdiff(Paises, paisesEconomiaAvancada)


#Gráficos para todos os países do dataframe
barplot(indiceBigMacMedioPais(Paises,10,BigMacData),xlab= "Países",ylab="IBM médio", main="IBM médio dos Países", names.arg = Paises, cex.names = 0.3, las =2)
barplot(varianciaPaises(Paises,10,BigMacData),xlab= "Países",ylab="Variância", main="Variância do IBM dos Países", names.arg = Paises, cex.names = 0.3, las =2)

#Gráfico para os países que são classificados como "economias avançadas"(segundo o FMI)
barplot(indiceBigMacMedioPais(paisesEconomiaAvancada,10,BigMacData),xlab= "Países",ylab="IBM médio", main="IBM médio dos Países de economia avançada", names.arg = paisesEconomiaAvancada, cex.names = 0.5, las =2)
barplot(varianciaPaises(paisesEconomiaAvancada,10,BigMacData),xlab= "Países",ylab="Variância", main="Variância do IBM dos Países de economia avançada", names.arg = paisesEconomiaAvancada, cex.names = 0.5, las =2)

#Gráfico para os outros países do dataframe
barplot(indiceBigMacMedioPais(outrosPaises,10,BigMacData),xlab= "Países",ylab="IBM médio", main="IBM médio dos Países de economia não avançada", names.arg = outrosPaises, cex.names = 0.5, las =2)
barplot(varianciaPaises(outrosPaises,10,BigMacData),xlab= "Países",ylab="Variância", main="Variância do IBM dos Países de economia não avançada", names.arg = outrosPaises, cex.names = 0.5, las =2)
