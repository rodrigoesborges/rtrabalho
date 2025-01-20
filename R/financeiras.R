require(httr)
require(jsonlite)
require(base)

#' Obtém os índices de inflação a partir do BCB
#'
#' Consulta o webservice do Banco Central do Brasil, obtendo alguns índices
#' de inflação em uma tabela com data e valor do índice.
#
#' @param indice Índice a ser consultado (padrão é IPCA). As opções são
#' "ipca" (para o IPCA/IBGE), "inpc" (INPC/IBGE) e "igpm" (IGP-M/FIPE).
#' @return Quadro de dados com índices de correção disponíveis. É composto
#' por duas colunas: a primeira contém a data da aplicação, e a segunda contém
#' o multiplicador (ou seja, o índice, em número decimal, somado a 1).
#' @examples
#' tabela = obtem_indices_bcb("inpc")
#' tabela2 = obtem_indices_bcb("ipca")
#'
obtem_indices_bcb <- function (indice="ipca")
{
  # URLs do serviço que disponibiliza alguns índices (não sei se há outros)
  # Inspirado no pacote rbcb, de Wilson Freitas - https://github.com/wilsonfreitas/rbcb
  ipca_url <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados"
  inpc_url <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.188/dados"
  igpm_url <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.189/dados"
  
  if(indice == "ipca")
  {
    tabela_indice <- jsonlite::fromJSON(httr::content(httr::GET(ipca_url),as="text"))
  }
  else if(indice == "inpc")
  {
    tabela_indice <- jsonlite::fromJSON(httr::content(httr::GET(inpc_url),as="text"))
  }
  else if(indice == "igpm")
  {
    tabela_indice <- jsonlite::fromJSON(httr::content(httr::GET(igpm_url),as="text"))
  }
  else
  {
    stop("Não foi possível obter índices especificados!")
  }
  
  # Converte em formato Date a primeira coluna, e em formato numérico
  # a segunda coluna, já como fator multiplicativo (divide o percentual soma 1)
  tabela_indice[,1] <- as.Date(lubridate::dmy(tabela_indice[,1]))
  tabela_indice[,2] <- (as.numeric(tabela_indice[,2]) / 100) + 1
  
  return(tabela_indice)
}

#' Função para corrigir valor monetário a partir de índices de inflação.
#'
#' @param v Valor a ser corrigido
#' @param d_inicio Data de início da correção, em formato Date
#' @param d_fim Data de fim da correção, em formato Date
#' @param indices Quadro de dados com duas colunas - primeira com datas de correção, e segunda com fatores de multiplicação (1+índice).
#' Pode ser passado o resultado da função obtem_indices_bcb.
#' @return O valor corrigido entre as datas de início e fim.
#' @examples
#' corrige_valor(1200,dmy("24/01/2015"),dmy("24/01/2016"),df_indices)
#' corrige_valor(1200,dmy("24/01/2015"),dmy("24/01/2016"),obtem_indices_bcb("ipca"))
corrige_valor <- function(v, d_inicio, d_fim, indices)
{
  if(is.null(indices))
  {
    stop("Por favor, passe os índices!")
  }
  
  if(!is.numeric(indices[,2]) | !is.Date(indices[,1]))
  {
    stop("Por favor, passe o quadro de dados dos índices com a data na primeira coluna e o valor de ajuste/correção na segunda.")
  }
  
  if(!is.numeric(v) & !is.integer(v))
  {
    stop("Por favor, passe o valor a ser corrigido em formato numérico.")
  }
  
  if(! (lubridate::is.Date(d_inicio) & lubridate::is.Date(d_fim)))
  {
    stop("Por favor, passe as datas no formato correto.")
  }
  
  if(d_fim < d_inicio)
  {
    stop("Por favor, informe uma data fim posterio à data inicial.")
  }
  
  # Seleciona somente os índices entre a data inicial e a final
  indices_aplicados <- indices[which(indices[,1] >= d_inicio & indices[,1] <= d_fim),]
  
  # Multiplica o vetor com os fatores de multiplicação, gerado índice de correção
  indice_composto <- prod(indices_aplicados[,2])
  
  # Corrige o valor
  v_corrigido <- v * indice_composto
  
  return(v_corrigido)
}