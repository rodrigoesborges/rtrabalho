#' Lê um arquivo CAGED
#'
#' Lê um arquivo com dados em formato padrão do CAGED, conforme
#' disponibilizado pelo MTE. O arquivo é lido em um quadro de dados (dataframe).
#' Por padrão, lê a linha de cabeçalho, mas atribui nomes mais mnemônicos às
#' variáveis (colunas). Permite, contundo, ler a partir da primeira linha,
#' para o caso de arquivos já trabalhados e que não contenham cabeçalho.
#'
#' @param arquivo Caminho para o arquivo a ser lido.
#' @param pula.cabecalho Parâmetro booleano indicando se deve ou não ignorar a
#' primeira linha. Por padrão, é verdadeiro (formato usual de arquivos baixados
#' do MTE, sem modificações).
#'
#' @return Um quadro de dados com os dados do CAGED lidos a partir do arquivo.
#'
#' @examples
#' le_caged(arquivo = "/caminho/arquivo.txt",pula.cabecalho = TRUE)
#'
le_caged <- function(arquivo,pula.cabecalho=TRUE) {
  
  # Testa o sistema de arquivos para a existência do arquivo solicitado.
  if(!file.exists(arquivo))
  {
    stop("Não foi possível abrir o arquivo!")
  }
  else
  {
    # Faz a leitura, atribuindos nomes mnemônicos às colunas.
    tryCatch(l <- read.table(arquivo,sep=";",dec=",",
                             col.names = c("ADMDESL","COMPET","MUNICIPIO","ANO","CBO",
                                           "CNAE1","CNAE2","CNAE2S","FAIXAEMP","GRAUINST",
                                           "HORAS","IBGESUBS","IDADE","APRENDIZ","DEFIC",
                                           "RACA","SALARIO","SALDOM","SEXO","TEMPOEM",
                                           "TIPOEST","TIPODEF","TIPOMOV","UF","BAIRROSP",
                                           "BAIRROFT","BAIRRORJ","DISTRITOSP","REGIAODF",
                                           "MESORREG","MICRORREG","RADMRJ","RADMSP",
                                           "RADMCORE","RADMCOR4","RGOVSP","RSENACPR",
                                           "RSENAIPR","RSENAISP","SRSENAIPR"),
                             skip = ifelse(pula.cabecalho,1,0),
                             colClasses = c("factor","factor","factor","numeric","factor",
                                            "factor","factor","factor","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","numeric",
                                            "factor","double","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric","character",
                                            "character","character","character","character","character",
                                            "character","character","character","character","character",
                                            "character","character","character","character","character")),
             error=function(err) { print(err) })
  }
  # Retorna o valor lido.
  return(l)
}
