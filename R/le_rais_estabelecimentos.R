#' Lê um arquivo RAIS estabelecimentos.
#'
#' Lê um arquivo com dados em formato padrão da RAIS estabelecimentos, conforme
#' disponibilizado pelo MTE. O arquivo é lido em um quadro de dados (dataframe).
#' Por padrão, lê a linha de cabeçalho, mas atribui nomes mais mnemônicos às
#' variáveis (colunas). Permite, contundo, ler a partir da primeira linha,
#' para o caso de arquivos já trabalhados e que não contenham cabeçalho.
#'
#' @param arquivo Caminho para o arquivo a ser lido.
#' @param pula.cabecalho Parâmetro booleano indicando se deve ou não ignorar a
#' primeira linha. Por padrão, é verdadeiro (formato usual de arquivos baixados
#' do MTE, sem modificações).
#' @param ano Parâmetro indicando o ano de leitura, tendo em vista que antes o layout do arquivo
#' varia conforme os anos.
#'
#' @return Um quadro de dados com os dados da RAIS estabelecimentos lidos a partir do arquivo.
#'
#' @examples
#' le_rais_estabelecimentos(arquivo = "/caminho/arquivo_estab.txt",pula.cabecalho = TRUE,anterior=FALSE)
#' #' le_rais_estabelecimentos(arquivo = "/caminho/arquivo_estab2012.txt",pula.cabecalho = TRUE,anterior=TRUE)
#'
le_rais_estabelecimentos <- function(arquivo,pula.cabecalho=TRUE,ano) {
  # Testa o sistema de arquivos para a exist�ncia do arquivo solicitado.
  if(!file.exists(arquivo))
  {
    stop("Nao foi possivel abrir o arquivo!")
  }
  else
  {
    # Faz a leitura, atribuindos nomes mnem�nicos �s colunas.
    tryCatch(l <- read.csv2(arquivo,sep=";",dec=",",skip = ifelse(pula.cabecalho,1,0),encoding = "latin1"),
             error=function(err) { print(err) })
    if(ano == 1990)
    {
      variaveis <- c("BAIRROSP","ESTOQUE","MUNICIPIO","TAMESTAB","TIPO_IDENT","UF","IBGESUBS","IBGESUBATIV")
    }
    else if(ano == 1991 | ano == 1992)
    {
      variaveis <- c("BAIRROSP","ESTOQUE","DISTRITOSP","MUNICIPIO","TAMESTAB","TIPO_IDENT","UF","IBGESUBS","IBGESUBATIV")
    }
    else if(ano == 1993)
    {
      variaveis <- c("ESTOQUE","MUNICIPIO","TAMESTAB","TIPO_IDENT","UF","IBGESUBS","IBGESUBATIV")
    }
    else if(ano == 1994)
    {
      variaveis <- c("BAIRROSP","CNAE95","DISTRITOSP","ESTOQUE","MUNICIPIO","NATESTB","TAMESTAB","TIPO_ESTBL","UF","IBGESUBS","IBGESUBATIV")
    }
    else if(ano == 1995 | ano == 1996)
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE95","DISTRITOSP","ESTOQUE","IND_RAISNEG","MUNICIPIO","NATJUR","REGADMDF","TAMESTAB","TIPOESTBL","UF","IBGESUBS")
    }
    else if(ano == 1997)
    {
      variaveis <- c("BAIRROFORT","BAIRRORJ","CNAE95","ESTOQUE","INDRAISNEG","MUNICIPIO","NATJUR","REGADMDF","TAMESTAB","TIPOESTBL","UF","IBGESUBS")
    }
    else if(ano == 1998)
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE95","DISTRITOSP","ESTOQUE","INDRAISNEG","MUNICIPIO","NATJUR","REGADMDF","TAMESTAB","TIPOESTBL","UF","IBGESUBS")
    }
    else if(ano == 1999 | ano == 2000)
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE95","DISTRITOSP","ESTOQUE","INDCEIVINC","INDPAT","INDRAISNEG","INDSIMPLES","MUNICIPIO","NATJUR","REGADMDF","TAMESTAB","TIPOESTBL","UF","IBGESUBS")
    }
    else if(ano == 2001)
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE95","DISTRITOSP","ESTCLTOUT","ESTOQUE","ESTOQUEESTA","INDCEIVINC","INDPAT","INDRAISNEG","INDSIMPLES","MUNICIPIO","NATJUR","REGADMDF","TAMESTAB","TIPOESTBL","UF","IBGESUBS")
    }
    else if(ano >= 2002 & ano <= 2013)
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE20","CNAE95","DISTRITOSP","ESTCLTOUT","ESTOQUE","ESTOQUEESTA","INDATIVANO","INDCEIVINC","INDPAT","INDRAISNEG","INDSIMPLES","MUNICIPIO","NATJUR","REGADMDF","CNAE20SUB","TAMESTAB","TIPOESTBL","TIPOESTBL2","UF","IBGESUBS")
    }
    else if(ano >= 2014)
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE20","CNAE95","DISTRITOSP","VINCULOSCLT","VINCULOSATIVOS","VINCULOSESTATUT","INDATIVANO","INDCEIVINC","INDPAT","INDRAISNEG","INDSIMPLES","MUNICIPIO","NATJUR","REGADMDF","CNAE20SUB","TAMESTAB","TIPOESTBL","TIPOESTBL2","UF","IBGESUBS","CEP")
    }
    else
    {
      variaveis <- c("BAIRROSP","BAIRROFORT","BAIRRORJ","CNAE20","CNAE95","DISTRITOSP","VINCULOSCLT","VINCULOSATIVOS","VINCULOSESTATUT","INDATIVANO","INDCEIVINC","INDPAT","INDRAISNEG","INDSIMPLES","MUNICIPIO","NATJUR","REGADMDF","CNAE20SUB","TAMESTAB","TIPOESTBL","TIPOESTBL2","UF","IBGESUBS","CEP")
    }
  }
  names(l) <- variaveis
  return(l)
}