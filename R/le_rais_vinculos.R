#' Lê um arquivo RAIS vínculos.
#'
#' Lê um arquivo com dados em formato padrão da RAIS vínculos, conforme
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
#' @return Um quadro de dados com os dados da RAIS vínculos lidos a partir do arquivo.
#'
#' @examples
#' le_rais_vinculos(arquivo = "/caminho/arquivo.txt",pula.cabecalho = TRUE,anterior=FALSE)
#' #' le_rais_vinculos(arquivo = "/caminho/arquivo2012.txt",pula.cabecalho = TRUE,anterior=TRUE)
#'
le_rais_vinculos <- function(arquivo,pula.cabecalho=TRUE,ano) {
  # Testa o sistema de arquivos para a existência do arquivo solicitado.
  if(!file.exists(arquivo))
  {
    stop("Nao foi possivel abrir o arquivo!")
  }
  else
  {
    # Faz a leitura, atribuindos nomes mnemônicos às colunas.
    tryCatch(l <- read.csv2(arquivo,sep=";",dec=",",skip = ifelse(pula.cabecalho,1,0)),
             error=function(err) { print(err) })
  }
  
  if(ano <= 1993)
  {
    colnames(l) = c("BAIRROSP","CAUSADESLI","DISTRITOSP","EMP3112","FAIXAETARIA","FAIXAREMUNDEZEMSM","FAIXAREMUNMEDIASM","FAIXATEMPODEEMPREGO","GRINSTRUCAO","MESADMISSAO","MESDESLIGAMENTO","MUNICIPIO","NACIONALIDADE","REMDEZEMBROSM","REMMEDIASM","SEXO","TAMESTAB","TEMPEMPR","TIPOESTBL","TIPOESTBL2","TPVINCULO","IBGESUBA","IBGESUBS","OCUPACAO")
  }
  else if(ano == 2015)
  {
    colnames(l) = c("BAIRROSP","BAIRROFORT","BAIRRORJ","CAUSAFAST1","CAUSAFAST2","CAUSAFAST3","CAUSADESLI","CBO2002","CLASCNAE20","CLASCNAE95","DISTRITOSP","EMP3112","FAIXAETARIA","FXHORACONTRAT","FAIXAREMUNDEZSM","FAIXAREMUNMEDIASM","FAIXATEMPOEMP","GRINSTRUCAO","HORASCONTR","IDADE","INDCEI","INDSIMPLES","MESADMISSAO","MESDESLIGAMENTO","MUNTRAB","MUNICIPIO","NACIONALIDADE","NATJURIDICA","DEFIC","QTDIASAFAS","RACA","REGADMDF","REMDEZRS","REMDEZSM","REMMEDRS","REMMEDIASM","SBCLAS20","SEXO","TAMESTAB","TEMPEMPR","TIPOADM","TIPOESTBL","TIPOESTBL2","TPDEFIC","TPVINCULO","IBGESUBS","VLJAN","VLFEV","VLMAR","VLABR","VLMAI","VLJUN","VLJUL","VLAGO","VLSET","VLOUT","VLNOV")
  }
  else if(ano > 2015)
  {
    colnames(l) = c("BAIRROSP","BAIRROFORT","BAIRRORJ","CAUSAFAST1","CAUSAFAST2","CAUSAFAST3","CAUSADESLI","CBO2002","CLASCNAE20","CLASCNAE95","DISTRITOSP","EMP3112","FAIXAETARIA","FXHORACONTRAT","FAIXAREMUNDEZSM","FAIXAREMUNMEDIASM","FAIXATEMPOEMP","GRINSTRUCAO","HORASCONTR","IDADE","INDCEI","INDSIMPLES","MESADMISSAO","MESDESLIGAMENTO","MUNTRAB","MUNICIPIO","NACIONALIDADE","NATJURIDICA","DEFIC","QTDIASAFAS","RACA","REGADMDF","REMDEZRS","REMDEZSM","REMMEDRS","REMMEDIASM","SBCLAS20","SEXO","TAMESTAB","TEMPEMPR","TIPOADM","TIPOESTBL","TIPOESTBL2","TPDEFIC","TPVINCULO","IBGESUBS","VLJAN","VLFEV","VLMAR","VLABR","VLMAI","VLJUN","VLJUL","VLAGO","VLSET","VLOUT","VLNOV","ANOCHEGADA")
  }
  else
  {
    colnames(l) = c("BAIRROSP","BAIRROFORT","BAIRRORJ","CAUSAFAST1","CAUSAFAST2","CAUSAFAST3","CAUSADESLI","CBO2002","CLASCNAE20","CLASCNAE95","DISTRITOSP","EMP3112","FAIXAETARIA","FXHORACONTRAT","FAIXAREMUNDEZSM","FAIXAREMUNMEDIASM","FAIXATEMPOEMP","GRINSTRUCAO","HORASCONTR","IDADE","INDCEI","INDSIMPLES","MESADMISSAO","MESDESLIGAMENTO","MUNTRAB","MUNICIPIO","NACIONALIDADE","NATJURIDICA","DEFIC","QTDIASAFAS","RACA","REGADMDF","REMDEZRS","REMDEZSM","REMMEDRS","REMMEDIASM","SBCLAS20","SEXO","TAMEST","TEMPEMPR","TIPOADM","TIPOESTBL","TIPOESTBL2","TPDEFIC","TPVINCULO")
  }
  
  # Retorna o valor lido.
  return(l)
}