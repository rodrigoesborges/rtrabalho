setClass("CAGED",representation(dados="data.frame"))

#' Cria um objeto CAGED
#'
#' Cria um objeto do tipo CAGED, que contém os microdados de um recorte específico
#' deste tipo de dados, lidos a partir da função \code{le_caged}. Como objeto,
#' o resultado pode ser usado em outras funções da classe.
#'
#' @param fdados Quadro de dados com as 40 colunas respectivas da leitura de
#' um arquivo padrão do MTE, para dados do CAGED.
#' @return O objeto CAGED, apto a ser aplicado em outras funções da classe.
CAGED <- function(fdados=data.frame())
{
  # Por precaução, atribui às colunas nomes mnemônicos (é possível que já tenham esses nomes).
  colnames(fdados) <- c("ADMDESL","COMPET","MUNICIPIO","ANO","CBO","CNAE1","CNAE2","CNAE2S","FAIXAEMP","GRAUINST","HORAS","IBGESUBS","IDADE","APRENDIZ","DEFIC","RACA","SALARIO","SALDOM","SEXO","TEMPOEM","TIPOEST","TIPODEF","TIPOMOV","UF","BAIRROSP","BAIRROFT","BAIRRORJ","DISTRITOSP","REGIAODF","MESORREG","MICRORREG","RADMRJ","RADMSP","RADMCORE","RADMCOR4","RGOVSP","RSENACPR","RSENAIPR","RSENAISP","SRSENAIPR")
  new("CAGED",dados=fdados)
}


CAGED_media_salarial <- function(ca=CAGED)
{
  return(mean(ca@dados$SALARIO))
}

CAGED_filtra_admitidos <- function(ca=CAGED)
{
  t1 <- ca@dados
  t2 <- t1[which(t1$ADMDESL=="01"),]
  t3 <- new("CAGED",dados=t2)
  return(t3)
}

CAGED_filtra_demitidos <- function(ca=CAGED)
{
  t1 <- ca@dados
  t2 <- t1[which(t1$ADMDESL=="02"),]
  t3 <- new("CAGED",dados=t2)
  return(t3)
}

CAGED_filtra_competencia <- function(ca=CAGED,compet=integer)
{
  t1 <- ca@dados
  t2 <- t1[which(t1$COMPET==compet),]
  t3 <- new("CAGED",dados=t2)
  return(t3)
}

CAGED_filtra_municipio <- function(ca=CAGED,municipio=integer)
{
  t1 <- ca@dados
  t2 <- t1[which(t1$MUNICIPIO==municipio),]
  t3 <- new("CAGED",dados=t2)
  return(t3)
}

CAGED_filtra_subsetoribge <- function(ca=CAGED,subsetoribge=integer)
{
  t1 <- ca@dados
  t2 <- t1[which(t1$IBGESUBS==subsetoribge),]
  t3 <- new("CAGED",dados=t2)
  return(t3)
}

CAGED_boxplot_salarios <- function(ca = CAGED)
{
  if(nrow(ca@dados) > 1)
  {
    boxplot(ca@dados$SALARIO)
  }
}

CAGED_agrupa_grande_setor <- function(ca = CAGED)
{
  t1 <- ca@dados
  subsetor <- as.factor(as.numeric(as.character(t1$IBGESUBS)))
  levels(subsetor) <- c(rep(1,14),2,rep(3,2),rep(4,7),5)
  t1$GRSETOR <- as.factor(as.numeric(subsetor))
  t2 <- new("CAGED",dados=t1)
  return(t2)
}

CAGED_filtra_setoribge <- function(ca = CAGED,grsetor=integer)
{
  if(is.null(ca@dados$GRSETOR))
  {
    ca <- CAGED_agrupa_grande_setor(ca)	
  }
  t1 <- ca@dados
  t2 <- t1[which(t1$GRSETOR==grsetor),]
  t3 <- new("CAGED",dados=t2)
  return(t3)
}

CAGED_cria_datas <- function(ca = CAGED)
{
  t1 <- ca@dados
  t1$DATAREF <- competencia_para_data(t1$COMPET)
  t2 <- new("CAGED",dados=t1)
  return(t2)
}