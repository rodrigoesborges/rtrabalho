competencia_para_data <- function(compet)
{
  # Obs.: criar a data de referência para o dia 1 de cada mês.
  data <- as.Date( paste( substr(as.character(compet),1,4), substr(as.character(compet),5,6),"1",sep="-" ) )	
  return(data)
}