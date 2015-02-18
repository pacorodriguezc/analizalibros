
limpia <- function(titulo) {
  # limpia de números, signos, blancos, etc... 
  texto <- scan(titulo, character(0), quote="")
  vector <- grep("[0-9]",texto)
  if (length(vector) != 0) texto <- texto[-vector]
  texto <- tolower(texto) # todo a minúsculas
  texto <- gsub(pattern = "[[:punct:]]", "", texto)
  vector <- nchar(texto) == 0
  texto <- texto[!vector]
  texto1 <- unique(texto) # quita palabras repetidas
  return(list(texto,texto1)) # regresa lista de todas las palabras y de las no repetidas
}

diferentes <- function(acumuladas, nuevas) {
  # saca palabras diferentes vs un vector
  if (!is.null(acumuladas)) {
    nuevas <- nuevas[!nuevas %in% acumuladas]
  } 
  return(nuevas)
}

ocurrencias <- function(texto, omitir) {
  # quita las palabras a omitir
  vector <- texto %in% omitir
  omitidas <- texto[!vector]
  return(omitidas)
}

# Inicializa variables
resumen <- data.frame(character(0), integer(0), integer(0), integer(0), integer(0), numeric(0))
unicas <- NULL
ranklist <- list()
path = "~/Documents/librostext/"
directory <- list.files(path, pattern = "*.txt") # lee los nombres de archivos a analizar
fechapub <- read.csv(paste(path,"fechas/fechas.txt", sep="")) # lee archivo de fechas de publicación

# lee archivo de palabras a omitir en el ranking (artículos, pronombres, etc.)
omitir <- scan("~/Documents/librostext/fechas/omitelenguaje.txt", character(0))
directory <- directory[match(fechapub$titulo,directory)] # asigna fecha de publicación a cada file

for (i in directory) {
  # regresa una lista con dos vectores, todas las palabras y sin repeticiones
  texto <- limpia(paste(path,i,sep="")) 
  l1 <- length(texto[[1]]) # num. de palabras en el texto
  l2 <- length(texto[[2]]) # num. de palabras no repetidas
  year <- fechapub[fechapub$titulo==i,2] # año de publicación
  titulo <- sub(".txt","",i) # extrae el titulo del nombre del archivo
  dif <- diferentes(unicas,texto[[2]]) # saca el núm. de palabras nuevas vs el acumulado
  unicas <- c(unicas,texto[[2]]) # acumula las palabras no repetidas en un vector 
  l3 <- length(dif) # num. de palabras nuevas
  # saca el ranking de las palabras repetidas, quitando las palabras a omitir
  ranking <- ocurrencias(texto[[1]],omitir)
  # las ordena de mayor a menor las 100 más repetidas
  ranklist[[titulo]] <- head(sort(table(ranking), decreasing = TRUE),100)
  # agrega un renglón al data frame de la obra i
  resumen <- rbind(resumen, data.frame(titulo,year,l1,l2,l3,(l2/l1*100)))
}
# Saca las palabras únicas de todas las obras
total <- unique(unicas)
total <- total[order(total)]
# agrega la columna del % de la obra vs. el total
resumen <- cbind(resumen,sapply(resumen[,4], function(x) x/length(total)*100))
names(resumen) <- c("titulo","año", "numpalabras","palabrasunicas","aportadas", "unicas","totalunicas")
print(ranklist) # lista de ranking de todas las obras
print(resumen) # muestra tabla de resultados