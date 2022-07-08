#' .hrc writing
#'
#' Creates a .hrc hierarchy from a correspondence table
#' Ecrit une hiérarchie .hrc à partir d'une table de correspondance
#'
#' @param corr_table Correspondence table, from most aggregated to most detailed
#' \cr
#' Data frame ; table de correspondance, du plus agrégé au
#' plus fin
#' @param output_name character string. Name for the output file (with no
#' extension) ; default is set to the same name as the correspondence table
#' \cr
#' Nom du fichier en sortie (sans extension), ; par défaut,
#' identique au nom de la table de correspondance
#' @param dir_name character string. Directory name for the hrc file
#' \cr
#' Nom du répertoire dans lequel écrire le fichier hrc
#' @param sort_table boolean. If TRUE, table will be sorted beforehand.
#' Recommended.\cr
#' Si TRUE, la table sera triée avant traitement. Recommandé.
#' @param rev boolean. If TRUE, column order is reversed.\cr
#' Si TRUE, inverse l'ordre des colonnes.
#' @param hier_lead_string #' character. (Single) character indicating the
#' hierarchy depth in the .hrc file \cr
#' Caractère unique repérant le niveau de profondeur dans le .hrc
#'
#' @details #' Creates a .hrc hierarchy file adapted to tau-Argus from a correspondence
#' table. The table must be written :
#' - from the most aggregated level to the most detailed
#' - well-nested: any fine level must be nested into a unique higher level
#' \cr
#' Ecrit un fichier de hiérarchie .hrc lisible par tau-Argus à
#' partir d'une table de corrrespondance la décrivant complètement.
#' Il est nécessaire que la table de correspondance soit :
#' - écrite du niveau le plus agrégé au plus fin
#' - bien emboîtée : un niveau fin appartient à un unique niveau supérieur
#'
#' @section Details about correspondence table & .hrc
#' Hierarchy files read by tau-Argus are expected to follow a strict pattern.
#' If there is a problem using the .hrc file (for instance, error making
#' totals), please check the following : \cr
#'
#' 1. Are columns ordered from most aggregated to most detailed ?
#' If they are in reverse order, you may want to use rev = TRUE or simply order
#' the correspondence table before trying again.\cr
#'
#' 2. Is the hierarchy well-nested ? Fine levels must systematically be nested
#' into unique higher levels.
#' If not, you will have to treat separately the different hierarchies, and then
#' merge masks until stable.\cr
#'
#' 3.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' astral_hrc <- data.frame(
#' type = c("planet", "planet", "star", "star", "star", "other", "other"),
#' details = c("telluric", "gasgiant", "basic", "whitedwarf", "browndwarf", "blackhole", "neutronstar" )
#' )
#' write_hrc2(astral_hrc)
#'

write_hrc2 <- function(corr_table,
                       output_name = NULL,
                       dir_name = NULL,
                       sort_table = TRUE,
                       rev = FALSE,
                       hier_lead_string = getOption("rtauargus.hierleadstring")
){

  d = dim.data.frame(corr_table)

  #### Basic verifications & formatting

  # Fill in output name
  if (is.null(output_name)) {
    givenfilename <- deparse(substitute(corr_table))
    output_name <- givenfilename
  }

  # Default directory
  dir_name <- if(is.null(dir_name)) getwd() else dir_name

  # Reverse column order if asked
  if (rev) corr_table <- rev(corr_table)

  # Make corr_table a data frame, or raise error
  corr_table <- tryCatch(
    {
      as.data.frame(corr_table)
    },
    error = function(msg){
      stop("Cannot coerce corr_table to a data frame")
      print(msg)
    }
  )

  # Check hier_lead_string
  if(nchar(hier_lead_string) != 1){
    stop("hier_lead_string should be 1 single character")
  }

  # Warn about presence of NAs
  if (sum(is.na(corr_table))>0){
    warning("Missing values in correspondence table will be ignored. If unintended, this can cause errors when using the .hrc file with tau-Argus.")
  }
  # (Todo : lister cas de NA non gênantes et bloquer les autres)

  # Try to detect a problem with detailed column
  if (sum(duplicated(corr_table[,d[2]]))>0) {
  warning("There are duplicates in the most detailed level (last column).
          Please check table")
  }

  # Check if all columns are character
  suspects <- NULL
  for (col in 1:d[2]){
    if (!is.character(corr_table[,col])) {
      suspects <- c(suspects, col)
    }
  }
  if (!is.null(suspects))  message("Note : the following columns are not of character type : ", colnames(corr_table)[suspects], ". There may be an issue reading the table.")

  #### Creating the hrc file

  # 0. Sort the correspondence table
  if (sort_table){
    for (j in 1:d[2]){
      corr_table <- corr_table[
        order(corr_table[,d[2]-j+1])
        ,]
    }
  }

  # 1. Create a 1-step lagged version of the correspondence table
  corr_table_decale <- rbind(
    corr_table[d[1]+1,],
    corr_table[1:(d[1]-1),]
  )
  # (same dimensions as corr_table ; any line i is equal to line (i-1) of the
  # original table, except for 1rst line which is just NAs)

  compare <- corr_table == corr_table_decale #<-- cells identical to their upper
  # neighbour
  missing <- is.na(corr_table)

  # 2. Add a fitting number of hier_lead_string to all
  # & erase cells identical to the one just before them

  for (colonne in 1:d[2]) {
    corr_table[,colonne] <- paste0(
      paste0(rep(hier_lead_string,colonne -1), collapse = ""),
      corr_table[,colonne],
      "\n")
  }
  corr_table[compare] <- ""
  corr_table[missing] <- ""

  # 3. Write corresponding table
  # Note that columns & cells are not separated by anything, but cells that have
  # not been erased still hold a line break ("\n") so that there will be line
  # breaks only after non-void characters.

  write.table(x = corr_table,
              file = paste(c(dir_name, "/", output_name, ".hrc"), collapse = "", sep = ""),
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE,
              sep="",
              eol = "")
}
