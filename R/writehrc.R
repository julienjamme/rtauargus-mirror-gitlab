#' Ecrit une hiérarchie .hrc à partir d'une table de correspondance
#' Creates a .hrc hierarchy from a correspondence table
#'
#' @param corr_table Data frame ; table de correspondance, du plus agrégé au
#' plus fin
#' Correspondence table, from most aggregated to most detailed
#' @param output_name Nom du fichier en sortie (sans extension), ; par défaut,
#' identique au nom de la table de correspondance
#' Name for the output file (with no extension) ; default is set to the same
#' name as the correspondence table
#' @param dir_name Nom du répertoire dans lequel écrire le fichier hrc
#' Directory name for the hrc file
#' @param hier_lead_string Caractère unique repérant le niveau de profondeur
#' dans le .hrc
#' (Single) character indicating the hierarchy depth in the .hrc file
#'
#' @details Ecrit un fichier de hiérarchie .hrc lisible par $\tau$-Argus à
#' partir d'une table de corrrespondance la décrivant complètement.
#' Il est nécessaire que la table de correspondance soit :
#' - écrite du niveau le plus agrégé au plus fin
#' - bien emboîtée : un niveau fin appartient à un unique niveau supérieur
#' Creates a .hrc hierarchy file adapted to $\tau$-Argus from a correspondence
#' table. The table must be written :
#' - from the most aggregated level to the most detailed
#' - well-nested: any fine level must be nested into a unique higher level
#'
#' @return Pas de sortie R.
#' No R output.
#'
#' @export
#'
#' @examples
#' corr_a88 = read.csv("data/correspondance_a88.csv",
#'   quote = "\"",colClasses = c(a88 = "character"))
#' write.hrc(corr_a88, "example_hrc")
#'

write.hrc <- function(corr_table,
                            output_name = NULL,
                            dir_name = NULL,
                            hier_lead_string = "@"){

  #### Basic verifications & formatting

  if (is.null(output_name)) {
    givenfilename <- deparse(substitute(corr_table))
    output_name <- givenfilename
  }

  if (!is.data.frame(corr_table)) warning("corr_table should be a data frame.")

  if(nchar(hier_lead_string) != 1){
    message("hier_lead_string has to be a string of only one character.")
    return(invisible(NULL))
  }

  corr_table <- as.data.frame(apply(corr_table, 2, FUN = as.character))

  dir_name <- if(is.null(dir_name)) getwd() else dir_name


  ####

  # 1. Create a 1-step lagged version of the correspondence table
  d = dim.data.frame(corr_table)
  corr_table_decale <- rbind(
    corr_table[d[1]+1,],
    corr_table[1:(d[1]-1),]
    )

  # 2. Add a fitting number of hier_lead_string to all
  # & erase cells identical to the one just before them

  compare <- corr_table == corr_table_decale #<-- cells identical to their upper
  # neighbour
  missing <- is.na(corr_table)

  for (colonne in 1:d[2]) {
    corr_table[,colonne] <- apply(
      as.data.frame(corr_table[,colonne]),
      1:2,
      FUN = arobase,
      num_col = colonne,
      hier_lead_string = hier_lead_string)
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


arobase <- function(case,num_colonne, hier_lead_string) {
  # Adds to a given cell :
  # - as many @ as necessary given the hierarchical depth
  # - a line break for the final writing of the file
  if (num_colonne == 1) {paste(case,"\n", collapse = "", sep = "")
    } else {
  paste(c(rep(hier_lead_string,num_colonne-1),case,"\n"),
          collapse = "", sep = "")
    }
}
