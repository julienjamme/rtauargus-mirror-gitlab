#' .hrc writing
#'
#' Creates a .hrc hierarchy from a correspondence table \cr
#' Ecrit une hiérarchie .hrc à partir d'une table de correspondance
#'
#' @param corr_table Data frame. Correspondence table, from most aggregated to most detailed
#' \cr
#' Table de correspondance, du plus agrégé au plus fin
#' @param output_name character string. Name for the output file (with no
#' extension) ; default is set to the same name as the correspondence table
#' \cr
#' Nom du fichier en sortie (sans extension) ; par défaut,
#' identique au nom de la table de correspondance
#' @param dir_name character string. Directory name for the hrc file
#' \cr
#' Nom du répertoire dans lequel écrire le fichier hrc
#' @param sort_table boolean. If TRUE, table will be sorted beforehand.
#' Recommended.\cr
#' Si TRUE, la table sera triée avant traitement. Recommandé.
#' @param rev boolean. If TRUE, column order is reversed.\cr
#' Si TRUE, inverse l'ordre des colonnes.
#' @param hier_lead_string character. (Single) character indicating the
#' hierarchy depth in the .hrc file
#' \cr
#' Caractère unique repérant le niveau de profondeur dans le .hrc
#'
#' @details Creates a .hrc hierarchy file adapted to tau-Argus from a correspondence
#' table. The table must be written :\cr
#' - from the most aggregated level to the most detailed \cr
#' - well-nested: any fine level must be nested into a unique higher level
#' \cr
#' Ecrit un fichier de hiérarchie .hrc lisible par tau-Argus à
#' partir d'une table de corrrespondance la décrivant complètement.
#' Il est nécessaire que la table de correspondance soit :\cr
#' - écrite du niveau le plus agrégé au plus fin\cr
#' - bien emboîtée : un niveau fin appartient à un unique niveau supérieur
#'
#' @section Details about correspondence table & .hrc:
#' Hierarchy files read by tau-Argus are expected to follow a strict pattern.
#' This function mimicks some of its rigidities.
#' \cr
#' 1. Columns must be ordered from most aggregated to most detailed.
#' If they are in reverse order, you may want to use rev = TRUE. In any other
#' case, please reorder columns by hand.\cr
#'
#' 2. Hierarchy must be well-nested : fine levels must systematically be nested
#' into unique higher levels.
#' If this is not compatible with your situation, you will have to treat
#' separately the different hierarchies, and then merge masks until stable.
#' \cr
#'
#' 3. All levels must be filled in for the write_hrc2 function to work properly.
#' This ensures that the alphabetical sorting purposely regroups equal levels
#' together. NAs would be sorted together and, thus, be separated from their
#' expected place in the hierarchy. Other problems may also arise if NAs are
#' mixed with regular values in the table : comparisons between a line and its
#' precedessor are made, that may fail if NAs are inputed.\cr
#'
#' 4. There are, however, a few cases when NAs can be left relevantly. Please
#' be careful with the following possibilities and check thoroughly the
#' resulting .hrc file, or consider filling in NAs beforehand :\cr
#' - for sparse hierarchies, where all higher levels are indicated only once
#' and then followed by NAs until jumping to the next category (see sparse
#' example) ;\cr
#' - for non-uniform hierarchies, where the lowest levels may not be filled in
#' for certain categories (but it has to be systematic).\cr
#'
#' @return Invisible. Path to the written .hrc file.
#' \cr
#' Chemin vers le fichier .hrc.
#'
#' @export
#'
#' @examples
#' # Standard example. Table is sorted directly by the function.
#' astral_hrc <- data.frame(
#' type = c("star", "other", "planet", "star", "star", "other", "planet"),
#' details = c("browndwarf", "neutronstar", "telluric", "standard",
#' "whitedwarf", "blackhole", "gasgiant" )
#' )
#' path_to_astral <- write_hrc2(astral_hrc)
#' read.table(path_to_astral) # visualising the output file
#'
#' # Wrong order:
#' reversed_astral <- rev(astral_hrc)
#' path_to_wrongly_ordered <- write_hrc2(reversed_astral)
#' read.table(path_to_wrongly_ordered)
#' # "star" written as a subtype of "browndwarf" (and also of whitedwarf).
#' # Solution :
#' path <- write_hrc2(reversed_astral, rev = TRUE)
#' read.table(path)
#'
#' # Sparse case: Here, NAs were inserted instead of repeating aggregated
#' # categories. Direct writing fails : NAs are sorted together before deletion
#' # and create wrongly isolated branches.
#' astral_sparse <- data.frame(
#' type = c("planet", "planet", "star", "star", "star", "other", "other"),
#' details = c("telluric", "gasgiant" , "standard", "browndwarf",
#' "whitedwarf", "blackhole","neutronstar")
#' )
#' write_hrc2(astral_sparse, output = "wrongly_written_sparse")
#' # Such a table can still be treated by switching off sorting.
#' write_hrc2(astral_sparse, sort_table = FALSE)
#'
#' # Non-uniform case still rightfully treated :
#' astral_nu <- data.frame(
#' type = c("planet", "planet", "star", "other"),
#' details = c("telluric", "gasgiant" , NA, NA)
#' )
#' path <- write_hrc2(astral_nu)
#' read.table(path)
#'
#' # Fatal case with NAs:
#' astral_na <- data.frame(
#' type = c("planet", NA, NA, "star", NA, "other", "other"),
#' details = c("telluric", "gasgiant" , "standard", "browndwarf",
#' "whitedwarf", "blackhole","neutronstar")
#' )
#' path_to_na_hrc <- write_hrc2(astral_na)
#' read.table(path_to_na_hrc)
#' # gasgiant, a type of planet, has been implicitly categorized as a star
#' # because of its missing upper category


write_hrc2 <- function(corr_table,
                       output_name = NULL,
                       dir_name = NULL,
                       sort_table = TRUE,
                       rev = FALSE,
                       hier_lead_string = getOption("rtauargus.hierleadstring")
){

  # Set default filename / directory
  if (is.null(output_name)) {
    givenfilename <- deparse(substitute(corr_table))
    output_name <- givenfilename
  }

  dir_name <- if(is.null(dir_name)) getwd() else dir_name

  d = dim.data.frame(corr_table)

  #### Basic verifications & formatting

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
    warning("Missing values in correspondence table will be erased If unintended, this can cause errors when using the .hrc file with tau-Argus.")
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
    rep("line1"),
    corr_table[1:(d[1]-1),]
  )
  # (same dimensions as corr_table ; any line i is equal to line (i-1) of the
  # original table, except for 1rst line which only purpose is making the
  # comparison return FALSE)

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

  loc_file <- paste0(c(dir_name, "/", output_name, ".hrc"), collapse = "")

  write.table(x = corr_table,
              file = loc_file,
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE,
              sep="",
              eol = "")

  invisible(loc_file)
}
