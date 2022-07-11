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
#'
#' 1. \strong{Ideal case}
#'
#' Here is how a correspondence table is assumed to look like:
#' | type   | detailed   |
#' |--------|------------|
#'   | planet | telluric   |
#'   | planet | gasgiant   |
#'   | star   | bluestar   |
#'   | star   | whitedwarf |
#'   | star   | browndwarf |
#'   | other  | blackhole  |
#'   | other  | pulsar     |
#'
#' Columns must be ordered from most aggregated to most detailed.
#' If they are in reverse order, you may want to use rev = TRUE. In any other
#' case, please reorder columns by hand.\cr
#'
#' Hierarchy must be well-nested : fine levels must systematically be nested
#' into unique higher levels. If this is not compatible with your situation,
#' you will have to split it in different hierarchies and insure common cells
#' are correctly protected (seek further documentation or help if needed).
#' \cr
#'
#' 2. \strong{Dealing with NAs}
#'
#' All levels must be filled in for the write_hrc2 function to work properly.
#' This ensures that the alphabetical sorting purposely regroups equal levels
#' together. NAs would be sorted together and, thus, be separated from their
#' expected place in the hierarchy. Other problems may also arise if NAs are
#' mixed with regular values in the table : comparisons between a line and its
#' precedessor are made, that may fail if NAs are inputed.\cr
#' There are, however, a few cases when NAs can be left relevantly. Please
#' be careful with the following possibilities and check thoroughly the
#' resulting .hrc file, or consider filling in NAs beforehand.
#'
#' 2.1 \emph{Sparse hierarchies} \cr
#' Hierarchy is sparse when NAs are inserted instead of repeating under a given
#' level.
#' #' | type   | detailed   |
#' |--------|------------|
#' | planet | telluric   |
#' |        | gasgiant   |
#' | star   | bluestar   |
#' |        | whitedwarf |
#' |        | reddwarf   |
#' | other  | blackhole  |
#' |        | pulsar     |
#' Processing such a file will result in wrongly written .hrc, since NAs will be
#' sorted together. It is still possible to deactivate sorting ; see sexample
#' below. This crucially requires that the table has already been sorted by the
#' user.
#'
#' 2.2 \emph{Non-uniform hierarchies}
#' Hierarchies with non-uniform depth happen when some levels are not detailed
#' to the  lowest detail, creating NAs.
#' | type   | detailed   |
#' |--------|------------|
#'   | planet | telluric   |
#'   | planet | gasgiant   |
#'   | star   |            |
#'   | other  | blackhole  |
#'   | other  | pulsar     |
#' Such cases still issue a warning for the presence of NAs, but do not pose
#' any problem.
#'
#' @return Invisible. Path to the written .hrc file.
#' \cr
#' Chemin vers le fichier .hrc.
#'
#' @export
#'
#' @examples
#' # 1. Standard example. Table is sorted directly by the function.
#' astral <- data.frame(
#'   type      = c("planet", "planet", "star", "star", "star", "other", "other"),
#'   detailed  = c("telluric", "gasgiant", "bluestar", "whitedwarf", "reddwarf", "blackhole", "pulsar")
#' )
#' path <- write_hrc2(astral)
#' read.table(path)
#'
#' # Wrong order:
#' astral_inv <- data.frame(
#'   detailed  = c("telluric", "gasgiant", "bluestar", "whitedwarf", "reddwarf", "blackhole", "pulsar"),
#'   type      = c("planet", "planet", "star", "star", "star", "other", "other")
#' )
#' path <- write_hrc2(astral_inv)
#' read.table(path)
#' # Because of the inverted order, everything is written backwards : planet is a
#' # subtype of gasgiant, etc.
#' # Correction :
#' path <- write_hrc2(astral_inv, rev = TRUE)
#' read.table(path)
#'
#' # 2.1 Sparse case
#' astral_sparse <- data.frame(
#'   type      = c("planet", NA, "star", NA, NA, "other", NA),
#'   detailed  = c("telluric", "gasgiant", "bluestar", "whitedwarf", "reddwarf", "blackhole", "pulsar")
#' )
#' # NAs in general are risky : as is, the alphabetical sorting scrambles it all.
#' path <- write_hrc2(astral_sparse)
#' read.table(path)
#' # Here, gasgiant and pulsar were misread as sublevels of 'star'.
#' # In order to correctly ignore NAs, sorting must be disabled.
#' path2 <- write_hrc2(astral_sparse2, sort_table = FALSE)
#' read.table(path2)
#'
#'
#' # 2.2 Non-uniform depth
#' # Non-uniform case still rightfully treated :
#' astral_nu <- data.frame(
#'   type      = c("planet", "planet", "star", "other", "other"),
#'   detailed  = c("telluric", "gasgiant", NA, "blackhole", "pulsar")
#' )
#' path <- write_hrc2(astral_nu)
#' read.table(path)
#'


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
    warning("Missing values in correspondence table will be ignored (see documentation).
            If unintended, this can cause errors when using the .hrc file with tau-Argus.")
  }
  # (Todo : lister cas de NA non gênantes et bloquer les autres)

  # Try to detect a problem with detailed column
  if (sum(duplicated(corr_table[,d[2]]))>0) {
    warning("There are duplicates in the expectedly most detailed level
    (last column). Please be sure columns are rightfully ordered.")
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
