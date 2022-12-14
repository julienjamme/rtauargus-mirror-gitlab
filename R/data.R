#' data - Turnover broken down by business sector and size of French companies (fake values).
#'
#' A tabular dataset containing the turnover broken down by Business sector
#' and Size of companies. Useful for playing with tab_ functions.
#'
#' @format A tibble/data frame with 414 rows and 5 variables:
#' \describe{
#'  \item{ACTIVITY}{business sector, hierarchical variables with three levels described
#'   in the activity_corr_table dataset. The root is noted "Total"}
#'   \item{SIZE}{size of the companies (Number of employees in three categories
#'   + overall category "Total")}
#'   \item{N_OBS}{Frequency, number of companies}
#'   \item{TOT}{turnover value in euros}
#'   \item{MAX}{turnover of the company which contributes the most to the cell.}
#' }
#' @seealso activity_corr_table
"turnover_act_size"

#' data - Turnover broken down by business sector and type of companies (fake values).
#'
#' A tabular dataset containing the turnover broken down by Business sector
#' and Type of companies. Useful for playing with tab_ functions.
#'
#' @format A tibble/data frame with 406 rows and 5 variables:
#' \describe{
#'   \item{ACTIVITY}{business sector, hierarchical variables with three levels described
#'   in the activity_corr_table dataset. The root is noted "Total"}
#'   \item{CJ}{Type of companies (3 categories + overall category "Total")}
#'   \item{N_OBS}{Frequency, number of companies}
#'   \item{TOT}{turnover}
#'   \item{MAX}{turnover of the company which contributes the most to the cell.}
#' }
#' @seealso activity_corr_table
"turnover_act_cj"

#' data - Turnover broken down by NUTS and size of French companies (fake values).
#'
#' A tabular dataset containing the turnover broken down by NUTS geographical localisation
#' and Size of companies. Useful for playing with tab_ functions.
#'
#' @format A tibble/data frame with 460 rows and 5 variables:
#' \describe{
#'  \item{NUTS}{nuts - european denomination of administrative levels.
#'  Hierarchical variables with two levels (nuts2 and nuts3) described
#'   in the nuts23_fr_corr_table dataset. The root is noted "Total"}
#'   \item{SIZE}{size of the companies (Number of employees in three categories
#'   + overall category "Total")}
#'   \item{N_OBS}{Frequency, number of companies}
#'   \item{TOT}{turnover value in euros}
#'   \item{MAX}{turnover of the company which contributes the most to the cell.}
#' }
#' @seealso nuts23_fr_corr_table
"turnover_nuts_size"

#' data - Turnover broken down by NUTS and size of French companies (fake values).
#'
#' A tabular dataset containing the turnover broken down by NUTS geographical localisation
#' and Type of companies. Useful for playing with tab_ functions.
#'
#' @format A tibble/data frame with 452 rows and 5 variables:
#' \describe{
#'  \item{NUTS}{nuts - european denomination of administrative levels.
#'  Hierarchical variables with two levels (nuts2 and nuts3) described
#'   in the nuts23_fr_corr_table dataset. The root is noted "Total"}
#'   \item{CJ}{Type of companies (3 categories + overall category "Total")}
#'   \item{N_OBS}{Frequency, number of companies}
#'   \item{TOT}{turnover value in euros}
#'   \item{MAX}{turnover of the company which contributes the most to the cell.}
#' }
#' @seealso nuts23_fr_corr_table
"turnover_nuts_cj"

#' data - Correspondence table describing the business sectors hierarchy.
#'
#' A dataset describing the nesting of three levels of business sectors, useful
#' when working with the ACTIVITY variables in the turnover_ datasets.
#'
#' @format A data frame with 92 rows and 3 variables:
#' \describe{
#'   \item{A10}{business sectors in 10 categories}
#'   \item{A21}{business sectors in 21 categories}
#'   \item{A88}{business sectors in 88 categories}
#' }
#' @details Use the \code{write_hrc2} function to create a .hrc file from this
#' correspondence table.
"activity_corr_table"

#' data - Correspondence table describing the NUTS hierarchy.
#'
#' A dataset describing the nesting of NUTS2 and NUTS3 levels for Metropolitan France, useful
#' when working with the NUTS variables in the turnover_ datasets.
#'
#' @format A data frame with 92 rows and 3 variables:
#' \describe{
#'   \item{NUTS2}{NUTS2 levels in France - equivalent of French "R??gions"}
#'   \item{NUTS3}{NUTS3 levels in France - equivalent of French "D??partements"}
#' }
#' @details Use the \code{write_hrc2} function to create a .hrc file from this
#' correspondence table.
"nuts23_fr_corr_table"

#' data - Turnover broken down by business sector, NUTS, and size of French companies (fake values).
#'
#' A tabular dataset containing the turnover broken down by Business sector, NUTS
#' (administrative areas) and Size of companies. The data is restricted to
#' only three NUTS2 of France (codes FR41, FR42 and FR43) and their
#' corresponding NUTS3 areas. Useful for playing with tab_ functions.
#'
#' @format A tibble/data frame with 3 168 rows and 6 variables:
#' \describe{
#'  \item{ACTIVITY}{business sector, hierarchical variables with three levels described
#'   in the activity_corr_table dataset. The root is noted "Total"}
#'   \item{NUTS}{nuts - european denomination of administrative levels.
#'  Hierarchical variables with two levels (nuts2 and nuts3) described
#'   in the nuts23_fr_corr_table dataset. Only "FR41", "FR42" and "FR43" NUTS2
#'   areas and their corresponding NUTS3 areas are in the data.
#'   The root is noted "Total_EAST"}
#'   \item{SIZE}{size of the companies (Number of employees in three categories
#'   + overall category "Total")}
#'   \item{N_OBS}{Frequency, number of companies}
#'   \item{TOT}{turnover value in euros}
#'   \item{MAX}{turnover of the company which contributes the most to the cell.}
#' }
#' @seealso
#' activity_corr_table
#' nuts23_fr_corr_table
"turnover_act_nuts_size"
