% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_rtauargus.R
\name{tab_rtauargus2}
\alias{tab_rtauargus2}
\title{Wrapper of tab_rtauargus adapted for \code{tab_multi_manager} function.}
\usage{
tab_rtauargus2(
  tabular,
  files_name = NULL,
  dir_name = NULL,
  explanatory_vars,
  totcode,
  hrc = NULL,
  secret_var = NULL,
  secret_prim = NULL,
  cost_var = NULL,
  value = "value",
  freq = "freq",
  ip = 10,
  suppress = "MOD(1,5,1,0,0)",
  ...
)
}
\arguments{
\item{tabular}{[\strong{mandatory}]
data.frame which contains the tabulated data and
an additional boolean variable that indicates the primary secret of type boolean \cr
([\strong{obligatoire}] data.frame contenant les données tabulées et
une variable supplémentaire indiquant le secret primaire de type booléen.)}

\item{files_name}{string used to name all the files needed to process.
All files will have the same name, only their extension will be different.}

\item{dir_name}{string indicated the path of the directory in which to save
all the files (.rda, .hst, .txt, .arb, .csv) generated by the function.}

\item{explanatory_vars}{[\strong{mandatory}] Vector of explanatory variables \cr
[\strong{obligatoire}] Variables catégorielles, sous forme  de vecteurs \cr
Example : \code{c("A21", "TREFF", "REG")} for a table crossing
\code{A21} x \code{TREFF} x \code{REG}
(Variable indiquant le secret primaire de type booléen:
prend la valeur "TRUE" quand les cellules du tableau doivent être masquées
par le secret primaire, "FALSE" sinon. Permet de créer un fichier d'apriori)}

\item{totcode}{Code(s) which represent the total of a categorical variable
(see section 'Specific parameters' for this parameter's syntax).
If unspecified for a variable(neither by default nor explicitly)
it will be set to  \code{rtauargus.totcode}. \cr
(Code(s) pour le total d'une variable catégorielle (voir
section 'Specific parameters' pour la syntaxe de ce paramètre). Les
variables non spécifiées (ni par défaut, ni explicitement) se verront
attribuer la valeur de \code{rtauargus.totcode}.)}

\item{hrc}{Informations of hierarchical variables (see section
'Hierarchical variables'). \cr
(Informations sur les variables hiérarchiques (voir section
'Hierarchical variables').)
(Caractère qui, répété n fois, indique que la valeur est
à n niveaux de profondeur dans la hiérarchie.)}

\item{secret_var}{Boolean variable which specifies the secret, primary or not :
 equal to "TRUE" if a cell is concerned by the secret,"FALSE" otherwise.
will  be exported in the apriori file. \cr
(Variable indiquant le secret  de type booléen:
prend la valeur "TRUE" quand les cellules du tableau doivent être masquées
"FALSE" sinon. Permet de créer un fichier d'apriori)}

\item{secret_prim}{Boolean variable which gives the primary secret : equal to
"TRUE" if a cell is concerned by the primary secret,"FALSE" otherwise.
will  be exported in the apriori file \cr}

\item{cost_var}{Numeric variable allow to change the cost suppression of a cell
for secondary suppression, it's the value of the cell by default, can be
specified for each cell, fill with NA if the cost doesn't need to be changed
for all cells \cr
(Variable numeric qui permet de changer la coût de suppression d'une cellule,
pris en compte dans les algorithmes de secret secondaire.Par défaut le coût
correspond à la valeur de la cellule.  peut être spécifié pour chacune des cellules,
peut contenir des NA pour les coûts que l'on ne souhaite pas modifier.)
(nombre minimal de décimales à afficher (voir section 'Number of decimals').)}

\item{value}{Name of the column containing the value of the cells. \cr
(Nom de la colonne contenant la valeur des cellules)}

\item{freq}{Name of the column containing the cell frequency. \cr
(Nom de la colonne contenant les effectifs pour une cellule)}

\item{ip}{Interval Protection Level (10 by default)}

\item{suppress}{[\strong{mandatory}]
Algortihm for secondary suppression (Tau-Argus batch syntax), and the
parameters for it.\cr
([\strong{obligatoire}] Algorithme de gestion du secret secondaire
(syntaxe batch de Tau-Argus), ainsi que les potentiels paramètres associés)}

\item{...}{Other arguments of \code{tab_rtauargus} function}
}
\value{
The original tabular is returned with a new
column called Status, indicating the status of the cell coming from Tau-Argus :
"A" for a primary secret due to frequency rule, "B" for a primary secret due
to dominance rule, "D" for secondary secret and "V" for no secret cell.
}
\description{
Wrapper of tab_rtauargus adapted for \code{tab_multi_manager} function.
}
\examples{
\dontrun{
library(dplyr)
data(turnover_act_size)

# Prepare data with primary secret ----
turnover_act_size <- turnover_act_size \%>\%
  mutate(
    is_secret_freq = N_OBS > 0 & N_OBS < 3,
    is_secret_dom = ifelse(MAX == 0, FALSE, MAX/TOT>0.85),
    is_secret_prim = is_secret_freq | is_secret_dom
  )

# Make hrc file of business sectors ----
data(activity_corr_table)
hrc_file_activity <- activity_corr_table \%>\%
  write_hrc2(file_name = "hrc/activity")

# Compute the secondary secret ----
options(
  rtauargus.tauargus_exe =
    "Y:/Logiciels/TauArgus/TauArgus4.2.2b1/TauArgus.exe"
)

res <- tab_rtauargus2(
  tabular = turnover_act_size,
  files_name = "turn_act_size",
  dir_name = "tauargus_files",
  explanatory_vars = c("ACTIVITY", "SIZE"),
  hrc = c(ACTIVITY = hrc_file_activity),
  totcode = c(ACTIVITY = "Total", SIZE = "Total"),
  secret_var = "is_secret_prim",
  value = "TOT",
  freq = "N_OBS"
)
}
}
\seealso{
\code{tab_rtauargus}
}
