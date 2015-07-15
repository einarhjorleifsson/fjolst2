#' fjolstOracle
#'
#' @name fjolstOracle
#' @docType data
#' @format Boolean, default FALSE
"fjolstOracle"

#' List of prey
#'
#' List of prey in Icelandic, english and latin
#'
#' @name allir.hopar
#' @docType data
NULL

#' List of all food items in the faeda database
#'
#' Name, icelandic name, group (fish, etc), english name and species code for
#' each group.
#'
#' @name faeduhopar
#' @docType data
NULL

#' Selected columns from the table f_flokkar.
#'
#' @name flokkar.col
#' @docType data
#' @format The format is: chr [1:12] "synis.id.id" "flokk.id" "ranfiskur"
#' "lenfl" ...
NULL

#' Names of columns in faeda.f_hopar
#'
#' @name hopar.col
#' @docType data
#' @format The format is: chr [1:5] "flokk.id" "faeduhopur" "fjoldi" "thyngd"
#' "melt"
NULL

#' Name of columns in the oracle tables fiskar.kvarnir.
#'
#' @name kvarnir.col.oracle
#' @docType data
#' @format The format is: chr [1:23] "synis_id" "tegund" "nr" "lengd" "kyn"
#' "kynthroski" ...
NULL

#' Splus or R correspondence of kvarnir.col.oracle.  "_ "replaced by "." .
#'
#' @name kvarnir.col
#' @docType data
#' @format The format is: chr [1:23] "synis.id" "tegund" "nr" "lengd" "kyn"
#' "kynthroski" ...
#'
NULL

#' Name of columns in the oracle tables fiskar.lengdir
#'
#' @name lengdir.col.oracle
#' @docType data
#' @format The format is: chr [1:6] "synis_id" "tegund" "lengd" "fjoldi" "kyn"
#' ...
#'
NULL

#' Splus or R correspondence of lengdir.col.oracle.  "_ "replaced by "." .
#'
#' @name lengdir.col
#' @docType data
#' @format The format is: chr [1:6] "synis.id" "tegund" "lengd" "fjoldi" "kyn"
#' ...
NULL

#' Columns in the table fiskar.numer in oracle.
#'
#' @name numer.col.oracle
#' @docType data
#' @format The format is: chr [1:16] "synis_id" "tegund" "fj_maelt"
#' "fj_kvarnad" ...
NULL

#' Columns in the dataframe all.nu.
#'
#' @name numer.col
#' @docType data
#' @format The format is: chr [1:16] "synis.id" "tegund" "fj.maelt"
#' "fj.kvarnad" ...
#'
NULL

#' Lengthgroup division in the bulked stomachs.
#'
#' @name old.stomach.breaks
#' @docType data
#'
NULL

#' Definition of the Bormicon regions.
#'
#' @name reg.bc
#' @docType data
#' @format The format is: List of ...
#'
NULL

#' Column name in stations file in oracle.
#'
#' @name stodvar.col.oracle
#' @docType data
#' @format The format is: chr [1:48] "fiskar.stodvar.synis_id" "leidangur"
#' "skip" "stod" ...
#'
NULL

#' Column name in stations frame.
#'
#' @name stodvar.col
#' @docType data
#' @format The format is: chr [1:56] "synis.id" "leidangur" "ar" "man" "dags"
#'
NULL

#' Column names in fiskar.stodvar with "_" replaced by ".".
#'
#' @name stodvar.col.splus
#' @docType data
#' @format The format is: chr [1:48] "synis.id" "leidangur" "skip" "stod" ...
#'
NULL

#' Subset of culumns in stationsfile.
#'
#' @name stodvar.std.col.1
#' @docType data
#' @format The format is: chr [1:25] "synis.id" "leidangur" "skip" "stod" ...
#'
NULL

#' Subset of columns in the stationfile.
#'
#' @name stodvar.std.col
#' @docType data
#' @format The format is: chr [1:25] "synis.id" "leidangur" "skip" "stod" ...
#'
NULL

#' Subset of columns in the stationfile.
#'
#' @name stodvar.std
#' @docType data
#' @format The format is: chr [1:27] "synis.id" "leidangur" "skip" "stod" ...
#'
NULL
