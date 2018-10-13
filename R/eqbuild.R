#' build a formula for multilevel/mixed model based on lists of variables
#'    for fixed and random effects
#'
#' \code{eq_mixed} build a formula for multilevel/mixed model
#'
#' @param dv string containing the dependent variable name
#' @param fev vector of strings containing variable names for fixed effects
#' @param rec list of strings containing factor variable names for random effects.
#'    Each list entry sould be a single string containing factor variables
#'    combined using the standard operators +,:,*,^ and /
#' @param rev list of strings containing variable names for random effects.
#'    Each list entry sould be a string vector containing variables
#'    combined using the standard operators +,:,* and ^
#' @param correlated list of logicals - if TRUE the effects of re variables
#'    are modeled as correlated.  FALSE is equivalent to the operator ||
#' @param addcons logical, if TRUE add a constant term to the fixed effects.
#'    Overrides 0 and -1 in fev
#' @return string
#' @export
eq_mixed <- function(dv,fev,rev,rec,correlated=TRUE,addcons=FALSE) {
  #  make sure fev has 1 or 0
  #  addcons only adds for fe, assume re specified okay
  #  addcons overrides -1 or 0 in fev
  if ((("0" %in% fev) | ("-1" %in% fev)) & !addcons) {
    fev <- setdiff(fev,c("1","-1"))
    fev <- union(fev,"0")
  } else {
    fev <- union(fev,"1")
    fev <- setdiff(fev,c("-1","0"))
  }
  stats::as.formula(paste0(dv," ~ ",paste0(fev,collapse=" + "),
                           re_terms(rec,rev,correlated)))
}
#' build a formula for fixed effect model corresponding to lists of variables
#'    for fixed and random effects
#'
#' \code{eq_linear} build a formula for simple model
#'
#' @param dv string containing the dependent variable name
#' @param fev vector of strings containing variable names for fixed effects
#' @param rev list of strings containing variable names for random effects.
#'    Each list entry sould be a string vector containing variables
#'    combined using the standard operators +,:,* and ^
#' @param morevars list of variables to include in model
#' @param addcons logical, if TRUE add a constant term to the fixed effects.
#'    Overrides 0 and -1 in fev
#' @return string
#' @export
eq_linear <- function(dv,fev,rev=NULL,morevars=NULL,addcons=TRUE) {
  #  make sure fev has 1 or 0
  rev <- unique(unlist(rev))
  if ((("0" %in% fev) | ("-1" %in% fev)) & !addcons) {
    fev <- setdiff(fev,c("1","-1"))
    fev <- union(fev,"0")
    rev <- setdiff(rev,c("-1","0","1"))
  } else {
    fev <- union(fev,"1")
    fev <- setdiff(fev,c("-1","0"))
    rev <- setdiff(rev,c("-1","0","1"))
  }
  stats::as.formula(paste0(dv," ~ ",
                           paste0(unique(c(rev,fev,morevars)),collapse=" + ")))
}

re_terms <- function(rec,rev="1",correlated=TRUE) {
  if (is.list(rec)) {
    if (!is.list(rev) & length(rev)==1) rev <- as.list(rep(rev,length(rec)))
    if (length(rec) != length(rev)) {
      stop("re_terms - lists rev and rec must be same length")
    }
    if (length(correlated)==1)
      correlated <- as.list(rep(correlated,length(rec)))
    rstring <- ""
    for (i in seq_along(rec)) {
      rstring <- c(rstring,
                   re_term(rec[[i]],rev[[i]],correlated[[i]]))
    }
  } else {
    rstring <- re_term(rec,rev,correlated)
  }
  paste0(unique(rstring),collapse="")
}
re_term <- function(rec,rev="1",correlated=TRUE) {
  # if (length(rec) > 1) stop ("re category list entry must not be a vector or list")
  #  make sure rev has 1 or 0
  if (("0" %in% rev) | ("-1" %in% rev)) {
    rev <- setdiff(rev,c("1","-1"))
    rev <- union(rev,"0")
  } else {
    rev <- union(rev,"1")
  }
  if (stringr::str_detect(paste0(unlist(rec),collapse=""),"/")) {
    temp <- gsub(" ","",rec)
    slashloc <- c(unlist(gregexpr(pattern ="/",temp)),stringr::str_length(temp)+1)
    temp <-  stringr::str_sub(rep(temp,length(slashloc)),1,slashloc[1:length(slashloc)]-1)
    rec <- gsub("/"," : ",temp)
  }
  rstring <- ""
  for (catvar in rec) {
    if (correlated) {
      rstring <- paste0(rstring," + ")
      rstring <-  paste0(rstring,"(",paste0(rev,collapse=" + "),
                         " | ",
                         catvar,
                         " )")
    } else {
      for (v in rev) {
        if  (v == "1") {
          rstring <- paste0(rstring,
                            " + ( 1 | ",catvar," )")
        } else if (v != "0") {
          rstring <- paste0(rstring,
                            " + ( ",v," + 0 | ",catvar," )")
        }
      }
    }
  }
  rstring
}

