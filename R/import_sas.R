#' Import SAS data into R
#'
#' @param sas_data  SAS survey data file
#' @param sas_formats_data    SAS formats data file (produced with CNTLOUT option of PROC FORMAT)
#' @param formats how are formats specified?
#' @param bool_levels variables that have these levels should be converted from factor (categorical) to logical
#' @param bool_true level(s) of these variables that should be set
#' to `TRUE`
#' @param bool_false level(s) of these variables that should be set
#' to `FALSE`
#' @param formats_func only needed if `formats` = "funcname"
#' @param keep_unformatted vector of variable names. Create unformatted versions
#' of these variables.
#'
#' The argument `formats` determines how formats are specified:
#' * "attr": in `attr(*, "format.sas")`
#' * "name": same as the variable name
#' * "funcname": formats_func("x") is a function that returns the format name
#' for variable "x"
#'
#' Levels in `bool_levels` that are neither in `bool_true` or
#' `bool_false` are set to `NA`.
#'
#' @return `data.frame`. If this is a complex survey, use `svydesign` to
#' create a survey design object.
#' @export
#'
#' @examples
#' \dontrun{
#' d1 = import_sas(sas_data, sas_formats_data)
#' }
import_sas = function(sas_data, sas_formats_data, formats
              , bool_levels = c("yes", "no")
              , bool_true = "yes"
              , bool_false = "no"
              , formats_func = NULL
              , keep_unformatted = NULL
              ) {
  assert_that(formats %in% c("attr", "name", "funcname"))
  if (formats == "funcname") assert_that(is.function(formats_func))
  bool_levels %<>% tolower %>% trimws
  bool_true %<>% tolower %>% trimws
  bool_false %<>% tolower %>% trimws
  assert_that(all(bool_true %in% bool_levels)
              , all(bool_false %in% bool_levels))
  keep_unformatted %<>% tolower %>% trimws

  df1 = read_sas(sas_formats_data) %>% as.data.frame
  assert_that(all(df1$START == df1$END))
  df1 = df1[,c("FMTNAME", "START", "LABEL")]
  df1$START %<>% trimws
  df1$LABEL %<>% trimws
  assert_that( noNA(df1))

  d1 = read_sas(sas_data) %>% as.data.frame
  c.nofmt = c.2v = c.log = c.fewf = c.ku = c()
  for (ii in names(d1)) {
    fmt = switch(formats
                 , attr = attr(d1[,ii], "format.sas")
                 , name = ii
                 , funcname = formats_func(ii)
                 , stop("Unknown formats type."))

    if ( (formats == "attr" && is.null(fmt))
         || (formats %in% c("name", "funcname") && !(fmt %in% df1$FMTNAME) ) ) {
      c.nofmt %<>% c(ii)
      # d1[,ii] %<>% .c2f
      next
    }
    if (fmt %>% startsWith("$")) {
      fmt %<>% substring(2)
    }
    if (length(fmt) == 1L && nzchar(fmt)) {
      lbl = attr(d1[,ii], "label")
      idx = which(df1$FMTNAME == fmt)
      if (length(idx) > 0) {
        f1 = df1[idx,]
        if (inherits(d1[,ii], "difftime")) {
          d1[,ii] %<>% as.numeric
        }
        if (is.numeric(d1[,ii])) {
          f1$START %<>% as.numeric %>% suppressWarnings
        }

        if (isTRUE(tolower(ii) %in% keep_unformatted)) {
          vn = paste0(ii, ".unformatted")
          assert_that(!(vn %in% names(d1)))
          d1[,vn] = d1[,ii]
          attr(d1[,vn], "label") = paste(lbl, "(unformatted)")
          assert_that( noNA(d1[,vn]))
          c.ku %<>% c(ii)
        }

        if (is_subset(unique(d1[,ii]), f1$START)) {
          # Normal
          d1[,ii] %<>% factor(
            levels = f1$START
            , labels = f1$LABEL
            , exclude = NULL)
          attr(d1[,ii], "label") = lbl
          assert_that( noNA( d1[,ii] ))

          if ( nlevels(droplevels(d1[,ii])) / nrow(f1) <= 0.3 ) {
            c.fewf %<>% c(ii)
          }
        } else {
          # special values
          vn = paste0(ii, ".special")
          assert_that(!(vn %in% names(d1)))

          if (is.numeric(d1[,ii])) {
            val0 = max(d1[,ii]) + 1
            assert_that(is.finite(val0))
          } else {
            val0 = make.unique( c(unique(d1[,ii]), "XXX") ) %>% tail(1)
          }
          assert_that(!(val0 %in% d1[,ii]))
          tmp = d1[,ii]
          idx = which(!(tmp %in% f1$START))
          assert_that(length(idx) > 0)
          tmp[idx] = val0
          lbls = c(f1$LABEL, "[Other]") %>% make.unique

          d1[,vn] = factor(x = tmp
                       , levels = c(f1$START, val0)
                       , labels = lbls
                       , exclude = NULL)
          attr(d1[,vn], "label") = paste(lbl, "(defined levels)")
          assert_that( noNA(d1[,vn]))

          if ( nlevels(droplevels(d1[,vn])) / length(lbls) <= 0.3 ) {
            c.fewf %<>% c(vn)
          }

          # no special values
          vn = paste0(ii, ".nospecial")
          assert_that(!(vn %in% names(d1)))
          d1[,vn] = d1[,ii]
          d1[which(d1[,vn] %in% f1$START),vn] = NA
          d1[,vn] %<>% .c2f
          attr(d1[,vn], "label") = paste(lbl, "(NA = special values - use caution)")

          # raw
          d1[,ii] %<>% .c2f
          attr(d1[,ii], "label") = paste(lbl, "(raw - use caution)")
          assert_that(noNA( d1[,ii] ))

          c.2v %<>% c(ii)
        }
      } else {
        stop(ii, ": format specified but not defined")
      }
    } else {
      stop(ii, ": format specified incorrectly")
    }
  }

  for (ii in names(d1)) {
    if (is.factor(d1[,ii]) && nlevels(d1[,ii]) <= length(bool_levels)) {
      lvl = levels(d1[,ii]) %>% tolower %>% trimws
      if( is_subset(lvl, bool_levels) ) {
        c.log %<>% c(ii)
        lbl = attr(d1[,ii], "label")
        tmp = d1[,ii] %>% tolower %>% trimws
        newvr = rep(NA, nrow(d1))
        newvr[tmp %in% bool_true] = TRUE
        newvr[tmp %in% bool_false] = FALSE
        d1[,ii] = newvr
        attr(d1[,ii], "label") = lbl
      }
    }
  }

  if (length(c.2v) > 0) {
    tmp = c.2v %>% paste(collapse=", ")
    paste("\nFormat applies only to some values, created multiple version of variable:", tmp) %>% message
  }
  if (length(c.log) > 0) {
    tmp = c.log %>% paste(collapse=", ")
    paste("\nYes / no variable - converted to logical:", tmp) %>% message
  }
  if (length(c.ku) > 0) {
    tmp = c.ku %>% paste(collapse=", ")
    paste("\nCreated unformatted versions at your request:", tmp) %>% message
  }
  if (length(c.nofmt) > 0) {
    tmp = c.nofmt %>% paste(collapse=", ")
    paste("\nVariables that have no format:", tmp) %>% message
  }
  if (length(c.fewf) > 0) {
    tmp = c.fewf %>% paste(collapse=", ")
    paste("\nMany category values not used - possible error:", tmp) %>% message
  }

  assert_that(
    all(d1 %>% sapply(class) %in% c("factor", "logical", "numeric", "character"))
  )
  invisible(d1)
}

# https://stackoverflow.com/questions/37656853/how-to-check-if-set-a-is-subset-of-set-b-in-r
is_subset = function(A,B) {
  setequal(intersect(A,B), A)
}

.c2f = function(x) {
  if (is.character(x)) {
    x %<>% factor(exclude = NULL)
  }
  x
}
