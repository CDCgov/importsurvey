#' Import SAS data into R, with survey design variables the same as those in NSLTCP RCC SU 2018 PUF.
#'
#' @param sas_data  SAS survey data file
#' @param sas_formats_data    SAS formats data file (produced with `PROC FORMAT` with the `CNTLOUT` option)
#' @param r_out   name of a new R data file
#' @param label   name of the survey
#'
#' @return (Nothing.)
#' @export
#'
#' @examples
#' \dontrun{
#' import_sas_rccsu2018puf(sas_data = "final2018rcc_su_puf.sas7bdat"
#' , sas_formats_data = "suformats_dataset.sas7bdat"
#' , r_out = "rccsu2018puf.rds"
#' , label = "RCC SU 2018 PUF")
#' }
import_sas_rccsu2018puf = function(sas_data, sas_formats_data, r_out
                                 , label = "") {
  assert_that(!file.exists(r_out)
              , msg = paste0("Output file ", r_out, " already exists."))
  assert_that(is.string(label), nzchar(label))

  d1 = import_sas(sas_data, sas_formats_data, formats = "funcname"
                  , bool_levels = c("yes", "selected", "no"
                                    , "not selected", "missing")
                  , bool_true = c("yes", "selected")
                  , bool_false = c("no", "not selected")
                  , formats_func = function(name) toupper(paste0(name, "f"))
  )

  # Including popsu produces an error.
  sdo = svydesign(
    ids = ~ 1
    , strata = ~ pufstrata2 + su_facid
    , fpc = ~ pufpopfac2 # + popsu
    , weights = ~ suwt
    , data = d1)
  attr(sdo, "label") = label
  attr(sdo$call, "srcref") = NULL

  message("\n*** Please verify that the correct survey design variables are used (ids, strata, weights, fpc, etc.): ")
  print(sdo)

  saveRDS(sdo, r_out)
}
