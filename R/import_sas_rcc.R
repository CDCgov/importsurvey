#' Import SAS data into R, with survey design variables the same as those in NSLTCP RCC 2018 PUF.
#'
#' @param sas_data  SAS survey data file
#' @param sas_formats_data    SAS formats data file (produced with `PROC FORMAT` with the `CNTLOUT` option)
#' @param r_out   name of a new R data file
#' @param label   name of the survey
#'
#' @return (Nothing.)
#' @family import
#' @export
#'
#' @examples
#' \dontrun{
#' import_sas_rcc2018puf(sas_data = "rcc2018puf.sas7bdat"
#' , sas_formats_data = "rcc_formats_dataset.sas7bdat"
#' , r_out = "rcc_2018_puf.rds"
#' , label = "RCC 2018 PUF")
#' }
import_sas_rcc2018puf = function(sas_data, sas_formats_data, r_out
                                 , label = "") {
  assert_that(!file.exists(r_out)
              , msg = paste0("Output file ", r_out, " already exists."))
  assert_that(is.string(label), nzchar(label))

  d1 = import_sas(sas_data, sas_formats_data, formats = "name"
                  , bool_levels = c("yes", "no", "missing")
                  , bool_true = "yes"
                  , bool_false = "no"
                  )

  sdo = svydesign(
    ids = ~ 1
    , strata = ~ PUFSTRATA
    , fpc = ~ PUFPOPFAC
    , weights = ~ FACWT
    , data = d1)
  attr(sdo, "label") = label
  attr(sdo$call, "srcref") = NULL

  message("\n*** Please verify that the correct survey design variables are used (ids, strata, weights, fpc, etc.): ")
  print(sdo)

  saveRDS(sdo, r_out)
}
