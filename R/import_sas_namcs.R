#' Import SAS data into R, with survey design variables the same as those in NAMCS 2019 PUF.
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
#' import_sas_namcs2019puf(sas_data = "namcs2019_sas.sas7bdat"
#' , sas_formats_data = "namcs_formats_dataset.sas7bdat"
#' , r_out = "namcs_2019_puf.rds"
#' , label = "NAMCS 2019 PUF")
#'
#' import_sas_namcs2019puf(sas_data = "ed2020_sas.sas7bdat"
#' , sas_formats_data = "ed_formats_dataset.sas7bdat"
#' , r_out = "ed_2020_puf.rds"
#' , label = "ED 2020 PUF")
#' }
import_sas_namcs2019puf = function(sas_data, sas_formats_data, r_out,
                                   label = "") {
  assert_that(!file.exists(r_out)
              , msg = paste0("Output file ", r_out, " already exists."))
  assert_that(is.string(label), nzchar(label))

  options(prettysurvey.import.bool_levels = c("yes", "no")
          , prettysurvey.import.bool_true = "yes"
          , prettysurvey.import.bool_false = "no")
  d1 = import_sas(sas_data, sas_formats_data, formats = "attr")

  sdo = svydesign(ids = ~ CPSUM
                  , strata = ~ CSTRATM
                  , weights = ~ PATWT
                  , data = d1)
  attr(sdo, "label") = label

  message("\n*** Please verify that the correct survey design variables are used (ids, strata, weights): ")
  print(sdo)

  saveRDS(sdo, r_out)
}
