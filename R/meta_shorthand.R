#' Meta shorthand
#' 
#' Add short descriptions to existing metadata
#' 
#' Dependencies: stringr, tidyr, dplyr
#' 
#' For info about H.8 balance sheet items, see:
#' H8 About page: https://www.federalreserve.gov/releases/h8/about.htm
#' FR2644 instructions: https://www.federalreserve.gov/apps/reportingforms/Report/Index/FR_2644
#' 
#' @param meta df; metadata produced in inst/convert_xml_to_dfs.R
#' 
#' @returns df; with short_desc additional field
meta_shorthand <- function(meta) {

  short <- meta %>%
    mutate(
      short_desc = dplyr::case_when(
        item == 1001 ~ "Bank credit",
        item == 1002 ~ "Total securities",
        item == 1003 ~ "Treasury and agency securities",
        item == 1301 ~ "Agency MBS",
        item == 1302 ~ "Treasury and agency non-MBS",
        item == 1011 ~ "Other securities",
        item == 1303 ~ "Other MBS",
        item == 1304 ~ "Other securities non-MBS",
        item == 1020 ~ "Total loans",
        item == 1023 ~ "C&I loans",
        item == 1026 ~ "Total real estate",
        item == 1221 ~ "Total residential real estate",
        item == 1027 ~ "HELOCs",
        item == 1220 ~ "Closed-end mortgages",
        item == 3219 ~ "Total CRE",
        item == 1215 ~ "CRE: Construction and land development",
        item == 1216 ~ "CRE: Farm",
        item == 1217 ~ "CRE: Multifamily",
        item == 1218 ~ "CRE: Nonfarm nonresidential",
        item == 1029 ~ "Total consumer",
        item == 1247 ~ "Credit cards",
        item == 3248 ~ "Consumer loans ex-credit cards",
        item == 1243 ~ "Auto loans",
        item == 2245 ~ "All other consumer loans",
        item == 3305 ~ "Other loans",
        item == 1030 ~ "NDFI loans",
        item == 2310 ~ "All other loans",
        item == 1043 ~ "Allowance for loan and lease losses",
        item == 1048 ~ "Cash",
        item == 3092 ~ "Fed funds and RRPs",
        item == 1047 ~ "Loans to banks",
        item == 3053 ~ "Other assets",
        item == 1151 ~ "Total assets",
        item == 1058 ~ "Total deposits",
        item == 1072 ~ "Large-time deposits",
        item == 1110 ~ "Other deposits",
        item == 3094 ~ "Borrowings",
        item == 1100 ~ "Net due to",
        item == 3095 ~ "Other liabilities",
        item == 1152 ~ "Total liabilities",
        item == 1091 ~ "Residual",
        TRUE ~ ""
      )
    )

  return(short)

}
