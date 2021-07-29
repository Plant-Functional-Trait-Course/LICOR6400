#' Calculate metrics for LI-COR 6400
#' @param licor licor object
#' @details All calculations are from the formulae in the excel file LI-COR6400 exports
#' @importFrom dplyr %>% select mutate if_else
#' @importFrom rlang .data
#' @examples
#' library("dplyr")
#' file <- system.file("0000-AAA8891-210718", package = "LICOR6400")
#' lic <- read_licor6400(file = file)
#' #add total leaf area - normally use a join
#' lic <- lic %>% mutate(total.leaf.area = 6)
#' calc_licor(licor = lic)
#' @export

# calculate extra leaf related metrics which scale from basic set of measurements
calc_licor <- function(licor){
  licor %>%
    select(-(.data$Photo:.data$CTleaf),-.data$BLC_1,-.data$BLCond) %>%
    mutate(
      BLC_1 = .data$Area * .data$BLCslope + .data$BLCoffst,
      fda = .data$Flow * 0.000001 / (.data$total.leaf.area * 0.0001),
      Photo = (.data$CO2R - .data$CO2S * (1000 - .data$H2OR) / (1000 - .data$H2OS)) * .data$fda,
      Trans = (.data$H2OS - .data$H2OR) / (1000 - .data$H2OS) * .data$fda,
      Tair_K = .data$Tleaf + 273.15,
      Twall_K = .data$Tair + 273.15,
      `R(W/m2)` = (.data$PARi * .data$f_parin + .data$PARo * .data$f_parout) * .data$alphaK,
      `Tl-Ta` = ((.data$`R(W/m2)` + 0.00000010773 * (.data$Twall_K ^ 4 - .data$Tair_K ^ 4)) -
                   .data$Trans * 44100) / (.data$BLC_1 * 51.4 + 0.00000043092 * .data$Tair_K ^ 3),
      CTleaf = .data$Tleaf + .data$`Tl-Ta` * .data$`EBal?`,
      SVTleaf = 0.61365 * exp(17.502 * .data$CTleaf / (240.97 + .data$CTleaf)),
      h2o_i = .data$SVTleaf * 1000 / .data$Press,
      h2odiff = .data$h2o_i - .data$H2OS,
      CTair = if_else(.data$`EBal?` != 0,
                      true = .data$Tleaf,
                      false = (.data$Tair + .data$Tleaf) / 2),
      SVTair = 0.61365 * exp(17.502 * .data$CTair / (240.97 + .data$CTair)),
      CndTotal = if_else(.data$h2odiff != 0,
                         true = (1000 - (.data$h2o_i + .data$H2OS) / 2) / .data$h2odiff * .data$Trans,
                         false = 0),
      vp_kPa = .data$H2OS * .data$Press / 1000,
      VpdA = .data$SVTair - .data$vp_kPa,
      BLCond = .data$BLC_1 * (.data$StmRat + 1) * (.data$StmRat + 1) /
        (.data$StmRat * .data$StmRat + 1),
      Cond = if_else(.data$CndTotal != 0,
                     true = 1 / (1 / .data$CndTotal - 1 / .data$BLCond),
                     false = 0),
      CndCO2 = 1 / (1.6 / .data$Cond + 1.37 / .data$BLCond),
      Ci = ((.data$CndCO2 - .data$Trans / 2) * .data$CO2S - .data$Photo) /
        (.data$CndCO2 + .data$Trans / 2),
      Ci_Pa = .data$Ci * .data$Press * 0.001,
      `Ci/Ca` = .data$Ci / .data$CO2S,
      RHsfc = (1 - .data$Trans * .data$Press / .data$SVTleaf / .data$Cond) * 100,
      C2sfc = .data$CO2S - .data$Photo / (.data$BLCond / 1.35),
      `AHs/Cs` = .data$Photo * .data$RHsfc / 100 / .data$C2sfc,
      Trmmol = .data$Trans * 1000,
      VpdL = .data$SVTleaf - .data$vp_kPa
    )
}
