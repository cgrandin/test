load.catches <- function(fn){
  ## Reads in the catches file, splits it up into two data frames,
  ##  one for catches and one for the catches/tac.
  ## fn - the filename with relative path
  ## In the catches data frame, any NA's will be replaced by zeroes.
  ## In the landings/tac table, they will remain NAs
  catches <- read.csv(fn)
  landings.vs.tac <-
    as.data.frame(cbind(Year = catches$Year,
                        Ustotal = catches$Ustotal,
                        CANtotal = catches$CANtotal,
                        TOTAL = catches$TOTAL,
                        TAC = catches$TAC,
                        TACCAN = catches$TACCAN,
                        TACUSA = catches$TACUSAXX))

  landings.vs.tac <-
    as.data.frame(cbind(landings.vs.tac,
                        landings.vs.tac$Ustotal / landings.vs.tac$TACUSA * 100,
                        landings.vs.tac$CANtotal / landings.vs.tac$TACCAN * 100,
                        landings.vs.tac$TOTAL / landings.vs.tac$TAC * 100))

  colnames(landings.vs.tac) <- c("Year",
                                 "Ustotal",
                                 "CANtotal",
                                 "TOTAL",
                                 "TAC",
                                 "TACCAN",
                                 "TACUSA",
                                 "USATTAIN",
                                 "CANATTAIN",
                                 "ATTAIN")
  catches <- catches[,!names(catches) %in% c("TAC", "TACCAN", "TACUSA")]
  catches[is.na(catches)] <- 0
  list(catches = catches,
       landings.vs.tac = landings.vs.tac)
}

make.catches.table2 <- function(catches,
                               start.yr,
                               end.yr,
                               xcaption = "default",
                               xlabel   = "default",
                               font.size = 9,
                               space.size = 10,
                               placement = "H"){
  ## Returns an xtable in the proper format for the executive summary catches
  ##
  ## catches - output of the load.catches function above.
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  if(start.yr > 1991){
    ## If start.yr > 1991 then US foreign, US JV, and Canadian foreign
    ##  will be removed since they are all zeroes.
    catches <- catches[,c("Year",
                          "atSea_US_MS",
                          "atSea_US_CP",
                          "US_shore",
                          "USresearch",
                          "Ustotal",
                          "CAN_JV",
                          "CAN_Shoreside",
                          "CAN_FreezeTrawl",
                          "CANtotal",
                          "TOTAL")]
    colnames(catches) <- c(latex.bold("Year"),
                           latex.mlc(c("US",
                                       "Mother-",
                                       "ship")),
                           latex.mlc(c("US",
                                       "Catcher-",
                                       "processor")),
                           latex.mlc(c("US",
                                       "Shore-",
                                       "based")),
                           latex.mlc(c("US",
                                       "Research")),
                           latex.mlc(c("US",
                                       "Total")),
                           latex.mlc(c("CAN",
                                       "Joint-",
                                       "Venture")),
                           latex.mlc(c("CAN",
                                       "Shore-",
                                       "side")),
                           latex.mlc(c("CAN",
                                       "Freezer",
                                       "Trawlers")),
                           latex.mlc(c("CAN",
                                       "Total")),
                           latex.bold("Total"))
  }else{
    colnames(catches) <- c("Year",
                           "US\nForeign",
                           "US\nJV",
                           "US\nMother-\nship",
                           "US\nCatcher-\nProcessor",
                           "US\nShore-\nbased",
                           "US\nResearch",
                           "US\nTotal",
                           "CAN\nForeign",
                           "CAN\nJoint-\nVenture",
                           "CAN\nShoreside",
                           "CAN\nFreezer-Trawler",
                           "CAN\nTotal",
                           "Total")
  }
  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  ## -1 below means leave the years alone and don't comma-seperate them
  catches[,-1] <-f(catches[-1])

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = placement,
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.catches.table <- function(catches,
                               start.yr,
                               end.yr,
                               xcaption = "default",
                               xlabel   = "default",
                               font.size = 9,
                               space.size = 10,
                               placement = "H"){
  ## Returns an xtable in the proper format for the executive summary catches
  ##
  ## catches - output of the load.catches function above.
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  if(start.yr > 1991){
    ## If start.yr > 1991 then US foreign, US JV, and Canadian foreign
    ##  will be removed since they are all zeroes.
    catches <- catches[,c("Year",
                          "atSea_US_MS",
                          "atSea_US_CP",
                          "US_shore",
                          "USresearch",
                          "Ustotal",
                          "CAN_JV",
                          "CAN_Shoreside",
                          "CAN_FreezeTrawl",
                          "CANtotal",
                          "TOTAL")]
    colnames(catches) <- c(latex.bold("Year"),
                           latex.mlc(c("US",
                                       "Mother-",
                                       "ship")),
                           latex.mlc(c("US",
                                       "Catcher-",
                                       "processor")),
                           latex.mlc(c("US",
                                       "Shore-",
                                       "based")),
                           latex.mlc(c("US",
                                       "Research")),
                           latex.mlc(c("US",
                                       "Total")),
                           latex.mlc(c("CAN",
                                       "Joint-",
                                       "Venture")),
                           latex.mlc(c("CAN",
                                       "Shore-",
                                       "side")),
                           latex.mlc(c("CAN",
                                       "Freezer",
                                       "Trawlers")),
                           latex.mlc(c("CAN",
                                       "Total")),
                           latex.bold("Total"))
  }else{
    colnames(catches) <- c("Year",
                           "US\nForeign",
                           "US\nJV",
                           "US\nMother-\nship",
                           "US\nCatcher-\nProcessor",
                           "US\nShore-\nbased",
                           "US\nResearch",
                           "US\nTotal",
                           "CAN\nForeign",
                           "CAN\nJoint-\nVenture",
                           "CAN\nShoreside",
                           "CAN\nFreezer-Trawler",
                           "CAN\nTotal",
                           "Total")
  }
  ## Filter for correct years to show and make thousand-seperated numbers
  ##  (year assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  ## -1 below means leave the years alone and don't comma-seperate them
  catches[,-1] <-f(catches[-1])

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(catches,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(catches))),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = placement,
        sanitize.text.function = function(x){x},
        size = size.string)
}
