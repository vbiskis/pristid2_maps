# Details ----
#' src_assign_protlev.R
#' Paper: [Putting sawfishes back on the map]
#' DOI: tbd
#' Author: Nikki Biskis
#' Date: 2 Mar 2024
#' - modified 2025-12-13
#' Content: 
#' + function to score protection level from zone (originally in src_fish.R)
#' -----------

assign_score <- function(row) {
  score <- 0
  
  # Fisheries
  if (!is.na(row$NoTrl) && row$NoTrl == TRUE) {
    score <- score + 3
  }
  
  if (!is.na(row$NoNet)) {
    if (row$NoNet == TRUE) {
      score <- score + 3
    } else if (row$NoNet == FALSE &&
               !is.na(row$NetProt) && row$NetProt <= row$Cap_Year &&
               !is.na(row$NFZ_Dist) && row$NFZ_Dist < 5 && row$NFZ_Dist >= 1) {
      score <- score + (4 - row$NFZ_Dist)
    }
  }
  
  # Coastal IUCN
  if (!is.na(row$IUCN)) {
    score <- score + switch(row$IUCN,
                            "I" = 10,
                            "II" = 9,
                            "IV" = 5,
                            "VI" = 0,
                            0
    )
  }
  
  # DPA
  if (!is.na(row$DPA) && row$DPA == TRUE) {
    score <- score + ifelse(row$DPATYPE == "A", 2, 1)
  }
  
  # FHA
  if (!is.na(row$FHA) && row$FHA == TRUE) {
    score <- score + ifelse(row$FHATYPE == "A", 2, 1)
  }
  
  # Proximity to IUCN areas
  if (!is.na(row$IIZone) && row$IIZone == FALSE &&
      !is.na(row$DistII) && !is.na(row$GAZII) &&
      row$GAZII <= row$Cap_Year &&
      row$DistII < 4 && row$DistII > 1) {
    score <- score + (4 - row$DistII)
  }
  
  if (!is.na(row$IVZone) && row$IVZone == FALSE &&
      !is.na(row$DistIV) && !is.na(row$GAZIV) &&
      row$GAZIV <= row$Cap_Year &&
      row$DistIV < 4 && row$DistIV > 2) {
    score <- score + (4 - row$DistIV) / 2
  }
  
  # Inland
  if (!is.na(row$InldProt) && row$InldProt == TRUE) {
    score <- score + switch(row$InldIUCN,
                            "II" = 9,
                            "III" = 8,
                            "IV" = 5,
                            0
    )
  }
  
  return(max(score, 0))
}