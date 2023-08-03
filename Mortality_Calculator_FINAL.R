####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico ########
#############################Candidate Number: 1071324##########################

#Used to calculate the delta probability of mortality when patient requirements/needs do not match patient treatment
#This function is sourced in Triage_Model_FINAL.R

mortality_calculator <- function(
    req_dest_record, #"itb" = intubated patients, "icu" = intensive care unit, and "dest" = patient's destination in the hospital
    delta_matrix,    #All matrices are written out in Matrices_and_Plotting_FINAL.R
    to_test = FALSE
) {
  
  total_delta <- 0
  # rr <- 0
  
  for (ii in seq_along(req_dest_record[, 1])) {
    this_delta <- 0
    rr <- -1
    if (req_dest_record[ii, "itb"] == "YES" && req_dest_record[ii, "icu"] == "YES") {
      rr <- 1
    } else if (req_dest_record[ii, "itb"] == "YES" && req_dest_record[ii, "icu"] == "NO") {
      rr <- 2
    } else if (req_dest_record[ii, "itb"] == "NO" && req_dest_record[ii, "icu"] == "YES") {
      rr <- 3
    } else if (req_dest_record[ii, "itb"] == "NO" && req_dest_record[ii, "icu"] == "NO") {
      rr <- 4
    } else {
  
    }
    if (rr > 0) {
      this_delta <- delta_matrix[rr, req_dest_record[ii, "dest"]]
    }
    
    if (to_test) {
      print(this_delta)
      if (is.na(this_delta) || is.null(this_delta)) {
        print(req_dest_record[ii, ])
      }
    }
    total_delta <- total_delta + this_delta
    
  }
  return(total_delta)
}

if (FALSE) {
  
  delta_matrix_test <- data.frame( #Used to calculate the delta probability of mortality when patient needs/requirements do not match patient treatment
    "ICU"         = c(0,       0,   NA,   NA),
    "H_Vent"      = c(0.15, 0.15,   NA,   NA),
    "ICU_no_vent" = c(0.2,   0.2,    0,   NA),
    "H"           = c(0.25, 0.25, 0.15,    0),
    "REJ"         = c(0.3,   0.3,  0.2,  0.15)
  )
  
  df <- data.frame( #Testing the Matrix Output
    "itb" = c("YES", "NO" , "YES", "NO", "NO",  "NA" ),
    "icu" = c("YES", "YES",  "NO", "NO", "NO",  "YES"),
    "dest" = c("H" ,   "H",   "H",  "H", "REJ", "H")
  )
  
  print(mortality_calculator(
    df, delta_matrix_test, to_test = TRUE
  ))
  
  
}