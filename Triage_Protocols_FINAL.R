####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico ########
#############################Candidate Number: 1071324##########################

#Functions in this script are sourced in Triage_Model_FINAL.R

## Patient destinations include: ICU, H_Vent, ICU_no_vent, H, REJ
# ICU = intensive care unit (w/ventilator available)
# H_Vent = general ward bed with ventilator
# ICU_no_vent = ICU no ventilation
# H = general ward
# REJ = rejected

triage_prob_healthy <- function(
    prob_die,
    th = 0.5 #Mortality probability threshold from Random Forest
) {
  return(prob_die < th)
}
triage_prob_severe <- function(
    prob_die,
    th = 0.5 #Mortality probability threshold from Random Forest
) {
  return(prob_die > th)
}

###Prioritising Healthy Patients Protocol
#Priotising patients with a mortality probability less than 50% (or threshold)
#Once occupancy threshold is met patients are prioritised given the output of the random forest
protocol_prob_die_hs <- function(
    op_dd_H, op_dd_ICU, op_dd_H_Vent,
    Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
    p_admitted, prob_die,
    #Occupancy Threshold
    th_ICU,
    th_H_Vent,
    th_H,
    #Probability Function
    triage_prob_func, # triage_prob_healthy or triage_prob_severe
    triage_prob_th
) {
  
  dest <- "NA"
  
  # out <- predict(rf, p_admitted, type = "prob")
  # prob_die <- out[1]
  # prob_die <- 0.5
  
  triage_accept <- triage_prob_func(prob_die = prob_die, th = triage_prob_th)
  
  if (p_admitted["Intubated"] == "YES") {
    if (op_dd_ICU / Total_ICU_Beds < th_ICU) {
      dest <- "ICU"
    } else {
      if (op_dd_ICU < Total_ICU_Beds && triage_accept) {
          dest <- "ICU"
      } else {
        dest <- "H_Vent"
      }
    }
  } else if (p_admitted["ICU"] == "YES") {
    if (op_dd_ICU / Total_ICU_Beds < th_ICU) {
      dest <- "ICU_no_vent"
    } else {
      if (op_dd_ICU < Total_ICU_Beds && triage_accept) {
          dest <- "ICU_no_vent"
        } else {
          dest <- "H"
        }
    }
  } else {
    dest <- "H"
  }
  
  if (dest == "H_Vent") {
    if (op_dd_H_Vent / Total_H_Vent < th_H_Vent) {
      dest <- "H_Vent"
    } else {
      if (triage_accept) {
        dest <- "H_Vent"
      } else {
        dest <- "H"
      }
    }
  }
  
  if (dest == "H_Vent" || dest == "H") {
    if (op_dd_H < Total_H_Beds) {
      #No modification to destination
    } else {
      dest <- "REJ"
    }
  }
  
  return(dest)
}

  
  
###Prioritising Severe Patients Protocol
#Prioritising patients with a mortality probability more than 50% (or threshold)
#Once occupancy threshold is met patients are prioritised given the output of the random forest
protocol_severe <- function(
    op_dd_H, op_dd_ICU, op_dd_H_Vent,
    Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
    p_admitted, rf
) {
  
  dest <- "NA"
  
  out <- predict(rf, p_admitted, type = "prob")
  prob_die <- out[1]
  
  if (p_admitted["Intubated"] == "YES") {
    if (op_dd_H_ICU / Total_ICU_Beds < th_ICU) {
      dest <- "ICU"
    } else if (op_dd_H_ICU / Total_ICU_Beds < Total_ICU_Beds && rf > 0.70) {
      dest <- "ICU"
    } else if (op_dd_H_Vent / Total_H_Vent < th_H_Vent) {
      dest <- "H_Vent"
    } else if (op_dd_H_Vent / Total_H_Vent < Total_H_Vent && rf > 0.70) {
      dest <- "H_Vent"
    } else if (op_dd_H / Total_H_Beds < th_H) {
      dest <- "H"
    } else if (op_dd_H / Total_H_Beds < Total_H_Beds && rf > 0.70) {
      dest <- "H"
    } else {
      dest <- "REJ"
    }
  } else {
    if (p_admitted["ICU"] == "YES") {
      if (op_dd_H_ICU / Total_ICU_Beds < th_ICU) {
        dest <- "ICU"
      } else if (op_dd_H_ICU / Total_ICU_Beds < Total_ICU_Beds && rf > 0.70) {
        dest <- "ICU"
      } else if (op_dd_H / Total_H_Beds < th_H) {
        dest <- "H"
      } else if (op_dd_H / Total_H_Beds < Total_H_Beds && rf > 0.70) {
        dest <- "H"
      } else {
        dest <- "REJ"
      }
    } else {
      if (op_dd_H / Total_H_Beds < th_H) {
        dest <- "H"
      } else if (op_dd_H / Total_H_Beds < Total_H_Beds && rf > 0.70) {
        dest <- "H"
      } else {
        dest <- "REJ"
      }
    }
  }
  
  return(dest)
}

####Baseline Protocol 
#Patients who need an ICU and/or ventilation are considered equal
#Patients are all given equal opportunity to receive critical resources
protocol_base <- function(
    op_dd_H, op_dd_ICU, op_dd_H_Vent,
    Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
    p_admitted
) {
  
  dest <- "NA"
  
  if (p_admitted["Intubated"] == "YES") {
    if (Total_ICU_Beds > op_dd_ICU) {
      dest <- "ICU"
    } else if (Total_H_Vent > op_dd_H_Vent && Total_H_Beds > op_dd_H) {
      dest <- "H_Vent"
    } else if (Total_H_Beds > op_dd_H) {
      dest <- "H"
    } else {
      dest <- "REJ"  
    }
  } else if (p_admitted["ICU"] == "YES") {
    if (Total_ICU_Beds > op_dd_ICU) {
      dest <- "ICU_no_vent"
    } else if (Total_H_Beds > op_dd_H) {
      dest <- "H"
    } else {
      dest <- "REJ"
    }
  } else {
    if (Total_H_Beds > op_dd_H) {
      dest <- "H"
    } else {
      dest <- "REJ"
    }
  }
  
  return(dest)
}

#Example where all resources are kept at less than 70% (or threshold) occupancy
protocol_b <- function(
    op_dd_H, op_dd_ICU, op_dd_H_Vent,
    Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
    p_admitted, rf,
    th_ICU    = 0.7,
    th_H_Vent = 0.7,
    th_H      = 0.7
) {
  dest <- "NA" # ICU, H, H_Vent, REJ
  
  out <- predict(rf, p_admitted, type = "prob")
  prob_die <- out[1]
  
  
  if (p_admitted["Intubated"] == "YES") {
    if (op_dd_H_ICU / Total_ICU_Beds < th_ICU) {
      dest <- "ICU"
    } else if (op_dd_H_Vent / Total_H_Vent < th_H_Vent) {
      dest <- "H_Vent"
    } else if (op_dd_H / Total_H_Beds < th_H) {
      dest <- "H"
    } else {
      dest <- "REJ"
    }
  } else {
    if (p_admitted["ICU"] == "YES") {
      if (op_dd_H_ICU / Total_ICU_Beds < th_ICU) {
        dest <- "ICU"
      } else if (op_dd_H / Total_H_Beds < th_H) {
        dest <- "H"
      } else {
        dest <- "REJ"
      }
    } else {
      if (op_dd_H / Total_H_Beds < th_H) {
        dest <- "H"
      } else {
        dest <- "REJ"
      }
    }
  }
  
  return(dest)
}

#Proof-of-Concept Workflow
protocol_a <- function(
    op_dd_H, op_dd_ICU, op_dd_H_Vent,
    Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
    p_admitted, rf
) {
  
  dest <- "NA" # ICU, H, H_VENT, REJ
  
  out <- predict(rf, p_admitted, type = "prob")
  prob_die <- out[1]
  
  
  p_mod <- p_admitted
  p_mod[1, "Intubated"] <- "NO"
  p_mod[1, "ICU"] <- "NO"
  out_nn <- predict(rf, p_mod, type = "prob")
  
  p_mod[1, "Intubated"] <- "YES"
  p_mod[1, "ICU"] <- "NO"
  out_yn <- predict(rf, p_mod, type = "prob")

  p_mod[1, "Intubated"] <- "NO"
  p_mod[1, "ICU"] <- "YES"
  out_ny <- predict(rf, p_mod, type = "prob")
  
  p_mod[1, "Intubated"] <- "YES"
  p_mod[1, "ICU"] <- "YES"
  out_yy <- predict(rf, p_mod, type = "prob")
  

  print(paste0(
    "nn:", out_nn[1],
    ", yn:", out_yn[1],
    ", ny:", out_ny[1],
    ", yy:", out_yy[1]
  ))
  
  
  return(dest)
}