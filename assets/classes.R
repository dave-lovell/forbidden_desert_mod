## Classes

#### Card ####  (name, description, effect)

card <- function(name = character(),
                 description = character(),
                 effect = NULL)
  {
  attr(effect, "name") <- name
  attr(effect, "description") <- description
  
  return(effect)
}

#### Player ####

player <- function(
  role = factor(levels = c(
    "Water Carrier",
    "Explorer",
    "Meteorologist",
    "Archaeologist",
    "Navigator",
    "Climber"
  )),
  waterLevel = factor(
    dplyr::case_when(
      role == "Water Carrier" ~ 5,
      role == "Climber" ~ 3, 
      role == "Archaeologist" ~ 3, 
      TRUE ~ 4
      ),
    levels = -1:(dplyr::case_when(
      role == "Water Carrier" ~ 5,
      role == "Climber" ~ 3, 
      role == "Archaeologist" ~ 3, 
      TRUE ~ 4
      ))
    ), 
  actionsRemaining = 4L,
  digPower = dplyr::case_when(
    role == "Archaeologist" ~ 2L,
    TRUE ~ 1L
  ),
  canClimb = dplyr::case_when(
      role == "Climber" ~ TRUE,
      TRUE ~ FALSE
      )
  ){

  stopifnot(role %in% c("Water Carrier","Explorer",
                        "Meteorologist","Archaeologist",
                        "Navigator","Climber"))
  
  attr(role, "waterLevel") <- waterLevel
  attr(role, "actionsRemaining") <- actionsRemaining
  attr(role, "digPower") <- digPower
  attr(role, "canClimb") <- canClimb
  
  return(role)
  
}

