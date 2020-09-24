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
  waterLevel = dplyr::case_when(
      role == "Water Carrier" ~ 5L,
      role == "Climber" ~ 3L, 
      role == "Archaeologist" ~ 3L, 
      TRUE ~ 4L
      ),
  maxWaterLevel = dplyr::case_when(
      role == "Water Carrier" ~ 5L,
      role == "Climber" ~ 3L, 
      role == "Archaeologist" ~ 3L, 
      TRUE ~ 4L
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
  attr(role, "maxWaterLevel") <- maxWaterLevel
  attr(role, "actionsRemaining") <- actionsRemaining
  attr(role, "digPower") <- digPower
  attr(role, "canClimb") <- canClimb
  
  return(role)
  
}

