## Functions

#### General Functions ####

## Add new player -------

add_new_player <- function(activePlayers = list()){
  
  allRoles <- list(
    "Water Carrier" = player("Water Carrier"),
    "Explorer" = player("Explorer"),
    "Meteorologist" = player("Meteorologist"),
    "Archaeologist" = player("Archaeologist"),
    "Navigator" = player("Navigator"),
    "Climber" = player("Climber")
  )
  
  availableRoles <- allRoles[!(allRoles %in% activePlayers)]
  
  newActivePlayers <- c(activePlayers, sample(availableRoles, 1))
  
  return(newActivePlayers)
}

#### Player Functions ####

give_water <- function(player = NULL,
                       amount = 1){
  stopifnot(amount >= 0)
  
  attr(player, "waterLevel") -> waterLevel
  attr(player, "maxWaterLevel") -> maxWaterLevel
  
  attr(player, "waterLevel") <- min(
    waterLevel + amount,
    maxWaterLevel
  )
  return(player)
}

deplete_water <- function(player = NULL,
                       amount = 1){
  stopifnot(amount >= 0)
  
  attr(player, "waterLevel") -> waterLevel
  
  attr(player, "waterLevel") <- max(
    waterLevel - amount,
    -1
  )
  return(player)
}
