## Functions

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
