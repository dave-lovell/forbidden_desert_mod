### Cards ###

secret_water_reserve <- card(
  name = "Secret Water Reserve",
  description = "All players on your tile receive two water",
  effect <- function(player){
    attr(player, "location") -> playerTile
    
  }
)