### Cards ###

secret_water_reserve <- card(
  name = "Secret Water Reserve",
  description = "All players on your tile receive two water",
  effect <- function(player){
    attr(player, "location") -> cardPlayerTile
    
    allPlayers <<- lapply(allPlayers,
                             function(thisPlayer) {
                               if(identical(
                                 attr(thisPlayer, "location"),
                                 cardPlayerTile)) {
                                 thisPlayer <- give_water(thisPlayer, 2)
                               }
                               return(thisPlayer)
                             })
    }
)
