#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   There are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat. The contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. This simulation will 
#'   test which strategy is better to stay or switch.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#' Select a door in the game.
#'
#' @description
#' `select_door()` picks one door out of the three created in the game.
#'
#' @details
#'   The player randomly selects a number between 1 and 3 to represent one of the three
#'   doors at the beginning of the game without knowing what is behind it.
#'
#' @param ... no arguments are used by the function.
#'
#' @return This function returns a number between 1 and 3.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' A goat door is opened.
#'
#' @description
#'   `open_goat_door()` picks a second door that has a goat behind it
#'   and is not the door the contestant selected `select_door()`
#'   function.
#'
#' @details
#'   This function reveals a goat for the contestant. A door that is not the 
#'   car and not the participants door is opened.
#'
#' @param
#'   The parameter game is a vector that factors in the current game.
#'   The parameter a.pick is an integer that represents the door number that
#'   the player picked first.
#'
#' @return
#'   This function returns a number between 1 and 3 which coincides with
#'   a goat behind the door.
#'
#' @examples
#' open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' Strategy stay or switch
#'
#' @description
#'   `change_door()` picks either the initial door identified during `select_door()`
#'   or the third remaining door.
#'
#' @details
#'   After the game player makes the initial door pick, and the game host opens a
#'   second door revealing a goat, the player must decide whether to stay with their
#'   original pick or choose the other remaining door.
#'
#' @param
#'   The parameter stay is a logical vector which can be true or false. It represents
#'   whether the player keeps their original pick or switches to the other door.
#'   The parameter opened.door is the number of the door that the host opened to
#'   reveal the goat.
#'   The parameter a.pick is an integer that represents the door number that
#'   the player picked first.
#'
#' @return
#'   This function returns a number between 1 and 3 which coincides with
#'   a stay or switch selection.
#'
#' @examples
#' change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#' Win or Lose Determination
#'
#' @description
#' `determine_winner()` tells us if the contestant won the car or lost the game.
#'
#' @details
#'   This step determines whether the player has won or lost based on whether
#'   the player chose to stay with their original pick or switch to the last
#'   remaining door. If a car is behind the final chosen door, the player won. If
#'   there is a goat behind the final chosen door, the player lost the game.
#'
#' @param
#'   The parameter final.pick is an integer 1, 2, or 3, that is the final door
#'   number choice the player chooses at the end.
#'   The parameter game is a vector that factors in the current game.
#'
#' @return This function returns a WIN or LOSE determination.
#'
#' @examples
#' determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#' An entire round of Monty Hall problem game
#'
#' @description
#'   `play_game()` runs through one whole round of the door game, executing each
#'   step in order.
#'
#' @details
#'   A game is created where there are three doors; one with a car behind it and
#'   two with goats behind them. The player selects a door number between 1 and 3.
#'   The game host then opens a second door with a goat behind it. The player
#'   must then decide to stay with their original pick or switch their pick to the
#'   remaining door. Whether the player switches or stays, if the door number they
#'   ultimately choose has a car behind it, they won. If it has a goat behind it,
#'   they lose.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'   This function returns a win or lose determination at the end of the
#'   game.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play 100 full games to see the probability of winning
#'
#' @description
#'   `play_n_games()` runs through 100 iterations of the full game to determine
#'   the probability of winning if the player stays or switches.
#'
#' @details
#'   This function adds a loop to the game, playing it "n" number of times to tell
#'   us the average proportion of wins the player can achieve with each strategy
#'   (stay or switch).
#'
#' @param The parameter n=100 represents the number of times the game will be ran
#' to determine the probabilities of winnign and losing.
#'
#' @return
#'   This function returns a table showing the proportion of wins and loses
#'   if the player stays with the original pick or switches to the remaining door.
#'
#' @examples
#' play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()
  
  return( results.df )
  
}