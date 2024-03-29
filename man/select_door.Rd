% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roxygen comments.R
\name{select_door}
\alias{select_door}
\title{Select a door in the game.}
\usage{
select_door()
}
\arguments{
\item{...}{no arguments are used by the function.}
}
\value{
This function returns a number between 1 and 3.
}
\description{
\code{select_door()} picks one door out of the three created in the game.
}
\details{
The player randomly selects a number between 1 and 3 to represent one of the three
doors at the beginning of the game without knowing what is behind it.
}
\examples{
select_door()

}
