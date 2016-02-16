library(shiny)
library(shinydashboard)
library(ggplot2)
library(MASS)
library(grid)
library(pander)

# Define function for generating plot of correlated data sets:
cor.example <- function(sigmax = 5, sigmay = 10, rho = 0.9, n = 40, range = TRUE) {
  
  covmat <- matrix(c(sigmax^2, rho*sigmax*sigmay, rho*sigmax*sigmay, sigmay^2), ncol = 2)  
  set.seed(12)  
  n <- n
  day <- c(rep(1, n), rep(2, n))
  data <- mvrnorm(n, c(0, 0), covmat)
  
  data.frame(data)
  
} # End cor function.



# ANOVA

library(ggplot2)
library(reshape2)
library(plyr)

# Nice colours to use for the figures:
DarkBlue <- "#005AA5"
MidBlue <- "#7489c0"
LightBlue <- "#b3cce2"

# DarkRed <- "#C42126"
DarkRed <- "#c22227"
MidRed <- "#d67a64"
LightRed <- "#f8b3ae"

# DarkYellow <- "#F5B319"
DarkYellow <- "#f3b21a"
MidYellow <- "#f8cd7b"
LightYellow <- "#fbdfac"

DarkGreen <- "#00824A"
DarkPurple <- "#662D91" 
DarkOrange <- "#F38520"

col.y <- "#F5B319"
col.r <- "#C42126"



df.values <- dget("randomValues")

findValues <- function(n = 10, s = 6, u = 0) {
  df.values[[paste(n,s,sep="_")]] + u
}
