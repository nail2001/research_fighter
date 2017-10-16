library(gtools)
library(ggplot2)

renderResult <- function(output, P1, P2, pointsCount) {
  P1Points <- sum(pointsCount[, 1])
  P2Points <- sum(pointsCount[, 2])
  if(P1Points > P2Points) {
    output$result <- renderUI({ HTML(paste(
      "<center><h3>The winner is...</h3><h1><font color='#ff8c00'>",
      P1$name, 
      "</font></h1><h3>(", 
      P1Points, 
      " points)</h3>", 
      sep="")) })
  } else if(P1Points < P2Points) {
    output$result <- renderUI({ HTML(paste(
      "<center><h3>The winner is...</h3><h1><font color='#0073ff'>",
      P2$name, 
      "</font></h1><h3>(", 
      P2Points, 
      " points)</h3>", 
      sep="")) })
  } else {
    output$result <- renderUI({ HTML(paste("<center><h1>DRAW!</h1><br>", sep="")) })
  }
    
  
}

rendertotalCitesAndIndexResultsTab <- function(output, P1, P2) {
  rA <- c(P1$total_cites, P1$h_index, P1$i10_index)
  rB <- c(P2$total_cites, P2$h_index, P2$i10_index)
  
  vs <- cbind(rA, rB, c(rep("X", length(rA))))
  
  vs[rA > rB, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins!</font></b>")
  vs[rA >= rB*2, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(X2)</font></b>")
  vs[rA >= rB*3, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(X3)</font></b>")
  vs[rA >= rB*5, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(PERFECT!)</font></b>")
  vs[rA >= rB*10, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(FATALITY!)</font></b>")
  vs[rA < rB, 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins!</font></b>")
  vs[rA*2 <= rB, 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(X2)</font></b>")
  vs[rA*3 <= rB, 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(X3)</font></b>")
  vs[rA*5 <= rB, 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(PERFECT!)</font></b>")
  vs[rA*10 <= rB, 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(FATALITY!)</font></b>")
  vs[rA == rB, 3] <- paste("<b><font color='#808080'>Draw!</font></b>")
  
  pointsCount <- cbind(c(rep(0, length(rA))), c(rep(0, length(rA))))
  pointsCount[rA > rB, 1] <- 100
  pointsCount[rA >= rB*2, 1] <- 200
  pointsCount[rA >= rB*3, 1] <- 300
  pointsCount[rA >= rB*5, 1] <- 500
  pointsCount[rA >= rB*10, 1] <- 1000
  pointsCount[rA < rB, 2] <- 100
  pointsCount[rA*2 <= rB, 2] <- 200
  pointsCount[rA*3 <= rB, 2] <- 300
  pointsCount[rA*5 <= rB, 2] <- 500
  pointsCount[rA*10 <= rB, 2] <- 1000
  pointsCount[rA == rB, 1] <- 0
  pointsCount[rA == rB, 2] <- 0
  
  colnames(vs) <- c(paste("<b><font color='#ff8c00'>", P1$name, "</font></b>"),
                    paste("<b><font color='#0073ff'>", P2$name, "</font></b>"),
                    "Result")
  rownames(vs) <- c("Total cites", "h-index", "i10-index")
  
  output$totalCitesAndIndexVsTable <- renderTable({vs}, 
                                    rownames = TRUE,
                                    caption = "<center><b><span style='color:#000000'>Total cites and index results</b></center>",
                                    caption.placement = getOption("xtable.caption.placement", "top"), 
                                    caption.width = getOption("xtable.caption.width", NULL),
                                    sanitize.text.function = function(x) x)
  
  return(pointsCount)
}

renderYearOfCiteResultsTab <- function(output, P1, P2) {
  citesA <- get_citation_history(P1$id) # wEU99lsAAAAJ
  citesB <- get_citation_history(P2$id) # dYWNWicAAAAJ
  
  if(nrow(citesA) == 0 || nrow(citesB) == 0) return(NULL)
  
  rA <- citesA$cites
  names(rA) <- citesA$year
  rB <- citesB$cites
  names(rB) <- citesB$year
  
  vs <- t(smartbind(rA, rB, rB, fill = 0))
  vs <- vs[order(rownames(vs)), ]
  
  vs[as.numeric(vs[, 1]) > as.numeric(vs[, 2]), 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*2, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(X2)</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*3, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(X3)</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*5, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(PERFECT!)</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*10, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(FATALITY!)</font></b>")
  vs[as.numeric(vs[, 1]) < as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1])*2 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(X2)</font></b>")
  vs[as.numeric(vs[, 1])*3 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(X3)</font></b>")
  vs[as.numeric(vs[, 1])*5 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(PERFECT!)</font></b>")
  vs[as.numeric(vs[, 1])*10 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(FATALITY!)</font></b>")
  vs[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 3] <- paste("<b><font color='#808080'>Draw!</font></b>")
  
  pointsCount <- cbind(c(rep(0, length(vs[, 1]))), c(rep(0, length(vs[, 1]))))
  pointsCount[as.numeric(vs[, 1]) > as.numeric(vs[, 2]), 1] <- 10
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*2, 1] <- 20
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*3, 1] <- 30
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*5, 1] <- 50
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*10, 1] <- 100
  pointsCount[as.numeric(vs[, 1]) < as.numeric(vs[, 2]), 2] <- 10
  pointsCount[as.numeric(vs[, 1])*2 <= as.numeric(vs[, 2]), 2] <- 20
  pointsCount[as.numeric(vs[, 1])*3 <= as.numeric(vs[, 2]), 2] <- 30
  pointsCount[as.numeric(vs[, 1])*5 <= as.numeric(vs[, 2]), 2] <- 50
  pointsCount[as.numeric(vs[, 1])*10 <= as.numeric(vs[, 2]), 2] <- 100
  pointsCount[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 1] <- 0
  pointsCount[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 2] <- 0

  colnames(vs) <- c(paste("<b><font color='#ff8c00'>", P1$name, "</font></b>"),
                    paste("<b><font color='#0073ff'>", P2$name, "</font></b>"),
                    "Result")
  
  output$yearOfCiteVsTable <- renderTable({vs}, 
                                          rownames = TRUE,
                                          caption = "<center><b><span style='color:#000000'>Year of citation results</b></center>",
                                          caption.placement = getOption("xtable.caption.placement", "top"), 
                                          caption.width = getOption("xtable.caption.width", NULL),
                                          sanitize.text.function = function(x) x)
  
  return(pointsCount)
}

renderYearOfPubResultsTab <- function(output, P1, P2) {
  citesA <- compare_scholars(P1$id, 1000)
  citesB <- compare_scholars(P2$id, 1000)
  
  citesA <- citesA[complete.cases(citesA), ]
  citesB <- citesB[complete.cases(citesB), ]
  
  rA <- citesA$cites
  names(rA) <- citesA$year
  rB <- citesB$cites
  names(rB) <- citesB$year
  
  vs <- t(smartbind(rA, rB, rB, fill = 0))
  vs <- vs[order(rownames(vs)), ]
  
  vs[as.numeric(vs[, 1]) > as.numeric(vs[, 2]), 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*2, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(X2)</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*3, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(X3)</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*5, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(PERFECT!)</font></b>")
  vs[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*10, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins! </font><font color=red>(FATALITY!)</font></b>")
  vs[as.numeric(vs[, 1]) < as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1])*2 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(X2)</font></b>")
  vs[as.numeric(vs[, 1])*3 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(X3)</font></b>")
  vs[as.numeric(vs[, 1])*5 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(PERFECT!)</font></b>")
  vs[as.numeric(vs[, 1])*10 <= as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins! </font><font color=red>(FATALITY!)</font></b>")
  vs[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 3] <- paste("<b><font color='#808080'>Draw!</font></b>")
  
  pointsCount <- cbind(c(rep(0, length(vs[, 1]))), c(rep(0, length(vs[, 1]))))
  pointsCount[as.numeric(vs[, 1]) > as.numeric(vs[, 2]), 1] <- 10
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*2, 1] <- 20
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*3, 1] <- 30
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*5, 1] <- 50
  pointsCount[as.numeric(vs[, 1]) >= as.numeric(vs[, 2])*10, 1] <- 100
  pointsCount[as.numeric(vs[, 1]) < as.numeric(vs[, 2]), 2] <- 10
  pointsCount[as.numeric(vs[, 1])*2 <= as.numeric(vs[, 2]), 2] <- 20
  pointsCount[as.numeric(vs[, 1])*3 <= as.numeric(vs[, 2]), 2] <- 30
  pointsCount[as.numeric(vs[, 1])*5 <= as.numeric(vs[, 2]), 2] <- 50
  pointsCount[as.numeric(vs[, 1])*10 <= as.numeric(vs[, 2]), 2] <- 100
  pointsCount[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 1] <- 0
  pointsCount[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 2] <- 0
  
  colnames(vs) <- c(paste("<b><font color='#ff8c00'>", P1$name, "</font></b>"),
                    paste("<b><font color='#0073ff'>", P2$name, "</font></b>"),
                    "Result")
  
  output$yearOfPubVsTable <- renderTable({vs}, 
                                          rownames = TRUE,
                                          caption = "<center><b><span style='color:#000000'>Year of publication results</b></center>",
                                          caption.placement = getOption("xtable.caption.placement", "top"), 
                                          caption.width = getOption("xtable.caption.width", NULL),
                                          sanitize.text.function = function(x) x)
  
  return(pointsCount)
}

render <- function(output, P1, P2) {
  pointsCountA <- rendertotalCitesAndIndexResultsTab(output, P1, P2)
  pointsCountB <- renderYearOfCiteResultsTab(output, P1, P2)
  if(!is.null(pointsCountB)) pointsCountC <- renderYearOfPubResultsTab(output, P1, P2)
  
  pointsCount <- rbind(pointsCountA, pointsCountB, pointsCountC)
  renderResult(output, P1, P2, pointsCount)
}