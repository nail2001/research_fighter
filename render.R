library(gtools)
library(ggplot2)

renderMainResultsTab <- function(output, P1, P2) {
  rA <- c(P1$total_cites, P1$h_index, P1$i10_index)
  rB <- c(P2$total_cites, P2$h_index, P2$i10_index)
  
  vs <- cbind(rA, rB, c(rep("X", length(rA))))
  
  vs[rA > rB, 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins!</font></b>")
  vs[rA < rB, 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins!</font></b>")
  vs[rA == rB, 3] <- paste("<b><font color='#808080'>Draw!</font></b>")
  
  colnames(vs) <- c(paste("<b><font color='#ff8c00'>", P1$name, "</font></b>"),
                    paste("<b><font color='#0073ff'>", P2$name, "</font></b>"),
                    "Result")
  rownames(vs) <- c("Total cites", "h-index", "i10-index")
  
  output$mainVsTable <- renderTable({vs}, 
                                    rownames = TRUE,
                                    caption = "<center><b><span style='color:#000000'>Main results</b></center>",
                                    caption.placement = getOption("xtable.caption.placement", "top"), 
                                    caption.width = getOption("xtable.caption.width", NULL),
                                    sanitize.text.function = function(x) x)
}

renderYearOfCiteResultsTab <- function(output, P1, P2) {
  citesA <- get_citation_history(P1$id)
  citesB <- get_citation_history(P2$id)
  
  rA <- citesA$cites
  names(rA) <- citesA$year
  rB <- citesB$cites
  names(rB) <- citesB$year
  
  vs <- t(smartbind(rA, rB, rB, fill = 0))
  vs <- vs[order(rownames(vs)), ]
  
  vs[as.numeric(vs[, 1]) > as.numeric(vs[, 2]), 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1]) < as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 3] <- paste("<b><font color='#808080'>Draw!</font></b>")
  
  colnames(vs) <- c(paste("<b><font color='#ff8c00'>", P1$name, "</font></b>"),
                    paste("<b><font color='#0073ff'>", P2$name, "</font></b>"),
                    "Result")
  
  output$yearOfCiteVsTable <- renderTable({vs}, 
                                          rownames = TRUE,
                                          caption = "<center><b><span style='color:#000000'>Year of citation results</b></center>",
                                          caption.placement = getOption("xtable.caption.placement", "top"), 
                                          caption.width = getOption("xtable.caption.width", NULL),
                                          sanitize.text.function = function(x) x)
  
  return(vs[,c(1,2)])
}

renderYearOfPubResultsTab <- function(output, P1, P2) {
  citesA <- compare_scholars(P1$id, 300)
  citesB <- compare_scholars(P2$id, 300)
  
  citesA <- citesA[complete.cases(citesA), ]
  citesB <- citesB[complete.cases(citesB), ]
  
  rA <- citesA$cites
  names(rA) <- citesA$year
  rB <- citesB$cites
  names(rB) <- citesB$year
  
  vs <- t(smartbind(rA, rB, rB, fill = 0))
  vs <- vs[order(rownames(vs)), ]
  
  vs[as.numeric(vs[, 1]) > as.numeric(vs[, 2]), 3] <- paste("<b><font color='#ff8c00'>", P1$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1]) < as.numeric(vs[, 2]), 3] <- paste("<b><font color='#0073ff'>", P2$name, "wins!</font></b>")
  vs[as.numeric(vs[, 1]) == as.numeric(vs[, 2]), 3] <- paste("<b><font color='#808080'>Draw!</font></b>")
  
  colnames(vs) <- c(paste("<b><font color='#ff8c00'>", P1$name, "</font></b>"),
                    paste("<b><font color='#0073ff'>", P2$name, "</font></b>"),
                    "Result")
  
  output$yearOfPubVsTable <- renderTable({vs}, 
                                          rownames = TRUE,
                                          caption = "<center><b><span style='color:#000000'>Year of publication results</b></center>",
                                          caption.placement = getOption("xtable.caption.placement", "top"), 
                                          caption.width = getOption("xtable.caption.width", NULL),
                                          sanitize.text.function = function(x) x)
  
  return(vs[,c(1,2)])
}

render <- function(output, P1, P2) {
  renderMainResultsTab(output, P1, P2)
  YearOfCite <- renderYearOfCiteResultsTab(output, P1, P2)
  YearOfPub <- renderYearOfPubResultsTab(output, P1, P2)
}