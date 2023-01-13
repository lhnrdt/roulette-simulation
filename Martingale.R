rm(list = ls())
library(R.utils)
library(foreach)
library(doSNOW)

drawBoard <- function(lwd){
  par(pty = "m",
      mar = c(0,0,0,0),
      bg = "#25562a",
      xpd = F,
      col = "white",
      col.axis = "white",
      col.lab = "white",
      col.main = "white",
      mfrow = c(1,1))
  if(missing(lwd)) lwd <- 2
  cex <- lwd * 0.55
  plot(x = 0:17, y = 0:17,
       xaxs = "i", yaxs = "i",
       xlab = "", ylab = "",
       ylim = c(0,7),
       xlim = c(0,17),
       axes = F,
       type = "n")
  
  rect(xleft = 3, ybottom = 3, xright = 15, ytop = 6, col = "red")
  rect(xleft   = c(4,7,10,13),
       ybottom = c(5,5, 5, 5),
       xright  = c(4,7,10,13) + 1,
       ytop    = c(5,5, 5, 5) + 1,
       col = "black")
  rect(xleft   = c(3,5,6,8,9,11,12,14),
       ybottom = c(4,4,4,4,4, 4, 4, 4),
       xright  = c(3,5,6,8,9,11,12,14) + 1,
       ytop    = c(4,4,4,4,4, 4, 4, 4) + 1,
       col = "black")
  rect(xleft   = c(4,6,7,10,12,13),
       ybottom = c(3,3,3, 3, 3, 3),
       xright  = c(4,6,7,10,12,13) + 1,
       ytop    = c(3,3,3, 3, 3, 3) + 1,
       col = "black")
  polygon(c(7.25, 8, 8.75, 8), y = c(1.5, 1.75, 1.5, 1.25), col = "red")
  polygon(c(9.25, 10, 10.75, 10), y = c(1.5, 1.75, 1.5, 1.25), col = "black")
  segments(x0 = c(  2,1.5, 2, 2,16,3,7,11,15, 3, 3,       4:14, 3, 3,5,13),
           y0 = c(  6,4.5, 6, 3, 3,6,6, 6, 6, 1, 2, rep(6, 11), 5, 4,1, 1),
           x1 = c(1.5,  2,16,16,16,3,7,11,15,15,15,       4:14,16,16,5,13),
           y1 = c(4.5,  3, 6, 3, 6,1,1, 1, 1, 1, 2, rep(3, 11), 5, 4,2, 2),
           col = "white", lwd = lwd)
  text(x = rep(seq(3.5,14.5,1), each = 3),
       y = rep(seq(3.5,5.5,1),12),
       srt = 90, labels = 1:36,  cex = cex)
  text(x = 2.25, y = 4.5, srt = 90, labels = 0, cex = cex)
  text(x = c(  5,  9, 13,  6, 12),
       y = c(2.5,2.5,2.5,1.5,1.5),
       labels = c("1st 12", "2nd 12", "3rd 12", "EVEN", "ODD"), cex = cex)
  text(x = c(  4, 14),
       y = c(1.5,1.5),
       labels = c("1 TO 18", "19 TO 36"), cex = cex * 0.75)
  text(x = c(15.5, 15.5, 15.5), y = c(3.5,4.5,5.5),
       srt = 90, labels = c("2 TO 1", "2 TO 1", "2 TO 1"), cex = cex * 0.5)
  int_pos <<- matrix(byrow = F, ncol = 2, data = c(2.25, rep(seq(3.5, 14.5, 1), each = 3), 4.5, rep(seq(3.5,5.5,1), 12)))
  str_pos <<- data.frame("green" = c(2.25,4.5),
                         "dozen1" = c(5, 2.5),
                         "dozen2" = c(9, 2.5),
                         "dozen3" = c(13, 2.5),
                         "coloumn1" = c(15.5, 5.5),
                         "coloumn2" = c(15.5, 4.5),
                         "coloumn3" = c(15.5, 3.5),
                         "low" = c(4, 1.5),
                         "high" = c(14, 1.5),
                         "even" = c(6, 1.5),
                         "odd" = c(12, 1.5),
                         "red" = c(8, 1.5),
                         "black" = c(10, 1.5))
}

placeChip <- function(amount, field){
  for(i in 1:length(field)){field[i] <- as.list(if(grepl("^[0-9]*$", field)[i]){as.numeric(field[i])}else field[i])}
  require(png)
  chip <- suppressWarnings(readPNG("chip.png"))
  chip_size <- .8
  dev.aspect <- dev.size("px")[1]/dev.size("px")[2]
  for(i in 1:length(field)){
    rasterImage(chip,
                (if(is.character(field[[i]])){getElement(str_pos, field[[i]])[1]}
                 else int_pos[field[[i]] + 1, 1]) - chip_size/2,
                
                (if(is.character(field[[i]])){getElement(str_pos, field[[i]])[2]}
                 else int_pos[field[[i]] + 1, 2]) - (7/34) * dev.size("px")[1]/dev.size("px")[2] * chip_size,
                
                (if(is.character(field[[i]])){getElement(str_pos, field[[i]])[1]}
                 else int_pos[field[[i]] + 1, 1]) + chip_size - chip_size/2,
                
                (if(is.character(field[[i]])){getElement(str_pos, field[[i]])[2]}
                 else int_pos[field[[i]] + 1, 2]) + (7/34) * dev.size("px")[1]/dev.size("px")[2] * chip_size
    )
    text(x = (if(is.character(field[[i]])){getElement(str_pos, field[[i]])[1]}else int_pos[field[[i]] + 1, 1]),
         y = (if(is.character(field[[i]])){getElement(str_pos, field[[i]])[2]}else int_pos[field[[i]] + 1, 2]),
         labels = amount[i], cex = 0.8)
  }
}

bet <- function(amount, field, draw){
  # Probabilities for each color
  pgreen <- 1  # default:  1
  pred <-   18   # default: 18
  pblack <- 18   # default: 18
  
  board <- matrix(1:36, byrow = T, ncol = 3)
  
  if(missing(draw)) draw = FALSE
  if(draw){
    drawBoard()
    placeChip(amount, field)
  }
  
  red <- c(seq(1,9,2),seq(12,18,2), seq(19,27,2), seq(30,36,2))
  
  profit <- - sum(amount)
  
  ticket <- sample(c(0:36), 1, prob = c(pgreen, rep(c(pred/18, pblack/18), 18)), replace = T)
  
  if(ticket == 0) color <- "green"
  if(ticket %in% red) {color <- "red"}
  if(ticket %in% setdiff(1:36, red)) color <- "black"
  
  if(ticket == 0 & "green" %in% field){
    profit <- profit + amount[match("green", field)] * 36
  }
  if (ticket %in% 1:12 & "dozen1" %in% field){
    profit <- profit + amount[match("dozen1", field)] * 3
  }
  if (ticket %in% 13:24 & "dozen2" %in% field){
    profit <- profit + amount[match("dozen2", field)] * 3
  }
  if (ticket %in% 25:36 & "dozen3" %in% field){
    profit <- profit + amount[match("dozen3", field)] * 3
  }
  if (ticket %in% board[,3] & "coloumn1" %in% field){
    profit <- profit + amount[match("coloumn1", field)] * 3
  }
  if (ticket %in% board[,2] & "coloumn2" %in% field){
    profit <- profit + amount[match("coloumn2", field)] * 3
  }
  if (ticket %in% board[,1] & "coloumn3" %in% field){
    profit <- profit + amount[match("coloumn3", field)] * 3
  }
  if (ticket %in% 1:18 &  "low" %in% field){
    profit <- profit + amount[match("low", field)] * 2
  }
  if (ticket %in% 19:36 & "high" %in% field){
    profit <- profit + amount[match("high", field)] * 2
  }
  if ((ticket %% 2) == 1 & "even" %in% field){
    profit <- profit + amount[match("even", field)] * 2
  }
  if ((ticket %% 2) == 0 & "odd" %in% field){
    profit <- profit + amount[match("odd", field)] * 2
  }
  if (ticket %in% red & "red" %in% field){
    profit <- profit + amount[match("red", field)] * 2 
  }
  if (ticket %in% setdiff(1:36, red) & "black" %in% field){
    profit <- profit + amount[match("black", field)] * 2
  }
  if (ticket %in% field){
    profit <- profit + amount[match(ticket, field)] * 36
  }
  result <- list("result" = ticket, "color" = color, "amount" = amount, "field" = field, "profit" = profit)
  return(result)
}

bet(amount = c(10, 25, 100),
    field = c("even", "low", 0),
    draw = TRUE)

betMartingale(amount = c(10),
              field = c("even"),
              max_loss = 128)

betMartingale <- function(amount, field, max_loss){
  if(!exists("current.amount")) {current.amount <<- amount}
  play <- bet(current.amount, field)
  if(play$profit <= 0){
    if(!missing(max_loss)){
      if(!exists("lost")){lost <<- 0}
      if(lost < max_loss){
        current.amount[1] <<- current.amount[1] * 2
        lost <<- lost + 1
        return(play)
      }else{
        lost <<- 0
        current.amount[1] <<- amount[1]
      }
    }else{
      current.amount[1] <<- current.amount[1] * 2
      return(play)
    }
  }else{
    current.amount[1] <<- amount[1]
    lost <<- 0
  }
  return(play)
}

betLabouchere <- function(amount, field, length){
  if(missing(length)) length <- 5
  if(!exists("current.amount")) {current.amount <<- amount}
  if(!exists("l")){l <<- seq(amount, by = amount, length.out = length)}
  current.amount <<- l[1] + tail(l,1)
  play <- bet(current.amount, field)
  if(play$profit <= 0){
    l[length(l)+1] <<- current.amount
  }else{
    l <<- l[-c(1,tail(l,1))]
  }
  return(play)
}

rm(l)
rm(amount)
rm(current.amount)
rm(length)

simulateStrategy <- function(iter, amount, field, max_loss, draw, printProgress) {
  if(missing(printProgress)) printProgress <- TRUE
  if(missing(draw)) draw <- TRUE
  if(missing(max_loss)) max_loss <- -1
  
  
  drawBoard()
  placeChip(amount, field)
  cat("\014")
  readline(prompt= paste("Are you sure that you want to simulate", iter, "iterations with this input?\n",
                         "Press [enter] to confirm and run simulation. Press [Esc] to cancel."))
  if(printProgress == TRUE) {
    pb <- txtProgressBar(min = 0, max = iter, style = 3)
    time0 <- Sys.time()
  }
  par(pty = "m",
      mar = c(5,4,4,2) + 0.1,
      bg = "#25562a",
      xpd = F,
      col = "white",
      col.axis = "white",
      col.lab = "white",
      col.main = "white")
  
  output <- list("Result" = NA, "Color" = NA, "Amount" = list(NA), "Field" = list(NA), "Profit" = NA)
  
  foreach(i = 1:iter) {
    if(max_loss != -1){
      play <- betMartingale(amount, field, max_loss)
      output$Result[i] <- play$result
      output$Color[i] <- play$color
      output$Amount[[i]] <- as.list(play$amount)
      output$Field[[i]] <- as.list(play$field)
      output$Profit[i] <- play$profit
      if(printProgress == TRUE) setTxtProgressBar(pb, i)
    }else{
      play <- betMartingale(amount, field)
      output$Result[i] <- play$result
      output$Color[i] <- play$color
      output$Amount[[i]] <- play$amount
      output$Field[[i]] <- play$field
      output$Profit[i] <- play$profit
      if(printProgress == TRUE) setTxtProgressBar(pb, i)
    }
  }
  
  if(draw == TRUE){
    if(max_loss != -1){
      ylim1 <- max(abs(cumsum(output$Profit)))
      ylim0 <- -ylim1
    }else{
      ylim1 <- max(cumsum(output$Profit))
      ylim0 <- min(cumsum(output$Profit)) 
    }
    
    plot(cumsum(output$Profit),
         panel.first = grid(),
         type = "h",
         ylab = "Profit",
         col = output$Color,
         lwd = (dev.size(units = "px")[1])/(2*i),
         lend = 1,
         pch = 20,
         ylim = c(ylim0, ylim1)
    )
    text_col <- "white"
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.1),
         label = if(max_loss == 0){
           paste("Betting ",
                 amount[1], " $ on ",
                 field[1],
                 sep = "")
         }
         else{
           paste("Martingale-betting ",
                 amount[1], "$ on ",
                 field[1],
                 if(max_loss != -1){
                   paste(" (Highest allowed bet: ",
                         amount[1] * (2 ^ max_loss), "$)", sep = "")
                 },
                 sep = "")
         },
         pos = 4,
         col = text_col)
    
    if(length(amount) > 1){
      text(x = iter * 0.05,
           y = ylim1 * (1 - 0.2),
           if(length(amount) > 2){
             paste("Also betting ", paste0(amount[2:(length(amount)-1)], "$ on ", field[2:(length(amount)-1)], sep = "", collapse = ", "),
                   " and ", tail(amount,1), "$ on ", field[length(field)], ".", sep = "")
           }else{
             paste("Also betting ", amount[2], "$ on ", field[2], sep = "")
           },
           pos = 4,
           col = text_col)
    }
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.3),
         paste("Iterations =", iter),
         pos = 4,
         col = text_col)
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.4),
         paste("Highest Bet: ", max(unlist(output$Amount)), "$", sep = ""),
         pos = 4,
         col = text_col)
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.5),
         paste("Profit: ", round(cumsum(output$Profit)[length(output$Profit)], 2), "$", sep = ""),
         pos = 4,
         col = text_col)
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.6),
         paste("Times Red:", sum(output$Color == "red")),
         pos = 4,
         col = text_col)
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.7),
         paste("Times Black:", sum(output$Color == "black")),
         pos = 4,
         col = text_col)
    text(x = iter * 0.05,
         y = ylim1 * (1 - 0.8),
         paste("Times Green:", sum(output$Color == "green")),
         pos = 4,
         col = text_col)
    abline(h = 0, col = "black", lwd = 3)
  }
  if(exists("Result")){rm("Result", inherits = TRUE)}
  Result <<- output
  rm("current.amount", inherits = T)
  cat("\014")
  if(printProgress == TRUE) round(Sys.time() - time0,2)
}

playMartingale <- function(amount, field, max_loss){
  par(pty = "m",
      mar = c(1,4,2,2) + 0.1,
      bg = "#25562a",
      xpd = F,
      col = "white",
      col.axis = "white",
      col.lab = "white",
      col.main = "white",
      mfrow = c(1,1))
  if(!exists("amount")){amount <<- amount}
  if(!exists("i")){i <<- 1}
  if(!exists("output")){output <<- as.data.frame(matrix(ncol = length(bet(1, "black")) + 2, nrow = i))}
  colnames(output) <<- c("Bet", "Result", "Color", "Field", "Won", "Profit")
  if(!missing(max_loss)){
    output[i,] <<- c(amount, betMartingale(amount, field, max_loss), NA)
  }else{
    output[i,] <<- c(amount, betMartingale(amount, field), NA)
  } 
  output$Profit <<- cumsum(output$Won) - cumsum(output$Bet)
  i <<- i + 1
  if(!missing(max_loss)) ylim <- max(abs(output$Profit))
  else ylim <- max(output$Profit)
  plot(cumsum(output$Won) - cumsum(output$Bet),
       panel.first = grid(),
       type = "h",
       ylab = "Profit",
       col = output$Color,
       lwd = (dev.size(units = "px")[1])/(4*i),
       lend = 1,
       pch = 20,
       xlim = c(0, i + 1),
       ylim = c(-ylim,ylim)
  )
  
  abline(h = 0, col = "black", lwd = 3)
  text_col <- "white"
  text(x = i * 0.05,
       y = ylim * (1 - 0.1),
       label = paste("Betting ", amount, " $ on ", field,
                     if(!(missing(max_loss))){
                       if(max_loss != 0) paste(" (Highest allowed bet: ", round(amount * (2 ^ max_loss), 2), "$)", sep = "")
                     },
                     sep = ""),
       pos = 4,
       col = text_col)
  text(x = i * 0.05,
       y = ylim * (1 - 0.2),
       paste("Iterations =", i - 1),
       pos = 4,
       col = text_col)
  text(x = i * 0.05,
       y = ylim * (1 - 0.3),
       paste("Highest Bet: ", round(max(output$Bet), 2), "$", sep = ""),
       pos = 4,
       col = text_col)
  text(x = i * 0.05,
       y = ylim * (1 - 0.4),
       paste("Profit: ", round(output$Profit[length(output$Profit)], 2), "$", sep = ""),
       pos = 4,
       col = text_col)
  text(x = i * 0.05,
       y = ylim * (1 - 0.5),
       paste("Times Red:", sum(output$Color == "red")),
       pos = 4,
       col = text_col)
  text(x = i * 0.05,
       y = ylim * (1 - 0.6),
       paste("Times Black:", sum(output$Color == "black")),
       pos = 4,
       col = text_col)
  text(x = i * 0.05,
       y = ylim * (1 - 0.7),
       paste("Times Green:", sum(output$Color == "green")),
       pos = 4,
       col = text_col)
  
}

resetMartingale <- function(){
  if(exists("i")){rm("i", inherits = TRUE)}
  if(exists("amount")){rm("amount", inherits = TRUE)}
  if(exists("output")){rm("output", inherits = TRUE)}
  if(exists("lost")){rm("lost", inherits = TRUE)}
}

animateMartingale <- function(iter, amount, field, max_loss) {
  
  readline(prompt="Press [enter] to continue")
  
  time0 <- Sys.time()
  resetMartingale()
  for(j in 1:iter){
    if(!missing(max_loss)){
      playMartingale(amount, field, max_loss)
    }else{
      playMartingale(amount, field)
    }
    date_time <- Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time)) < .1){}
    print(paste("Progress: ", round((j/iter)*100, 0), "%", sep = ""))
  }
  print(Sys.time() - time0)
}

simulateProfits <- function(sets, plays, max_loss){
  time0 <- Sys.time()
  Profits <- NA
  
  for(i in 1:sets){
    if(!missing(max_loss)){
      simulateStrategy(iter = plays, amount = 1, "black", max_loss = max_loss, draw = FALSE, printProgress = FALSE)
    }
    else{
      simulateStrategy(iter = plays, amount = 1, "black", max_loss = , draw = FALSE, printProgress = FALSE)
    }
    Profits[i] <- Result$Profit[length(Result$Profit)]
    print(paste("Progress: ", round((i/sets)*100, 0), "%", sep = ""))
  }
  
  plot(Profits, panel.first = c(grid(col = "black"), abline(h = 0, col = "black")), pch = 20,
       col = rgb(1,1,1,1),
       lwd = 1,
       col.sub = "red",
       main = paste("Outcomes of", sets, "sets of", plays, "plays"),
       sub = paste("Average profit =", mean(Profits)))
  abline(h = mean(Profits), col = "red")
  print(Sys.time() - time0)
}

resetMartingale()
animateMartingale(iter = 100, amount = 1, field = "red", max_loss = 128)

playMartingale(amount = c(20), field = c("red"), max_loss = 128)

simulateProfits(sets = 100, plays = 1000, max_loss = 0)



simulateStrategy(1500, amount = c(.15), field = c("red"), max_loss = 13)

field <- c("red", "black", "green", "odd", "high")
amount <- c(10,20,30,40,50)

View(data.frame(Result$Profit, Result$Color))


