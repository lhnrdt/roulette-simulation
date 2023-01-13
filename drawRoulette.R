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

placeChip <- function(field, amount_0){
  require(png)
  chip <- suppressWarnings(readPNG("chip.png"))
  chip_size <- .8
  dev.aspect <- dev.size("px")[1]/dev.size("px")[2]
  rasterImage(chip,
              (if(is.character(field)){getElement(str_pos, field)[1]}else int_pos[field + 1, 1]) - chip_size/2,
              (if(is.character(field)){getElement(str_pos, field)[2]}else int_pos[field + 1, 2]) - (7/34) * dev.size("px")[1]/dev.size("px")[2] * chip_size,
              (if(is.character(field)){getElement(str_pos, field)[1]}else int_pos[field + 1, 1]) + chip_size - chip_size/2,
              (if(is.character(field)){getElement(str_pos, field)[2]}else int_pos[field + 1, 2]) + (7/34) * dev.size("px")[1]/dev.size("px")[2] * chip_size
  )
  text(x = (if(is.character(field)){getElement(str_pos, field)[1]}else int_pos[field + 1, 1]),
       y = (if(is.character(field)){getElement(str_pos, field)[2]}else int_pos[field + 1, 2]),
       labels = amount_0, cex = 0.8)
}


drawBoard()
grid(nx = 17*2, ny = 7*2, lty = "dotted", col = "green", lwd = .1)
grid(nx = 17, ny = 7, lty = "solid", col = "orange", lwd = 2)
placeChip("green", 10)
