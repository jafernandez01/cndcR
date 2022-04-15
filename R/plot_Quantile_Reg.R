
linpInit <- function(mCall, LHS, data, ...){

  xy <- sortedXyData(mCall[["x"]], LHS, data)
  if(nrow(xy) < 3){
    stop("Too few distinct input values to fit a linear-plateau")
  }

  ## Dumb guess for a and b is to fit a linear regression to half the data
  xy1 <- xy[1:floor(nrow(xy)/2),]
  fit1 <- stats::lm(xy1[,"y"] ~ xy1[,"x"])
  ## Atomic bomb approach to kill a mosquito
  objfun <- function(cfs){
    pred <- linp(xy[,"x"], a=cfs[1], b=cfs[2], xs=cfs[3])
    ans <- sum((xy[,"y"] - pred)^2)
    ans
  }
  cfs <- c(coef(fit1),mean(xy[,"x"]))
  op <- try(stats::optim(cfs, objfun, method = "L-BFGS-B",
                         upper = c(Inf, Inf, max(xy[,"x"])),
                         lower = c(-Inf, -Inf, min(xy[,"x"]))), silent = TRUE)

  if(class(op) != "try-error"){
    a <- op$par[1]
    b <- op$par[2]
    xs <- op$par[3]
  }else{
    ## If it fails I use the mean for the breakpoint
    a <- coef(fit1)[1]
    b <- coef(fit1)[2]
    xs <- mean(xy[,"x"])
  }

  value <- c(a, b, xs)
  names(value) <- mCall[c("a","b","xs")]
  value
}

#' @rdname SSlinp
#' @return linp: vector of the same length as x using the linear-plateau function
#' @export
linp <- function(x, a, b, xs){

  .asym <- a + b * xs

  .value <- (x < xs) * (a + b * x) + (x >= xs) * .asym

  ## Derivative with respect to a when (x < xs)
  ## .exp1 <- deriv(~ a +  b * x + c * x^2, "a")
  .exp1 <- 1 ## ifelse(x < xs, 1, 1)

  ## Derivative with respect to b
  ## if x < xs: .exp2 <- deriv(~ a +  b * x + c * x^2, "b")
  ## if x >= xs: .exp2 <- deriv(~ a +  b * xs + c * x^2, "b")
  .exp2 <- ifelse(x < xs, x, xs)

  ## Derivative with respect to xs
  ## .exp3 <- deriv(~ a +  b * xs, "xs")
  .exp3 <- ifelse(x < xs, 0, b)

  .actualArgs <- as.list(match.call()[c("a","b","xs")])

  ##  Gradient
  if (all(unlist(lapply(.actualArgs, is.name)))) {
    .grad <- array(0, c(length(.value), 3L), list(NULL, c("a","b","xs")))
    .grad[, "a"] <- .exp1
    .grad[, "b"] <- .exp2
    .grad[, "xs"] <- .exp3
    dimnames(.grad) <- list(NULL, .actualArgs)
    attr(.value, "gradient") <- .grad
  }
  .value
}

#' @rdname SSlinp
#' @export
SSlinp <- selfStart(linp, initial = linpInit, c("a","b","xs"))



# Regression: minW vs. b coefficient

#m_a1 = lm(A2 ~ minW, data = biblioCNDC)
data("biblioCNDC")
biblioCNDC <- biblioCNDC %>% dplyr::rename(A2 = b)
m_a2 = quantreg::nlrq(A2 ~ SSlinp(minW, a ,b,xs), data = biblioCNDC, tau = 0.05 )
ndat_a2 = expand.grid(minW = seq(min(biblioCNDC$minW, na.rm = T), max(biblioCNDC$minW, na.rm = T), 0.1))
ndat_a2$preds = quantreg::predict(m_a2, newdata = ndat_a2)

figQuantile_Reg <- biblioCNDC %>%
  ggplot() +
  geom_point(aes(x = minW, y = A2), shape = 19, size = 1.7, fill = "#3d3c3c", alpha = .8) +
  geom_line(data = ndat_a2, aes(x = minW, y = preds), color = "#E64B35FF", linetype = "solid", size = .5) +

  xlab(expression("Minimum value of biomass (Mg ha"^"-1"~")")) + ylab(expression(paste(italic("b"), " coefficient"))) +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0.5,0.2,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.3,0.5,0.2), "cm"), size = 13))
