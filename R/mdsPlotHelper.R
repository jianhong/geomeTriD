getSlope <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  return(list(
    xm = (x1 + x2) / 2,
    ym = (y1 + y2) / 2,
    dx = dx,
    dy = dy,
    slope = dy / dx
  ))
}

calcSquareControlPoints <- function(x1, y1, x2, y2, curvature, angle, ncp) {
  gs <- getSlope(x1, y1, x2, y2)
  end <- (gs$slope > 1 | (gs$slope < 0 & gs$slope > -1))
  if (curvature < 0) {
    end <- !end
  }
  startx <- ifelse(end, x1, ifelse(abs(gs$slope) > 1, x2 - gs$dx,
    x2 - sign(gs$slope) * gs$dy
  ))
  starty <- ifelse(end, y1, ifelse(abs(gs$slope) > 1, y2 - sign(gs$slope) *
    gs$dx, y2 - gs$dy))
  endx <- ifelse(end, ifelse(abs(gs$slope) > 1, x1 + gs$dx, x1 +
    sign(gs$slope) * gs$dy), x2)
  endy <- ifelse(end, ifelse(abs(gs$slope) > 1, y1 + sign(gs$slope) *
    gs$dx, y1 + gs$dy), y2)
  cps <- calcControlPoints(
    startx, starty, endx, endy, curvature,
    angle, ncp
  )
  ncurve <- length(x1)
  cps$x <- interleave(ncp, ncurve, cps$x, startx, endx, end)
  cps$y <- interleave(ncp, ncurve, cps$y, starty, endy, end)
  list(x = cps$x, y = cps$y, end = end)
}

calcControlPoints <- function(x1, y1, x2, y2, curvature, angle, ncp) {
  gs <- getSlope(x1, y1, x2, y2)
  if (is.null(angle)) {
    angle <- ifelse(gs$slope < 0, 2 * atan(abs(gs$slope)), 2 *
      atan(1 / gs$slope))
  } else {
    angle <- angle / 180 * pi
  }
  sina <- sin(angle)
  cosa <- cos(angle)
  cornerx <- gs$xm + (x1 - gs$xm) * cosa - (y1 - gs$ym) * sina
  cornery <- gs$ym + (y1 - gs$ym) * cosa + (x1 - gs$xm) * sina
  beta <- -atan((cornery - y1) / (cornerx - x1))
  sinb <- sin(beta)
  cosb <- cos(beta)
  newx2 <- x1 + gs$dx * cosb - gs$dy * sinb
  newy2 <- y1 + gs$dy * cosb + gs$dx * sinb
  scalex <- (newy2 - y1) / (newx2 - x1)
  newx1 <- x1 * scalex
  newx2 <- newx2 * scalex
  ratio <- 2 * (sin(atan(curvature))^2)
  origin <- curvature - curvature / ratio
  if (curvature > 0) {
    hand <- "right"
  } else {
    hand <- "left"
  }
  oxy <- calcOrigin(newx1, y1, newx2, newy2, origin)
  ox <- oxy$x
  oy <- oxy$y
  dir <- switch(hand,
    left = -1,
    right = 1
  )
  maxtheta <- pi + sign(origin * dir) * 2 * atan(abs(origin))
  theta <- seq(0, dir * maxtheta, dir * maxtheta / (ncp + 1))[c(
    -1,
    -(ncp + 2)
  )]
  costheta <- cos(theta)
  sintheta <- sin(theta)
  cpx <- ox + ((newx1 - ox) %*% t(costheta)) - ((y1 - oy) %*%
    t(sintheta))
  cpy <- oy + ((y1 - oy) %*% t(costheta)) + ((newx1 - ox) %*%
    t(sintheta))
  cpx <- cpx / scalex
  sinnb <- sin(-beta)
  cosnb <- cos(-beta)
  finalcpx <- x1 + (cpx - x1) * cosnb - (cpy - y1) * sinnb
  finalcpy <- y1 + (cpy - y1) * cosnb + (cpx - x1) * sinnb
  list(x = as.numeric(t(finalcpx)), y = as.numeric(t(finalcpy)))
}

calcOrigin <- function(x1, y1, x2, y2, origin) {
  gs <- getSlope(x1, y1, x2, y2)
  oslope <- -1 / gs$slope
  tmpox <- ifelse(!is.finite(gs$slope), gs$xm,
    ifelse(!is.finite(oslope),
      gs$xm + origin * gs$dx / 2,
      gs$xm + origin * gs$dx / 2
    )
  )
  tmpoy <- ifelse(!is.finite(gs$slope), gs$ym + origin * gs$dy / 2,
    ifelse(!is.finite(oslope), gs$ym, gs$ym + origin * gs$dy / 2)
  )
  sintheta <- -1
  ox <- gs$xm - (tmpoy - gs$ym) * sintheta
  oy <- gs$ym + (tmpox - gs$xm) * sintheta
  list(x = ox, y = oy)
}

interleave <- function(ncp, ncurve, val, sval, eval, e) {
  sval <- rep(sval, length.out = ncurve)
  eval <- rep(eval, length.out = ncurve)
  result <- matrix(NA, ncol = ncurve, nrow = ncp + 1)
  m <- matrix(val, ncol = ncurve)
  for (i in 1L:ncurve) {
    if (e[i]) {
      result[, i] <- c(m[, i, drop = TRUE], eval[i])
    } else {
      result[, i] <- c(sval[i], m[, i, drop = TRUE])
    }
  }
  as.numeric(result)
}
