### doom like
library(tcltk)

# map 1 :)

# # 1 = wall, 0 = floor, 2 = art wall (purple tint)
# MAP <- matrix(c(
#   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#   1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
#   1,0,1,1,0,1,2,0,1,0,1,1,0,1,1,1,
#   1,0,1,0,0,0,2,0,0,0,1,0,0,0,1,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,1,0,0,0,1,0,0,0,1,0,0,0,1,1,
#   1,0,1,1,0,1,1,0,1,0,1,1,0,1,1,1,
#   1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
#   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
# ), nrow = 16, ncol = 16, byrow = TRUE)



## Map 2

# ── MAP ─────────────────────────────────────────────────────
# 1 = wall, 0 = floor, 2 = art wall (purple tint)
# MAP KEY:
#  0 = floor (open space)
#  1 = solid wall
#  2 = art wall (purple)  — gallery paintings
#  3 = accent wall (teal) — decorative pillars
# MAP <- matrix(c(
#   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,1,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,0,0,0,0,0,0,0,0,0,0,0,2,1,1,2,0,0,0,0,0,0,0,0,0,0,0,2,0,1,
#   1,0,2,0,1,1,0,0,0,0,1,1,0,0,2,1,1,2,0,0,1,1,0,0,0,0,1,1,0,2,0,1,
#   1,0,2,0,1,0,0,0,0,0,0,1,0,0,2,1,1,2,0,0,1,0,0,0,0,0,1,0,0,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,0,1,0,0,0,0,0,0,1,0,0,2,1,1,2,0,0,1,0,0,0,0,0,1,0,0,2,0,1,
#   1,0,2,0,1,1,0,0,0,0,1,1,0,0,2,1,1,2,0,0,1,1,0,0,0,0,1,1,0,2,0,1,
#   1,0,2,0,0,0,0,0,0,0,0,0,0,0,2,1,1,2,0,0,0,0,0,0,0,0,0,0,0,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,
#   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
#   1,1,0,3,0,0,0,0,0,0,0,0,0,0,3,1,1,3,0,0,0,0,0,0,0,0,0,0,0,3,1,1,
#   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
#   1,1,0,3,0,0,0,0,0,0,0,0,0,0,3,1,1,3,0,0,0,0,0,0,0,0,0,0,0,3,1,1,
#   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
#   1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,0,0,3,0,0,0,3,0,0,0,3,0,1,1,0,3,0,0,0,3,0,0,0,3,0,0,2,0,1,
#   1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,0,0,3,0,0,0,3,0,0,0,3,0,1,1,0,3,0,0,0,3,0,0,0,3,0,0,2,0,1,
#   1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,2,0,0,0,0,0,0,0,0,0,0,2,1,1,2,0,0,0,0,0,0,0,0,0,0,2,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,1,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,
#   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
#   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
# ), nrow = 32, ncol = 32, byrow = TRUE)

MAP <- matrix(c(
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,
  1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1,1,
  1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,1,
  1,0,1,0,1,0,1,1,1,1,1,0,1,1,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,0,1,1,
  1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,1,1,
  1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,
  1,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,1,1,
  1,0,1,1,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,
  1,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,1,
  1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,1,1,1,0,1,1,1,0,1,0,1,1,
  1,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,1,1,
  1,0,1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,1,0,1,1,1,0,1,1,1,0,1,1,
  1,0,1,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,1,1,
  1,0,1,0,1,1,1,0,1,1,1,1,1,0,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,
  1,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,
  1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,0,1,0,1,1,
  1,0,1,0,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,
  1,0,1,0,1,1,1,0,1,1,1,1,1,1,1,0,1,0,1,0,1,1,1,1,1,0,1,1,1,0,1,1,
  1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,1,1,
  1,0,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,
  1,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,
  1,0,1,0,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,0,1,0,1,1,1,1,
  1,0,0,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,0,0,1,0,1,0,1,0,1,1,
  1,0,1,1,1,1,1,1,1,0,1,0,1,0,1,1,1,0,1,0,1,0,1,1,1,0,1,0,1,0,1,1,
  1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,1,1,
  1,1,1,0,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,0,1,1,
  1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,1,1,
  1,0,1,1,1,1,1,0,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,1,0,1,0,1,0,1,1,
  1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
), nrow=32, ncol=32, byrow=TRUE)


MAP_H <- nrow(MAP)
MAP_W <- ncol(MAP)

# set
W        <- 640L
H        <- 480L
FOV      <- pi / 5  #pfield of view
NUM_RAYS <- W
MOVE_SPD <- 0.07
ROT_SPD  <- 0.045

px <- 8.5
py <- 7.5
pa <- 0.4   # angle in radians (0 = East)

keys <- list(w=FALSE, s=FALSE, a=FALSE, d=FALSE, q=FALSE, e=FALSE)

shade_col <- function(r, g, b, dist) {
  f <- max(0.08, 1 - dist / 10)
  sprintf("#%02X%02X%02X",
          as.integer(r * f),
          as.integer(g * f),
          as.integer(b * f))
}

wall_col <- function(side, dist) {
  if (side == 0L) shade_col(180, 140, 100, dist)
  else            shade_col(130, 100,  70, dist)
}

art_col <- function(side, dist) {
  if (side == 0L) shade_col(160,  80, 160, dist)
  else            shade_col(110,  50, 110, dist)
}

accent_col <- function(side, dist) {
  if (side == 0L) shade_col(60, 180, 160, dist)
  else            shade_col(40, 130, 115, dist)
}



# dda rayacster
raycast <- function() {
  half_fov <- FOV / 2
  results  <- vector("list", NUM_RAYS)

  for (i in seq_len(NUM_RAYS)) {
    ray_angle <- pa - half_fov + FOV * (i - 1L) / NUM_RAYS
    rdx <- cos(ray_angle)
    rdy <- sin(ray_angle)

    map_x <- as.integer(px)
    map_y <- as.integer(py)

    delta_x <- if (rdx == 0) 1e30 else abs(1 / rdx)
    delta_y <- if (rdy == 0) 1e30 else abs(1 / rdy)

    if (rdx < 0) { step_x <- -1L; side_x <- (px - map_x) * delta_x }
    else         { step_x <-  1L; side_x <- (map_x + 1 - px) * delta_x }
    if (rdy < 0) { step_y <- -1L; side_y <- (py - map_y) * delta_y }
    else         { step_y <-  1L; side_y <- (map_y + 1 - py) * delta_y }

    side      <- 0L
    hit       <- FALSE
    wall_type <- 1L

    for (step in 1:64) {
      if (side_x < side_y) {
        side_x <- side_x + delta_x; map_x <- map_x + step_x; side <- 0L
      } else {
        side_y <- side_y + delta_y; map_y <- map_y + step_y; side <- 1L
      }
      if (map_x >= 1L && map_x <= MAP_W && map_y >= 1L && map_y <= MAP_H) {
        cell <- MAP[map_y, map_x]
        if (cell > 0L) { hit <- TRUE; wall_type <- cell; break }
      }
    }

    if (!hit) { results[[i]] <- list(dist=100, side=0L, wall_type=1L); next }

    perp_dist <- if (side == 0L)
      (map_x - px + (1 - step_x) / 2) / rdx
    else
      (map_y - py + (1 - step_y) / 2) / rdy

    results[[i]] <- list(dist=max(0.1, perp_dist), side=side, wall_type=wall_type)
  }
  results
}



# ── frame with tckl()
draw_frame <- function(cv) {
  cpath <- as.character(cv)

  tcl(cv, "delete", "all")

  # strop
  tcl(cv, "create", "rectangle", 0, 0, W, H %/% 2L,   "-fill", "#1A1A2E", "-outline", "")

  # podn
  tcl(cv, "create", "rectangle", 0, H %/% 2L, W, H, "-fill", "#2C1E10", "-outline", "")

  rays  <- raycast()
  col_w <- max(1L, as.integer(W / NUM_RAYS))

  for (i in seq_len(NUM_RAYS)) {
    r       <- rays[[i]]
    wall_h  <- as.integer(H / r$dist)
    y_start <- as.integer((H - wall_h) / 2L)
    y_end   <- y_start + wall_h
    x0      <- (i - 1L) * col_w
    x1      <- x0 + col_w
    col     <- if (r$wall_type == 2L) art_col(r$side, r$dist)
    else                   wall_col(r$side, r$dist)
    # ostalo
    tcl(cv, "create", "rectangle", x0, y_start, x1, y_end,  "-fill", col, "-outline", "")
  }

# zemljevid (left cortner top)
  mm <- 4L
  for (row in 1:MAP_H) {
    for (col in 1:MAP_W) {
      fill <- if (MAP[row, col] == 1L) "#666666"
      else if (MAP[row, col] == 2L) "#AA55AA"
      else "#222222"
      tcl(cv, "create", "rectangle", (col-1)*mm, (row-1)*mm, col*mm, row*mm, "-fill", fill, "-outline", "")
    }
  }

  #jst na mapi
  pdx <- (px - 0.5) * mm
  pdy <- (py - 0.5) * mm
  tcl(cv, "create", "oval", pdx-2, pdy-2, pdx+2, pdy+2, "-fill", "#FF4444", "-outline", "")

   tcl(cv, "create", "text", 10, H - 8, "-anchor", "sw", "-fill", "#AAAAAA", "-font", "Courier 8",
      "-text", "Kurzor=Premikanje  ESC=Izhod")

}

# gameloop
update_game <- function() {
  new_px <- px
  new_py <- py

  if (keys$w) { new_px <- px + cos(pa)*MOVE_SPD; new_py <- py + sin(pa)*MOVE_SPD }
  if (keys$s) { new_px <- px - cos(pa)*MOVE_SPD; new_py <- py - sin(pa)*MOVE_SPD }
  if (keys$q) { new_px <- px + cos(pa - pi/2)*MOVE_SPD; new_py <- py + sin(pa - pi/2)*MOVE_SPD }
  if (keys$e) { new_px <- px + cos(pa + pi/2)*MOVE_SPD; new_py <- py + sin(pa + pi/2)*MOVE_SPD }
  if (keys$a) pa <<- pa - ROT_SPD
  if (keys$d) pa <<- pa + ROT_SPD

  # stena, se ustavi samo če je celica odprta
  cx <- as.integer(new_px); cy <- as.integer(py)
  if (cx >= 1 && cx <= MAP_W && cy >= 1 && cy <= MAP_H && MAP[cy, cx] == 0L)
    px <<- new_px

  cx <- as.integer(px);  cy <- as.integer(new_py)
  if (cx >= 1 && cx <= MAP_W && cy >= 1 && cy <= MAP_H && MAP[cy, cx] == 0L)
    py <<- new_py

  draw_frame(canvas)
  tcl("after", 16, update_game) #cca 60fps
}

tt     <- tktoplevel()
tktitle(tt) <- "Doorm --> Doom in R "
canvas <- tkcanvas(tt, width = W, height = H, bg = "#000000")
tkpack(canvas, fill = "both", expand = TRUE)


# kontrole
press_key <- function(K) {
  k <- as.character(K)
  if (k %in% c("w","W","Up"))    keys$w <<- TRUE
  if (k %in% c("s","S","Down"))  keys$s <<- TRUE
  if (k %in% c("a","A","Left"))  keys$a <<- TRUE
  if (k %in% c("d","D","Right")) keys$d <<- TRUE
  if (k %in% c("q","Q"))         keys$q <<- TRUE
  if (k %in% c("e","E"))         keys$e <<- TRUE
  if (k == "Escape")             tkdestroy(tt)
}

release_key <- function(K) {
  k <- as.character(K)
  if (k %in% c("w","W","Up"))    keys$w <<- FALSE
  if (k %in% c("s","S","Down"))  keys$s <<- FALSE
  if (k %in% c("a","A","Left"))  keys$a <<- FALSE
  if (k %in% c("d","D","Right")) keys$d <<- FALSE
  if (k %in% c("q","Q"))         keys$q <<- FALSE
  if (k %in% c("e","E"))         keys$e <<- FALSE
}

tkbind(tt, "<KeyPress>",   function(K) press_key(K))
tkbind(tt, "<KeyRelease>", function(K) release_key(K))

tkfocus(tt)
update_game()
tkwait.window(tt)


## ~/Documents/tomaztk_github/Useless_R_functions/functions/Doom_Stuff.R


