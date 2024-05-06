library(Rcpp)
library(viridis)
library(geoR)
library(CircStats)
sourceCpp("funs.cpp")
load("ppws-mask.RData")
load("ppws.RData")

image_xyz <- function(x, y, z, ...){
    u.x <- sort(unique(x))
    u.y <- sort(unique(y))
    z.mat <- squarify(cbind(x, y), z)
    image(u.x, u.y, z.mat, ..., xlab = "x", ylab = "y")
}

calc.dists <- function(points1, points2){
    apply(points1, 1, function(x) sqrt((x[1] - points2[, 1])^2 + (x[2] - points2[, 2])^2))
}

calc.bearings <- function(points1, points2){
    x.diff <- apply(points2, 1, function(x) x[1] - points1[, 1])
    y.diff <- apply(points2, 1, function(x) x[2] - points1[, 2])
    out <- atan(x.diff/y.diff)
    out[y.diff < 0] <- out[y.diff < 0] + pi
    out[y.diff >= 0 & x.diff < 0] <- out[y.diff >= 0 & x.diff < 0] + 2*pi
    out
}

which.closest <- function(locs){
    d <- calc.dists(locs, ppws)
    apply(d, 2, function (x) which(x == min(x))[1])
}

measure.covariates <- function(skip.wait = FALSE){
    z <- rep(0, nrow(ppws))
    cols <- "grey"
    n.locs <- 24
    happy <- FALSE
    while (!happy){
        message(paste("\nClick on the map to select", n.locs, "locations at which to measure spatial covariates.\n"))
        image_xyz(ppws[, 1], ppws[, 2], z, asp = 1, zlim = c(0, 1), col = cols)
        locs <- matrix(0, nrow = n.locs, ncol = 2)
        for (i in 1:n.locs){
            l <- locator(1)
            locs[i, ] <- c(l$x, l$y)
            points(locs[i, , drop = FALSE], pch = 16)
        }
        h <- readline("Are you happy with these selections? \n\nType 'y' to deploy your team to collect data. Type 'n' to reselect.\n")
        if (h == "yes" | h == "y" | h == "Y" | h == "Yes"){
            happy <- TRUE
        }
    }
    if (!skip.wait){
        message("Flying to Cambodia...")
        pb.flight <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.1)
            setTxtProgressBar(pb.flight, i)
        }
        message("\nHiking to Phnom Prich Wildlife Sanctuary...")
        pb.hike <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.hike, i)
        }
        message("\nDeploying fieldworkers...")
        pb.field <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.025)
            setTxtProgressBar(pb.field, i)
        }
        message("\nTaking elevation measurements...")
        pb.elevation <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.01)
            setTxtProgressBar(pb.elevation, i)
        }
        message("\nMeasuring canopy...")
        pb.canopy <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.02)
            setTxtProgressBar(pb.canopy, i)
        }
        message("\nIdentifying tree species...")
        pb.tree <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.03)
            setTxtProgressBar(pb.tree, i)
        }
        message("\n")
    }
    closest.points <- which.closest(locs)
    canopy.height <- canopy.height.ppws[closest.points]
    elevation <- elevation.ppws[closest.points]
    forest.type <- forest.type.ppws[closest.points]
    data.frame(x = locs[, 1], y = locs[, 2], canopy.height = canopy.height,
               elevation = elevation, forest.type = forest.type)
}

conduct.survey <- function(skip.wait = FALSE){
    z <- rep(0, nrow(ppws))
    cols <- "grey"
    n.sessions <- 18
    happy <- FALSE
    while (!happy){
        message(paste("\nClick on the map to select", n.sessions,
                      "locations at which to deploy clusters of three listening posts.\n"))
        image_xyz(ppws[, 1], ppws[, 2], z, asp = 1, zlim = c(0, 1), col = cols)
        points(villages.ppws, pch = 16)
        legend("topleft", legend = "Village", pch = 16)
        traps <- vector(mode = "list", length = n.sessions)
        for (i in 1:n.sessions){
            l <- locator(1)
            traps[[i]] <- cbind(x = c(l$x - 1000, l$x, l$x + 1000), y = rep(l$y, 3))
            points(traps[[i]], pch = 4)
        }
        h <- readline("Are you happy with these selections? \n\nType 'y' to deploy your team to conduct acoustic surveys. Type 'n' to reselect.\n")
        if (h == "yes" | h == "y" | h == "Y" | h == "Yes"){
            happy <- TRUE
        }
    }
    out <- sim.det(s.ppws, traps, c(20, 500, 50))
    if (!skip.wait){
        message("\nRedeploying fieldworkers...")
        pb.field <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.025)
            setTxtProgressBar(pb.field, i)
        }
        message("\nListening for gibbons...")
        pb.listen <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.listen, i)
        }
        message("\nMatching detections to calls...")
        pb.match1 <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.match1, i)
        }
        message("\nMatching calls to gibbon groups...")
        pb.match2 <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.match2, i)
        }
        message("\nCollating detection data...")
        pb.collate <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.01)
            setTxtProgressBar(pb.collate, i)
        }
        message("\nFlying home...")
        pb.home <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.1)
            setTxtProgressBar(pb.home, i)
        }
        message("\nSnuggling chickens...")
        pb.chicken <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(ifelse(i < 5, 1, 0.05))
            setTxtProgressBar(pb.chicken, i)
        }
        message("\n")
    }
    out
}

sim.pop <- function(df, D.calc){
    mask <- ppws
    ## Calculating density for every mask cell.
    D <- D.calc(df)
    ## Number of cells in the mask.
    n.cells <- nrow(mask)
    ## Width of each cell, assuming they're in a grid and the first
    ## two are adjacent.
    cell.width <- max(abs(c(mask[2, 1] - mask[3, 1], mask[2, 2] - mask[3, 2])))
    ## Cell area in square metres.
    cell.area <- cell.width^2
    ## Cell area in hectares.
    cell.area.ha <- cell.area/10000
    ## Expected number of animals in each cell.
    exp.n.per.cell <- D*cell.area.ha
    ## Simulating number of animals in each cell.
    n.per.cell <- rpois(n.cells, exp.n.per.cell)
    ## Animal locations, at first in the centre of each cell.
    s <- cbind(rep(mask[, 1], n.per.cell), rep(mask[, 2], n.per.cell))
    ## Total number of animals.
    n.animals <- nrow(s)
    ## Jittering so that the locations are uniform within cells.
    s[, 1] <- s[, 1] + runif(n.animals, -cell.width/2, cell.width/2)
    s[, 2] <- s[, 2] + runif(n.animals, -cell.width/2, cell.width/2)
    s
}

sim.det <- function(s, traps, pars){
    ## Number of traps.
    n.sessions <- length(traps)
    ## Number of sessions.
    n.traps <- sapply(traps, nrow)
    ## Number of animals.
    n.animals <- nrow(s)
    ## Extracting parameters.
    lambda0 <- pars[1]
    sigma <- pars[2]
    kappa <- pars[3]
        ## Distances between animals and traps.
    dists <- lapply(traps, function(x) calc.dists(s, x))
    ## Bearings between animals and traps.
    bearings <- lapply(traps, function(x) t(calc.bearings(x, s)))
    ## Creating capture histories.
    captures <- vector(mode = "list", length = n.sessions)
    for (i in 1:n.sessions){
        det.probs <- 1 - exp(-lambda0*exp(-dists[[i]]^2/(2*sigma^2)))
        capt.sess <- matrix(rbinom(n.animals*n.traps[i], 1, det.probs),
                            nrow = n.animals, byrow = TRUE)
        bearing.sess <- matrix(0, nrow = n.animals, ncol = n.traps[i])
        dets <- which(capt.sess == 1, arr.ind = TRUE)
        n.dets <- nrow(dets)
        session.sess <- rep(i, n.dets)
        occasion.sess <- rep(1, n.dets)
        ID.sess <- trap.sess <- bearing.sess <- numeric(n.dets)
        for (j in seq_len(nrow(dets))){
            ID.sess[j] <- dets[j, 1]
            trap.sess[j] <- dets[j, 2]
            bearing.sess[j] <- rvm(1, bearings[[i]][dets[j, 1], dets[j, 2]],
                                   kappa)
        }
        detected <- apply(capt.sess, 1, function(x) any(x == 1))
        captures[[i]] <- data.frame(session = session.sess,
                                    ID = ID.sess,
                                    occasion = occasion.sess,
                                    trap = trap.sess,
                                    bearing = bearing.sess)
    }
    captures <- do.call(rbind, captures)
    list(captures = captures, traps = traps)
}

plot.cov <- function(df, cov.name){
    locs <- data.frame(df$x, df$y)
    if (nrow(df) < nrow(ppws)){
        closest.points <- which.closest(locs)
        cov <- rep(NA, nrow(ppws))
        cov[closest.points] <- df[[cov.name]]
    } else {
        cov <- df[[cov.name]]
    }
    if (is.character(cov)){
        cov <- as.numeric(as.factor(cov))
    }
    z <- rep(0, nrow(ppws))
    image_xyz(ppws[, 1], ppws[, 2], z, col = "grey", asp = 1)
    col <- viridis(20)
    image_xyz(ppws[, 1], ppws[, 2], cov, col = col, asp = 1, add = TRUE)
}

interpolate.covs <- function(df){
    data.frame(x = ppws[, 1], y = ppws[, 2], acre:::par_extend_create(loc.cov = cov.df, mask = list(ppws))$output$data$mask[, -c(1, 2)])
    
}

compare.to.truth <- function(df){
    ppar <- par(mfrow = c(3, 2))
    true.df <- data.frame(elevation = elevation.ppws,
                          canopy.height = canopy.height.ppws,
                          forest.type = forest.type.ppws)
    plot.cov(df, "elevation")
    plot.cov(true.df, "elevation")
    plot.cov(df, "canopy.height")
    plot.cov(true.df, "canopy.height")
    plot.cov(df, "forest.type")
    plot.cov(true.df, "forest.type")
    par(ppar)
}
