extractMovements <- function(res, offset, threshold = 0.6, days = 2, plot = TRUE, na.break = 3, exclude = NULL) {
  
  twl <- do.call("rbind", res[c(1,2)])[order(do.call("rbind", res[c(1,2)])[,1]),c(1:2)]
  
  if(length(threshold)==1) threshold <- rep(threshold, 4)
  threshold[is.na(threshold)] <- -1
  
  if(plot) opar <- par(mfrow = c(4,1), mar = c(2,3,1,4), oma = c(3,3,1,3))
  
  if(plot) layout(matrix(c(1:10), ncol = 2, byrow = T), widths = c(0.8, 0.2))
  
  ind1 <- apply(res[[1]][,c(-c(1:2))], 1, function(x) all(x<=threshold[1]))
  
  if(plot) {
    plot(res[[1]]$Twilight, hourOffset(as.hour(res[[1]]$Twilight), offset%%24), pch = 21, 
         bg = adjustcolor("firebrick", alpha.f = 0.2), yaxt = "n", ylab = "")
    points(res[[1]]$Twilight[ind1], hourOffset(as.hour(res[[1]]$Twilight), offset%%24)[ind1], pch = 16, col = "firebrick")
    
    par(new = TRUE)
    plot(res[[1]]$Twilight, rep(1, nrow(res[[1]])), type = "n", ylim = c(0,1), las = 1, xaxt = "n", yaxt = "n", ylab = "")
    abline(h = threshold[1], lty = 2, col = adjustcolor("grey80", alpha.f = 0.8))
    segments(res[[1]]$Twilight, apply(res[[1]][,c(-c(1:2))], 1, min, na.rm = T),
             res[[1]]$Twilight, apply(res[[1]][,c(-c(1:2))], 1, max, na.rm = T), col = adjustcolor("grey50", alpha.f = 0.5))
    points(res[[1]]$Twilight,   apply(res[[1]][,c(-c(1:2))], 1, max, na.rm = T), pch = 16, cex = 0.4)
    axis(4, las = 1)
  }
  
  h <- hist(do.call("c", res[[1]][,c(-c(1:2))]), breaks = seq(0, 1, length = 11), plot = FALSE)
  if(plot)barplot(h$counts, horiz = T, col = ifelse(h$mids<=threshold[1], "firebrick", "grey90"))
  
  ind2 <- apply(res[[2]][,c(-c(1:2))], 1, function(x) all(x<=threshold[2]))
  
  if(plot) {
    plot(res[[2]]$Twilight, hourOffset(as.hour(res[[2]]$Twilight), offset%%24), pch = 21, 
         bg = adjustcolor("cornflowerblue", alpha.f = 0.2), yaxt = "n", ylab = "")
    points(res[[2]]$Twilight[ind2], hourOffset(as.hour(res[[2]]$Twilight), offset%%24)[ind2], pch = 16, col = "cornflowerblue")
    
    par(new = TRUE)
    plot(res[[2]]$Twilight, rep(1, nrow(res[[2]])), type = "n", ylim = c(0,1), las = 1, xaxt = "n", yaxt = "n", ylab = "")
    abline(h = threshold[2], lty = 2, col = adjustcolor("grey80", alpha.f = 0.8))
    segments(res[[2]]$Twilight, apply(res[[2]][,c(-c(1:2))], 1, min, na.rm = T),
             res[[2]]$Twilight, apply(res[[2]][,c(-c(1:2))], 1, max, na.rm = T), col = adjustcolor("grey50", alpha.f = 0.5))
    points(res[[2]]$Twilight,   apply(res[[2]][,c(-c(1:2))], 1, max, na.rm = T), pch = 16, cex = 0.4)
    axis(4, las = 1)
  }
  h <- hist(do.call("c", res[[2]][,c(-c(1:2))]), breaks = seq(0, 1, length = 11), plot = FALSE)
  if(plot) barplot(h$counts, horiz = T, col = ifelse(h$mids<=threshold[2], "firebrick", "grey90"))
  
  
  ind3 <- apply(res[[3]][,-1], 1, function(x) all(x<=threshold[3]))
  
  if(plot) {
    plot(res[[3]]$Time, hourOffset(as.hour(res[[3]]$Time)), pch = 21, 
         bg = adjustcolor("orange", alpha.f = 0.2), yaxt = "n", ylab = "")
    points(res[[3]]$Time[ind3], hourOffset(as.hour(res[[3]]$Time))[ind3], pch = 16, col = "orange")
    par(new = TRUE)
    plot(res[[3]]$Time, rep(1, nrow(res[[3]])), type = "n", ylim = c(0,1), las = 1, xaxt = "n", yaxt = "n", ylab = "")
    abline(h = threshold[3], lty = 2, col = adjustcolor("grey80", alpha.f = 0.8))
    segments(res[[3]]$Time, apply(res[[3]][,c(-c(1))], 1, min, na.rm = T),
             res[[3]]$Time, apply(res[[3]][,c(-c(1))], 1, max, na.rm = T), col = adjustcolor("grey50", alpha.f = 0.5))
    points(res[[3]]$Time,   apply(res[[3]][,c(-c(1))], 1, max, na.rm = T), pch = 16, cex = 0.4)
    axis(4, las = 1)
  }
  
  h <- hist(do.call("c", res[[3]][,-1]), breaks = seq(0, 1, length = 11), plot = FALSE)
  if(plot) barplot(h$counts, horiz = T, col = ifelse(h$mids<=threshold[3], "firebrick", "grey90"))
  
  
  
  ind4 <- apply(res[[4]][,-1], 1, function(x) all(x<=threshold[4]))
  if(plot) {
    plot(res[[4]]$Time, hourOffset(as.hour(res[[4]]$Time+15*60)), pch = 21, 
         bg = adjustcolor("darkgreen", alpha.f = 0.2), yaxt = "n", ylab = "")
    points(res[[4]]$Time[ind4], hourOffset(as.hour(res[[4]]$Time))[ind4], pch = 16, col = "darkgreen")
    par(new = TRUE)
    plot(res[[4]]$Time, rep(1, nrow(res[[4]])), type = "n", ylim = c(0,1), las = 1, xaxt = "n", yaxt = "n", ylab = "")
    abline(h = threshold[4], lty = 2, col = adjustcolor("grey80", alpha.f = 0.8))
    segments(res[[4]]$Time, apply(res[[4]][,c(-c(1))], 1, min, na.rm = T),
             res[[4]]$Time, apply(res[[4]][,c(-c(1))], 1, max, na.rm = T), col = adjustcolor("grey50", alpha.f = 0.5))
    points(res[[4]]$Time,   apply(res[[4]][,c(-c(1))], 1, max, na.rm = T), pch = 16, cex = 0.4)
    axis(4, las = 1)
  }
  
  h <- hist(do.call("c", res[[4]][,-1]), breaks = seq(0, 1, length = 11), plot = FALSE)
  if(plot) barplot(h$counts, horiz = T, col = ifelse(h$mids<=threshold[4], "firebrick", "grey90"))
  
  if(!is.null(na.break)) {
    ind5.0 <- which(diff(as.numeric(twl$Twilight)/60/60)>median(diff(as.numeric(twl$Twilight)/60/60))*na.break)+c(0, 1)
    ind5 <- rep(FALSE, nrow(twl)); ind5[ind5.0] <- TRUE
  } else ind5 <- rep(FALSE, length = nrow(res$Noon))
  
  tm <- as.POSIXct(c(as.numeric(min(twl$Twilight))-500, as.numeric(max(twl$Twilight))+500,                  ## Start/End
                     as.numeric(res[[1]]$Twilight[ind1])-500, as.numeric(res[[2]]$Twilight[ind2])-500,      ## Rise/Set 
                     as.numeric(res[[3]]$Time[ind3])-17*60*60, as.numeric(res[[4]]$Time[ind4])-17*60*60,    ## Moon/Noon
                     as.numeric(twl$Twilight[ind5])+ c(+500, -500)) ,                                      
                   origin = "1970-01-01", tz = "GMT")
  
  gr <- cut(as.numeric(twl[,1]), unique(as.numeric(tm[order(tm)])), labels = FALSE)
  
  if(is.numeric(exclude)) {
    
    tmp <- split(data.frame(twl$Twilight, gr), f = gr)
    gr_tmp <- as.vector(unlist(lapply(tmp, function(x) {
      if(nrow(x)>1 & nrow(x)>exclude*2) {
        tmp01 <- x[,2] 
        tmp01[c(1:exclude, (nrow(x)-exclude+1):nrow(x))] <- NA
        tmp01
      } else {x[,2]}
    })))
    
    gr <- na.approx(gr_tmp, rule = 3)
  }
  
  
  tbl <- as.data.frame(table(gr))
  
  gr[gr%in%tbl[tbl[,2]<=(days*2),1]] <- NA
  
  out <- c(1)
  for(i in 2:nrow(twl)) out <- c(out, ifelse(is.na(gr[i]) | (!is.na(gr[i]) & is.na(gr[i-1])) | gr[i]!=gr[i-1], out[i-1]+1, out[i-1]))
  
  if(plot) cols <- rainbow(max(out, na.rm = T))[sample(1:max(out, na.rm = T))]
  
  if(plot) {
    plot(twl$Twilight,  hourOffset(as.hour(twl$Twilight)), 
         type  = "n", xlab = "", ylab = "", yaxt = "n")
    r <- range(hourOffset(as.hour(twl$Twilight)))
    for(i in unique(out[!is.na(out)])){
      if(length(twl$Twilight[!is.na(out) & out==i])>1) {
        rect(min(twl$Twilight[!is.na(out) & out==i]), r[1], 
             max(twl$Twilight[!is.na(out) & out==i]), r[2], lty = 2, col = "grey90", border = "grey40") 
      }
    }
    points(twl$Twilight,  
           hourOffset(as.hour(twl$Twilight)), 
           pch = 21, bg = ifelse(out%in%as.data.frame(table(out))[which(as.data.frame(table(out))[,2]==1),1], "grey99", cols[out]), yaxt = "n")
    
    par(opar)
  }
  
  out
}

validGroup <- function(twl, gr, calib, alpha, beta, ...) {
  
  
  path <- thresholdPath(twl$Twilight, twl$Rise, zenith = calib[2]-3) 
  x0 <- path$x
  
  x0 <- cbind(tapply(path$x[,1],gr,median), 
              tapply(path$x[,2],gr,median))
  
  z0 <- trackMidpts(x0)
  
  
  model <- groupedThresholdModel(twl$Twilight,
                                 twl$Rise,
                                 group = gr,
                                 twilight.model = "ModifiedGamma",
                                 alpha = alpha,
                                 beta =  beta,
                                 x0 = x0,
                                 z0 = z0,
                                 zenith = calib[2])
  
  x.proposal <- mvnorm(S = diag(c(0.005, 0.005)), n = nrow(x0))
  z.proposal <- mvnorm(S = diag(c(0.005, 0.005)), n = nrow(z0))
  
  # Fit the model
  fit <- estelleMetropolis(model, x.proposal, z.proposal, iters = 10, thin = 20, verbose = F)
  
  
  
  x0_fit <- chainLast(fit$x)
  z0_fit <- chainLast(fit$z)
  
  
  model <- groupedThresholdModel(twl$Twilight,
                                 twl$Rise,
                                 group = gr,
                                 twilight.model = "Gamma",
                                 alpha = alpha,
                                 beta =  beta,
                                 x0 = x0,
                                 z0 = z0,
                                 zenith = calib[2])
  
  x.proposal <- mvnorm(chainCov(fit$x), s = 0.3)
  z.proposal <- mvnorm(chainCov(fit$z), s = 0.3)
  fit <- tryCatch(estelleMetropolis(model, x.proposal, z.proposal, x0 = chainLast(fit$x),
                                    z0 = chainLast(fit$z), iters = 300, thin = 20, verbose = FALSE), error = function(e) NULL)
  !is.null(fit)
}

landMask <- function(xlim, ylim, n = 2,index) {
  
  data(wrld_simpl, package = "maptools")
  
  # create empty raster with desired resolution
  r = raster(nrows = n * diff(ylim), ncols = n * diff(xlim), xmn = xlim[1],
             xmx = xlim[2], ymn = ylim[1], ymx = ylim[2], crs = proj4string(wrld_simpl))
  
  # create a raster for the stationary period, in this case by giving land a value of 1
  rs = rasterize(wrld_simpl, r, 1, silent = TRUE)
  rs[is.na(rs)] <- 0
  # make the movement raster the same resolution as the stationary raster, but allow the bird to go anywhere by giving all cells a value of 1
  rm = rs; rm[] = 1
  
  # stack the movement and stationary rasters on top of each other
  mask = stack(rs, rm)
  
  xbin = seq(xmin(mask),xmax(mask),length=ncol(mask)+1)
  ybin = seq(ymin(mask),ymax(mask),length=nrow(mask)+1)
  mask = as.array(mask)[nrow(mask):1,,sort(unique(index)),drop=FALSE]
  
  function(p) mask[cbind(.bincode(p[,2],ybin),.bincode(p[,1],xbin), index)]
}