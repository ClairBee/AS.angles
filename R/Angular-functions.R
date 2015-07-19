
#' Get angle from each point to its k nearest neighbours
#'
#' For each point in the set, uses \code{knnx.index} to obtain the k nearest neighbours, then applies \code{atan2} to get the angle between them.
#' @param pts A two-column matrix containing the coordinates of the points to be compared.
#' @param k Number of neighbours to be included for each point.
#' @return A (k+2)-column matrix; the first two columns contain each point's x and y coordinates, the remainder containing the angles found
#' @export
#' @examples
#' k.2 <- k.nearest.angles(p, 2)
k.nearest.angles <- function(pts, k) {
    
    nn <- knn.index(pts, k = k)
    angles <- matrix(NA, nrow = nrow(pts), ncol = k)
    for (i in 1:nrow(pts)) {
        for (j in 1:k) {
            diff.x <- pts[nn[i,j], 1] - pts[i, 1]
            diff.y <- pts[nn[i,j], 2] - pts[i, 2]
            angles[i, j] <- atan2(diff.y, diff.x)
        }
    }
    cbind(pts, angles)
}


#' Display angles between nearest neighbours
#'
#' Plot points & display the angle to each point's nearest neighbours
#' @param angles A matrix containing x and y coordinates in its first column, and angles to be plotted around each point in the remaining columns.
#' @param l Length of lines to be plotted
#' @export
#' @examples
#' show.directions(k.2)
show.directions <- function(angles, l = 0.01) {
    plot(angles[,1:2], col = "lightgrey", pch = 20, cex = 0.5, asp = T)
    
    for (i in 1:nrow(angles)) {
        for (j in 3:ncol(angles)) {
            if (!is.na(angles[i,j])) {
                x.end <- angles[i,1] + (cos(angles[i,j]) * l)
                y.end <- angles[i,2] + (sin(angles[i,j]) * l)
                segments(angles[i,1], angles[i,2], x.end, y.end)
            }
        }
    }
}


#' Extract angles between all points in a certain radius
#'
#' Takes a set of x, y coordinates and, using \code{atan2}, returns a matrix of the angles from each point defined to all points within a specified radoius. (If no radius is specified, the angles from each point to every other point are returned)
#' @param pts A two-column matrix containing the coordinates of the points to be compared.
#' @param radius The radius (in the coordinate system of the plan) defining the area around each point within which the angles will be calculated.
#' @return A (n x n+2) matrix containing the x, y coordinates in its first two columns, and the angles measured to every other point in the remaining columns. If no angle was measured, NA is returned in the appropriate cell.
#' @export
#' @examples
#' r0.2 <- pointwise.angles.atan2(p, radius = 5)
pointwise.angles.atan2 <- function(pts, radius) {
    if(missing(radius)) {
        x.r <- max(p[,1]) - min(p[,1])
        y.r <- max(p[,2]) - min(p[,2])
        radius <- sqrt(x.r^2 + y.r^2)
    }
    r <- radius^2
    
    angles <- matrix(nrow = nrow(pts), ncol = nrow(pts))
    for (i in 1:nrow(pts)) {
        for (j in 1:nrow(pts)) {
            if (j == i) {angles[i,j] <- NA}
            else {
                diff.x <- pts[j, 1] - pts[i, 1]
                diff.y <- pts[j, 2] - pts[i, 2]
                
                # if distance is greater than specified radius, ignore
                if (diff.x^2 + diff.y^2 <= r) 
                {angles[i,j] <- atan2(diff.y, diff.x)}
                else {angles[i,j] <- NA}
            }
        }
    }
    cbind(pts,angles)
}


#' Highlight a given point and all points within a certain radius
#'
#' All points in the set are plotted; the specified point is highlighted in red, and all points to which angles were measured are highlighted in blue. Allows the user to assess the number of points measured by \code{pointwise.angles.atan2} for diffrerent radii.
#' @param angles A matrix containing x and y coordinates in its first column, and angles to be plotted around each point in the remaining columns.
#' @param n The index (row number) of the point to be plotted.
#' @export
#' @examples
#' show.selected(r0.2, n = 0)
show.selected <- function(angles, n = 1) {
    plot(angles[, 1:2], pch = 20, cex = 0.5, asp = T)
    points(angles[n, 1:2], col = "red", pch = 16)
    points(angles[!is.na(angles[n,-c(1,2)]),1:2], col = "blue")
}