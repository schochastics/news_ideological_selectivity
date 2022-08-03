# network extraction methods ----
pmi <- function(A, t = 0) {
  reach <- rowSums(A) / ncol(A)
  exp_mat <- outer(reach, reach, "*")
  W <- (A %*% t(A)) / ncol(A)
  B <- log(W / exp_mat) > t
  igraph::graph_from_adjacency_matrix(B, mode = "undirected", diag = FALSE)
}

phic <- function(A, p = 0.05) {
  D <- (A %*% t(A))
  R <- rowSums(A)
  N <- ncol(A)
  RR <- suppressWarnings(outer(R, R, "*"))
  Phi <- (D * N - RR) / sqrt(diag(N - R) %*% RR %*% diag(N - R))
  tmat <- Phi * suppressWarnings(sqrt(outer(R, R, "pmax")) - 2) / sqrt(1 - Phi^2)
  # we cannot use the standard values for big N because we do not have big N
  pmat <- outer(R, R, function(x, y) qt(p = p / 2, df = pmax(x, y), lower.tail = FALSE))
  igraph::graph_from_adjacency_matrix(tmat > pmat, mode = "undirected", diag = FALSE)
}

disparity1 <- function(A, p = 0.05) {
  W <- A %*% t(A)
  diag(W) <- 0
  suppressMessages(backbone::disparity(W, class = "igraph", alpha = p))
}

sdsm1 <- function(A, p = 0.05) {
  suppressMessages(backbone::sdsm(A, class = "igraph", alpha = p))
}

# create all networks from the data ----

create_networks <- function(dt, political = FALSE, weights = FALSE, fixN = FALSE, reach = 0.01, cutoff = 3) {
  n_panelist <- length(unique(dt[["panelist_id"]]))
  outlets <- unique(dt[type != "", c("panelist_id", "domain")])[, .N, by = .(domain)][N >= n_panelist * reach][["domain"]]
  n_outlets <- length(outlets)
  dt1 <- dt[domain %in% outlets]
  if (fixN) {
    peeps <- dt[type!=""& political=="political" & duration>=120]
    peeps <- unique(peeps[["panelist_id"]])
    # peeps <- unique(dt[political == "political"][, .(panelist_id)])[["panelist_id"]]
    dt1 <- dt1[panelist_id %in% peeps]
  }
  if (!political) {
    el <- dt1[duration >= cutoff, c("panelist_id", "domain")]
    el <- el[, .N, by = .(panelist_id, domain)]
    g <- netUtils::bipartite_from_data_frame(el[, c("panelist_id", "domain")], "panelist_id", "domain")
    A <- as_incidence_matrix(g)
    l1 <- pmi(A)
    l2 <- disparity1(A, p = 0.05)
    l3 <- phic(A, p = 0.05)
    l4 <- sdsm1(A, p = 0.05)
    if (weights) {
      W <- A %*% t(A)
      A1 <- as_adj(l1, sparse = FALSE)
      l1 <- graph_from_adjacency_matrix(W * A1, "undirected", weighted = "weight")

      A2 <- as_adj(l2, sparse = FALSE)
      l2 <- graph_from_adjacency_matrix(W * A2, "undirected", weighted = "weight")

      A3 <- as_adj(l3, sparse = FALSE)
      l3 <- graph_from_adjacency_matrix(W * A3, "undirected", weighted = "weight")
      
      A4 <- as_adj(l4, sparse = FALSE)
      l4 <- graph_From_adjacency_matrix(W * A4, "undirected", weighted = "weight")
    }
  } else {
    el_pol <- dt1[duration >= cutoff & political != "", c("panelist_id", "domain")]
    el_pol <- el_pol[, .N, by = .(panelist_id, domain)]
    g <- netUtils::bipartite_from_data_frame(el_pol[, c("panelist_id", "domain")], "panelist_id", "domain")
    A <- as_incidence_matrix(g)
    l1 <- pmi(A)
    l2 <- disparity1(A, p = 0.05)
    l3 <- phic(A, p = 0.05)
    l4 <- sdsm1(A, p = 0.05)
    if (weights) {
      W <- A %*% t(A)
      A1 <- as_adj(l1, sparse = FALSE)
      l1 <- graph_from_adjacency_matrix(W * A1, "undirected", weighted = "weight")

      A2 <- as_adj(l2, sparse = FALSE)
      l2 <- graph_from_adjacency_matrix(W * A2, "undirected", weighted = "weight")

      A3 <- as_adj(l3, sparse = FALSE)
      l3 <- graph_from_adjacency_matrix(W * A3, "undirected", weighted = "weight")
      
      A4 <- as_adj(l4, sparse = FALSE)
      l4 <- graph_From_adjacency_matrix(W * A4, "undirected", weighted = "weight")
    }
  }
  if (vcount(l2) < vcount(l1)) {
    idx <- which(!V(l1)$name %in% V(l2)$name)
    l2 <- add.vertices(l2, length(idx), attr = list(name = V(l1)$name[idx]))
  }
  if (vcount(l3) < vcount(l1)) {
    idx <- which(!V(l1)$name %in% V(l3)$name)
    l3 <- add.vertices(l3, length(idx), attr = list(name = V(l1)$name[idx]))
  }
  if (vcount(l4) < vcount(l1)) {
    idx <- which(!V(l1)$name %in% V(l4)$name)
    l4 <- add.vertices(l4, length(idx), attr = list(name = V(l1)$name[idx]))
  }
  
  list(pmi = l1, disparity = l2, phi = l3,sdsm = l4)
}

#population sd as used by Fletcher et al 2020
psd <- function(x,n){
  if(!missing(n)){
    stopifnot(length(n)==length(x))
    x <- rep(x,n)
  }
  mx <- mean(x)
  sqrt(mean((x-mx)^2))
}