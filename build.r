meta = c("Sweden", "Riksdag")
mode = "fruchtermanreingold"

for(l in rev(unique(m$legislature))) {
  
  data = subset(m, legislature == l & n_au > 1)
  cat("Legislature", l, ":", nrow(data), "cosponsored bills, ")
  
  rownames(s) = gsub("\\D", "", s$url)
  
  # check for missing sponsors
  # u = unlist(strsplit(data$authors, ";"))
  # u = na.omit(u[ !u %in% gsub("\\D", "", s$url) ])
  # if(length(u)) {
  #   u = table(u)
  #   cat("Missing", length(u), "sponsors", sum(u), "mentions:\n")
  #   print(u)
  # }
    
  #
  # directed edge list
  #
  
  edges = rbind_all(lapply(data$authors, function(i) {
    
    w = unlist(strsplit(i, ";"))
    
    d = s[ w, "uid" ]
    d = expand.grid(i = d, j = d[ 1 ], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))
  
  #
  # edge weights
  #
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
  
  cat(nrow(edges), "edges, ")
  
  # small bug affecting 9 edge in 1998-2002
  # caused by special character in sponsor name (Carl-Erik Skårman)
  if(any(edges$gsw > 1)) {
    
    cat("dropped", length(edges$gsw[ edges$gsw > 1 ]), "with gsw > 1, ")
    edges = subset(edges, gsw <= 1)
    
  }
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$date, 1, 4))),
                                        collapse = " to "))
  
  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(m, legislature == l)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")

  rownames(s) = s$uid
  
  n %v% "url" = as.character(s[ network.vertex.names(n), "url" ])
  n %v% "sex" = as.character(s[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(s[ network.vertex.names(n), "born" ])
  n %v% "party" = as.character(s[ network.vertex.names(n), "party" ])
  n %v% "partyname" = as.character(s[ network.vertex.names(n), "partyname" ])
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  s$nyears = sapply(s$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) < substr(l, 1, 4))
  })
  n %v% "nyears" = as.numeric(s[ network.vertex.names(n), "nyears" ])
  n %v% "constituency" = as.character(s[ network.vertex.names(n), "county" ])
  n %v% "photo" = as.character(s[ network.vertex.names(n), "photo" ])
  
  print(table(n %v% "nyears"))

  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  #
  # weighted measures
  #
  
  n = get_modularity(n, weights = "raw")
  n = get_modularity(n, weights = "nfw")
  n = get_modularity(n, weights = "gsw")
  
  n = get_centrality(n, weights = "raw")
  n = get_centrality(n, weights = "nfw")
  n = get_centrality(n, weights = "gsw")
  
  #
  # network plot
  #
    
  if(plot) {
    
    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
    
    ggnet_save(n, file = paste0("plots/net_se"),
               i = colors[ s[ n %e% "source", "party" ] ],
               j = colors[ s[ n %e% "target", "party" ] ],
               q, colors, order)
    
  }
  
  #
  # save objects
  #
  
  # clean up vertex names from uid number
  network.vertex.names(n) = gsub("\\s\\d+", "", network.vertex.names(n))
  set.edge.attribute(n, "source", gsub("\\s\\d+", "", n %e% "source"))
  set.edge.attribute(n, "target", gsub("\\s\\d+", "", n %e% "target"))
  
  assign(paste0("net_se", substr(l, 1, 4)), n)
  assign(paste0("bills_se", substr(l, 1, 4)), data)
  assign(paste0("edges_se", substr(l, 1, 4)), edges)
  
  #
  # export gexf
  #
  
  if(gexf)
    get_gexf(paste0("net_se", l), n, meta, mode, colors, extra = "constituency")

}

save(list = ls(pattern = "^(net|edges|bills)_se\\d{4}$"), file = "data/net_se.rda")

if(gexf)
  zip("net_se.zip", dir(pattern = "^net_se\\d{4}-\\d{4}\\.gexf$"))
