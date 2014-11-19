# hi Sweden

dir.create("data", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE) # delete when done (large, 1.3GB)
dir.create("plots", showWarnings = FALSE)
dir.create("photos", showWarnings = FALSE)

library(GGally)
library(grid)
library(jsonlite)
library(igraph)
library(network)
library(sna)
library(plyr)
library(rgexf)
library(stringr)
library(tnet)
library(XML)

plot = TRUE
gexf = TRUE

colors = c(
  "V" = "#E41A1C", # Vänsterpartiet, red
  "MP" = "#4DAF4A", # Miljöpartiet, green
  "PP" = "#984EA3", # Piratpartiet, purple
  "S" = "#F781BF", # Socialdemokraterna, pink
  "C" = "#A65628", # Centerpartiet, agrarian, brown
  "M" = "#FF7F00", # Moderaterna, orange
  "KD" = "#377EB8", # Kristdemokraterna, blue
  "NYD" = "#FFFF33", # Ny Demokrati, yellow
  "FP" = "#80B1D3", # Folkpartiet, light blue (light orange: #FDB462)
  "SD" = "#444444", # Sverigedemokraterna, far-right, dark grey
  "-" = "#AAAAAA" # unaffiliated (William Petzäll), light grey
)
order = names(colors)

# MP bills

file = "data/motioner.csv"
if(!file.exists(file)) {

  years = c("2010-2013", "2006-2009", "2002-2005", "1998-2001", "1990-1997")
  
  for(i in years) {
    f = paste0("data/mot-", i, ".json.zip")
    if(!file.exists(f))
      download.file(paste0("http://data.riksdagen.se/dataset/dokument/mot-", i, ".json.zip"),
                    f, mode = "wb")
  }
  
  for(i in dir("data", "json.zip$", full.names = TRUE))
    unzip(i, exdir = "raw")
  
  files = dir("raw", pattern = "json$", full.names = TRUE)
  
  m = data.frame()
  
  for(i in rev(files)) {
    
    d = fromJSON(i, flatten = TRUE)$dokumentstatus
    
    m = rbind(m, data.frame(
      uid = d$dokument$hangar_id,
      doc = d$dokument$dok_id,
      date = as.Date(d$dokument$datum),
      authors = paste0(d$dokintressent$intressent$intressent_id, collapse = ";"),
      stringsAsFactors = FALSE))
    
    if(!which(files == i) %% 1000)
      cat(which(files == i), "\n")
  
  }
  
  write.csv(m, file, row.names = FALSE)
  
}

m = read.csv(file, stringsAsFactors = FALSE)
m$n_au = 1 + str_count(m$authors, ";")

table(substr(m$doc, 1, 2)) # session years

# note: a few bills from 1990-1991 are included; see URL below for details
# http://data.riksdagen.se/sv/sa-funkar-dokument-id

m$legislature = substr(m$doc, 1, 2)
m$legislature[ m$legislature %in% c("GE", "GF", "GG", "GH") ] = "1990-1994"
m$legislature[ m$legislature %in% c("GI", "GJ", "GK", "GL") ] = "1994-1998"
m$legislature[ m$legislature %in% c("GM", "GN", "GO", "GP") ] = "1998-2002"
m$legislature[ m$legislature %in% c("GQ", "GR", "GS", "GT") ] = "2002-2006"
m$legislature[ m$legislature %in% c("GU", "GV", "GW", "GX") ] = "2006-2010"
m$legislature[ m$legislature %in% c("GY", "GZ", "H0", "H1") ] = "2010-2014"

r = unlist(strsplit(m$authors, ";"))
cat("Found", nrow(m), "bills",
    sum(m$n_au > 1), "cosponsored",
    length(r), "sponsors\n")

# sponsors

if(!file.exists("data/ledamoter.csv")) {
  
  h = htmlParse("http://data.riksdagen.se/Data/Ledamoter/Ledamoter-2010-2014/")
  h = xpathSApply(h, "//select[@name='iid']/option/@value")
  
  cat("Scraping", length(h), "recent sponsors ")

  r = unique(c(r, h[ h != "" ]))
  cat(length(r), "total missing sponsor(s)\n")
  
  s = data.frame() # initialize
  
} else {
  
  s = read.csv("data/ledamoter.csv", stringsAsFactors = FALSE)
  s = subset(s, grepl("\\d", url)) # avoid scraper bug
  
  r = unique(r[ !r %in% gsub("\\D", "", s$url) ]) # append new sponsors
  
}

if(length(r)) {
  
  cat("Scraping", length(r), "missing sponsor(s)\n")
  
  for(i in rev(r)) {
    
    cat(sprintf("%4.0f", which(i == r)), i, "")
    h = try(xmlParse(paste0("http://data.riksdagen.se/personlista/?iid=", i)))
    
    if(!"try-error" %in% class(h)) {
      
      from = min(as.numeric(substr(xpathSApply(h, "//uppdrag[roll_kod='Riksdagsledamot']/from", xmlValue), 1, 4)))
      to = max(as.numeric(substr(xpathSApply(h, "//uppdrag[roll_kod='Riksdagsledamot']/tom", xmlValue), 1, 4)))
      job = xpathSApply(h, "//uppgift[kod='en' and typ='titlar']/uppgift", xmlValue)
      name = paste(xpathSApply(h, "//tilltalsnamn", xmlValue), xpathSApply(h, "//efternamn", xmlValue))
      
      if(length(name)) {
        
        s = rbind(s, data.frame(name,
                                born = xpathSApply(h, "//fodd_ar", xmlValue),
                                sex = xpathSApply(h, "//kon", xmlValue),
                                party = xpathSApply(h, "//parti", xmlValue),
                                county = xpathSApply(h, "//valkrets", xmlValue),
                                status = xpathSApply(h, "//status[1]", xmlValue),
                                from = ifelse(is.infinite(from), NA, from),
                                to = ifelse(is.infinite(to), NA, to),
                                nyears = ifelse(is.infinite(to - from), NA, to - from),
                                job = ifelse(is.null(job), NA, job),
                                url = paste0("http://data.riksdagen.se/personlista/?iid=", i, "&utformat=html"),
                                photo = xpathSApply(h, "//bild_url_80", xmlValue),
                                stringsAsFactors = FALSE))
        
      cat(tail(s, 1)$name, "\n")
        
      } else {
        
        cat(" empty\n")
        
      }
      
    } else {
      
      cat(" failed\n")
      
    }
    
  }
  
  write.csv(s, "data/ledamoter.csv", row.names = FALSE)
  
}

s = read.csv("data/ledamoter.csv", stringsAsFactors = FALSE)

s$url = gsub("\\D", "", s$url)

# download photos
for(i in unique(s$url)) {
  photo = paste0("photos/", i, ".jpg")
  if(!file.exists(photo)) {
    try(download.file(paste0("http://data.riksdagen.se/filarkiv/bilder/ledamot/", i, "_80.jpg"),
                      photo, mode = "wb", quiet = TRUE), silent = TRUE)
  }
  # empty photos are 791 bytes
  if(!file.exists(photo) | file.info(photo)$size < 1000) {
    file.remove(photo) # will warn if missing
    s$photo[ s$url == i ] = 0
  }
}

s$nyears = s$nyears + 1
s$sex = ifelse(s$sex == "kvinna", "F", "M")

s$county = gsub("( )?, plats |(s)? (kommun|län)|\\d", "", s$county)
s$county = gsub("s norra och östra", " North+East", s$county) # Skånes
s$county = gsub("s norra", " North", s$county) # Västra Götaland
s$county = gsub("s östra", " East", s$county)
s$county = gsub("s södra", " South", s$county)
s$county = gsub("s västra", " West", s$county)
s$county = paste(s$county, "County")
s$county[ s$county == " County" ] = NA

s$party[ s$party %in% c("", "-") ] = "IND"

s$partyname = NA
s$partyname[ s$party == "V" ] = "Vänsterpartiet"
s$partyname[ s$party == "MP" ] = "Miljöpartiet"
s$partyname[ s$party == "S" ] = "Socialdemokraterna"
s$partyname[ s$party == "C" ] = "Centerpartiet"
s$partyname[ s$party == "M" ] = "Moderaterna"
s$partyname[ s$party == "NYD" ] = "Ny Demokrati"
s$partyname[ s$party == "KD" ] = "Kristdemokraterna"
s$partyname[ s$party == "FP" ] = "Folkpartiet"
s$partyname[ s$party == "PP" ] = "Piratpartiet"
s$partyname[ s$party == "SD" ] = "Sverigedemokraterna"
s$partyname[ s$party == "IND" ] = "Independent"

cat("Found", nrow(s), "MPs", ifelse(nrow(s) > n_distinct(s$name),
                                    "(non-unique names)\n",
                                    "(unique names)"))

s$uid = paste(s$name, gsub("\\D", "", s$url))

for(l in rev(unique(m$legislature))) {
  
  data = subset(m, legislature == l & n_au > 1)
  cat("Legislature", l, ":", nrow(data), "cosponsored bills, ")
  
  rownames(s) = gsub("\\D", "", s$url)
  
  edges = rbind.fill(lapply(data$authors, function(i) {
    
    w = unlist(strsplit(i, ";"))
    
    d = s[ w, "uid" ]
    d = subset(expand.grid(d, d), Var1 != Var2)
    d = unique(apply(d, 1, function(x) paste0(sort(x), collapse = "_")))

    if(length(d))
      return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    else
      return(data.frame())
    
  }))
  
  # raw edge counts
  count = table(edges$d)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ d, function(x) sum(1 / x), data = edges)
  
  # raw counts
  edges$count = as.vector(count[ edges$d ])
  
  edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$d),
                     j = gsub("(.*)_(.*)", "\\2", edges$d),
                     w = edges$w, n = edges[, 3])
  
  # network
  
  n = network(edges[, 1:2 ], directed = FALSE)
  cat(network.edgecount(n), "edges,", network.size(n), "nodes\n")

  n %n% "title" = paste("Riksdag", paste0(range(substr(data$date, 1, 4)), collapse = " to "))

  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(m, legislature == l)$n_au)
  
  rownames(s) = s$uid
  n %v% "url" = as.character(s[ network.vertex.names(n), "url" ])
  n %v% "name" = s[ network.vertex.names(n), "name" ]
  n %v% "sex" = s[ network.vertex.names(n), "sex" ]
  n %v% "born" = s[ network.vertex.names(n), "born" ]
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "partyname" = s[ network.vertex.names(n), "partyname" ]
  n %v% "nyears" = s[ network.vertex.names(n), "nyears" ]
  n %v% "county" = s[ network.vertex.names(n), "county" ]
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  n %v% "coalition" = ifelse(n %v% "party" %in% c("S", "V", "MP"), "Leftwing", # Rödgröna
                             ifelse(n %v% "party" == "SD", NA, "Rightwing"))   # Alliansen
  
  network::set.edge.attribute(n, "source", as.character(edges[, 1]))
  network::set.edge.attribute(n, "target", as.character(edges[, 2]))
  
  network::set.edge.attribute(n, "weight", edges[, 3])
  network::set.edge.attribute(n, "count", edges[, 4])
  network::set.edge.attribute(n, "alpha",
                              as.numeric(cut(n %e% "count", c(1:4, Inf),
                                             include.lowest = TRUE)) / 5)
  
  # modularity
  
  nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
  E(nn)$weight = edges[, 3]
  
  i = s[ V(nn)$name, "party" ]
  i[ i %in% c("IND") ] = NA # unaffiliateds
  
  nn = nn - which(is.na(i))
  i = as.numeric(factor(i[ !is.na(i) ]))
  
  n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)
  
  walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
  
  # max. partition
  maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
  walktrap = walktrap[[ maxwalks ]]
  
  n %n% "modularity_walktrap" = modularity(walktrap)
  
  louvain = multilevel.community(nn)
  
  n %n% "modularity_louvain" = modularity(louvain)
  
  n %n% "modularity_maximized" = n %n% "modularity" /
    max(c(n %n% "modularity_walktrap", n %n% "modularity_louvain"))
  
  # weighted adjacency matrix to tnet
  tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
  
  # weighted degree and distance
  wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
  dist = distance_w(tnet)
  wdeg$distance = NA
  wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
  wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
  names(wdeg) = c("node", "degree", "distance", "clustering")
  
  n %v% "degree" = wdeg$degree
  n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
  
  n %v% "distance" = wdeg$distance
  n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
  
  n %v% "clustering" = wdeg$clustering    # local
  n %n% "clustering" = clustering_w(tnet) # global
  
  # edge colors
  
  i = colors[ s[ n %e% "source", "party" ] ]
  j = colors[ s[ n %e% "target", "party" ] ]
  
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"
  
  print(table(n %v% "party", exclude = NULL))
  
  # number of bills cosponsored
  nb = sapply(n %v% "url", function(x) {
    nrow(subset(data, grepl(x, authors))) # ids are unique numbers
  })
  n %v% "n_bills" = as.vector(nb)
  
  if(plot) {
    
    n %v% "size" = as.numeric(cut(n %v% "degree", quantile(n %v% "degree"), include.lowest = TRUE))
    g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, # mode = "kamadakawai",
                               segment.color = party) +
                           geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                           geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                           scale_size_continuous(range = c(6, 12)) +
                           scale_color_manual("", values = colors, breaks = order) +
                           theme(legend.key.size = unit(1, "cm"),
                                 legend.text = element_text(size = 16)) +
                           guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
    
    print(g)
    
    ggsave(paste0("plots/net_se", l, ".pdf"), g, width = 12, height = 9)
    ggsave(paste0("plots/net_se", l, ".jpg"), g + theme(legend.position = "none"),
           width = 12, height = 12, dpi = 72)
    
  }
  
  assign(paste0("net_se", substr(l, 1, 4)), n)
  assign(paste0("bills_se", substr(l, 1, 4)), data)
  assign(paste0("edges_se", substr(l, 1, 4)), edges)
  
  if(gexf) {
    
    rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
    mode = "fruchtermanreingold"
    meta = list(creator = "rgexf",
                description = paste(mode, "placement", nrow(data), "bills"),
                keywords = "parliament, sweden")
    
    node.att = data.frame(url = n %v% "url",
                          party = n %v% "partyname",
                          bills = n %v% "n_bills",
                          county = n %v% "county",
                          distance = round(n %v% "distance", 1),
                          photo = n %v% "photo",
                          stringsAsFactors = FALSE)
    
    people = data.frame(id = as.numeric(factor(network.vertex.names(n))),
                        label = network.vertex.names(n),
                        stringsAsFactors = FALSE)
    
    relations = data.frame(
      source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
      target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
      weight = round(n %e% "weight", 2), count = n %e% "count")
    relations = na.omit(relations)
    
    # check all weights are positive after rounding
    stopifnot(all(relations$weight > 0))

    nodecolors = lapply(n %v% "party", function(x)
      data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
    nodecolors = as.matrix(rbind.fill(nodecolors))
    
    # node placement
    position = do.call(paste0("gplot.layout.", mode),
                       list(as.matrix.network.adjacency(n), NULL))
    position = as.matrix(cbind(round(position, 1), 1))
    colnames(position) = c("x", "y", "z")
    
    # clean up vertex names from uid number
    people$label = gsub("\\s\\d+", "", people$label)
    
    # save with compressed floats
    write.gexf(nodes = people, nodesAtt = node.att,
               edges = relations[, 1:2 ], edgesWeight = relations[, 3],
               nodesVizAtt = list(position = position, color = nodecolors,
                                  size = round(n %v% "degree", 1)),
               # edgesVizAtt = list(size = relations[, 4]),
               defaultedgetype = "undirected", meta = meta,
               output = paste0("net_se", l, ".gexf"))
               
  }

}

save(list = ls(pattern = "^(net|edges|bills)_se\\d{4}$"), file = "data/net_se.rda")

if(gexf)
  zip("net_se.zip", dir(pattern = "^net_se\\d{4}-\\d{4}\\.gexf$"))

# have a nice day
