dir.create("data", showWarnings = FALSE)

library(GGally)
library(network)
library(tnet)
library(qdap)
library(stringr)
library(XML)

colors = c(
  "V" = "#E41A1C", # Vänsterpartiet, red
  "MP" = "#4DAF4A", # Miljöpartiet, green
  "S" = "#F781BF", # Socialdemokraterna, pink
  "C" = "#A65628", # Centerpartiet, agrarian, brown
  "M" = "#FF7F00", # Moderaterna, orange
  "KD" = "#377EB8", # Kristdemokraterna, blue
  "FP" = "#984EA3", # Folkpartiet, purple
  "SD" = "#444444", # Sverigedemokraterna, far-right, dark grey
  "-" = "#AAAAAA" # unaffiliated (William Petzäll), light grey
)
order = names(colors)

r = htmlParse("http://www.riksdagen.se/sv/Dokument-Lagar/Forslag/Motioner/")
r = xpathSApply(r, "//a[contains(@href, 'Motioner/?p=')]", xmlValue)
r = na.omit(as.numeric(gsub("\\D", "", r)))

root = "http://www.riksdagen.se"
r = max(r):min(r)
r = 100#sample(r, 500) # r = 100# 

get_info = function(y, x) {
  y = y[ grepl(x, y) ]
  ifelse(length(y), paste0(gsub(x, "", y), collapse = ";"), NA)
}

for(i in r) {

  if(!file.exists(paste0("data/page", i, ".csv"))) {
    
    cat("Scraping page", sprintf("%3.0f", i), "")
    
    h = htmlParse(paste0("http://www.riksdagen.se/sv/Dokument-Lagar/Forslag/Motioner/?p=", i))
    h = xpathSApply(h, "//a[contains(@href, '/sv/Dokument-Lagar/Forslag/Motioner/')]/@href")
    h = h[ grepl("?text=true", h) ]
    
    links = data.frame()
    for(j in h) {
      
      hh = try(htmlParse(paste0(root, j)))
      if(!"try-error" %in% class(hh)) {
        
        u = xpathSApply(hh, "//ul[contains(@class, 'documentlinks')]/li/a[@class='arrow']/@href")
        k = try(htmlParse(paste0(root, u)))
        
        if(!"try-error" %in% class(k)) {
          
          nfo = c(xpathSApply(k, "//div[contains(@class, 'splitcol')][1]/div/h2", xmlValue),
                  scrubber(xpathSApply(k, "//div[contains(@class, 'splitcol')][1]/ul[2]/li", xmlValue)))
          
          dat = scrubber(xpathSApply(k, "//div[contains(@class, 'splitcol')][2]/div/ul/li", xmlValue))
          dat = dat[ grepl("Inlämning", dat) ]
          
          aul = xpathSApply(k, "//div[contains(@class, 'splitcol')][2]/ul/li/a/@href")
          aul = gsub("/sv/ledamoter-partier/Hitta-ledamot/Ledamoter/|/$", "", aul)
          
          # when no links are provided
          aut = xpathSApply(k, "//div[contains(@class, 'splitcol')][2]/ul/li", xmlValue)
          
          vot = xpathSApply(k, "//ul[@class='statelist']/li", xmlValue)
          vot = vot[ !grepl("Behandlas", vot) ]
          
          com = get_info(vot, "Utskottets förslag:  ")
          vot = get_info(vot, "Kammarens beslut: ")
          
          if(!length(aul) & length(aut))
            aul = aut

          if(length(aul))
            links = rbind(links, data.frame(uid = nfo[1],
                                            date = gsub("Inlämning: ", "", dat),
                                            category = gsub("Motionskategori: ", "", nfo[2]),
                                            ref = gsub("Partinummer: ", "", nfo[3]),
                                            type = gsub("Motionstyp: ", "", nfo[4]),
                                            url = gsub("/sv/Dokument-Lagar/Forslag/Motioner/|/$", "", as.vector(u)),
                                            authors = paste0(aul, collapse = ";"),
                                            chamber0 = str_count(vot, "Avslag"),
                                            chamber1 = str_count(vot, "Bifall"),
                                            committee0 = str_count(com, "Avslag"),
                                            committee1 = str_count(com, "Bifall"),
                                            stringsAsFactors = FALSE))

          cat(".")
          
        } else {

          cat("x")

        }
        
      } else {
        
        cat("X")

      }
      
    }

    cat("\n")
    write.csv(links, paste0("data/page", i, ".csv"), row.names = FALSE)
    
  }

}

data = rbind.fill(lapply(dir("data", pattern = "page\\d+.csv$", full.names = TRUE),
                         read.csv, stringsAsFactors = FALSE))

r = unique(unlist(strsplit(data$authors, ";")))
r = r[ grepl("\\d", r) ]
cat("Found", nrow(data), "bills", length(r), "sponsors\n")

# MPs

if(!file.exists("data/ledamoter.csv")) {
  
  get_mps = function(x) {
    h = htmlParse(paste0(root, "/sv/ledamoter-partier/Hitta-ledamot/", x))
    u = unique(xpathSApply(h, "//a[contains(@href, 'Ledamoter/')]/@href"))
    u = gsub("/sv/ledamoter-partier/Hitta-ledamot/Ledamoter/|/$", "", u)
    return(u)
  }
  
  # possible sponsors
  u = unique(c(
    get_mps("Bokstavsordning"), # currently in office
    get_mps("Ersattare"), # replacements
    get_mps("Ledamoter-som-avgatt"), # resigned or died
    get_mps("Kandiderar-i-valet-2014"), # just elected
    get_mps("Kandiderar-inte-i-valet-2014") # others
  ))
  
  cat("Scraping", length(u), "possible sponsors ")
  r = unique(r[ !r %in% u ])

  cat(length(r), "missing sponsors")
  mps = data.frame() # initialize

} else {
  
  mps = read.csv("data/ledamoter.csv", stringsAsFactors = FALSE)
  r = unique(r[ !r %in% mps$url ]) # append new sponsors
  
}

if(length(r)) {
  
  cat("Scraping", length(r), "missing sponsor(s)\n")

  for(i in rev(r)) {
    
    cat(sprintf("%4.0f", which(i == r)), i, "")
    h = htmlParse(paste0(root, "/sv/ledamoter-partier/Hitta-ledamot/Ledamoter/", i))
    
    t = scrubber(xpathSApply(h, "//title", xmlValue))
    name = gsub("(.*)\\((.*)\\)(.*)", "\\1", t)
    party = gsub("(.*)\\((.*)\\)(.*)", "\\2", t)
    
    get_info = function(x) {
      y = h[ grepl(x, h) ]
      ifelse(length(y), gsub(x, "", y), NA)
    }
    
    h = xpathSApply(h, "//div[@class='commissioner-info']/ul[1]/li", xmlValue)
    mps = rbind(mps, data.frame(url = i, name, party,
                                partyname = get_info("Parti: "),
                                constituency = get_info("Valkrets: "),
                                born = as.numeric(get_info("Född år: ")),
                                job = get_info("Titel: "),
                                stringsAsFactors = FALSE))
    
    cat(gsub(" - riksdagen.se", "", t), "\n")
    
  }
  
  write.csv(mps, "data/ledamoter.csv", row.names = FALSE)

}

mps = read.csv("data/ledamoter.csv", stringsAsFactors = FALSE)
mps$name = scrubber(mps$name)
mps$longname = paste0(mps$name, " (", mps$party, ")")

cat("Found", nrow(mps), "MPs", ifelse(nrow(mps) > n_distinct(mps$name),
                                      "(non-unique names)\n",
                                      "(unique names)\n"))

r = data = rbind.fill(lapply(dir("data", pattern = "page\\d+.csv$", full.names = TRUE),
                             read.csv, stringsAsFactors = FALSE))

# print(table(substr(r$date, 1, 4)))

r$n_au = 1 + str_count(r$authors, ";")
# print(table(r$category[ r$n_au > 1 ], r$type[ r$n_au > 1 ]))
# print(table(r$n_au))
# print(table(r$n_au > 1))

fix = !grepl("\\d", r$authors)
if(length(fix)) {
  
  rownames(mps) = mps$longname
  
  r$authors[ fix ] = sapply(r$authors[ fix ], function(x) {
    y = unlist(strsplit(x, ";"))
    paste0(mps[ y, "url" ], collapse = ";")
  })

  cat("Fixed", sum(fix), "sponsor links\n")

}

data = subset(r, type != "Enskild motion" & n_au > 1)
cat("Using", nrow(data), "cosponsored bills\n")

print(apply(data[, 8:11 ], 2, sum))

edges = lapply(unique(data$uid), function(i) {
  
  d = subset(data, uid == i)
  d = unlist(strsplit(d$authors, ";"))
  d = mps$name[ mps$url %in% d ]
  d = expand.grid(d, d)
  d = subset(d, Var1 != Var2)
  d$uid = apply(d, 1, function(x) paste0(sort(x), collapse = "_"))
  d = unique(d$uid)
  if(length(d)) {
    d = data.frame(i = gsub("(.*)_(.*)", "\\1", d),
                   j = gsub("(.*)_(.*)", "\\2", d),
                   w = length(d))
    return(d)
  } else {
    return(data.frame())
  }
  
})

edges = rbind.fill(edges)
edges$uid = apply(edges, 1, function(x) paste0(sort(x[ 1:2 ]), collapse = "_"))

# raw edge counts
count = table(edges$uid)

# Newman-Fowler weights (weighted quantity of bills cosponsored)
edges = aggregate(w ~ uid, function(x) sum(1 / x), data = edges)

# raw counts
edges$count = as.vector(count[ edges$uid ])

edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                   j = gsub("(.*)_(.*)", "\\2", edges$uid),
                   w = edges$w, n = edges[, 3])

# network

n = network(edges[, 1:2 ], directed = FALSE)
n %n% "title" = paste("Riksdagen", paste0(range(substr(data$date, 1, 4)), collapse = "-"))

rownames(mps) = mps$name
n %v% "name" = mps[ network.vertex.names(n), "name" ]
n %v% "party" = mps[ network.vertex.names(n), "party" ]
n %v% "constituency" = mps[ network.vertex.names(n), "constituency" ]
n %v% "born" = mps[ network.vertex.names(n), "born" ]
n %v% "url" = mps[ network.vertex.names(n), "url" ]

network::set.edge.attribute(n, "source", as.character(edges[, 1]))
network::set.edge.attribute(n, "target", as.character(edges[, 2]))

network::set.edge.attribute(n, "weight", edges[, 3])
network::set.edge.attribute(n, "count", edges[, 4])
network::set.edge.attribute(n, "alpha",
                            as.numeric(cut(n %e% "count", c(1:4, Inf),
                                           include.lowest = TRUE)) / 5)

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

party = n %v% "party"
names(party) = network.vertex.names(n)

n %v% "coalition" = ifelse(party %in% c("S", "V", "MP"), "Rödgröna",
                           ifelse(party == "SD", NA, "Alliansen"))

i = colors[ mps[ n %e% "source", "party" ] ]
j = colors[ mps[ n %e% "target", "party" ] ]

party = as.vector(i)
party[ i != j ] = "#AAAAAA"

print(table(n %v% "party", exclude = NULL))

n %v% "size" = as.numeric(cut(n %v% "degree", quantile(n %v% "degree"), include.lowest = TRUE))
g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, #mode = "kamadakawai",
                           segment.color = party) +
                       geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                       geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                       scale_size_continuous(range = c(6, 12)) +
                       scale_color_manual("", values = colors, breaks = order) +
                       theme(legend.key.size = unit(1, "cm"),
                             legend.text = element_text(size = 16)) +
                       guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))

print(g)

ggsave("riksdag.pdf", g, width = 12, height = 9)
ggsave("riksdag.png", g, width = 12, height = 9, dpi = 72)

save(n, g, edges, file = "riksdag.rda")

# have a nice day
