# MP bills (slow, over 100,000 JSON files to parse)

file = "data/motioner.csv"
if(!file.exists(file)) {
  
  m = data_frame()
  years = c("2014-2017", "2010-2013", "2006-2009", "2002-2005", "1998-2001", "1990-1997", "1980-1989")
  
  for (i in years) {
    
    f = paste0("data/mot-", i, ".json.zip")
    
    if (!file.exists(f))
      download.file(paste0("http://data.riksdagen.se/dataset/dokument/mot-", i, ".json.zip"),
                    f, mode = "wb")
    
    unzip(f, exdir = "raw")
    f = list.files("raw", pattern = "json$", full.names = TRUE)

    cat(i, ": parsing", sprintf("%6.0f", length(f)), "bills...")
    for (j in f) {
      
      d = fromJSON(readLines(j, warn = FALSE), flatten = TRUE)$dokumentstatus
      
      m = rbind(m, data_frame(
        uid = d$dokument$hangar_id,
        doc = d$dokument$dok_id,
        date = as.Date(d$dokument$datum),
        authors = paste0(d$dokintressent$intressent$intressent_id, collapse = ";")
      ))
      
    }
    
    cat("done, ", sprintf("%6.0f", nrow(m)), "total bills\n")
    file.remove(f)
    
  }
  
  write.csv(m, file, row.names = FALSE)
  
}

m = read.csv(file, stringsAsFactors = FALSE)
m$n_au = 1 + str_count(m$authors, ";")

table(substr(m$doc, 1, 2)) # session years

# parliamentary sessions, 1988-2018 (years before/after excluded from dataset)
# see <http://data.riksdagen.se/sv/sa-funkar-dokument-id>
m$legislature = substr(m$doc, 1, 2)
m$legislature[ m$legislature %in% c("GC", "GD", "GE") ] = "1988-1991" # Sep 1988: 88/9, 89/0, 90/1
m$legislature[ m$legislature %in% c("GF", "GG", "GH") ] = "1991-1994" # Sep 1991: 91/2, 92/3, 93/4
m$legislature[ m$legislature %in% c("GI", "GJ", "GK", "GL") ] = "1994-1998" # Sep 1994: 94/5, 95/6, 96/7, 97/8
m$legislature[ m$legislature %in% c("GM", "GN", "GO", "GP") ] = "1998-2002" # Sep 1998: 98/9, 99/0, 00/1, 01/2
m$legislature[ m$legislature %in% c("GQ", "GR", "GS", "GT") ] = "2002-2006" # Sep 2002: 02/3, 03/4, 04/5, 05/6
m$legislature[ m$legislature %in% c("GU", "GV", "GW", "GX") ] = "2006-2010" # Sep 2006: 06/7, 07/8, 08/9, 09/0
m$legislature[ m$legislature %in% c("GY", "GZ", "H0", "H1") ] = "2010-2014" # Sep 2010: 10/1, 11/2, 12/3, 13/4
m$legislature[ m$legislature %in% c("H2", "H3", "H4", "H5") ] = "2014-2018" # Sep 2014: 14/5, 15/6, 16/7, 17/8

table(m$legislature, exclude = NULL)
m = filter(m, nchar(legislature) == 9)

r = unlist(strsplit(m$authors, ";"))

cat("Found", nrow(m), "bills",
    sum(m$n_au > 1), "cosponsored",
    n_distinct(r), "sponsors\n")

# sponsors

if(!file.exists("data/ledamoter.csv")) {
  
  s = data_frame() # initialize
  
} else {
  
  s = read.csv("data/ledamoter.csv", stringsAsFactors = FALSE)
  s = subset(s, grepl("\\d", url)) # avoid scraper bug
  
  r = r[ !r %in% gsub("\\D", "", s$url) ] # only new sponsors
  
}

# empty pages
r = unique(r[ !r %in% c("0584306280501", "0338066970018", "0844235199717", "0000000000000") ])

if(length(r)) {
  
  cat("Scraping", length(r), "missing sponsor(s)\n")
  
  for(i in rev(r)) {
    
    cat(sprintf("%4.0f", which(r == i)), str_pad(i, 14, "right"))
    
    file = paste0("raw/mp-", i, ".xml")
    if(!file.exists(file))
      try(download.file(paste0("http://data.riksdagen.se/personlista/?iid=", i),
                        file, quiet = TRUE, mode = "wb"), silent = TRUE)
    
    if(!file.info(file)$size) {
      
      cat(": failed\n")
      file.remove(file)
      
    } else {
      
      h = xmlParse(file)
      
      name = paste(xpathSApply(h, "//tilltalsnamn", xmlValue), xpathSApply(h, "//efternamn", xmlValue))
      
      from = substr(xpathSApply(h, "//uppdrag[organ_kod='kam']/from", xmlValue), 1, 4)
      to = substr(xpathSApply(h, "//uppdrag[organ_kod='kam']/tom", xmlValue), 1, 4)
      mdts = apply(cbind(from, to), 1, paste0, collapse = "-")
      mdts = lapply(mdts, function(x) {
        y = as.numeric(unlist(strsplit(x, "-")))
        seq(y[1], y[2])
      })
      mdts = paste0(sort(unique(unlist(mdts))), collapse = ";")

      job = xpathSApply(h, "//uppgift[kod='en' and typ='titlar']/uppgift", xmlValue)
      
      if(length(name)) {
        
        s = rbind(s, data_frame(
          name,
          born = xpathSApply(h, "//fodd_ar", xmlValue),
          sex = xpathSApply(h, "//kon", xmlValue),
          party = xpathSApply(h, "//parti", xmlValue),
          county = xpathSApply(h, "//valkrets", xmlValue),
          status = xpathSApply(h, "//status[1]", xmlValue),
          mandate = mdts,
          job = ifelse(is.null(job), NA, job),
          url = paste0("http://data.riksdagen.se/personlista/?iid=", i, "&utformat=html"),
          photo = xpathSApply(h, "//bild_url_80", xmlValue)
        ))
        
        cat(":", tail(s, 1)$name, "\n")
        
      } else {
        
        cat(": empty\n")
        
      }
      
    }
    
  }
  
  s$job[ s$job == "" ] = NA
  
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
  } else {
    s$photo[ s$url == i ] = 1
  }
}

s$sex = ifelse(s$sex == "kvinna", "F", "M")

# convert constituencies to Wikipedia English handles

# note: constituency is very often missing in legislatures before 1994 due
# to a revision in the national apportionment of fixed constituency seats

s$county = gsub("( )?, plats |(s)? (kommun|län)|\\d", "", s$county)
s$county = gsub("s norra och östra", " North_East", s$county) # Skånes
s$county = gsub("s norra", " North", s$county) # Västra Götaland
s$county = gsub("s östra", " East", s$county)
s$county = gsub("s södra", " South", s$county)
s$county = gsub("s västra", " West", s$county)
s$county = ifelse(s$county == "", NA, paste(s$county, "County"))
s$county = gsub("\\s", "_", s$county)

# version used in the networks
s$constituency = s$county

# version used in the GEXF exports (compatible with Wikipedia English)
s$county = gsub("_(North|East|South|West|North_East)", "", s$county)
s$county[ s$county == "Göteborg_County" ] = "Gothenburg"
s$county[ s$county == "Malmö_County" ] = "Malmö"

s$party[ s$party == "-" ] = "IND"
stopifnot(!is.na(groups[ s$party ]))

table(s$partyname, exclude = NULL)

# solve duplicates
s = arrange(s, name, born) %>%
  group_by(name) %>%
  mutate(n = n(), o = 1:n()) %>%
  group_by() %>%
  mutate(name = ifelse(n == 1, name, paste0(name, "-", o))) %>%
  select(-n, -o)

stopifnot(!duplicated(s$name))
cat("Found", nrow(s), "MPs\n")

stopifnot(!duplicated(s$url))
s$uid = paste(s$name, gsub("\\D", "", s$url))
s = data.frame(s)
