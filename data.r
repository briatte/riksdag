# MP bills

file = "data/motioner.csv"
if(!file.exists(file)) {
  
  years = c("2010-2013", "2006-2009", "2002-2005", "1998-2001", "1990-1997", "1980-1989")
  
  for(i in years) {
    f = paste0("data/mot-", i, ".json.zip")
    if(!file.exists(f))
      download.file(paste0("http://data.riksdagen.se/dataset/dokument/mot-", i, ".json.zip"),
                    f, mode = "wb")
  }
  
  for(i in dir("data", "json.zip$", full.names = TRUE))
    unzip(i, exdir = "raw")
  
  # limit to years 1988-2014 (excluding years before and after)
  files = dir("raw", pattern = "^(g[c-z]|h[0-1])(.*)json$", full.names = TRUE)
  
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

# parliamentary sessions, 1988-2014 (years before/after excluded from dataset)
# see <http://data.riksdagen.se/sv/sa-funkar-dokument-id>
m$legislature = substr(m$doc, 1, 2)
m$legislature[ m$legislature %in% c("GC", "GD", "GE") ] = "1988-1991" # Sept. 1988: 88/9, 89/0, 90/1
m$legislature[ m$legislature %in% c("GF", "GG", "GH") ] = "1991-1994" # Sept. 1991: 91/2, 92/3, 93/4
m$legislature[ m$legislature %in% c("GI", "GJ", "GK", "GL") ] = "1994-1998" # Sept. 1994: 94/5, 95/6, 96/7, 97/8
m$legislature[ m$legislature %in% c("GM", "GN", "GO", "GP") ] = "1998-2002" # Sept. 1998: 98/9, 99/0, 00/1, 01/2
m$legislature[ m$legislature %in% c("GQ", "GR", "GS", "GT") ] = "2002-2006" # Sept. 2002: 02/3, 03/4, 04/5, 05/6
m$legislature[ m$legislature %in% c("GU", "GV", "GW", "GX") ] = "2006-2010" # Sept. 2006: 06/7, 07/8, 08/9, 09/0
m$legislature[ m$legislature %in% c("GY", "GZ", "H0", "H1") ] = "2010-2014" # Sept. 2010: 10/1, 11/2, 12/3, 13/4

r = unlist(strsplit(m$authors, ";"))
cat("Found", nrow(m), "bills",
    sum(m$n_au > 1), "cosponsored",
    length(r), "sponsors\n")

# sponsors

if(!file.exists("data/ledamoter.csv")) {
  
  # this stopped working afetr the 2014 election
  # h = htmlParse("http://data.riksdagen.se/Data/Ledamoter/Ledamoter-2010-2014/")
  # h = xpathSApply(h, "//select[@name='iid']/option/@value")
  # 
  # cat("Scraping", length(h), "recent sponsors ")
  # 
  # r = unique(c(r, h[ h != "" ]))
  # cat(length(r), "total missing sponsor(s)\n")
  
  s = data.frame() # initialize
  
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
      try(download.file(paste0("http://data.riksdagen.se/personlista/?iid=", i), file, quiet = TRUE, mode = "wb"), 
          silent = TRUE)
    
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
        
        s = rbind(s, data.frame(name,
                                born = xpathSApply(h, "//fodd_ar", xmlValue),
                                sex = xpathSApply(h, "//kon", xmlValue),
                                party = xpathSApply(h, "//parti", xmlValue),
                                county = xpathSApply(h, "//valkrets", xmlValue),
                                status = xpathSApply(h, "//status[1]", xmlValue),
                                mandate = mdts,
                                job = ifelse(is.null(job), NA, job),
                                url = paste0("http://data.riksdagen.se/personlista/?iid=", i, "&utformat=html"),
                                photo = xpathSApply(h, "//bild_url_80", xmlValue),
                                stringsAsFactors = FALSE))
        
        cat(":", tail(s, 1)$name, "\n")
        
      } else {
        
        cat(": empty\n")
        
      }
      
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
  } else {
    s$photo[ s$url == i ] = 1
  }
}

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

table(s$partyname, exclude = NULL)

# solve duplicates

s$name[ s$url == "0778776381700" ] = "Anders Andersson-1"
s$name[ s$url == "0777657837318" ] = "Anders Andersson-2"
s$name[ s$url == "0404836900409" ] = "Arne Andersson-1"
s$name[ s$url == "0275356156309" ] = "Arne Andersson-2"
s$name[ s$url == "0825004049003" ] = "Göran Persson-1"
s$name[ s$url == "0780069612618" ] = "Göran Persson-2"
s$name[ s$url == "0977860675311" ] = "Gunilla Carlsson-1"
s$name[ s$url == "0875221317013" ] = "Gunilla Carlsson-2"
s$name[ s$url == "0310304705606" ] = "Jan Andersson-1"
s$name[ s$url == "0273732267214" ] = "Jan Andersson-2"
s$name[ s$url == "0180442748612" ] = "Johan Andersson-1"
s$name[ s$url == "0964730940415" ] = "Johan Andersson-2"
s$name[ s$url == "0810105099207" ] = "Lars Gustafsson-1"
s$name[ s$url == "0534056896314" ] = "Lars Gustafsson-2"
s$name[ s$url == "0805469037201" ] = "Lennart Pettersson-1"
s$name[ s$url == "0235326752716" ] = "Lennart Pettersson-2"
s$name[ s$url == "0420875837811" ] = "Margareta Persson-1"
s$name[ s$url == "0213183979300" ] = "Margareta Persson-2"
s$name[ s$url == "0590096877813" ] = "Marianne Andersson-1"
s$name[ s$url == "0754692641206" ] = "Marianne Andersson-2"
s$name[ s$url == "0391621769505" ] = "Roland Larsson-1"
s$name[ s$url == "043961676409" ]  = "Roland Larsson-2"
s$name[ s$url == "0160374603998" ] = "Sten Andersson-1"
s$name[ s$url == "0892012091809" ] = "Sten Andersson-2"

# duplicates solved
stopifnot(nrow(s) == n_distinct(s$name))
cat("Found", nrow(s), "MPs\n")

s$uid = paste(s$name, gsub("\\D", "", s$url))
