# party colors

colors = c(
  "V"   = "#E41A1C", # Vänsterpartiet      -- red
  "MP"  = "#B3DE69", # Miljöpartiet        -- light green
  "S"   = "#F781BF", # Socialdemokraterna  -- pink
  "C"   = "#4DAF4A", # Centerpartiet       -- green
  "FP"  = "#FF7F00", # Folkpartiet         -- orange
  "KD"  = "#377EB8", # Kristdemokraterna   -- blue
  "M"   = "#80B1D3", # Moderaterna         -- light blue
  "SD"  = "#FFFF33", # Sverigedemokraterna -- yellow (2010-2014, no overlap with NYD)
  "NYD" = "#FFFF33", # Ny Demokrati        -- yellow (1991-1994, no overlap with SD)
  "IND" = "#AAAAAA"  # unaffiliated (William Petzäll) -- light grey
)

# ParlGov Left/Right scores

scores = c(
  "V"   = 1.5,
  "MP"  = 3.4,
  "S"   = 3.4,
  "C"   = 5.8,
  "FP"  = 6.3,
  "KD"  = 7.2,
  "M"   = 7.9,
  "SD"  = 8.7,
  "NYD" = 9,
  "IND" = Inf
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]
