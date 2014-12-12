This repository contains code to build cosponsorship networks from bills (and motions) passed in the [Swedish Parliament](http://www.riksdagen.se/).

- [interactive demo](http://briatte.org/riksdag)
- [static plots](http://briatte.org/riksdag/plots.html)

The data come from the Riksdag's [open data portal](http://data.riksdagen.se/).

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. Once unzipped to the `raw` folder, the raw JSON data files will take up to 1.5 GB of disk space -- make sure that you have at least that amount of free space on your hard drive.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

The sponsors data are read from the JSON dumps available on the Riksdag's open data portal.

- `uid` -- bill unique identifier (int)
- `doc` -- document identifier (the first two characters are [legislature codes](http://data.riksdagen.se/sv/sa-funkar-dokument-id))
- `legislature` -- legislature years, imputed from `doc`
- `date` -- bill date (yyyy-mm-dd)
- `authors` -- semicolon-separated sponsor unique identifiers
- `n_au` -- total number of sponsors

## Sponsors

The sponsors data are read from the XML listings available on the Riksdag's open data portal.

- `name` -- sponsor name
- `born` -- year of birth (num)
- `sex` -- gender (F/M), read from the profile
- `party` -- political party, abbreviated
- `partyname` -- political party, full name
- `constituency` -- constituency (sometimes empty)
- `status` -- last parliamentary status
- `mandate` -- semicolon-separated mandate years, used to compute the `nyears` seniority variable
- `job` -- occupation
- `url` -- profile URL, shortened to unique numeric identifier
- `photo` -- photo URL, coded as a numeric dummy (0/1); photo URLs are based on profile URLs
- `uid` -- sponsor unique identifier, composed of `name` and `url`
