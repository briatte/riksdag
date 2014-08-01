A scraper for Swedish private bills that builds their [cosponsorship network](http://jhfowler.ucsd.edu/cosponsorship.htm). Get the full legislation network in R by running `riksdag.r`. More data are available through the Riksdag's [open data](http://data.riksdagen.se/) portal.

[![](riksdag.png)](riksdag.png)

The data are a random sample of 553 cosponsored bills submitted between September 2011 and April 2014. The nodes are sized proportionally to their [weighted degree](http://toreopsahl.com/2010/04/21/article-node-centrality-in-weighted-networks-generalizing-degree-and-shortest-paths/) and are placed by a Fruchterman-Reingold force-directed algorithm. The ties are undirected and are colorized when they connect two MPs of the same party.
