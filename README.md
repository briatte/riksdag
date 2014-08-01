A scraper for Swedish private bills to build their [cosponsorship network](http://jhfowler.ucsd.edu/cosponsorship.htm). See the picture below for a graph extract with short comments, or replicate the full legislation graph in R by running `riksdag.r` (takes quite a few hours to complete).

[![](demo.png)](demo.png)

The graph is undirected and drawn with Fruchterman-Reingold. The data are a random sample of 553 cosponsored bills submitted between September 2011 and April 2014. The nodes are proportional to their [weighted degree](http://toreopsahl.com/2010/04/21/article-node-centrality-in-weighted-networks-generalizing-degree-and-shortest-paths/).
