This repository creates U.S. Congress bill cosponsorship network data similar to that of [Fowler, Waugh and Sohn][fws], ranging from 1973 to today in both chambers. The code mimicks the code used for the [parlnet](https://github.com/briatte/parlnet) project, which builds legislative cosponsorship networks for European countries.

Static plots of the networks are viewable on [that page](http://f.briatte.org/parlviz/congress/plots.html).

# HOWTO

Replicate by running `make.r` in R after checking the dependencies at the top of the script.

The `01-data.r` script downloads information on bills and sponsors:

- The bills data come from the [Library of Congress](https://www.congress.gov) via [The Sunlight Foundation](http://sunlightfoundation.com/)'s JSON dumps of the data (which, incidentally, also includes amendments and votes). See [this documentation page](https://github.com/unitedstates/congress/wiki/bills) for the details, and [this page](https://github.com/unitedstates/congress/wiki) for links to the dumps. It is possible to get just the bills data [from GovTrack.us](https://www.govtrack.us/developers/data), but it would introduce a dependency to `rsync` (and of course, it is possible to scrape THOMAS.gov directly, but that would duplicate excellent existing work). The code gets the full data dumps but only parses the bills. It does so very slowly because all data are read from the ZIP archives directly. 
- The sponsors data come (in YAML format, which is nice) from [this repository](https://github.com/unitedstates/congress-legislators), which does a wonderful job at matching the various identifiers that might apply to Congressmen, and also provides tons of additional things like social media accounts. The code will extract the sponsors' THOMAS identifiers, ICPSR identifiers, full names, date of birth, gender and constituency (i.e. state, without the district number for Representatives). The data include one row per sponsor term, which makes it easy to compute seniority on the fly while building the cosponsorship networks.

The `01-data.r` script exports cosponsorships as CSV edge lists instead of adjacency matrices. The edge lists feature one bill per row and the following variables:

- `file`: the filename from which the information come from (which includes the chamber of introduction, the Congressional session of introduction, the type of the bill, and the bill number); 
- `date` and `status`: the date of introduction and outcome status of the bill (there is a nice guide to bill status codes at the end of [this page](https://github.com/unitedstates/congress/wiki/bills#bill-status-codes)); the JSON data also include tons of additional information
- `sponsor`: the THOMAS identifier of the bill sponsor (bills without a THOMAS identifier for their sponsors, i.e. a very small number of committee bills, are ignored during parsing and are not included in the edge lists);
- `cosponsors` and `withdrawn`: the THOMAS identifiers of the bill cosponsors, separated by semicolons; the `withdrawn` column contains the identifiers for withdrawn sponsors, who might have cosponsored the bill again and might therefore also appear in the `cosponsors` list

The `02-build.r` script weights the network ties by using Gross, Kirkland and Shalizi's [weighted propensity to cosponsor](http://www.latinodecisions.com/files/4013/3840/2978/Gross-Kirkland-Shalizi_Multilevel-Cosponsorship_PolAnlys-submission.pdf), which is useful for subsetting the plots to a certain tie intensity. Below are two example graphs showing the 113rd Congress at two different edge weight thresholds:

![](http://f.briatte.org/parlviz/congress/plots/hr113.png)![](http://f.briatte.org/parlviz/congress/plots/se113.png)

Node colors show party affiliations (Democrats in blue, Republicans in red, independents in green), and placement is Fruchterman-Reingold force-directed. It should be very easy to play with the `network` objects produced by `02-build.r` to produce different plots, including plots suited for [interactive visualization](http://f.briatte.org/parlviz/).

[fws]: http://jhfowler.ucsd.edu/cosponsorship.htm

# BONUS

The two separate scripts in the `modularity` folder will compute and plot the party-based modularity scores of the networks, using the same method as [Waugh  _et al._](http://arxiv.org/abs/0907.3509) to also compute the maximum modularity of the networks. Each modularity score is computed against the three different weighting schemes carried by the networks, as well as against unweighted ties.

The code will also compare empirical modularity to modularity in randomly rewired graphs with identical degree distributions, and empirical modularity to modularity from a randomised partition vector, like [Kirkland](https://jhkirkla.wordpress.com/2012/11/28/hypothesis-testing-for-group-structure-in-legislative-networks/) suggests. That part of the code is particularly slow if you keep it at 1,000 simulations per network like the code does by default.

Please feel free to [open an issue](issues) if you have comments or suggestions.
