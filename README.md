# TidyScientometrix
A (R) collection of mapping tools from natural language processing, multidimensional network analysis, and API interfaces for scientometric mapping .

This package is still under development, but have a bit of patience, soon first updates are to follow, and enable the easy reconstruction of the analysis done in the paper:

[Rakas, Marija, and Hain, Daniel S. (in press). "The state of innovation system research: What happens beneath the surface?." Research Policy](https://www.sciencedirect.com/science/article/pii/S0048733319301027)

It will furthermore include convenient function to compile static or interactive bibliometric reports, as for examplle illustrated at [this example of analysing the research field related to "Sustainablity transistions"](https://raw.githack.com/daniel-hain/transitions_bibliometrics_2019/master/notebooks/91_descriptives.html)

# Planned functionality
In detail, the package will enable:

* Confortable access to bibliographic data via the Scopus API
* The construction of co-citation and bibliographic coupling networks
* Community detection
* Mapping the interaction between co-citation and bibliographic coupling communities
* Connectivity mapping between co-citation and bibliographic coupling communities
* Coherence & diversity mapping of coupling communities
* LDA topic modeling of paper abstracts

# Install package

Again, up to now, most functionality is not working properly. However, if you want to peak a bit, feel free to check out the current development version with:

:) :)

```{r}
devtools::install_github("daniel-hain/TidyScientometrix", update = "always")
```



