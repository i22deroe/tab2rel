# Tabular data to Relational data converter

tab2rel is a R script designed to transform tabular data (usually csv files or xlsx files) into relational data, outputted into yaml format.

It also offers the opposite transformation (rel2tab), but please bear in mind that while the tab2rel method is more or less tuneable into an accurate yaml file, this other one may yield a rough approximation to the original tabular dataset.

It is advised to be used as a tool to save some time when converting data between these two formats rather than an automated conversion package.

## Installation

Make sure you have dplyr, reshape2, yaml and purrr installed.

```r
install.packages(c('dplyr', 'reshape2', 'yaml', 'purrr))
```

It has been tested for R versions newer than 4.x.x. Please bear in mind that ideally rlang version should be newer than 0.4.2, but some support has been added.

## Usage

```r
source('tab2rel.R')

tab_x <- as.data.frame(datasets::HairEyeColor)

rel_x = tab2rel(tab_x, 3)

rel_x %>%
   cat()
 
```

## Disclaimer
Tabular and relational data structures are not necessarily complimentary and there may be situations in which designing the right relational equivalent to a tabular structure will be complicated. Please bear in mind at all times that this is by no means an automated solution, since its only aim is to ease this conversion in scenarios where you have control over detailes as the column sorting and order.