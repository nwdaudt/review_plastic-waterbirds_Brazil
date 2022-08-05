# A synthesis of waterbirds interacting with plastics in Brazil (up to December 2021)

Here you will find all data and code needed to reproduce the results from [Daudt et al. 2022](https://github.com/nwdaudt/review_pastic-waterbirds_Brazil#citation).

In this study, we synthesised all available literature (peer-reviewed, book chapters, and academic theses) through an exhaustive review to understand the current status of knowledge about waterbirds (continental and marine) interacting with plastics in Brazil.

Brazil is the fifth-largest county and has one of the most extensive hydrological systems in the world. Due to its large extension, Brazil offers a variety of critical habitats for more than 200 waterbird species, which are well-recognized sentinel species. We found substantial gaps: only a third of occurring waterbird species were investigated; coastal and marine studies predominate, and the vast majority did not primarily aim to study interactions between waterbirds and plastics.

***
## Project structure

Workspace is set as follows, and we used an .RProj file to wrap it.

```shell
review_plastic-waterbirds_Brazil
├── README.md
├── data_raw
│   ├── index_data.csv
│   ├── orders_* .csv
│   ├── families_* .csv
│   ├── species_* .csv
│   └── taxa_* .csv
├── results
│   └── (plots & table)
├── scripts
│   ├── data_analysis.R
│   └── table1.R
└── review_plastic-waterbirds_Brazil.Rproj.Rproj
```

In `data_raw` you will find the raw data saved as .csv files. 
* `index_data` summarises all information gathered for each document, and specifies the document "ID" which links to `orders_*`, `families_*` and `species_*` files;
* `orders_*`, `families_*` and `species_*` files summarises (yes/no == 1/0) if that order, family or species was `_analysed` by that study, and in `_with_plastic` if it reports plastic for that particular taxa;
* `taxa_*` shows number of species by `_order` or `_family`, how many were analysed regarding plastic interactions, and observations on our choices of inclusion/exclusion of species within that taxon.

You should be able to reproduce all results using scrips in `scripts` and files from `data_raw`. At the end of both R scripts you can find the `sessionInfo()` and associated list of all packages, versions, and dependencies needed.

---
## Contributors

[Nicholas W. Daudt](https://github.com/nwdaudt).

***
## Citation
Please refer to the original article if using any piece of this repository (code and/or data). This repository is under CC BY 4.0 license.

Daudt, N.W.; Bugoni, L.; Nunes, G.T. (2022) A synthesis of interactions between waterbirds and plastics in Brazil reveals substantial knowledge gaps and opportunities for research. (in review)

Thanks!