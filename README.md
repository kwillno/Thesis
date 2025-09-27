# Thesis

This repository contains the code used for my thesis titled *Reconstruction of temperature fields and comparison against NORA3 over southern Norway*. This repository contains all the code used in the final product. 

The abstract of the thesis is given here: 
```
This work is split in two main parts. The first part concerns the construction of statistical models of the monthly mean temperature fields in southern Norway based on data from observation stations. The statistical models are Latent Gaussian Models that are modelled continuously over the domain using the Stochastic Partial Differential Equation approach and INLA. Model selection is done using Deviance Information Criterion and Watanabe Akaike Information Criterion (Widely Applicable Information Criterion) scores in addition to using a Leave-group-out Cross Validation procedure with Logarithmic Score and Dawid Sebastiani Score as scoring functions. The chosen statistical model has linear parameters for altitude, time, easting, northing and distance from the sea. The model has random effects for year and each specific station, and a Gaussian Markov Random Field term with Matérn covariance for spatial composition.

The second part is to compare the fields realised from the statistical models against mean temperature fields from the NORA3 reanalysis. For comparing the models the measures used are Bias, Error and the Continuous Ranked Probability Score. Because observation station data are lacking for high altitudes the comparison is only made for areas lower than 1000masl. When comparing in space the models show generally similar results. The statistical model predicts higher temperatures than NORA3 in most areas. There is also less difference in the east of Norway and along the western coast. The models diverge more in the north of the domain and for some months more than others, especially the month of May. When comparing in time, the models disagree to some extent, but the disagreement is generelly independent of time, For some months in some counties however there seems to be a postitive linear trend in time in the comparison, suggesting there is still some disagreement as to how the models model increase in temperature with time. This is the case especially for inland counties in the month of April.
```

A link to the thesis will be added [here](#) when it is published. 

A general description of the structure of this repository and the files within it follows. 

### `comp`

This folder contains code for comparing the realsations of the statistical models to the NORA3 reanalysis. 

- `MakeTables.R`  
  Used for making table of comparison scores in Latex format.
- `bias.R`  
  Calculates Bias and Error for the comparison. See Section 5.4 in thesis.
- `crps.R`  
  Calculates Continuous Ranked Probability Score for the comparison. See Section 5.5 in thesis.
- `timePlots.R`  
  Creates plots for comparisons done in time. See section 5.6 in thesis. 


### `data`

This folder contains files that manipulate or plot data for use in or comparison against models. 

- `aggregate.R`  
  Aggregates ECA data from daily data to monthly data using WMO guidelines.
- `altitudeMerge.R`  
  Merges multiple altitude fields and stores the raster for later use.
- `coordinateFields.R`  
  Calculates coordinate fields and store them for later use.
- `counties.R`  
  Filtering and simplification of county objects.
- `descriptive.R`  
  Creates descriptive plots of the data for explanatory use.
- `distanceToSeaField.R`  
  Calculates and store field of distance to sea.
- `domain.R`  
  Generates mesh and stores domain variables for later use.
- `observations.R`  
  Extract the data of interest from the full ECA dataset.
- `reanalysis.R`  
  Downloads and treats NORA3 data for use. 


### `models`

This folder contains code files for each of the models constructed and realised in this project. In addition a code file for comparing cross-validation, DIC and WAIC scores is given. 

- `allCV.R`  
  Makes table of all model validation scores
- `altCoor`  
	- `model.R`  
	  Defines and fits model.
	- `realise.R`  
	  Realises model.
- `altDist`  
	- `model.R`  
	- `realise.R`  
- `altRW2`  
	- `model.R`  
	- `realise.R`  
- `altTime`  
	- `model.R`  
	- `realise.R`  
- `distStat`  
	- `model.R`  
	- `realise.R`  


