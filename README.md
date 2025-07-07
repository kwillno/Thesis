# Thesis

This repository contains the code used for my thesis titled *Reconstruction of temperature fields and comparison against NORA3 over southern Norway*. This repository contains all the code used in the final product. 

The abstract of the thesis will be given here: 
```
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


