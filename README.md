cpg-statrisk
============

The R scripts posted here develop and apply a multimodel ensemble that can be used to forecast the onset of episodes of state-sponsored mass killing in countries worldwide. This project is described in more detail in the following paper:

[URL for posted paper]

In the past, researchers using statistical methods to provide early warning on mass atrocities have sought to develop a single best model for forecasting, and they usually have attempted to test hypotheses about the causes of mass atrocities as they go. By contrast, the research described in these scripts focuses exclusively on the problem of forecasting, and the solution we have adopted involves an ensemble approach that averages predictions from several models. Over time, this ensemble should produce more accurate predictions than any single statistical model or expert forecaster would, and it lays a foundation for continuing research by leaving the door open to incorporating probabilistic forecasts from new models and other sources as they are developed.

This project is part of a larger program undertaken and funded by the U.S. Holocaust Memorial Museum's Center for the Prevention of Genocide (CPG) that aims to to build and run a public early-warning system to routinely assesses risks of mass atrocities in countries worldwide using the best available methods.

The scripts perform the following tasks:

1. data-compilation: Creates and assembles a number of data sets from public sources to produce a time-series cross-sectional data set with country-years as the units of observation covering the period 1945-2013.
2. data-transformation: Performs various transformations on selected variables in the data set assembled in step 1 to prepare for statistical modeling.
3. validation: Uses k-fold cross-validation to assess the forecasting power of the proposed multimodel ensemble.
4. estimation: Estimates the component models in the ensemble using data from 1960 to 2012.
5. prediction: Applies the models estimated in step 4 to data assembled in early 2013 to generate forecasts for calendar 2013.

At present, the data-compilation and data-transformation scripts are posted to show the provenance of the data used in steps 3 and 4. Step 3 produces the "raw" version, Step 4 the "cooked." Researchers interested in replicating and extending this work can download the data set produced in step 2 in tab-delimited format from this URL:

https://docs.google.com/file/d/0B5wyt4eDq98GQnNOeWtHOEZiMDA/edit?usp=sharing

We are eager to improve on and expand this set of models. If you would like to recommend a model to add to the ensemble, a data set to add to our compilation, or an alternative approach to aggregating the component forecasts, PLEASE email me at ulfelder@gmail.com. Of course, if you discover any bugs in the R code or have any recommendations on ways to make them cleaner or more efficient, I'm eager to hear from you, too.

