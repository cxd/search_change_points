# A simple method for changepoint detection.

The procedure is illustrated in the notebook:


https://colab.research.google.com/drive/13oRwkLW27TJldA7nr7eITv8mlKgLI19O

The examples in this notebook make use of two different types of data sets from the R data package.

- WWWusage 
 - https://stat.ethz.ch/R-manual/R-patched/library/datasets/html/WWWusage.html
- airquality
 - https://stat.ethz.ch/R-manual/R-patched/library/datasets/html/airquality.html

However any univariate timeseries indexed by a timestamp can be used.

The source file "search_changepoints.R" provides a method __search_changepoints_numeric__.
This method takes as input the data frame, the set of indexes in the data set to use, the column index of the time index value (such as date, timestamp, hour etc), the name of the time index column, the name of the value column, and the size of the lag to create.

In the example below the lagCols variable defines t and t minus 1 to t minus 10 $(t, t_{m1}, t_{m2}, ..., t_{m10})$.

The method internally can be broken into the following procedure.

1. Generate lags of size N.
2. Normalise the lagged matrix.
3. Perform a PCA (in the manner of an SSA https://en.wikipedia.org/wiki/Singular_spectrum_analysis)
4. Use the eigenvector coefficients associated with first component (explaining most variance) from the resulting scores of the PCA as the component representing the variance in the signal.
5. Convert that component into a lagged timeseries matrix of same lag size N.
6. For each window perform pairwise comparison of variance in each window in order to compute a score indicating whether the ration of variance is significant based on the ftest or bartletts test.

This last step utilises the tests the null hypothesis where the variances are assumed equal between the pairs of windows.

The result is a table, the first two columns "date1", "date2" indicate the start timestamp of the first window, and the start timestamp of the second window respectively. The width of the window is the stepsize which the process uses to step through the generated lags.
The other columns include a number of statistics for the windows as well as the p values for the variance tests.

The ftest and bartlett test are sensitive to non-normal data, so further investigation using other tests of variance will be useful.

- https://en.wikipedia.org/wiki/Bartlett%27s_test
- https://en.wikipedia.org/wiki/F-test
