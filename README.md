# Quick Analysis
Quickly import and analyze battery cycling data, with most of the data cleaning done for you automatically!


## Features
Everything is live-updating and interactive, so you can change anything at any time!

### Import

![Import](Import.PNG "Import")

* Import heterogenous datasets from a variety of file types, formats, and structures
* Stitch together files by giving them the same Battery ID and appending a #1 or #2 etc. at the end
* Automatically identify and standardize time stamps across data sources
* Merge dissimilar column names using an interactive heatmap and diagnostic tools
* Datasets are automatically sorted by type and ID, and can be easily changed at any time
* Label table supports autofill, copy-paste, and drag-and-drop to improve ease of use
* Customize charting colors at any time using hex colors or named colors (default colorset is monospaced pastel)

### Explore

![Explore](Explore.PNG "Explore")

* Continuous variable analysis allows you to explore and compare time series
* Discrete variable analysis allows you to reduce cycling data by finding intersections of x-y pairs
* Filter down your dataset by any combination of variables to help find what you're looking for
* Toggle calculation of the average and standard error of the mean by type
* Check out a derived version of the cycling protocol and investigate testing differences between batteries
* Choose between a line graph and a bar graph depending on the size of your dataset
* Work through raw data in the table tab using the filtering tools, or download as a csv for use in an external application
* Long-form data is best suited for relational computations, such as PivotTable in Excel
* Wide-form data is best suited for ordinal computations, such as making gaphs in Excel

### Share

![Share](Share.PNG "Share")

* Choose between three different themes for your high resolution downloadable graph
* Move the legend around to a non-intrusive location
* Change the font size to improve readability
* Optionally add a chart title
* Select a custom x and y range for graphing
* Add a horizontal line to call attention to a critical event

## Coming soon
* Download interactive widgets as html files for easy sharing
* Possible markdown report generation for reproducible research
* Manual mode for graph creation
* Add annotations to graphs
* Recommendations tab to detect patterns and exceptions within the dataset
* Always improving the underlying decision algorithms
