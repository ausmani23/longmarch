# The Long March: Deep Democracy in Cross-National Perspective

This repository contains replication materials for "The Long March: Deep Democracy in Cross-National Perspective" forthcoming at _Social Forces_. The paper is by Mohammad Ali Kadivar, Benjamin Bradlow, and myself. Responsibility for the code is mine. 

## What do you need? 

To replicate this work, you will need: 

1. R, a popular and free statistical software. I wrote these files in R 3.4.1, but it should be forwards-compatible (i.e., you should be able to run these if you download the current version). John Fox offers a [useful intro](https://socialsciences.mcmaster.ca/jfox/Courses/soc740/R-install-instructions.html), if you don't have prior experience. 
2. Several additional packages, which don't come installed with R. If you run these scripts without installing these, R will throw an error noting that you haven't yet installed a required package. To make life easier, I have included a script 'installpacks.R'. This generates a list of all the packages we run and, if you remove the comments before the last line, installs them. 
3. Ghostscript, which I use for fancy fonts in graphs. For detailed instructions on how to install Ghostscript, see [this blogpost](http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html). Once you do this, open the file 'dirs.R' and make sure that 'gsdir_full' points to the directory that contains the Ghostscript executable. If you install GS in your 'Program Files' directory, you shouldn't need to change anything. (If you are happy without fancy fonts, edit the last lines in the '..output.R' files so that embed_fonts() does not run). 

## How to use it? 

To run this code successfully, please follow the instructions below:

1. Before you run the scripts, load the longmarch.Rproj file. This ensures that the default working directory is the "code" subfolder.
2. Run the code sequentially (in order of filename). If you run a later file without having run an earlier file, the script will probably throw an error. 
3.  Figures are output as .pdf's and tables as .csv's, but if you'd like standalone .tex files (or .tex) fragments with the explanatory foototes, run 99_figures.R and 99_tables.R. 99_figures.R will also create a folder called 'upload' in the 'output' folder, to which it will copy all figures that I use in the paper and supplementary appendix

Below I describe briefly describe what each file does. 

+ 10_mainmods.R: Runs main models
+ 11_robustmods.R: Runs robustness checks
+ 12_mods_regtable.R: Generates regression tables
+ 13_mods_graphs.R: Generates coefplots
+ 17_robustmods_output.R: Generates output from robustness checks
+ 20_descriptive_output.R: Generates descriptive output
+ 30_predictions.R: Runs counterfactuals
+ 31_predictions_stats.R: Calculates stats from counterfactuals
+ 32_predictions_output.R: Generates output from counterfactuals
+ 99_figures.R: Complies .tex file of figures
+ 99_tables.R: Compiles .tex file of tables
+ 99_write.R: Compiles statistics from analysis for writing

NB: Scripts that are not numbered contain functions or other code that is called by one or more of these scripts.

## Questions and Comments? 

If you encounter any issues with this code or with reproducing my results, please let me know at adaner.usmani[at]gmail.com. 
