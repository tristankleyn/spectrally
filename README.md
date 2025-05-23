<div align="center">
  <img width="1100" alt="image" src="https://github.com/user-attachments/assets/156b4746-59f5-476b-82e8-0b4000d8a737" />
  <img width="1100" alt="image" src="https://github.com/user-attachments/assets/3ec22265-7bcf-4bee-a53d-bd1161b92d14" />
  <img width="1100" alt="image" src="https://github.com/user-attachments/assets/3e437c48-babe-414b-9bf8-78b1e257ec59" />
</div>

### Spectrally: A simple, intuitive workflow for spectral analysis

##
Spectrally is a repostitory containing code notebooks designed for facilitating analysis of spectral data, where spectra can be any series of measurements forming a 1-dimensional spectrum. The aim of the notebook is to allow users to load any kind of spectral data, analyse and compare spectra, and export results easily without having to code themselves. Several parameters used for the analysis are flexible and can be adjusted to achieve optimal results.

### spectrally/

> #### └── [formattedData](https://github.com/tristankleyn/spectrally/tree/main/formattedData)/
> Contains formatted standard spectral tables in .csv format for different types of example spectral data

> #### └── [formattingScripts](https://github.com/tristankleyn/spectrally/tree/main/formattingScripts)/
> Contains scripts for converting specific types of input data into standard spectral tables (compatible with groupSpectra.qmd)

> #### └── [rawData](https://github.com/tristankleyn/spectrally/tree/main/rawData)/
> Contains raw example data to format into standard spectral tables (compatible with groupSpectra.qmd)

> #### └── groupSpectra.qmd
> Quarto notebook for analysing spectra (main script)

> > #### └── spectraFunctions.R
> Script containing functions required by groupSpectra.qmd

##
## Spectrally: Step-by-step

##
Analysis with Spectrally consists of the following steps:

#### 1) Load required functions from separate script
Running the cell to load required functions often shows warnings - just run it a second time to make these go away.

<img width="400" alt="image" src="https://github.com/user-attachments/assets/fe2ede09-4028-4bb5-b674-d6c1302cf187" />


#### 2) Read in your spectral data
To anayse spectral data in Spectrally, your data must be prepared in a standardized way and saved in .CSV format, where the first column provides the ID of each sample, the columns following ID contain optional metadata on sample, and all columns afterwards contain the spectral data with columns labelled 1 to N, where N is the number of spectral values for each sample. The image below shows an example of this format for an [open-access Kaggle dataset](https://www.kaggle.com/datasets/andriitrelin/cells-raman-spectra) with Raman spectroscopy data on cancerous and normal cells. Here, type and trial represent metavariables for the data and spectral values are provided in numbered columns to their right:

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/9881ae0b-03ef-4783-b3bf-e5ffbb3260d1" />
</p>

<p align="justify">
This required formaatting is explained in the Spectrally notebook and is checked automatically when your data is loaded. 
</p>

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/a89799c1-7d13-4e6e-b92d-90fb105444d2" />
</p>


#### 3) Optional step: Reduce size of dataset to speed up analysis
After reading in your data in the correct format, there is the option to down sample and/or compress your data to improve efficiency in analysis. This really only makes a difference for calculating pairwise similarities later in the script, which is computationally expensive and can be significantly sped up by reducing your dataset by compression of spectra or down sampling. 

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/8c05fc61-559c-4e8a-946f-8d34ee3118c6" />
</p>

#### 4) Visualise your data
Two cells for plotting your data in different ways are available in the next part of the Spectrally notebook. Using the function overlaySpectra(), the first cell outputs a plot overlaying spectral data grouped by specified metavariables. In the example below, Raman spectroscopy data is shown overlaid for different types of cells (columns) across numbered trials (rows). Several parameters can be adjusted here to contrl plot aesthetics such as grid lines, plot borders, and font sizes.

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/3bb65b74-0117-432d-8681-57ed48492998" />
</p>

A second plotting cell allows for direct spectral comparison between two indiviudal samples, while the code cells after this second plot demonstrate how spectral distance can calculated by averaging distance between spectra using varying window sizes.

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/2418a344-67e0-4141-9c78-7c22c555651b" />
</p>


#### 5) Analyzing spectral similarity across the dataset
The next part of the Spectrally workflow employs multi-dimensional scaling (MDS) to visualize spectral similarity across in the dataset in two dimensions. The primary objective of MDS is to represent a set of observations in a lower-dimensional Euclidean space while preserving the inter-observation distances in the original data (explained in further detail [here](https://www.bristol.ac.uk/media-library/sites/cmm/migrated/documents/chapter3.pdf)). The input for MDS is a pairwise distance matrix between all observations - naturally, this is computationally expensive to calculate and may take up to an hour or longer for datasets containing hundreds of observations. The result obtained from MDS is a plot such as the one below, which shows Raman spectra in MDS space grouped by metavariables such as cell type and trial.

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/64790c16-a347-43a4-b371-e55d21c8d8b1" />
</p>


#### 6) Test for statistical differences between groups
The final part of the Spectrally workflow allows for simple statistical comparisons to made between groups based on their distributions in MDS space. The code cell shown below lists distinct groups based on the variables selected in the previous code cell:

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/b61b2615-3d18-4b4e-a606-cbcb1d4536c6" />
</p>

And the following code cell conducts a [permutation test](https://statisticsbyjim.com/glossary/permutation-test/) between select groups, whereby distances between means of random samples from each group are compared to distances between random samples. The proportion of times where the observed distance between means was greater than chance is shown and can compared with a significance level (alpha) to estimate whether or not there is a statistical difference between the select groups.

<p align="left">
<img width="400" alt="image" src="https://github.com/user-attachments/assets/a73765b8-4019-40b8-b34a-0d982956556e" />
</p>
