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
Analysis with Spectrally consists of the following steps:

#### 1) Load required functions from separate script
Running the cell to load required functions often shows warnings - just run it a second time to make these go away.

<img width="441" alt="image" src="https://github.com/user-attachments/assets/6dc3572d-96b8-4b53-b1eb-2bd2144906f9" />


