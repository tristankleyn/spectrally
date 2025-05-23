#### Spectrally parameters
##

| Parameter name | Description |
|----------|----------|
| downsample_factor | Proportion (0-1) of data to keep after down sampling. For example, a value of 0.01 would result in down sampling a dataset of size 1,000 items to 10 items. |
| compress_factor | Size of moving average window for compressing spectra with zero overlap between windows. For example, a value of 2 would compress spectra of length 100 to length 50. |
| subsetVar | Metavariable for subsetting data before plotting. The parameter "subsetVarLevel" then specifies which level of the subsetVar metavariable to show in the plot |
| subsetVarLevel | Species which level of the parameter "subsetVar" to show in the plot. Only used if "subsetVar" is specified. |
| xScale | xScale translates default spectral indices (columns 1 to N) to a specific scale. Can be left as NULL to use indices as x-values. |
| xLims | X-value limits for displaying data. |
| XYLabs | Labels for x and y axes |
| gridLines | Specifies how grid lines are displayed on the plot |
| plotBorders | Specifies whether or not borders are shown around plots |
| axisTitleFontSize | Font size for axis title text |
| axisTitleMargin | Margin of space separating axes titles and axes |
| axisLabelFontSize | Font size for axis tick label text |
| groupLabelFontSize | Font size for group labels |
| yBreaks | Number of breaks in y-axis |
| theme | Color scheme for plot borders |
| savePlot | Specifies whether or not to export plot to figures folder |
