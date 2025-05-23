| Parameter name | Description | Input type/range |
|---|---|---|
| axisLabelFontSize | Font size for axis tick label text | 1+ |
| axisTitleFontSize | Font size for axis title text | 1+ |
| axisTitleMargin | Margin of space separating axes titles and axes | 1+ |
| compress_factor | Size of moving average window for compressing spectra with zero overlap between windows. For example, a value of 2 would compress spectra of length 100 to length 50. | 1+ |
| downsample_factor | Proportion (0-1) of data to keep after down sampling. For example, a value of 0.01 would result in down sampling a dataset of size 1,000 items to 10 items. | 0-1 |
| gridLines | Specifies how grid lines are displayed on the plot | 'both', 'h', or 'v' |
| groupLabelFontSize | Font size for group labels | 1+ |
| plotBorders | Specifies whether or not borders are shown around plots | TRUE or FALSE |
| savePlot | Specifies whether or not to export plot to figures folder | TRUE or FALSE |
| subsetVar | Metavariable for subsetting data before plotting. The parameter "subsetVarLevel" then specifies which level of the subsetVar metavariable to show in the plot | text (e.g. 'species') |
| subsetVarLevel | Species which level of the parameter "subsetVar" to show in the plot. Only used if "subsetVar" is specified. | 1+ |
| theme | Color scheme for plot borders | 'dark' or 'light' |
| xLims | X-value limits for displaying data. | double, numeric (e.g. c(0, 100) |
| xScale | xScale translates default spectral indices (columns 1 to N) to a specific scale. Can be left as NULL to use indices as x-values. | double, numeric (e.g. c(0, 100) |
| XYLabs | Labels for x and y axes | double, text (e.g. c('Time (s)', 'Abundance')) |
| yBreaks | Number of breaks in y-axis | 1+ |
