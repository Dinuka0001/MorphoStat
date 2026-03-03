# MorphoStat

<p align="center">
  <img src="https://img.shields.io/badge/R-Shiny-blue?style=for-the-badge&logo=R" alt="R Shiny">
  <img src="https://img.shields.io/badge/Version-1.0.0-green?style=for-the-badge" alt="Version">
  <img src="https://img.shields.io/badge/License-MIT-yellow?style=for-the-badge" alt="License">
</p>

**MorphoStat** is a modern, interactive R Shiny application for statistical morphological analysis using geometric morphometrics. It provides a comprehensive suite of tools for analyzing landmark-based morphometric data with publication-ready visualizations.

## Features

### Data Input
- **Multiple file formats**: Morphologika (.txt), TPS (.tps), NTS (.nts), FCSV (3D Slicer)
- **Project save/load**: Save and restore complete analysis sessions (.rds)
- **Auto-detection**: Automatically detects 2D/3D landmark configurations

### Analysis Pipeline
- **Generalized Procrustes Analysis (GPA)**: Removes size, position, and rotation effects
- **Principal Component Analysis (PCA)**: Identifies major axes of shape variation
- **PERMANOVA**: Tests for significant shape differences between groups
- **PERMDISP**: Tests for homogeneity of group dispersions
- **Allometry Analysis**: Evaluates shape-size relationships
- **Pairwise Comparisons**: Multiple group comparisons with distance metrics

### Visualizations
- **PCA Plots**: 2D colored, black & white, and 3D scatter plots
- **Shape Deformations**: Mean, min, and max PC shape visualization
- **Procrustes Superimposition**: View all specimens or individual samples
- **Wireframe Diagrams**: Multiple anatomical views (dorsal, sagittal, coronal)
- **Statistical Plots**: Centroid size boxplots, allometry regression, eigenvalue scree plots
- **Outlier Detection**: Identify potential outliers based on Procrustes distance

### Customization
- **Custom group colors**: Assign specific colors to groups
- **Group renaming**: Rename groups for publication
- **Data spread visualization**: Confidence ellipses, convex hulls, density contours
- **Diagram transformations**: Flip and rotate shape visualizations
- **Export options**: PNG and SVG formats with customizable dimensions

## Installation

### Prerequisites
Make sure you have R (>= 4.0.0) installed. You'll also need the following R packages:

```r
install.packages(c(
  "shiny",
  "shinydashboard", 
  "bslib",
  "shinyjs",
  "ggplot2",
  "DT",
  "colourpicker",
  "scatterplot3d",
  "rgl"
))

# Bioconductor/specialized packages
install.packages("geomorph")
install.packages("RRPP")
install.packages("Morpho")
install.packages("vegan")
install.packages("rstatix")
```

### Running the App

1. Clone this repository:
```bash
git clone https://github.com/yourusername/morphostat.git
cd morphostat
```

2. Open R or RStudio and run:
```r
shiny::runApp("App.R")
```

Or run directly:
```r
shiny::runApp("path/to/morphostat")
```

## Usage

### Quick Start

1. **Select input type**: Choose your data format (Morphologika, TPS, NTS, or FCSV)
2. **Upload data**: Use the file input to upload your landmark data
3. **Configure wireframe** (optional): Define landmark connections
4. **Run analysis**: Click "Run Analysis" to perform GPA, PCA, and statistical tests
5. **Explore results**: Navigate through tabs to view plots and statistics
6. **Export**: Download results, plots, or save the entire project

### Input File Formats

#### Morphologika Format (.txt)
```
[individuals]
10
[landmarks]
48
[dimensions]
3
[names]
Specimen_01
...
[labels]
Group_A
...
[rawpoints]
12.345 23.456 5.678
...
```

#### TPS Format (.tps)
```
LM=48
12.345 23.456
13.456 24.567
...
ID=Specimen_01
```

#### Wireframe Links
Define landmark connections as comma-separated pairs:
```
1,2, 2,3, 3,4, 4,5, 5,1
```

## Screenshots

*Coming soon*

## Statistical Methods

| Method | Purpose | Package |
|--------|---------|---------|
| GPA | Procrustes superimposition | geomorph |
| PCA | Shape variation analysis | geomorph |
| PERMANOVA | Group difference testing | vegan, RRPP |
| PERMDISP | Dispersion homogeneity | vegan |
| Allometry | Size-shape relationship | RRPP |

## Citation

If you use MorphoStat in your research, please cite:

```
Adasooriya, D. (2026). MorphoStat: An Interactive R Shiny Application for 
Statistical Morphological Analysis. Version 1.0.0.
```

Also cite the underlying R packages:
- Adams DC, Collyer ML, Kaliontzopoulou A. 2024. Geomorph: Software for geometric morphometric analyses. R package version 4.0.8.
- Oksanen J, et al. 2022. vegan: Community Ecology Package. R package version 2.6-4.

## Author

**Dinuka Adasooriya**  
Yonsei University College of Dentistry

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## Acknowledgments

- The R Shiny team for the excellent framework
- The geomorph package authors for their comprehensive morphometrics tools
- The vegan package authors for multivariate analysis tools
