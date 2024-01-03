# Importance of Cellphone Mobility Data in Bayesian Disease Mapping Models Concerning COVID-19

## Overview

This repository contains the code used in my project thesis. In this project thesis, COVID-19 case count data from the spanish regions Madrid and Castilla Y Leon were analyzed in a spatial and Bayesian setting. The approach for the spatial analysis uses the well-known Besag-York-Mollié model, and extends this model by incorporating cellphone mobility data as an addiational ICAR term to better explain the observed spatial variation. Most of the work done in this project thesis is a reproduction of the results in the paper by Slater et al (2022), but expands upon this work by implementing the model in R-INLA rather than Stan, and by using MakeMyPrior to specify the priors. In addition to replicating the results in a new framework, further extensions were made to explore the impact and importance of cellphone mobility data in these models. The repository contains the code used to obtain all the results and figures, but does not contain the data used. For access to the data, contact either the authors of Slater et al (2022) or me. 

## Features

- **Integrated Nested Laplace Approximation (INLA):** Utilizing the R-INLA package, this project implements INLA for efficient and approximate Bayesian inference.

- **Cellphone Mobility Data Integration:** To better explain the observed spatial variation, the models used in this thesis incorporate cellphone mobility data.

- **MakeMyPrior:** The MakeMyPrior package is employed to customize and fine-tune prior distributions, allowing for a flexible and tailored approach in the Bayesian modeling process.

## Getting Started

1. Clone this repository: `git clone https://github.com/Markutr/ProjectThesis.git`
2. Install the required R packages (check the imported packages in the provided scripts in the folder named "r"): `install.packages(c("INLA", "MakeMyPrior", ...))`
3. Set appropriate working directory (modify in each script): `setwd("path to cloned repository")`
5. Move the data to the appropriate location: /data
6. Run the provided scripts in the follwing order to reproduce the analysis: dataProcessing.R, inference.R and then makePlots.R

## Acknowledgments

I express my gratitude to the contributors of R-INLA and MakeMyPrior, my supervisor Andrea Riebler for excellent guidance, and the authors of Slater et al (2022) for supplying me with the data and code used in their paper.

## References

Slater, Justin J., et al. "Capturing spatial dependence of COVID-19 case counts with cellphone mobility data." Spatial Statistics 49 (2022): 100540.
