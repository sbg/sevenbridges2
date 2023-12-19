# sevenbridges2 <!-- omit in toc -->

<!-- badges: start -->
  [![R-CMD-check](https://github.com/sbg/sevenbridges2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbg/sevenbridges2/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## Overview <!-- omit in toc -->

sevenbridges2 is an API client package that provides an interface for the [Seven Bridges Platform](https://www.sevenbridges.com/) (US, EU, China), [Cancer Genomics Cloud](https://www.cancergenomicscloud.org/), [CAVATICA](http://www.cavatica.org/), and [BioData Catalyst Powered by Seven Bridges](https://platform.sb.biodatacatalyst.nhlbi.nih.gov/) public APIs.

The [Seven Bridges Platform](https://www.sevenbridges.com/) is a cloud-based environment for conducting bioinformatics analysis. It is a central hub for teams to store, analyze, and jointly interpret their bioinformatic data. The Platform co-locates analysis pipelines alongside the largest genomic datasets to optimize processing, allocating storage, and compute resources on demand.

The [Cancer Genomics Cloud (CGC)](https://www.cancergenomicscloud.org/), powered by [Seven Bridges](https://www.sevenbridges.com/), is also a cloud-based computation environment. It was built as one of three pilot systems funded by the [National Cancer Institute](https://www.cancer.gov) to explore the paradigm of colocalizing massive genomics datasets, like The [Cancer Genomics Atlas (TCGA)](https://cancergenome.nih.gov), alongside secure and scalable computational resources to analyze them. The CGC makes more than a petabyte of multi-dimensional data available immediately to authorized researchers. You can add your data to analyze alongside TCGA using predefined analytical workflows or your own tools.

[CAVATICA](http://www.cavatica.org/), powered by [Seven Bridges](https://www.sevenbridges.com), is a data analysis and sharing platform designed to accelerate discovery in a scalable, cloud-based compute environment where data, results, and workflows are shared among the world's research community. CAVATICA is built in collaboration with the Children Hospital of Philadelphia and it is focused on pediatric data.

The [BioData Catalyst](https://platform.sb.biodatacatalyst.nhlbi.nih.gov/), powered by [Seven Bridges](https://www.sevenbridges.com), is a computational environment on the cloud. It hosts several genomic datasets, alongside tools for analyzing genomic information, and features for collaboration.

## Table of Contents <!-- omit in toc -->

- [Installation](#installation)
  - [Development Version](#development-version)
- [Features](#features)
  - [Flexible Authentication Methods](#flexible-authentication-methods)
  - [Complete API R Client](#complete-api-r-client)
  - [Cross Environment Support](#cross-environment-support)
- [Contribute](#contribute)
- [Copyright](#copyright)

## Installation

Easiest way is to install package from CRAN:

``` r
# Install package from CRAN:
install.packages("sevenbridges2")
```
### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of sevenbridges2 from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("sbg/sevenbridges2@develop")
```

## Features

The `sevenbridges2` package includes the following features:

### Flexible Authentication Methods

Multiple authentication methods support.

#### Direct authentication

To use direct authentication, users need to specify one of `platform` or `url`,
with the corresponding `token`. Examples of direct authentication:

```r
# Direct authentication with setting platform parameter
a <- Auth$new(
  token = "<your_token>",
  platform = "aws-us"
)
```

#### Authentication via system environment variables

To set the two environment variables in your system, you could use
the function `sbg_set_env()`. For example:

```r
# Set environment variables
sevenbridges2:::sbg_set_env(
  url = "https://cgc-api.sbgenomics.com/v2",
  token = "<your_token>"
)

# Authenticate using environment variables
a <- Auth$new(from = "env")
```

#### Authentication via a user configuration file

You collect and manage your credentials for multiple accounts across various 
Seven Bridges environments:

```r
# Authenticate using file configuration and specific profile
a <- Auth$new(from = "file", profile_name = "aws-us-<username>")
```


Please check `vignette("Authentication_and_Billing", package = "sevenbridges2")` for technical details about all available authentication methods.

### Complete API R Client

A complete API R client with a user-friendly, object-oriented API with printing and support operations for API requests relating to users, billing, projects, files, apps, and tasks. Short examples are also included, as shown below:

```r
# Get a project by pattern-matching its name
p <- a$projects$query("demo")

# Get a project by its id
p <- a$project$get(id = "username/demo")

# List files in project
p$list_files()

# Create upload job and set destination project
upload_job <- a$upload(
  path = "/path/to/your/file.txt",
  project = destination_project,
  overwrite = TRUE,
  init = TRUE
)
```

Please check `vignette("quickstart", package = "sevenbridges2")` for technical details about all available API methods and features.

### Cross Environment Support

Cross-platform support for Seven Bridges environments, such as [Seven Bridges Platform](https://www.sevenbridges.com/) (US, EU, China), [Cancer Genomics Cloud](https://www.cancergenomicscloud.org/), [CAVATICA](http://www.cavatica.org/), and [BioData Catalyst Powered by Seven Bridges](https://platform.sb.biodatacatalyst.nhlbi.nih.gov/) on either Amazon Web Services or Google Cloud Platform.

## Documentation

[Docs website](https://r-stack.pages.sbgenomics.com/sevenbridges2/)

## Contribute

Please file bug reports/feature requests on the [issue page](https://github.com/sbg/sevenbridges/issues), or create pull requests [here](https://github.com/sbg/sevenbridges2/pulls).

Contributors should sign the [Seven Bridges Contributor Agreement](https://secure.na1.echosign.com/public/esignWidget?wid=CBFCIBAA3AAABLblqZhAqt_9rHEqy2MggS0uWRmKHUN2HYi8DWNjkgg5N68iKAhRFTy7k2AOEpRHMMorxc_0*) before submitting a pull request.

## Copyright

© 2023 Seven Bridges Genomics, Inc. All rights reserved.

This project is licensed under the terms of the [Apache License 2.0](LICENSE).
