# aeconomicar

![Lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
![CI](https://github.com/aeconomica/aeconomicar/workflows/CI/badge.svg)
[![codecov.io](http://codecov.io/github/aeconomica/aeconomicar/coverage.svg?branch=main)](http://codecov.io/github/aeconomica/aeconomicar?branch=main)
[![Documentation](https://img.shields.io/badge/docs-dev-blue.svg)](https://aeconomica.github.io/aeconomicar/dev)

aeconomicar provides quick and easy access to the [Aeconomica](https://aeconomica.io) data API.

To use this package, you will need to sign up for a free Aeconomica account to get an API key. You can find the API key in the [account page](https://aeconomica.io/account) of your
Aeconomica account.

## Installation

aeconomicar is not registered in CRAN. As such to install to the lastest release, run:
  ```
library(remotes)
remotes::install_github("https://github.com/aeconomica/aeconomicar.git", ref = "stable")
```

(Alternatively you can install the development version by removing the `ref = "stable"`)

## Usage

To grab data for a series - e.g. the level of GDP - simply run (where "YOUR_API_KEY" is the API key from your Aeconomica account):

```
library(aeconomicar)
set_apikey("YOUR_API_KEY")
fetch_series("GDP")
```

To grab an entire dataset of series, and their dimensions - such as ABS weekly jobs by State - just run:

```
library(aeconomicar)
set_apikey("YOUR_API_KEY")
fetch_dataset("WJP_STATE")
```

In each case the series or dataset keys can be found by searching the Aeconomica website. When viewing a series or dataset, the key is shown in the top right.
You can also click on the "R" button in the bottom left to get the code you need for the dataset you are viewing.
