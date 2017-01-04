S&P 500 Stock Predictions
-------------------------------

A dataset of stock predictions submitted by forum community users, as part of the bogleheads.org stock market prediction contest.

This project collects all user guesses for the ending price of the stock market's S&P 500 at the end of the year. Users submit their guesses within the first 10 days of the year.

## The Dataset

Download the [raw](https://raw.githubusercontent.com/primaryobjects/stock-predictions/master/data/predictions-raw.csv) or [pre-processed](https://raw.githubusercontent.com/primaryobjects/stock-predictions/master/data/predictions.csv) dataset as a CSV file.

The dataset contains the following fields:

"id" - A unique id for the original form post.

"author" - User name of the guess.

"history" - Total number of posts made by the user.

"date" - Date of the guess.

"bid" - S&P 500 ending year guess.

"bull" - True if the guess is greater than the S&P 500 opening day price.

The predictions-raw CSV file contains stock market predictions, collected in their raw form, directly from user postings. The pre-processed CSV file removes bids beyond the contest ending data, removes duplicates, removes missing bids, removes the introduction post, and adds the "bull" column.

## Results

The following charts were produced from the dataset.

![2017 S&P 500 Predicitions](https://raw.githubusercontent.com/primaryobjects/stock-predictions/master/images/bids-2017.png)

![Are predictions bullish or bearish in 2017?](https://raw.githubusercontent.com/primaryobjects/stock-predictions/master/images/bullsvsbears-2017.png)

![Histogram of Predicitions](https://raw.githubusercontent.com/primaryobjects/stock-predictions/master/images/histogram-2017.png)

![Overview of Predicitions](https://raw.githubusercontent.com/primaryobjects/stock-predictions/master/images/overview-2017.png)

## References

[Bogleheads.org](https://www.bogleheads.org/)

[DieHards Stock Prediction Archive](http://www.lostoak.com/ls/diehards/contest/)

## Copyright

Copyright (c) 2017 Kory Becker http://primaryobjects.com/kory-becker

## Author

Kory Becker
http://www.primaryobjects.com