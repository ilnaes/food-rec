# Ingredients recommender

An item-based collaborative filtering recommender for recipe ingredients.  The data used in training was scraped from Epicurious in 11/2021 and cleaned using an [updated NYTimes CRF model](https://github.com/mtlynch/ingredient-phrase-tagger).

Relevant files are:

* scrape.py - script to scrape info from Epicurious into json form
* process\_json.Rmd - format the json into csv and added the processed ingredients
* prep.R - various other ad-hoc cleaning for ingredients
* eda.Rmd ([html notebook](https://ilnaes.github.io/food-rec/eda.nb.html)) - EDA on the recipes and ingredients data
* collab-filtr.Rmd ([html notebook](https://ilnaes.github.io/food-rec/collab-filtr.html)) - code for cross validating recommender over various parameters
* food-rec/ingr.rec.R - R code for building recommender object
* food-rec/app.R - shiny app code

A working shiny app is at https://ilnaes.shinyapps.io/ingredients-rec/.
