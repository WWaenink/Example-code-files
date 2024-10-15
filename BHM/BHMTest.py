#this is just a little script I'm using to get more familiar with bayesian hierarchical models.
#the intended output is the annual chance of a bear being killed, based on multiple years of data. Realistically you'd probably use year as a random effect, but I didn't to keep it simple (I also didn't use any random effect of year in generating the data)
#posterior is examined in another script so I don't have to rerun the model fitting every time.

#most important assumptions
#-a bear being killed does not impact bear population

#stuff to do
#include more variables
#do this more explicitly without a package, maybe in R

import pymc as pm
import pandas as pd
import numpy as np
import os
import arviz as az
#import plotly.express as px

def run_model(data):
    #create model
    with pm.Model() as model:
        #hyperpriors - half cauchy because the sd has to be positive, high beta to make it weakly informative.
        sd_country = pm.HalfCauchy('sd_country', beta = 5) 
        sd_county = pm.HalfCauchy('sd_county', beta = 5)
        
        #setting country and county effect, mean is no difference and sd is the hyperprior
        country_effect = pm.Normal('country_effect', mu = 0, sigma = sd_country, shape = len(data['Country'].unique()))
        county_effect = pm.Normal('county_effect', mu = 0, sigma = sd_county, shape = len(data['County'].unique()))
        
        #setting base prior (-2 corresponds to around a 10% chance after the logit transformation, sigma is high to make it very weakly informative)
        beta_0 = pm.Normal('beta_0', mu = -2, sigma = 5)
        
        #combine levels into linear model
        logit_p = beta_0 + country_effect[data["Country_id"].values] + county_effect[data["County_id"].values]
        #pm.Deterministic is used to make sure this just a deterministic function, since the stochastic part is already included in the line above. I'm just doing this because I don't know how PyMC works and I'm getting chatgpt to explain it to me, but I want to see what happens if I change this and how it reacts.
        p = pm.Deterministic('p', pm.math.invlogit(logit_p))
        
        #final likelihood function
        chanceOfDeath = pm.Binomial("chanceOfDeath", n = data['bear_pop'].values, p = p, observed = data['bears_killed'].values)
        
        #sample posterior
        post_sample = pm.sample(2000, tune = 1000, target_accept = 0.95, init='adapt_diag')
        
    return(post_sample)

if __name__ == '__main__':

    #change directory
    os.chdir("C:/Code Projects/python/BHMTest")
    
    #read in data
    bearKillsdfWide = pd.read_csv("bearskilled.csv", header = 0)
    bearKillsdf = (
        bearKillsdfWide
        .melt(id_vars = ["County", "Country", "bear_pop"], 
              value_vars = [f'Year_{i}' for i in range(1,31)],
              var_name = "year",
              value_name = "bears_killed")
        .assign(Country_id = lambda df: df['Country'].astype('category').cat.codes,
                County_id = lambda df: df['County'].astype('category').cat.codes)
        )
    
    
    pd.melt(bearKillsdfWide, id_vars = ["County", "Country", "bear_pop"], 
                          value_vars = [f'Year_{i}' for i in range(1,31)],
                          var_name = "year",
                          value_name = "bears_killed")
    print(bearKillsdf)

    post_sample = run_model(bearKillsdf)
    az.to_netcdf(post_sample, "posterior_data.nc")
    
