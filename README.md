[![Build Status](https://travis-ci.org/y-kamiya/strips-haskell.svg?branch=master)](https://travis-ci.org/y-kamiya/strips-haskell)

# STRIPS
STRIPS is a kind of automated planning algorithm.   
I adopted A* algorithm (searching for backward) to find appropriate plan

## How to Use
All you need is to define actions expressing domain you want to solve.    
Domain is expressed by list of Action in this library.  

please see example of blocks world problem.  
https://github.com/y-kamiya/strips-haskell/examples

and it is helpful to see the slide, for example    
https://piazza-resources.s3.amazonaws.com/i7w20eqdso51qs/iae8f9l7qff5rm/stripsintro.pdf?AWSAccessKeyId=AKIAIEDNRLJ4AZKBW6HA&Expires=1479094340&Signature=5ZArQb2KpBtraAdsya%2FSUz5bMb8%3D

## References
I referred to this program by lua to create the library.  
https://github.com/yhase7/lua_planner/blob/master/sample/blockworld_strips.lua
