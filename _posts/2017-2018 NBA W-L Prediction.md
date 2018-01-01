---
title: "NBA W/L Prediction"
author: "REX(RUIZHE) ZHOU"
categories: analysis
date: "1/1/2018"
---
<script type="text/javascript" async  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
   
**Introduction**

2017-2018 NBA season has been started for three months (Oct to Dec), and I conduct a nba win/loss prediction for the
whole rest of the games based on the games that had played. 

**Analysis**
Data Source is based on [Baseketball References](https://www.basketball-reference.com/). During this prediction analysis, I will use **Team Per Game Stats**, **Opponent Per Game Stats**, and **Miscellaneous Stats**, and take data in these three csv as my prediction features. The final result will give the probabilities of a team winning instead of providing absolute "W/L" result.

The result will based on the performance that had played and `Elo Score` each team obtained.

The `Elo rating system` is a method for calculating the relative skill levels of team in competitor-versus-competitor games, and it firstly used in chess. 

The intuitive understanding of `Elo Score` is assuming levels that team *A* and *B* perform in is $R_A$ and $R_B$, then the expectation of *A* wins *B* is

$$
E_A = \frac{1}{1+10^{\frac{R_B - R_A}{400}}}
$$

the expectation of *B* wins *A* is

$$
E_B = \frac{1}{1+10^{\frac{R_A - R_B}{400}}}
$$

If the true scoring $S_A$ is different from $E_A$, then the level score would be adjusted by

$$
R_A^{new} = R_A^{old} + K(S_A - R_A^{old})
$$

and $K$ will be given accordingly

**Requirement**

- `Python 3.6`
- module `numpy`
- module `pandas`
- module `sklearn`

