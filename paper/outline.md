# Estimating the Effect of Washington's 2021 Capital Gains Tax

## Abstract
- [ ] Information specifying the tax
- [ ] Description of our goal
- [ ] Description of our methodology

## Introduction
- [ ] Summary of the tax, it's history of controversy, and apparent effects
- [ ] Answer to what specifically we are measuring
- [ ] Short summary of our "novel" synthetic control pipeline mentioning inspiration from Abadie (2010)
- [ ] Outline of the rest of the paper

## Data
- [ ] Thorough description of IRS SOI dataset: who maintains it, why was it created?
- [ ] Description of raw migration variables: N1, N2, and AGI
- [ ] Introduction to notation for migration variables
- [ ] Figure breaking down shares of each age group and AGI class per year

## Methodology
- [ ] Description of transformed migration variables: what do they represent, why are they more useful?
- [ ] Introduction to notation for transformed migration variales
- [ ] Process to filter out states with high bilateral migration with Washington: why is this imporant?
- [ ] Process to filter out states with significant taxes affecting high-income residents around 2021: which states and why is this important?
- [ ] Process to weight donor states on low-income pre-trends: why is this OK and important?
- [ ] Difference-in-difference computation explanation
- [ ] Prose justification of parallel trends and no anticipation, along with plans to verify (visual plot + search trends analysis)
- [ ] Description of permutation placebo tests to measure significance of estimates

## Results
- [ ] List of states with high bilateral migration with Washington, justified with distribution plots and Tukey Fences
- [ ] Table listing donor weights for all states
- [ ] Figure showing the synthetic control unit exhibits parallel trends for both low-income + mid-income (all years) and high-income (before 2021)
- [ ] Table showing pre-trend difference-in-difference estimates are small
- [ ] Results for search trends analysis, disproving anticipation
- [ ] Difference-in-difference estimates for all income/age combinations
- [ ] Figure overlaying Washinton's migration uptick in 2021 versus other states
- [ ] Results of permutation placebo tests

## Discussion
- [ ] What does our estimate (and its uncertainty) mean?
- [ ] Policy implications

### Limitations
- [ ] Possibility of no anticipations being violated
- [ ] Difficulties satisfying no spillovers when out-migration of one state is an in-migration of another

### Extensions
- [ ] Larger dataset of all taxes targeting high-income residents, not just Washington

## Conclusion
