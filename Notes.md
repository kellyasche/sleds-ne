- Population
	- Non college grads
	- categories 5-years after high school
		- No MN employment record
		- Not meaningful

# Question on non-college grads and workforce categories timeline

My question;

>To what extent is early post-high-school labor force disconnection persistent versus intermittent during the first five years after exit from formal education, and what factors are associated with these patterns?

We will filter out the population so we have the following;
1. Non-college grads
2. Has 5 years of workforce categories not including "Attending ps".
	1. For those that attended: examine workforce state for 5 years following the last year they attended post-secondary.
	2. For those that never attended: examine workforce state for 5 years following high school graduation.

For the first part of this analysis, we will group categories into the following;
- Attached
	- Meaningful emp region
	- Meaningful emp MN
- Disconnected
	- Not meaningful
	- No MN emp record

We will then examine the five years of categories and create one column for each individual with the following labels;
- **Persistently Disconnected:** Disconnected in 4 or 5 of the first 5 years
- **Disconnected (Broad):** Disconnected in 3 of the first 5 years
- **Intermittently Disconnected:** Disconnected in 1–2 of the first 5 years
- **Consistently Attached:** Disconnected in 0 of the first 5 years

The second part of this analysis will then refine the categories of disconnected-ness. 
1. Not meaningful workforce participation in the region
2. Not meaningful workforce participation outside the region
3. Not meaningful workforce participation in both outside and inside the region

## Methodology
We will do two steps;
1. Description of datasets for both the disconnect level categories and the regional disconnection categories.
2. Test the relationships between variables and these categories.

The variables we will use are;
- Demographic
	- `RaceEthnicity`
	- `SpecialEdStatus`
- High school characteristics
	- `Dem_Desc`
	- `avg.unemp.rate`
	- `avg.wages.pct.state` 
- High school academics
    - `pseo.participant`
    - `took.ACT`
    - `cte.achievement`
    - `total.cte.courses.taken`
    - `ACTCompositeScore`
    - `MCA.M`
    - `MCA.R`
    - `MCA.S`
- Post-secondary
    - `attended.ps`





