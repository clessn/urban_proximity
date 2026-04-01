# Urban Proximity & Indigenous Attitudes

**Research project:** Does perceived proximity to Indigenous communities shape attitudes toward them, and is that proximity systematically biased toward urban/well-off reserves?

## Research Question

Canadians who live near a reservation think they "know" Indigenous communities, but what they actually know is an atypical, economically advantaged slice. The majority of Indigenous communities — remote, underfunded, struggling — are invisible to most Canadians.

## Theoretical Mechanism

- **Proximity** → Contact hypothesis → More favorable attitudes
- **But:** Proximity is biased toward urban, relatively well-off reserves
- **Prediction:** Proximity to *urban* reserves ↑ favorable attitudes; proximity to *remote/poor* reserves → null effect (invisibility)

## Data Sources

- **Canadian Election Study (CES) 2021** — dependent variables (attitudes toward Indigenous people)
- **Indian Reserve geographic boundaries** — Statistics Canada / CIRNAC shapefiles
- **Community Well-Being Index (CWB)** — Crown-Indigenous Relations, composite SES index
- **Statistics Canada CMA/CA classifications** — urban/rural classification of reserves

## Variables

### Dependent Variable
- Indigenous attitude/perception items from CES 2021

### Independent Variable
- Distance (km) from respondent CSD to nearest First Nation reserve centroid

### Moderator
- Community Well-Being Index score of nearest reserve
- Urban/rural classification (CMA/CA/remote)

### Controls
- Education, age, province, political ideology, media consumption

## Key Analysis

Interaction: **Distance × CWB index** predicting attitudes
- Near wealthy reserves → most favorable attitudes
- Near poor/remote reserves → null or negative (invisibility hypothesis)

## Language

R

## Structure

```
data/          # raw data (not tracked)
data-clean/    # processed data
R/             # analysis scripts
output/        # figures and tables
```
