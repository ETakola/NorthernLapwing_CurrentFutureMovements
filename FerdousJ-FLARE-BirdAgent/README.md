# FLARE (Future Lapwing Responses to Expansions)
This project was created and executed by Jannatul Ferdous (@JannatulMoon0204) during a Research Assistant position in the BIOECOS group in the Department of Landscape Ecology at Helmholtz Centre for Environmental Research (UFZ).

## Project Overview

This project simulates movement patterns of the Northern Lapwing (_Vanellus vanellus_) using an Individual-Based Modelling (IBM) approach. By combining GPS tracking data with habitat information derived from landcover raster data, it explores how these birds interact with their environment and how habitat preferences influence their movement behavior. We explored how the expansion of forests in the Netherlands can affect the movement of northern lapwings.

## Features

- **Empirical movement integration:** Agents follow observed GPS trajectories adjusted by habitat sensitivity.
- **Habitat sensitivity analysis:** Calculates habitat preference scores based on usage relative to availability.
- **Hypothetical landscape scenarios:** Models how changes in habitat distribution affect bird movement and behavior.
- **Data export:** Outputs sensitivity scores and detailed movement logs as Excel files.
- **Optional visualization:** Supports side-by-side animations comparing current and hypothetical landscapes (disabled by default).

## Methodology

- Initializes agents with real-world GPS data and a landcover raster.
- Models individual bird movements step-by-step, recording habitat use, step length, and turning angles.
- Incorporates habitat sensitivity to influence movement choices in a hypothetical scenario using a “SmartBirdAgent”.
- Allows for scenario comparison to assess potential impacts of landscape changes on bird behavior.

### Visualization

- Animation code is included but disabled by default due to performance considerations.
- To enable, uncomment the animation section near the end of the script.

## Outputs

- Habitat sensitivity scores (`.xlsx`)
- Detailed movement logs for each bird agent (`.xlsx`)
- (Optional) Animation video comparing scenarios (`.mp4`)
