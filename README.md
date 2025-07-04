# Modelling movement patterns of Northern Lapwings (_Vanellus vanellus_); present and future dynamics

## Overview

This repository hosts the codebase, documentation, and supporting data for a movement ecology project focused on the Northern Lapwing (_Vanellus vanellus_). The goal of this project is to model and simulate lapwing movement at the individual level to investigate population dynamics under varying environmental and anthropogenic conditions.

_Here is a picture of a Northern Lapwing: https://cdn.download.ams.birds.cornell.edu/api/v1/asset/609204339/2400_

The folder "EsguerraJ-HabitatSelection" contains the material for the Master thesis of Johanna Esguerra. The thesis is available at https://opus.bsz-bw.de/hft/frontdoor/index/index/start/0/rows/10/sortfield/score/sortorder/desc/searchtype/simple/query/esguerra/docId/988 and the code and data for the scientific publication of the thesis.

The folder "FerdousJ-FLARE-BirdAgent" contains the report, code and data of the project FLARE (Futre Lapwing Responses to Expansions), created and executed by Jannatul Ferdous. 


## Scientific Motivation

The Northern Lapwing is a ground-nesting shorebird of conservation concern across its range, especially in Western Europe. Key threats include:

- **Agricultural intensification**, leading to habitat loss and reduced nesting success.
- **Predation pressure**, particularly from generalist mesopredators in human-modified landscapes.
- **Climate variability**, affecting breeding phenology and resource availability.

Despite conservation efforts, population declines persist, in part due to complex and interacting ecological mechanisms that are difficult to assess through observational studies alone. 

## Project Objectives

- Develop a robust IBM framework and use step selection functions for simulating Northern Lapwing ecology across space and time.
- Quantify the relative influence of environmental factors on movement patterns.
- Evaluate conservation scenarios under alternative land use and climate regimes.

## Methodological Framework

- **Spatial resolution**: Grid-based landscape incorporating habitat heterogeneity (e.g., grasslands, croplands, wet areas).
- **Temporal resolution**: Daily time steps across breeding season (March–August) and non-breeding season.
- **Agents**: Individuals with defined life history attributes and state variables (e.g., age, location, reproductive status).
- **Statistics**: We used agent modelling and step-selection functions to explore the movement patterns of individual birds.

## Why Step Selection Functions?

Step Selection Functions (SSFs) are a powerful analytical approach used to model animal movement decisions by quantifying how individuals select their next step in relation to environmental features and constraints. Unlike simple random or rule-based movement models, SSFs incorporate empirical movement data to probabilistically describe habitat preferences and avoidance behaviors at a fine spatial and temporal scale.

Using SSFs allows us to:

- **Capture fine-scale movement decisions**, reflecting how individuals navigate habitat features, avoid predators, and respond to disturbances.
- **Incorporate spatially explicit environmental variables** directly into movement modeling, improving ecological realism.
- **Allow assessment of habitat connectivity and landscape use**, which are crucial for identifying priority areas for conservation.
- **Facilitate exploration of movement responses to environmental change**, enabling predictions under scenarios like habitat modification or climate shifts.

## Why Individual-Based Modelling?

IBMs are particularly suitable for studying population-level responses:

- **Behavioral and demographic heterogeneity**: Individuals vary in age, reproductive status, habitat preference, and movement decisions.
- **Fine-scale spatial dynamics**: Nest-site selection, predator avoidance, and territory establishment are highly spatial.
- **Nonlinear and emergent processes**: Population-level outcomes (e.g., local extinction or resilience) can emerge from simple individual rules.
- **Management scenario testing**: Evaluate the impact of conservation interventions such as agri-environment schemes, predator control, and habitat restoration.

This project leverages IBM to integrate empirical data with theoretical models to inform adaptive management strategies.
