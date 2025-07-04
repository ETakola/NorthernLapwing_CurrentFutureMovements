# Individual-Based Modelling of Northern Lapwings (_Vanellus vanellus_)

## Overview

This repository hosts the codebase, documentation, and supporting data for an individual-based modelling (IBM) project focused on the Northern Lapwing (_Vanellus vanellus_). The goal of this project is to simulate lapwing behavioral ecology at the individual level to investigate population dynamics under varying environmental and anthropogenic conditions.

The IBM framework enables mechanistic exploration of spatially and temporally explicit processes, allowing for a more realistic representation of species-environment interactions than aggregate-level models. By capturing individual variability in behavior, movement, reproduction, and survival, we aim to identify critical drivers of lapwing population trends and assess conservation and land management interventions.

Hwew ia a picture of a Northern Lapwing: https://cdn.download.ams.birds.cornell.edu/api/v1/asset/609204339/2400

## Scientific Motivation

The Northern Lapwing is a ground-nesting shorebird of conservation concern across its range, especially in Western Europe. Key threats include:

- **Agricultural intensification**, leading to habitat loss and reduced nesting success.
- **Predation pressure**, particularly from generalist mesopredators in human-modified landscapes.
- **Climate variability**, affecting breeding phenology and resource availability.

Despite conservation efforts, population declines persist, in part due to complex and interacting ecological mechanisms that are difficult to assess through observational studies alone. Individual-based models provide a tool to explore such complexity.

## Why Individual-Based Modelling?

IBMs are particularly suitable for studying species like the Northern Lapwing due to:

- **Behavioral and demographic heterogeneity**: Individuals vary in age, reproductive status, habitat preference, and movement decisions.
- **Fine-scale spatial dynamics**: Nest-site selection, predator avoidance, and territory establishment are highly spatial.
- **Nonlinear and emergent processes**: Population-level outcomes (e.g., local extinction or resilience) can emerge from simple individual rules.
- **Management scenario testing**: Evaluate the impact of conservation interventions such as agri-environment schemes, predator control, and habitat restoration.

This project leverages IBM to integrate empirical data with theoretical models to inform adaptive management strategies.

## Project Objectives

- Develop a robust IBM framework for simulating Northern Lapwing ecology across breeding seasons.
- Quantify the relative influence of environmental factors on movement patterns.
- Evaluate conservation scenarios under alternative land use and climate regimes.

## Methodological Framework

- **Spatial resolution**: Grid-based landscape incorporating habitat heterogeneity (e.g., grasslands, croplands, wet areas).
- **Temporal resolution**: Daily time steps across breeding season (March–August) and non-breeding season.
- **Agents**: Individuals with defined life history attributes and state variables (e.g., age, location, reproductive status).
- **Statistics**: We used agent modelling and step-selection functions to explore the movement patterns of individual birds.
