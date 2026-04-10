# Tactical Evolution & Player Roles in the NBA (1997–2023)

> Clustering · EDDA classification · sports analytics · 25-season longitudinal study · R

A statistical investigation into how player roles in the NBA have transformed over 25 seasons — from rigid five-position archetypes toward a fluid, skill-based model. The study combines classification analysis and unsupervised clustering to test a concrete hypothesis: that traditional positional boundaries have statistically dissolved in the modern game.

---

## Research Question

Has the statistical distinctiveness of traditional NBA positions (PG, SG, SF, PF, C) declined over time — and does a simpler, three-role model better describe the modern game?

The study tests **JJ Redick's three-category framework**: Ball Handler, Off-Ball Player, and Big — a practitioner's model grounded in how the game is actually played today.

---

## Dataset

Per-game statistics, advanced metrics, and physical attributes across seasons from 1997 to 2023.

| Feature Group | Examples |
|---------------|---------|
| Per-game stats | Points, assists, rebounds, shooting splits |
| Advanced metrics | Usage Rate, Net Rating, True Shooting % |
| Physical attributes | Height, weight |

---

## Methods

### 1. Classification analysis — EDDA (Eigenvalue Decomposition Discriminant Analysis)

Traditional positions used as class labels. EDDA trained on each era to measure how well statistical profiles separate the five positions. **Rising misclassification error = blurring positional boundaries.**

| Era | Misclassification Rate |
|-----|----------------------|
| 1997–2002 | ~24% |
| 2018–2023 | ~40% |

A 16-point increase in error over 25 seasons is not noise — it reflects a structural shift in how the game is played. Players of the same nominal position are now statistically more similar to players at other positions than they were a generation ago.

### 2. Cluster analysis — validating the 3-role model

Unsupervised clustering on the 2019–2023 era to test whether the data naturally organises into Redick's three functional categories.

| Cluster | Statistical Signature |
|---------|-----------------------|
| Ball Handler | High assist rate, high usage, primary playmakers |
| Off-Ball Player | Efficient perimeter shooting, high three-point volume |
| Big | Dominant rebounding, blocks, size metrics |

The three-cluster solution aligns cleanly with the practitioner model — providing data-driven validation of a framework that coaches and analysts already use informally.

---

## Key Finding

The modern NBA has shifted from positional specialisation to **skill and versatility** as the primary organising principle. The three-pointer has been the main structural driver — it has homogenised the offensive profiles of guards and forwards, collapsing what were once statistically distinct roles into a single perimeter archetype (the Off-Ball Player).

The EDDA misclassification trajectory is the clearest quantitative signal: what was a ~24% overlap in 1997 is now ~40% — and the trend shows no sign of reversing.

---

## Why This Matters for Sports Analytics

Positional labels in player databases are increasingly poor proxies for actual role and function. Models that use traditional position as a feature — for valuation, scouting, or lineup optimisation — are working with a degraded signal. Role-based frameworks derived from data (like the three-cluster model here) are more descriptively accurate and more useful as features in downstream ML applications.

---

## Stack

`R` `EDDA` `cluster analysis` `discriminant analysis` `sports analytics` `NBA` `longitudinal analysis` `unsupervised learning`
