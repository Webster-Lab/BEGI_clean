#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 28 17:34:31 2026

@author: etipps
"""

# =============================================================================
# Ogata-Banks Monte Carlo — Publication-Ready Figure for Spyder
# Both plots in one figure with a shared y-axis
# =============================================================================
import os
from math import sqrt, erfc
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import pandas as pd
import warnings

warnings.simplefilter(action='ignore', category=pd.errors.PerformanceWarning)

# =============================================================================
# Style constants
# =============================================================================
LINE_COLOR         = '#440154'   # dark viridis purple
PANEL_HEADER_COLOR = '#D9D9D9'
PANEL_BG_COLOR     = 'white'
BORDER_COLOR       = '#AAAAAA'
LINE_ALPHA         = 0.20
LINE_WIDTH         = 0.7

plt.rcParams.update({
    'font.family':       'sans-serif',
    'font.sans-serif':   ['Arial', 'Helvetica', 'DejaVu Sans'],
    'font.size':         10,
    'axes.labelsize':    10,
    'xtick.labelsize':   9,
    'ytick.labelsize':   9,
    'axes.linewidth':    0.6,
    'axes.facecolor':    PANEL_BG_COLOR,
    'figure.facecolor':  'white',
    'xtick.major.width': 0.6,
    'ytick.major.width': 0.6,
    'xtick.major.size':  4,
    'ytick.major.size':  4,
    'xtick.direction':   'out',
    'ytick.direction':   'out',
    'axes.grid':         False,
    'figure.dpi':        150,
    'savefig.dpi':       300,
})

# =============================================================================
# Helpers
# =============================================================================
def add_strip(ax, label):
    strip = ax.inset_axes([0, 1.04, 1, 0.12], transform=ax.transAxes)
    strip.set_facecolor(PANEL_HEADER_COLOR)
    for spine in strip.spines.values():
        spine.set_edgecolor(BORDER_COLOR)
        spine.set_linewidth(0.6)
    strip.set_xticks([])
    strip.set_yticks([])
    strip.text(0.5, 0.5, label, transform=strip.transAxes,
               ha='center', va='center', fontsize=10)

def style_ax(ax):
    ax.set_facecolor(PANEL_BG_COLOR)
    ax.grid(False)
    for spine in ax.spines.values():
        spine.set_edgecolor(BORDER_COLOR)
        spine.set_linewidth(0.6)

# =============================================================================
# Ogata-Banks solution
# =============================================================================
def ogatabanks(c_source, space, time, dispersion, velocity):
    velocity = np.round(velocity, 3)
    term1 = erfc((space - velocity * time) / (2.0 * sqrt(dispersion * time)))
    term2 = np.exp(np.clip(velocity * space / dispersion, -500, 500))
    term3 = erfc((space + velocity * time) / (2.0 * sqrt(dispersion * time)))
    return c_source * 0.5 * (term1 + term2 * term3)

def stats_at_pt(x_loc=25,percentiles=[5, 50, 90]):
    avg = results_df.loc[x_loc, :].mean()
    std = results_df.loc[x_loc, :].std()
    percentiles = np.percentile(results_df.loc[x_loc, :], percentiles)
    return avg, std, percentiles

# =============================================================================
# Parameters
# =============================================================================
c_source   = 11.0
space      = 200.0
time       = 30.0
#dispersion = 1.0
alpha      = 1.0 # logitudinal dispersivity (L) 
#NOTE: ogata banks uses dispersion coefficient, not dispersivity, 
# but we will dyanmically calc that in the Monte Carlo sampling below
porosity   = 0.2
ksat       = 0.4
head       = 0.015

# =============================================================================
# Monte Carlo sampling
# =============================================================================
np.random.seed(42)
nsamples = 1000

k_samples          = np.random.normal(np.log10(ksat),       0.75, nsamples)
alpha_samples = np.random.normal(np.log10(alpha), .5, nsamples)
head_grad_samples  = np.clip(np.random.normal(head,     0.004, nsamples), 0.005, 0.025)
porosity_samples   = np.clip(np.random.normal(porosity, 0.05,  nsamples), 0.05,  0.35)

# =============================================================================
# Figure — shared y-axis: sharey=True removes redundant tick labels on ax2
# =============================================================================
fig, ax = plt.subplots(
    1, 1,
    figsize=(6, 4),
    # sharey=True,                          # <-- shared y-axis
    # gridspec_kw={'wspace': 0.08}          # panels close together since no gap needed
)

# fig.subplots_adjust(left=0.10, right=0.97, top=0.82, bottom=0.15)

# # --------------------------------------------------------------------------
# # Panel 1 — Concentration Profile (C vs distance)
# # --------------------------------------------------------------------------
# x = np.linspace(0, 200, 1001)
# # style_ax(ax1)
# results_df = pd.DataFrame({'x': x})

# for samp in range(nsamples):
#     k_s = 10 ** k_samples[samp]
#     D_s = 10 ** dispersion_samples[samp]
#     v_s = (k_s * head_grad_samples[samp]) / porosity_samples[samp]
#     c   = np.array([ogatabanks(c_source, xi, time, D_s, v_s) for xi in x])
#     results_df.loc[:, f'sample_{samp}'] = c

# results_df.set_index('x', inplace=True)

# avg, std, percentiles = stats_at_pt(x_loc=50, percentiles=[5, 50, 95])
# print(avg, std, percentiles)

# nsamples = 5000

# k_samples          = np.random.normal(np.log10(ksat),       0.75, nsamples)
# dispersion_samples = np.random.normal(np.log10(dispersion), .5, nsamples)
# head_grad_samples  = np.clip(np.random.normal(head,     0.004, nsamples), 0.005, 0.025)
# porosity_samples   = np.clip(np.random.normal(porosity, 0.05,  nsamples), 0.05,  0.35)

# x = np.linspace(0, 200, 1001)
# # style_ax(ax1)
# results_df = pd.DataFrame({'x': x})

# for samp in range(nsamples):
#     k_s = 10 ** k_samples[samp]
#     D_s = 10 ** dispersion_samples[samp]
#     v_s = (k_s * head_grad_samples[samp]) / porosity_samples[samp]
#     c   = np.array([ogatabanks(c_source, xi, time, D_s, v_s) for xi in x])
#     results_df.loc[:, f'sample_{samp}'] = c

# results_df.set_index('x', inplace=True)

# avg, std, percentiles = stats_at_pt(x_loc=50, percentiles=[5, 50, 95])
# print(avg, std, percentiles)

#1000 and 5000 samples changed too much from run to run (based on random seed)
#converged at 10,000

nsamples = 10000
alpha_samples      = np.random.normal(np.log10(alpha),       0.5, nsamples)
k_samples          = np.random.normal(np.log10(ksat),       0.75, nsamples)
# dispersion_samples = np.random.normal(np.log10(dispersion), .5, nsamples)
head_grad_samples  = np.clip(np.random.normal(head,     0.004, nsamples), 0.005, 0.025)
porosity_samples   = np.clip(np.random.normal(porosity, 0.05,  nsamples), 0.05,  0.35)

x = np.linspace(0, 300, 301)
# style_ax(ax1)
results_df = pd.DataFrame({'x': x})

for samp in range(nsamples):
    k_s = 10 ** k_samples[samp]
    alpha_s = 10 ** alpha_samples[samp]
    v_s = (k_s * head_grad_samples[samp]) / porosity_samples[samp]
    D_c = alpha_s * v_s
    c   = np.array([ogatabanks(c_source, xi, time, D_c, v_s) for xi in x])
    # if c[-1] > 6:
    #     # print(c[-1],k_s, porosity_samples[samp],head_grad_samples[samp]) #unrealistically high conc at 300m
    #     #likely due to very high velocity from high K, low porosity, and/or high head gradient. Skipping this sample.
    #     continue
    results_df.loc[:, f'sample_{samp}'] = c
    ax.plot(x, c, color=LINE_COLOR, lw=LINE_WIDTH, alpha=LINE_ALPHA,
             rasterized=True)

ax.set_xlim(0, 300)
ax.set_ylim(0, c_source * 1.05)
ax.xaxis.set_major_locator(ticker.MultipleLocator(50))
ax.yaxis.set_major_locator(ticker.MultipleLocator(2))
ax.set_xlabel('Distance from river (m)')
ax.set_ylabel('DO Concentration (mg/L)')
add_strip(ax, f'Concentration Profile   (t = {time:.0f} days,  n = {nsamples})')

results_df.set_index('x', inplace=True)
# =============================================================================
# Save
# =============================================================================
fig.savefig(os.path.join('..','plots','ogatabanks_pub.png'), bbox_inches='tight')
fig.savefig(os.path.join('..','plots','ogatabanks_pub.pdf'), bbox_inches='tight')
# plt.show()

print("Saved: ogatabanks_pub.png / .pdf")

#stats for paper
avg, std, percentiles = stats_at_pt(x_loc=25, percentiles=[5, 50, 95])
print(f'95% of realizations showed a conc at or below {percentiles[2]} mg/L at 25 m from river')

avg, std, percentiles = stats_at_pt(x_loc=50, percentiles=[5, 50, 95])
print(f'95% of realizations showed a conc at or below {percentiles[2]} mg/L at 50 m from river')

results_200m = [1 if x > 0.2 else 0 for x in results_df.loc[200, :]]
print("Realizations with concentrations exceeding 0.2 mg/L at 200 m from river: " + str(np.sum(results_200m)))
print("Total realizations: " + str(len(results_200m)))
print("Percentage: " + str(np.sum(results_200m)/len(results_200m) * 100) + "%")
results_df.to_csv(os.path.join('..','plots','ogatabanks_results.csv'))