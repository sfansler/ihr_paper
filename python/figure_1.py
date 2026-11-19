"""
Three-panel schematic: minute-level wearable HR signal at three temporal resolutions.
  A. Landmark metrics   B. Diurnal range   C. Heart rate excursions
 
Edit any of the CONFIG values below, then re-run:
    python hr_figure.py
Outputs hr_figure.svg (vector, opens in Inkscape/Illustrator), .pdf and .png.
"""
 
from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

HERE = Path(__file__).resolve().parent.parent / "figures"
 
# ----------------------------------------------------------------------
# CONFIG — colors, fonts, sizes
# ----------------------------------------------------------------------
NAVY   = "#1c3557"   # main signal / curve
RED    = "#c9342b"   # peaks, max HR, accents
BAND_1 = "#9fc0dd"   # wide diurnal band
BAND_2 = "#5f92bf"   # inner diurnal band
BLUE   = "#2f6fa8"   # event onset/offset dots
PINK   = "#f0b7b2"   # excursion fill
GREY   = "#6b7280"   # secondary annotation text
 
FS_TITLE  = 11
FS_LABEL  = 9
FS_TICK   = 8
FS_ANNOT  = 8.5
FS_CAP    = 7.8
 
plt.rcParams.update({
    "font.family": "DejaVu Sans",
    "axes.linewidth": 0.9,
    "axes.edgecolor": "#333333",
    "xtick.major.width": 0.9,
    "ytick.major.width": 0.9,
    "xtick.labelsize": FS_TICK,
    "ytick.labelsize": FS_TICK,
    "svg.fonttype": "none",      # keeps text editable in Inkscape
})
 
WHITEBOX = dict(facecolor="white", edgecolor="none", alpha=0.85,
                boxstyle="round,pad=0.18")
 
 
def style_axes(ax):
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.tick_params(length=3, pad=2)
 
 
def panel_title(ax, letter, text):
    ax.text(0.0, 1.045, f"{letter}.", transform=ax.transAxes,
            fontsize=FS_TITLE, fontweight="bold", color=RED,
            va="bottom", ha="left")
    ax.text(0.064, 1.045, text, transform=ax.transAxes,
            fontsize=FS_TITLE, fontweight="bold", color=NAVY,
            va="bottom", ha="left")
 
 
def caption(ax, text):
    ax.text(0.5, -0.30, text, transform=ax.transAxes, ha="center", va="top",
            fontsize=FS_CAP, color=GREY, style="italic")
 
 
# ----------------------------------------------------------------------
# Figure scaffold
# ----------------------------------------------------------------------
fig, axes = plt.subplots(1, 3, figsize=(13.0, 3.9))
fig.subplots_adjust(left=0.055, right=0.985, top=0.86, bottom=0.24, wspace=0.30)
axA, axB, axC = axes
rng = np.random.default_rng(7)
 
# ======================================================================
# PANEL A — Landmark metrics
# ======================================================================
t = np.linspace(0, 24, 24 * 60)
awake = 1 / (1 + np.exp(-(t - 6.6) / 0.30)) - 1 / (1 + np.exp(-(t - 22.4) / 0.45))
base = (50
        + 24 * awake
        + 8 * np.exp(-0.5 * ((t - 13.0) / 2.4) ** 2)
        + 9 * np.exp(-0.5 * ((t - 18.6) / 1.8) ** 2))
# layered noise: slow drift + medium wander + minute-to-minute jitter
slow   = np.convolve(rng.normal(0, 10.0, t.size), np.ones(90) / 90, mode="same")
medium = np.convolve(rng.normal(0, 7.0, t.size), np.ones(15) / 15, mode="same")
jitter = rng.normal(0, 3.2, t.size)
wiggle = slow + medium + jitter
hr = base + wiggle * (0.35 + 0.85 * awake)
# short bouts of activity through the day
for centre, height, width in [(9.4, 14, .55), (11.6, 10, .40),
                              (15.2, 17, .45), (20.0, 12, .50)]:
    hr += height * np.exp(-0.5 * ((t - centre) / width) ** 2)
# random minute-level activity bursts scattered across the waking day
for _ in range(26):
    c = rng.uniform(6.8, 22.0)
    hr += rng.uniform(6, 20) * np.exp(-0.5 * ((t - c) / rng.uniform(0.05, 0.22)) ** 2)
# sensor artifacts: brief spikes and dropouts, a few minutes wide
for _ in range(9):
    c = rng.uniform(0.5, 23.5)
    hr += rng.choice([-1, 1]) * rng.uniform(12, 26) * \
          np.exp(-0.5 * ((t - c) / rng.uniform(0.015, 0.04)) ** 2)
# morning exercise spike -> daily max
hr += 62 * np.exp(-0.5 * ((t - 7.3) / 0.18) ** 2)
hr = np.clip(hr, 38, None)
 
# RHR = middle of the nighttime HR distribution (asleep hours)
night = (t < 5.5) | (t > 23.0)
RHR, MAXHR = float(np.median(hr[night])), float(hr.max())
t_peak = float(t[hr.argmax()])
 
axA.plot(t, hr, color=NAVY, lw=0.85)
axA.axhline(MAXHR, xmin=0.0, xmax=24 / 27.5, color=RED, lw=0.8,
            ls=(0, (4, 3)), alpha=0.75)
axA.axhline(RHR, xmin=0.0, xmax=24 / 27.5, color=GREY, lw=0.8,
            ls=(0, (4, 3)), alpha=0.85)
axA.plot([t_peak], [MAXHR], marker="v", ms=5, color=RED, zorder=5)
 
# "max HR" — sits in open space to the right of the spike, leader to the peak
axA.annotate("max HR", xy=(t_peak + 0.25, MAXHR), xytext=(11.4, MAXHR + 9),
             fontsize=FS_ANNOT, color=RED, fontweight="bold",
             ha="left", va="center",
             arrowprops=dict(arrowstyle="-", color=RED, lw=0.8,
                             shrinkA=2, shrinkB=2))
# "RHR" — mid-panel, directly under the "max HR" label, above the RHR line
axA.text(11.4, RHR - 8, "RHR", fontsize=FS_ANNOT, color=GREY,
         ha="left", va="bottom", fontweight="bold")
# "HRR" — double arrow parked in the margin, clear of the RHR label
_hrr_lo = RHR + 2
axA.add_patch(FancyArrowPatch((26.7, _hrr_lo), (26.7, MAXHR),
                              arrowstyle="<->", mutation_scale=8,
                              color=NAVY, lw=1.0))
axA.text(26.2, (_hrr_lo + MAXHR) / 2, "HRR", fontsize=FS_ANNOT, color=NAVY,
         ha="right", va="center", rotation=90, fontweight="bold")
 
axA.set_xlim(0, 27.5)
axA.set_ylim(30, MAXHR + 22)
axA.set_xticks([0, 6, 12, 18, 24])
axA.set_xticklabels(["12a", "6a", "12p", "6p", "12a"])
axA.set_xlabel("time of day", fontsize=FS_LABEL)
axA.set_ylabel("HR (bpm)", fontsize=FS_LABEL)
# centre the x-label under 12p (data spans 0-24 inside a 0-27.5 axis)
axA.xaxis.set_label_coords(12 / 27.5, -0.1)
style_axes(axA)
axA.spines["bottom"].set_bounds(0, 24)
panel_title(axA, "A", "Landmark metrics")
caption(axA, "Global scalars from one day (RHR, max HR, HRR)")
 
# ======================================================================
# PANEL B — Diurnal range
# ======================================================================
tb = np.linspace(0, 24, 600)
med = (56
       + 20 * np.exp(-0.5 * ((tb - 11.0) / 4.2) ** 2)
       + 9 * np.exp(-0.5 * ((tb - 18.0) / 2.6) ** 2))
spread = 4.0 + 5.5 * np.exp(-0.5 * ((tb - 13.5) / 5.5) ** 2)
 
axB.fill_between(tb, med - 2.0 * spread, med + 2.0 * spread,
                 color=BAND_1, alpha=0.55, lw=0)
axB.fill_between(tb, med - 1.0 * spread, med + 1.0 * spread,
                 color=BAND_2, alpha=0.55, lw=0)
axB.plot(tb, med, color=NAVY, lw=1.9)
 
# dHR(t) — measured just after 6p, clear of the excursions
x0 = 18.6
i0 = int(np.argmin(np.abs(tb - x0)))
lo, hi = med[i0] - 2 * spread[i0], med[i0] + 2 * spread[i0]
axB.add_patch(FancyArrowPatch((x0, lo), (x0, hi), arrowstyle="<->",
                              mutation_scale=8, color="#12233d", lw=1.2))
axB.annotate("dHRR(t)", xy=(x0 + 0.15, hi), xytext=(21.3, hi + 10),
             fontsize=FS_ANNOT, color="#12233d", fontweight="bold",
             ha="center", va="center",
             arrowprops=dict(arrowstyle="-", color="#12233d", lw=0.8,
                             shrinkA=2, shrinkB=2))
# median curve label — parked below the band in empty space
i_med = int(np.argmin(np.abs(tb - 5.2)))
axB.annotate("median dHR(t)", xy=(5.2, med[i_med]),
             xytext=(4.6, 33), fontsize=FS_ANNOT, color=NAVY,
             ha="center", va="center", fontweight="bold",
             arrowprops=dict(arrowstyle="-", color=NAVY, lw=0.8,
                             shrinkA=2, shrinkB=2))
 
# HR excursions — span the dHRR(t) range: base at its bottom, peak at its top
dhrr_lo = med - 2 * spread
dhrr_hi = med + 2 * spread
exc_centers = [8.3, 12.5, 16.7]
exc_w = [0.30, 0.32, 0.28]
exc_peaks = []
for c, w in zip(exc_centers, exc_w):
    win = (tb > c - 3 * w) & (tb < c + 3 * w)
    ic = int(np.argmin(np.abs(tb - c)))
    amp = dhrr_hi[ic] - dhrr_lo[ic]
    gauss = np.exp(-0.5 * ((tb[win] - c) / w) ** 2)
    y = dhrr_lo[win] + amp * gauss
    axB.fill_between(tb[win], dhrr_lo[win], y, color=RED, alpha=0.35, lw=0, zorder=3)
    axB.plot(tb[win], y, color=RED, lw=1.3, zorder=4)
    ipk = int(np.argmax(gauss))
    exc_peaks.append((tb[win][ipk], y[ipk]))

box_xy = (12.5, 104)
axB.text(*box_xy, "HR excursions", fontsize=FS_ANNOT, color=RED, fontweight="bold",
         ha="center", va="center",
         bbox=dict(facecolor="white", edgecolor=RED, lw=1.0,
                   boxstyle="round,pad=0.3"))
for xpk, ypk in exc_peaks:
    axB.annotate("", xy=(xpk, ypk + 1.2), xytext=box_xy,
                 arrowprops=dict(arrowstyle="->", color=RED, lw=0.9,
                                 shrinkA=15, shrinkB=2))

axB.set_xlim(0, 24)
axB.set_ylim(26, 112)
axB.set_xticks([0, 6, 12, 18, 24])
axB.set_xticklabels(["12a", "6a", "12p", "6p", "12a"])
axB.set_xlabel("time of day", fontsize=FS_LABEL)
axB.set_ylabel("HR (bpm)", fontsize=FS_LABEL)
style_axes(axB)
panel_title(axB, "B", "Diurnal heart rate range with HR excursions")
caption(axB, "Diurnal median curve and range, with example HR excursions")
 
# ======================================================================
# PANEL C — Heart rate excursions
# ======================================================================
tc = np.linspace(0, 110, 800)
BASE, PEAK, TPK = 58.0, 116.0, 45.0
rise = np.exp(-0.5 * ((tc - TPK) / 11.0) ** 2)
fall = np.exp(-0.5 * ((tc - TPK) / 17.0) ** 2)
# small increase at each tail
tails = (np.exp(-0.5 * ((tc - 0.0) / 6.0) ** 2)
         + np.exp(-0.5 * ((tc - 110.0) / 8.0) ** 2))
ev = BASE + (PEAK - BASE) * np.where(tc <= TPK, rise, fall) + 7.0 * tails
# nadir = lowest point of each flank, between the tail bump and the peak
_lw = (tc > 3) & (tc < 33)
_rw = (tc > 80) & (tc < 107)
li = np.arange(tc.size)[_lw][np.argmin(ev[_lw])]
ri = np.arange(tc.size)[_rw][np.argmin(ev[_rw])]
# shift the whole event so the first nadir sits at t = 0
off = tc[li]
tc = tc - off
TPK -= off
 
THRESH = 70.0
above = ev >= THRESH
t_on, t_off = tc[above][0], tc[above][-1]
 
# rising flank red, falling flank blue; tails past each nadir take the other color
_red = ((tc >= tc[li]) & (tc <= TPK)) | (tc >= tc[ri])
_blue = (tc <= tc[li]) | ((tc >= TPK) & (tc <= tc[ri]))
axC.fill_between(tc, BASE, ev, where=_red & (ev > BASE),
                 color=PINK, alpha=0.60, lw=0)
axC.fill_between(tc, BASE, ev, where=_blue & (ev > BASE),
                 color=BAND_2, alpha=0.50, lw=0)
axC.plot(tc, ev, color=NAVY, lw=1.9)
axC.axhline(BASE, color=GREY, lw=0.8, ls=(0, (4, 3)), alpha=0.85)
axC.plot([TPK], [PEAK], marker="*", ms=11, color=RED, zorder=5)
# nudge the left dot a hair inward so it clears the axis; nadir is flat here
li_dot = int(np.argmin(np.abs(tc - (tc[li] + 1.3))))
axC.plot([tc[li_dot], tc[ri]], [ev[li_dot], ev[ri]], "o", ms=5,
         color=BLUE, zorder=5)
for _xn, _yn, _dx in [(tc[li_dot], ev[li_dot], 3.5), (tc[ri], ev[ri], 0.0)]:
    axC.text(_xn + _dx, _yn - 5.5, "nadir", fontsize=FS_ANNOT, color=BLUE,
             ha="center", va="top", fontweight="bold")
 
# peak — above the curve, nothing underneath it
axC.text(TPK, PEAK + 7, "peak", fontsize=FS_ANNOT, color=RED,
         ha="center", va="bottom", fontweight="bold")
# amplitude — arrow on the peak, label just clear of the rising flank
axC.add_patch(FancyArrowPatch((TPK, BASE), (TPK, PEAK), arrowstyle="<->",
                              mutation_scale=8, color="#12233d", lw=1.2))
axC.text(TPK + 2.3, BASE + 0.62 * (PEAK - BASE), "amplitude", fontsize=FS_ANNOT,
         color="#12233d", ha="center", va="center", rotation=90,
         fontweight="bold", bbox=WHITEBOX)
# duration — arrow between onset/offset, label below in open space
axC.add_patch(FancyArrowPatch((t_on, THRESH), (t_off, THRESH),
                              arrowstyle="<->", mutation_scale=8,
                              color=BLUE, lw=1.2))
axC.text((t_on + t_off) / 2, THRESH - 8, "duration", fontsize=FS_ANNOT,
         color=BLUE, ha="center", va="center", fontweight="bold",
         bbox=WHITEBOX)
# rise — leader to the rising left flank
_xr = 26.0 - off
axC.annotate("rise", xy=(_xr, ev[np.argmin(np.abs(tc - _xr))]),
             xytext=(_xr - 5.0, 106.0), fontsize=FS_ANNOT, color=GREY,
             ha="center", va="center", style="italic",
             arrowprops=dict(arrowstyle="-", color=GREY, lw=0.8,
                             shrinkA=2, shrinkB=2))
# recovery — off to the side of the tail, leader line to the falling flank
axC.annotate("recovery", xy=(72.0 - off, ev[np.argmin(np.abs(tc - (72.0 - off)))]),
             xytext=(90.0 - off, 100.0), fontsize=FS_ANNOT, color=GREY,
             ha="center", va="center", style="italic",
             arrowprops=dict(arrowstyle="-", color=GREY, lw=0.8,
                             shrinkA=2, shrinkB=2))
 
axC.set_xlim(0, 110 - off)
axC.set_ylim(40, PEAK + 26)
axC.set_xticks([0, 25, 50, 75])
axC.set_xlabel("time of excursion (min)", fontsize=FS_LABEL)
axC.set_ylabel("HR (bpm)", fontsize=FS_LABEL)
style_axes(axC)
panel_title(axC, "C", "Heart rate excursion")
caption(axC, "One excursion's amplitude and duration")
 
# ----------------------------------------------------------------------
for ext in ("svg", "pdf", "png", "tiff"):
    fig.savefig(HERE / f"fig1_concept.{ext}", dpi=400,
                bbox_inches="tight", facecolor="white")
print(f"done -> {HERE}")
 
