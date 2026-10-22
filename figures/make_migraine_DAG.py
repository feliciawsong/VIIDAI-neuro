# Builds the migraine DAG as SVG (hand-laid-out so nothing overlaps)
W, H = 1200, 900
OFF = 34
BW = {1: 230, 2: 230, 3: 230, 4: 150}
BH = 54
X = {1: 150, 2: 470, 3: 800, 4: 1080}
S = {  # style: fill, stroke, dash, text color, italic
 "exp":  ("#FCE4C8", "#C46A00", None, "#1F1F1F", False),
 "conf": ("#D6EFD8", "#2E7D32", None, "#1F1F1F", False),
 "meas": ("#FFFFFF", "#4A4A4A", None, "#1F1F1F", False),
 "unm":  ("#F4F4F4", "#8C8C8C", "6,4", "#555555", True),
 "out":  ("#1F3A5F", "#1F3A5F", None, "#FFFFFF", False),
}
nodes0 = {  # id: (col, y, lines, style)
 "sex":   (1, 170, ["Sex (female)"], "conf"),
 "pov":   (1, 400, ["Poverty / marginalization", "(proxy: Indigenous language)"], "conf"),
 "age":   (1, 640, ["Age"], "conf"),
 "horm":  (2, 70,  ["Hormonal changes"], "unm"),
 "lift":  (2, 170, ["Heavy lifting", "(part of exertion/heat score)"], "exp"),
 "heat":  (2, 260, ["Hot working conditions", "(part of exertion/heat score)"], "exp"),
 "hours": (2, 350, ["Long work hours"], "meas"),
 "pest":  (2, 440, ["Pesticide exposure"], "meas"),
 "htn":   (2, 640, ["Hypertension"], "meas"),
 "exer":  (3, 170, ["Physical exertion"], "unm"),
 "dehy":  (3, 260, ["Dehydration"], "unm"),
 "sleep": (3, 350, ["Sleep loss, skipped meals"], "unm"),
 "odor":  (3, 440, ["Chemical odors"], "unm"),
 "mig":   (4, 305, ["Migraine"], "out"),
}
nodes = {k: (c, y + OFF, l, st) for k, (c, y, l, st) in nodes0.items()}
def box(n):
    c, y, lines, st = nodes[n]; w = BW[c]
    return X[c] - w / 2, y - BH / 2, w, BH
def R(n): x, y, w, h = box(n); return x + w, y + h / 2
def L(n): x, y, w, h = box(n); return x, y + h / 2
def T(n, dx=0): x, y, w, h = box(n); return x + w / 2 + dx, y
def Bt(n, dx=0): x, y, w, h = box(n); return x + w / 2 + dx, y + h
out = []
A = out.append
A(f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" font-family="Arial, Helvetica, sans-serif">')
A('<defs>'
  '<marker id="a" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#3A3A3A"/></marker>'
  '<marker id="g" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#9A9A9A"/></marker>'
  '</defs>')
A(f'<rect width="{W}" height="{H}" fill="#FFFFFF"/>')
A('<text x="600" y="28" text-anchor="middle" font-size="20" font-weight="bold" fill="#1F1F1F">Directed acyclic graph (DAG): hypothesized pathways to migraine</text>')
for c, t in ((1, "Background factors"), (2, "Work and health conditions"), (3, "Triggers (not measured)"), (4, "Outcome")):
    A(f'<text x="{X[c]}" y="66" text-anchor="middle" font-size="13" font-weight="bold" fill="#5A5A5A">{t.upper()}</text>')
def line(p, q, grey=False, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    A(f'<line x1="{p[0]:.0f}" y1="{p[1]:.0f}" x2="{q[0]:.0f}" y2="{q[1]:.0f}" stroke="{"#9A9A9A" if grey else "#3A3A3A"}" stroke-width="1.6"{d} marker-end="url(#{"g" if grey else "a"})"/>')
def curve(p, c, q, grey=False, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    A(f'<path d="M{p[0]:.0f},{p[1]:.0f} Q{c[0]:.0f},{c[1]:.0f} {q[0]:.0f},{q[1]:.0f}" fill="none" stroke="{"#9A9A9A" if grey else "#3A3A3A"}" stroke-width="1.6"{d} marker-end="url(#{"g" if grey else "a"})"/>')
# --- edges ---
line(R("sex"), L("horm")); line(R("sex"), L("lift")); line(R("sex"), L("heat"))
for t in ("lift", "heat", "hours", "pest"): line(R("pov"), L(t))
line(R("age"), L("htn"))
line(R("lift"), L("exer")); line(R("heat"), L("dehy")); line(R("hours"), L("sleep")); line(R("pest"), L("odor"))
mx, my = L("mig")
for t, dy in (("exer", -18), ("dehy", -6), ("sleep", 6), ("odor", 18)): line(R(t), (mx, my + dy), grey=True, dash="6,4")
curve(R("horm"), (960, 60 + OFF), T("mig", -40), grey=True, dash="6,4")
curve((265, 415 + OFF), (750, 760 + OFF), Bt("mig", -40), dash="3,3")
curve(R("htn"), (980, 680 + OFF), Bt("mig", -20 + 40))
curve(Bt("age"), (1150, 900 + OFF), Bt("mig", 60))
A(f'<text x="660" y="{591 + OFF}" text-anchor="middle" font-size="12" fill="#3A3A3A" font-style="italic">Other pathways (not measured):</text>')
A(f'<text x="660" y="{606 + OFF}" text-anchor="middle" font-size="12" fill="#3A3A3A" font-style="italic">stress, food insecurity, poor sleep, access to care</text>')
# --- nodes ---
for n, (c, y, lines, st) in nodes.items():
    fill, stroke, dash, tc, it = S[st]; x, yy, w, h = box(n)
    d = f' stroke-dasharray="{dash}"' if dash else ""
    A(f'<rect x="{x:.0f}" y="{yy:.0f}" width="{w}" height="{h}" rx="8" fill="{fill}" stroke="{stroke}" stroke-width="{2 if st in ("exp","conf","out") else 1.4}"{d}/>')
    fs = 17 if st == "out" else 14
    for i, t in enumerate(lines):
        ty = y + (i - (len(lines) - 1) / 2) * 17 + 5
        A(f'<text x="{X[c]}" y="{ty:.0f}" text-anchor="middle" font-size="{fs if i == 0 else 12}" fill="{tc}"'
          f'{" font-style=\"italic\"" if it else ""}{" font-weight=\"bold\"" if st == "out" else ""}>{t}</text>')
# --- legend ---
ly = 790
items = [("exp", "Exposure tested"), ("conf", "Confounder adjusted for"), ("meas", "Measured, other"), ("unm", "Not measured"), ("out", "Outcome")]
lx = 40
for st, t in items:
    fill, stroke, dash, tc, it = S[st]; d = f' stroke-dasharray="{dash}"' if dash else ""
    A(f'<rect x="{lx}" y="{ly}" width="26" height="16" rx="3" fill="{fill}" stroke="{stroke}" stroke-width="1.5"{d}/>')
    A(f'<text x="{lx+34}" y="{ly+13}" font-size="13" fill="#1F1F1F">{t}</text>')
    lx += 34 + len(t) * 7.2 + 30
A(f'<text x="40" y="{ly+46}" font-size="13" fill="#1F1F1F"><tspan font-weight="bold">Adjustment sets:</tspan> exertion/heat (heavy lifting, hot conditions) → migraine: sex + Indigenous language.  Hypertension → migraine: sex + age.</text>')
A(f'<text x="40" y="{ly+68}" font-size="12" fill="#5A5A5A">Arrows show assumed causal direction based on prior literature; the cross-sectional data cannot confirm direction. Dashed arrows pass through unmeasured factors.</text>')
A('</svg>')
open("/home/user/VIIDAI-neuro/figures/migraine_DAG.svg", "w").write("\n".join(out))
print("svg written")
