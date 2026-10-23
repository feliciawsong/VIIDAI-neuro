# Builds the neuropathic pain DAG as SVG (hand-laid-out so nothing overlaps)
W, H = 1200, 1010
BW = {1: 230, 2: 230, 3: 240, 4: 170}
BH = 54
X = {1: 150, 2: 470, 3: 805, 4: 1085}
S = {
 "main": ("#FCE4C8", "#C46A00", None, "#1F1F1F", False),   # main exposure tested
 "expl": ("#FFFFFF", "#C46A00", None, "#1F1F1F", False),   # exploratory exposure tested
 "conf": ("#D6EFD8", "#2E7D32", None, "#1F1F1F", False),
 "unm":  ("#F4F4F4", "#8C8C8C", "6,4", "#555555", True),
 "out":  ("#1F3A5F", "#1F3A5F", None, "#FFFFFF", False),
 "rev":  ("#FDECEC", "#C62828", "6,4", "#8E1B1B", True),
}
nodes = {
 "age":   (1, 235, ["Age"], "conf"),
 "sex":   (1, 430, ["Sex"], "conf"),
 "pov":   (1, 640, ["Indigenous language", "(marker of marginalization)"], "conf"),
 "obese": (2, 150, ["Obesity / BMI"], "main"),
 "diab":  (2, 235, ["Diabetes"], "main"),
 "htn":   (2, 320, ["Hypertension"], "main"),
 "rep":   (2, 430, ["Repetitive hand movements"], "expl"),
 "lift":  (2, 515, ["Heavy lifting, bending"], "expl"),
 "cold":  (2, 600, ["Cold/wet conditions"], "expl"),
 "pest":  (2, 685, ["Pesticide exposure"], "expl"),
 "alc":   (2, 770, ["Alcohol use"], "expl"),
 "metab": (3, 192, ["Metabolic nerve damage", "(high blood sugar, inflammation)"], "unm"),
 "vasc":  (3, 320, ["Small-vessel damage"], "unm"),
 "comp":  (3, 472, ["Nerve compression", "(e.g., carpal tunnel)"], "unm"),
 "irrit": (3, 600, ["Cold-related nerve irritation"], "unm"),
 "tox":   (3, 727, ["Neurotoxicity"], "unm"),
 "neuro": (4, 470, ["Neuropathic pain"], "out"),
 "left":  (4, 640, ["Left or reduced", "hand-intensive work"], "rev"),
}
def box(n):
    c, y, lines, st = nodes[n]; w = BW[c]
    h = BH if len(lines) == 1 else 60
    return X[c] - w / 2, y - h / 2, w, h
def R(n, dy=0): x, y, w, h = box(n); return x + w, y + h / 2 + dy
def L(n, dy=0): x, y, w, h = box(n); return x, y + h / 2 + dy
def T(n, dx=0): x, y, w, h = box(n); return x + w / 2 + dx, y
def Bt(n, dx=0): x, y, w, h = box(n); return x + w / 2 + dx, y + h
out = []; A = out.append
A(f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" font-family="Arial, Helvetica, sans-serif">')
A('<defs>'
  '<marker id="a" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#3A3A3A"/></marker>'
  '<marker id="g" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#9A9A9A"/></marker>'
  '<marker id="r" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#C62828"/></marker>'
  '</defs>')
A(f'<rect width="{W}" height="{H}" fill="#FFFFFF"/>')
A('<text x="600" y="30" text-anchor="middle" font-size="20" font-weight="bold" fill="#1F1F1F">Directed acyclic graph (DAG): hypothesized pathways to neuropathic pain</text>')
for c, t in ((1, "Background factors"), (2, "Measured exposures"), (3, "Mechanisms (not measured)"), (4, "Outcome")):
    A(f'<text x="{X[c]}" y="70" text-anchor="middle" font-size="13" font-weight="bold" fill="#5A5A5A">{t.upper()}</text>')
for txt, y in (("METABOLIC", 108), ("WORK / MECHANICAL", 384), ("TOXIC", 646)):
    A(f'<text x="{X[2]}" y="{y}" text-anchor="middle" font-size="11" font-weight="bold" fill="#C46A00" letter-spacing="1">{txt}</text>')
def edge(p, q, kind="k", dash=None, curve=None):
    col = {"k": "#3A3A3A", "g": "#9A9A9A", "r": "#C62828"}[kind]
    d = f' stroke-dasharray="{dash}"' if dash else ""
    if curve:
        A(f'<path d="M{p[0]:.0f},{p[1]:.0f} Q{curve[0]:.0f},{curve[1]:.0f} {q[0]:.0f},{q[1]:.0f}" fill="none" stroke="{col}" stroke-width="1.6"{d} marker-end="url(#{"a" if kind=="k" else kind})"/>')
    else:
        A(f'<line x1="{p[0]:.0f}" y1="{p[1]:.0f}" x2="{q[0]:.0f}" y2="{q[1]:.0f}" stroke="{col}" stroke-width="1.6"{d} marker-end="url(#{"a" if kind=="k" else kind})"/>')
# Background -> exposures
for t in ("obese", "diab", "htn"): edge(R("age"), L(t))
ax, ay = T("age"); bx, by = T("neuro", -50)
A(f'<path d="M{ax:.0f},{ay:.0f} C300,5 1220,5 {bx:.0f},{by:.0f}" fill="none" stroke="#3A3A3A" stroke-width="1.6" marker-end="url(#a)"/>')  # age -> neuropathic pain (over the top)
edge(R("sex"), L("obese", 10)); edge(R("sex"), L("lift")); edge(R("sex"), L("alc"))
for t in ("rep", "lift", "cold", "pest"): edge(R("pov"), L(t))
# Metabolic chain
edge(Bt("obese"), T("diab"))
edge(L("obese", -12), L("htn", -12), curve=(300, 235))
edge(R("obese"), L("metab", -8)); edge(R("diab"), L("metab", 12)); edge(R("htn"), L("vasc"))
# Work / toxic -> mechanisms
edge(R("rep"), L("comp", -10)); edge(R("lift"), L("comp", 12)); edge(R("cold"), L("irrit"))
edge(R("pest"), L("tox", -8)); edge(R("alc"), L("tox", 12))
# Mechanisms -> outcome
nx, ny = L("neuro")
for t, dy in (("metab", -20), ("vasc", -10), ("comp", 0), ("irrit", 10), ("tox", 20)):
    edge(R(t), (nx, ny + dy), kind="g", dash="6,4")
# Reverse causation
edge(Bt("neuro"), T("left"), kind="r", dash="6,4")
A(f'<text x="{X[4]}" y="{box("left")[1] + 82:.0f}" text-anchor="middle" font-size="11" fill="#8E1B1B" font-style="italic">Reverse causation: pain may push</text>')
A(f'<text x="{X[4]}" y="{box("left")[1] + 96:.0f}" text-anchor="middle" font-size="11" fill="#8E1B1B" font-style="italic">people out of repetitive work, so</text>')
A(f'<text x="{X[4]}" y="{box("left")[1] + 110:.0f}" text-anchor="middle" font-size="11" fill="#8E1B1B" font-style="italic">it can look "protective" in our data</text>')
# Nodes
for n, (c, y, lines, st) in nodes.items():
    fill, stroke, dash, tc, it = S[st]; x, yy, w, h = box(n)
    d = f' stroke-dasharray="{dash}"' if dash else ""
    sw = 2 if st in ("main", "conf", "out", "expl") else 1.4
    A(f'<rect x="{x:.0f}" y="{yy:.0f}" width="{w}" height="{h}" rx="8" fill="{fill}" stroke="{stroke}" stroke-width="{sw}"{d}/>')
    for i, t in enumerate(lines):
        ty = y + (i - (len(lines) - 1) / 2) * 17 + 5
        fs = (16 if st == "out" else 14) if i == 0 else 11.5
        A(f'<text x="{X[c]}" y="{ty:.0f}" text-anchor="middle" font-size="{fs}" fill="{tc}"'
          f'{" font-style=\"italic\"" if it else ""}{" font-weight=\"bold\"" if st == "out" else ""}>{t}</text>')
# Legend
ly = 850
items = [("main", "Main exposure tested"), ("expl", "Exploratory exposure"), ("conf", "Adjusted for"),
         ("unm", "Not measured"), ("out", "Outcome"), ("rev", "Reverse causation")]
lx = 40
for st, t in items:
    fill, stroke, dash, tc, it = S[st]; d = f' stroke-dasharray="{dash}"' if dash else ""
    A(f'<rect x="{lx}" y="{ly}" width="26" height="16" rx="3" fill="{fill}" stroke="{stroke}" stroke-width="1.5"{d}/>')
    A(f'<text x="{lx+34}" y="{ly+13}" font-size="13" fill="#1F1F1F">{t}</text>')
    lx += 34 + len(t) * 7.1 + 26
A(f'<text x="40" y="{ly+44}" font-size="13" fill="#1F1F1F"><tspan font-weight="bold">Adjustment sets:</tspan> obesity/BMI: age + sex (not diabetes, which is on the pathway).  Diabetes, hypertension: age + obesity.</text>')
A(f'<text x="40" y="{ly+64}" font-size="13" fill="#1F1F1F">Work exposures and pesticides: age + Indigenous language.  Alcohol: age + sex.</text>')
A(f'<text x="40" y="{ly+88}" font-size="12" fill="#5A5A5A">Arrows show assumed causal direction based on prior literature; the cross-sectional data cannot confirm direction. Dashed arrows pass through unmeasured mechanisms.</text>')
A('</svg>')
open("/home/user/VIIDAI-neuro/figures/neuropathic_DAG.svg", "w").write("\n".join(out))
print("svg written")
