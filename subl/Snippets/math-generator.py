#!/usr/bin/env python3

maps = [
    (
        "a",
        "α",
        r"\alpha",
    ),
    (
        "b",
        "β",
        r"\beta",
    ),
    (
        "c",
        "ξ",
        r"\xi",
    ),
    (
        "d",
        "δ",
        r"\delta",
    ),
    (
        "e",
        "ε",
        r"\epsilon",
    ),
    (
        "f",
        "φ",
        r"\phi",
    ),
    (
        "g",
        "γ",
        r"\gamma",
    ),
    (
        "h",
        "θ",
        r"\theta",
    ),
    (
        "i",
        "ι",
        r"\iota",
    ),
    (
        "k",
        "κ",
        r"\kappa",
    ),
    (
        "l",
        "λ",
        r"\lambda",
    ),
    (
        "m",
        "μ",
        r"\mu",
    ),
    (
        "n",
        "ν",
        r"\nu",
    ),
    (
        "p",
        "π",
        r"\pi",
    ),
    (
        "q",
        "ψ",
        r"\psi",
    ),
    (
        "r",
        "ρ",
        r"\rho",
    ),
    (
        "s",
        "σ",
        r"\sigma",
    ),
    (
        "t",
        "τ",
        r"\tau",
    ),
    (
        "u",
        "υ",
        r"\upsilon",
    ),
    (
        "w",
        "ω",
        r"\omega",
    ),
    (
        "x",
        "χ",
        r"\times",
    ),
    (
        "y",
        "η",
        r"\eta",
    ),
    (
        "z",
        "ζ",
        r"\zeta",
    ),
]

# TODO: Make it cycle through cdlatex-like levels (e.g. sin, exp, int, etc. in addition).
for char, unic, tex in maps:
    with open(f"latex-{char}-1.sublime-snippet", "w") as fout:
        fout.write(
            f"""
            <snippet>
            <scope>text.tex.latex, meta.environment.math</scope>
            <tabTrigger>{char}</tabTrigger>
            <content><![CDATA[{tex}]]></content>
            </snippet>
            """
        )

    with open(f"latex-{char}-2.sublime-snippet", "w") as fout:
        fout.write(
            f"""
            <snippet>
            <scope>text.tex.latex, meta.environment.math</scope>
            <tabTrigger>{tex}</tabTrigger>
            <content><![CDATA[{unic}]]></content>
            </snippet>
            """
        )

    with open(f"latex-{char}-3.sublime-snippet", "w") as fout:
        fout.write(
            f"""
            <snippet>
            <scope>text.tex.latex, meta.environment.math</scope>
            <tabTrigger>{unic}</tabTrigger>
            <content><![CDATA[{char}]]></content>
            </snippet>
            """
        )
