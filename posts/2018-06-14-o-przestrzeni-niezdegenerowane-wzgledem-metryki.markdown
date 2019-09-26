---
documentclass: extarticle
fontsize: 18pt
title: O podprzestrzeni niezdegenerowanej względem metryki
date: 2018-06-14 00:16:16
tags: matematyka, polish
---
![$\mathrm{ker}\,g=\{x\in V |\ \forall y\in V: g(x,y)=0\} = V^{\perp}$](./images/direct_sum.png){width=100%}

**Twierdzenie**[^0]**:** Jeśli $V=W \oplus \mathrm{ker}\,g$, to $W$ jest niezdegenerowana względem formy metrycznej
$g$.

**Dowód:** Niech $x\in\mathrm{ker}\,g_{W}\subset W$ i niech $V\ni y = y_1 + y_2$, gdzie $y$ jednoznacznie rozkłada się na $y_1\in W$ i $y_2 \in \mathrm{ker}\,g$. Wówczas z liniowości w drugim argumencie: 

$$g(x,y) = g(x, y_1 + y_2) = g(x, y_1) + g(x, y_2).$$ 

Ale:

* $g(x, y_1) = g_{W}(x, y_1) = 0$, bo $y_1\in W$,
* $g(x, y_2) = 0$[^1], bo $y_2\in\mathrm{ker}\,g$.

Zatem $g(x,y)=0\ \forall y\in V$, czyli $x\in\mathrm{ker}\,g$. 
Ponieważ $\mathrm{ker}\, g \cap W = \{0\}$[^2], to $\ x=0$.
Zatem $\mathrm{ker}\, g_W = \{0\}$. □ 

[^0]: Twierdzenie znalazłem w podręczniku A. Herdegena, [„Algebra liniowa i geometria”](http://eigenspace.pl), który autor niedawno udostępnił w [pdf](http://eigenspace.pl/herdegen_algebra.pdf) zachęcając przy tym do wpłat na rzecz hospicjum [Alma Spei](http://almaspei.pl/2018/01/10/e-podrecznik-prof-uj/). Przeprowadzony w książce dowód niewprost (tw. 1, §14) łatwo da zastąpić się niniejszym dowodem wprost.
[^1]: Bo jesli $g(x, y_2) = 0$, to $g(y_2, x) = 0$.
[^2]: Z powodu liniowej niezależności składników sumy prostej.

