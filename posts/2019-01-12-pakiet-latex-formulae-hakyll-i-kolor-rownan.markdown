---
title: Pakiet latex-formulae-hakyll i kolor równań
date: 2019-01-12 21:27:28
tags: haskell, hakyll, polish
---

### Wstęp

Ponieważ [Hakyll](https://jaspervdj.be/hakyll/) oparty jest na [Pandocu](https://pandoc.org/), to zagnieżdżanie matematyki za pomocą prerenderowanych grafik wprost z lokalnej instalacji LaTeXu jest proste i daje wiele swobody: zyskujemy dostęp do wszystkich lokalnie dostępnych pakietów, w szczególności do diagramów [TikZa](https://taeer.bar-yam.me/blog/posts/hakyll-tikz/). Służą do tego trzy uzupełniające się pakiety: 

* [latex-formulae-hakyll](https://hackage.haskell.org/package/latex-formulae-hakyll),
* [latex-formulae-image](https://hackage.haskell.org/package/latex-formulae-image), 
* [latex-formulae-pandoc](https://hackage.haskell.org/package/latex-formulae-pandoc). 

Niestety, obecnie są one w dość wczesnej wersji i wczytując się w ich źródła widzimy na przykład, że preambuły służące do renderowania środowisk matematycznych są haniebnie [hardcodowane](https://hackage.haskell.org/package/latex-formulae-image-0.1.1.4/docs/src/Image.LaTeX.Render.html#displaymath), zaś ich typ dobrze byłoby uzupełnić o instancję monoidu. Umożliwiłoby to dopisywanie pakietów do danej preambuły i rozwiązało wiele trywialnych problemów -- w szczególności zmianę koloru renderowanych napisów. 

### Jak to działa?

Hakyll kompiluje posty z formatu markdown za pomocą monady [reguł](https://jaspervdj.be/hakyll/reference/src/Hakyll.Core.Rules.Internal.html#Rules). Bez wikłania się w szczegóły przytoczmy dokumentację [latex-formulae-hakyll](https://hackage.haskell.org/package/latex-formulae-hakyll-0.2.0.4/docs/Hakyll-Contrib-LaTeX.html), która podpowiada poniższą minimalną regułę:

```haskell
match "posts/*.markdown" $ do
  route $ setExtension "html"
  compile $ pandocCompilerWithTransformM
    (renderFormulae 
      defaultPandocFormulaOptions)
```

[defaultPandocformulaOptions](https://hackage.haskell.org/package/latex-formulae-pandoc-0.2.0.6/docs/src/Image.LaTeX.Render.Pandoc.html#convertAllFormulaeDataURI) jest typu rekordowego [PandocFormulaOptions](https://hackage.haskell.org/package/latex-formulae-pandoc-0.2.0.6/docs/src/Image.LaTeX.Render.Pandoc.html#PandocFormulaOptions).

``` haskell
defaultPandocFormulaOptions :: PandocFormulaOptions
defaultPandocFormulaOptions = PandocFormulaOptions
   { shrinkBy = 2
   , errorDisplay = displayError
   , formulaOptions = \case DisplayMath -> displaymath; _ -> math
   }
```
gdzie funkcja `formulaOptions` jest typu `MathType -> FormulaOptions`; jej przytoczona wyżej definicja korzysta z rozszerzenia [LambdaCase](http://storm-country.com/blog/LambdaCase). Reaguje ona na dwa rodzaje środowisk LaTeXu: `displaymath` i pozostałe. W zależności od środowiska wybiera wspomnianą preambułę [displaymath](https://hackage.haskell.org/package/latex-formulae-image-0.1.1.4/docs/src/Image.LaTeX.Render.html#displaymath) albo [math](https://hackage.haskell.org/package/latex-formulae-image-0.1.1.4/docs/src/Image.LaTeX.Render.html#math) wraz z nazwą środowiska i wyjściową rozdzielczością obrazków.


Powstaje problem: jak zmienić kolor równań? Domyślnie LaTeX produkuje równania w kolorze czarnym. Gdyby typ [PandocFormulaOptions](https://hackage.haskell.org/package/latex-formulae-pandoc-0.2.0.6/docs/src/Image.LaTeX.Render.Pandoc.html#PandocFormulaOptions) był monoidem łatwo można byłoby zdefiniować swoje opcje i konkatenować je z domyślnymi. W ten sposób uzyskalibyśmy kontrolę nad kolorem i wiele więcej.

### Jak to naprawić?

Można oczywiscie napisać taką instancję; ~~zajmę się tym jak tylko czas i ochota pozwoli~~[^1]. Ponieważ najtrwalsze są jednak rozwiązania prowizoryczne, wystarczy zdefiniować swój parametr typu [PandocFormulaOptions](https://hackage.haskell.org/package/latex-formulae-pan 
  doc-0.2.0.6/docs/src/Image.LaTeX.Render.Pandoc.html#PandocFormulaOptions), a w nim swoją funkcję `formulaOptions` z parametrami typu [FormulaOptions](https://hackage.haskell.org/package/latex-formulae-image-0.1.1.4/docs/src/Image.LaTeX.Render.html#FormulaOptions). Rozwiązałem to następująco:

```haskell
themedFormulaOptions :: PandocFormulaOptions
themedFormulaOptions = PandocFormulaOptions 
    { shrinkBy = 2
    , errorDisplay = displayError
    , formulaOptions = \case DisplayMath -> specMath "displaymath"; _ -> specMath "math"
    } 

specMath :: String -> FormulaOptions
specMath s = FormulaOptions "\\usepackage{amsmath}\
    \\\usepackage{tikz}\
    \\\usepackage{amsfonts}\
    \\\usepackage{xcolor}\
    \\\definecolor{fg}{HTML}{c5d4db}\
    \\\everymath\\expandafter{\
        \\\the\\everymath \\color{fg}}\
    \\\everydisplay\\expandafter{\
        \\\the\\everydisplay \\color{fg}}" s 275
```

Wówczas w miejscu zmiennej `preamble` w funkcji [imageForFormula](https://hackage.haskell.org/package/latex-formulae-image-0.1.1.4/docs/src/Image.LaTeX.Render.html#imageForFormula) zostanie umieszczona poniższa preambuła LaTeXu

```LaTeX
\usepackage{amsmath}
\usepackage{tikz}
\usepackage{amsfonts}
\usepackage{xcolor}

\definecolor{fg}{HTML}{c5d4db}
\everymath\expandafter{\the\everymath \color{fg}}
\everydisplay\expandafter{\the\everydisplay \color{fg}}
```  
Ostatnie trzy linijki to snippet, który na prędko wygrzebałem ze [SO](https://tex.stackexchange.com/questions/211780/how-put-color-in-all-math-mode). Podmienia on kolor fontów w środowiskach `math` i `displaymath` na zdefiniowany kolor `fg`.

Pozostaje `themedFormulaOptions` zaaplikować do `renderFormulae` w regule dla plików markdown.

```haskell
match "posts/*.markdown" $ do
  route $ setExtension "html"
  compile $ pandocCompilerWithTransformM
    (renderFormulae 
      themedFormulaOptions)
```

W rezultacie otrzymujemy równania w zdefiniowanym kolorze:
<center>
[$$L_f(x) = \sum_{i=0}^n f(x_i) \prod_{j=0 \land j\ne i}^n \frac{x-x_j}{x_i-x_j}.$$](https://en.wikipedia.org/wiki/Polynomial_interpolation)
</center>
 
[^1]: Jak mówiłem, tak zrobiłem: PR z instancją monoidu i dodatkowe objaśnienie można znaleźć [tutaj](https://github.com/liamoc/latex-formulae/pull/10) :-)
