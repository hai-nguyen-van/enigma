Wehrmacht Enigma I Machine
===================

> Functional-style execution of the historic encryption machine in OCaml

![Photograph: Linda Nylind for the Guardian](enigma.jpg "Photograph: Linda Nylind for the Guardian")

Usage
-------------------

This snipet of code aims to show the execution of *Wehrmacht Enigma I* in a functional approach. Each original electromechanical component is described as a functional operator of the following kind

* `_ @% _` : plugboard
* `_ @$ _` : reflector
* `_ @* _` : rotor symbol-substitution
* `_ @^ _` : rotor stepping
* `_ @^^^ _` : rotors turnover

Combining altogether allows to finally define the machine execution as a composition of operators.

Disclaimer
-------------------

May contain bugs. Not fully tested yet.

THE PROVIDER MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY, USE, OR PERFORMANCE OF THIS PROGRAM OR ABOUT ANY CONTENT OR INFORMATION MADE ACCESSIBLE BY THESE, FOR ANY PURPOSE.
