#summary Ideëen voor bestandsformaat.

# Bestandsformaten #

We willen spaken-bestanden uiteraard op kunnen slaan. Hier kunnen we
experimenteren met verschillende formaten. We gaan ons best doen om het opslagformaat
pluggable te houden.


## Eigen formaat ##

```
name "middelloodlijn"

# name the assumptions and assign them positions for in the UI
points {
  p1: (0,0)
  p2: (1,1)
}

# specify the elements to export when using this theorem
export l i1 i2

# construct the exports using the points by applying theorems
construct {
  # circle, cc_intersection and line are builtin theorems (axioms)

  c1: circle p1 p1 p2
  c2: circle p2 p1 p2
  
  i1 i2: cc_intersection c1 c2
  # this is just shorthand notation for
  # i1 i2: cc_intersection p1 p1 p2 p2 p1 p2
  
  l: line i1 i2
}
```


## XML ##

```
<spaken>
  <theorem name="Middelloodlijn">
    <point id="p1" x="0" y="0" />
    <point id="p2" x="1" y="1" />
    <export element="l" />
    <export element="i1" />
    <export element="i2" />
    <construction>
      <circle id="c1">
        <input ref="p1" />
        <input ref="p1" />
        <input ref="p2" />
      </circle>
      <circle id="c2">
        <input ref="p2" />
        <input ref="p1" />
        <input ref="p2" />
      </circle>
      <applytheorem name="ccintersection">
        <inputs ref="c1" />  <!-- shorthand for <input> for p1,p1,p2 -->
        <inputs ref="c2" />
        <output id="i1" />
        <output id="i2" />
      </ccintersection>
        <line id="l">
        <input ref="i1" />
        <input ref="i2" />
        <!-- line takes care of the output -->
      </line>
    </construction>
  </theorem>
</spaken>
```
