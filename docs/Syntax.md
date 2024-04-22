[Railroad Generator](https://tabatkins.github.io/railroad-diagrams/generator.html#)
Variable declaration:
```
Diagram(
  'ident',
  Choice(0,
    Sequence(': Type', Optional(Sequence('=', 'expr'), 'skip')),
    Sequence(':=', 'expr'),
  ),
  ';'
)
```
Everything else declaration:
```
Diagram(
  'ident',
  '::',
  ZeroOrMore(
    Sequence('-', 'attrib'),
  ),
  Optional(
	    Sequence('(', ZeroOrMore(Sequence('ident', ':', 'Type', ',')), ')', '=>'),
  ),
  Choice(0,
    'function',
    'struct',
    'class',
    'enum',
    'union',
    'template',
    'frag',
    'vert',
    'comp',
    'geom',
  ),
)
```
Ident:
```
Diagram(
  Choice(0,
    'alpabetical',
    '_',
  ),
  ZeroOrMore(
    Choice(0, 
      'alphabetical',
      'numerical',
      '_',
    )
  ),
)
```
