# MustaPAS

Mustache implementation in simple procedural Pascal

# Design Goal

* Simple procedural interface in single unit
* Ease of maintenance and extension
* Fast enough, but not need to be the fastest

# Interface

There's only a single function:

```pascal
procedure Render(const Template: TStream; const Context: TJSONObject; Result: TStream);
```

The use of `TStream` allows input from and output to any `TStream` descendant,
be it direct string, files, sockets, whatever you can imagine.

# Current Compliance Level

* interpolation (&#9989;)
* comments (&#10060;)
* delimiters (&#10060;)
* inverted (&#10060;)
* partials (&#10060;)
* sections (&#10060;)
