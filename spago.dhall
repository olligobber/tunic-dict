{ name = "tunic-dict"
, dependencies =
	[ "effect"
	, "prelude"
	, "halogen"
	, "ordered-collections"
	, "transformers"
	, "strings"
	, "maybe"
	, "halogen-svg-elems"
	, "foldable-traversable"
	, "group"
	, "arrays"
	, "console"
	, "control"
	, "integers"
	, "orders"
	, "tuples"
	, "unsafe-coerce"
	, "web-html"
	]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
