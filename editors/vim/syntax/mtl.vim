if exists("b:current_syntax")
  finish
endif

syn match mtlComment "//.*$"
syn region mtlString start=+"+ skip=+\\\\\|\\"+ end=+"+
syn region mtlString start=+'+ skip=+\\\\\\|\\'+ end=+'+
syn keyword mtlKeyword import if else while for return break continue extern struct null fn def macro block impl static do defer switch case union enum
syn keyword mtlBoolean true false
syn match mtlBracket /\v[\[\]]/
syn match mtlNumber /\v\<\d+(\.\d+)?([eE][+-]?\d+)?\>/
syn match mtlFunc /\v\w+\ze\s*\(/
syn match mtlOperator /[-+*\/=<>!&|]+/
syn match mtlType /\v:\s*\zs((\*|\[[^]]*\])\s*)*[A-Za-z_]\w*\ze/

hi def link mtlComment Comment
hi def link mtlString String
hi def link mtlKeyword Keyword
hi def link mtlBoolean Boolean
hi def link mtlType Type
hi def link mtlNumber Number
hi def link mtlFunc Function
hi def link mtlOperator Operator
hi def link mtlBracket Special

let b:current_syntax = "mtl"
