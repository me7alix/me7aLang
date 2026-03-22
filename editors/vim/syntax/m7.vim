if exists("b:current_syntax")
  finish
endif

syn match m7Comment "//.*$"
syn region m7String start=+"+ skip=+\\\\\|\\"+ end=+"+
syn region m7String start=+'+ skip=+\\\\\\|\\'+ end=+'+
syn keyword m7Keyword import if else while for return break continue extern struct null fn def macro block impl static
syn keyword m7Boolean true false
syn keyword m7Type int float f32 bool u0 iptr uint i64 u64 i32 u32 i16 u16 i8 u8 uptr
syn match m7Number /\v\<\d+(\.\d+)?([eE][+-]?\d+)?\>/
syn match m7Func /\v\w+\ze\s*\(/
syn match m7Bracket /\v[\[\]]/
syn match m7Operator /[-+*\/=<>!&|]+/

hi def link m7Comment Comment
hi def link m7String String
hi def link m7Keyword Keyword
hi def link m7Boolean Boolean
hi def link m7Type Type
hi def link m7Number Number
hi def link m7Func Function
hi def link m7Operator Operator
hi def link m7Bracket Special

let b:current_syntax = "m7"
