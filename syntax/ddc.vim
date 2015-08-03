" Language:   DDC
" Maintainer: Jacob Stanley
" Version:    0.0

if exists("b:current_syntax")
  finish
endif

setlocal iskeyword+=',#

"
" Keywords
"

syntax keyword ddcKeyword module
syntax keyword ddcKeyword import
syntax keyword ddcKeyword export
syntax keyword ddcKeyword foreign
syntax keyword ddcKeyword type
syntax keyword ddcKeyword value
syntax keyword ddcKeyword data
syntax keyword ddcKeyword in
syntax keyword ddcKeyword of
syntax keyword ddcKeyword letrec
syntax keyword ddcKeyword letcase
syntax keyword ddcKeyword private
syntax keyword ddcKeyword extend
syntax keyword ddcKeyword using
syntax keyword ddcKeyword withregion
syntax keyword ddcKeyword let
syntax keyword ddcKeyword case
syntax keyword ddcKeyword purify
syntax keyword ddcKeyword forget
syntax keyword ddcKeyword box
syntax keyword ddcKeyword run
syntax keyword ddcKeyword weakeff
syntax keyword ddcKeyword weakclo
syntax keyword ddcKeyword with
syntax keyword ddcKeyword where
syntax keyword ddcKeyword do
syntax keyword ddcKeyword match
syntax keyword ddcKeyword if
syntax keyword ddcKeyword then
syntax keyword ddcKeyword else
syntax keyword ddcKeyword otherwise

"
" Witness Constructors
"

syntax keyword ddcWitnessConstructor pure
syntax keyword ddcWitnessConstructor empty
syntax keyword ddcWitnessConstructor use
syntax keyword ddcWitnessConstructor read
syntax keyword ddcWitnessConstructor alloc

"
" Symbols / Literals
"

syntax match   ddcRoundBra        "\v\("
syntax match   ddcRoundKet        "\v\)"
syntax match   ddcSquareBra       "\v\["
syntax match   ddcSquareKet       "\v\]"
syntax match   ddcBraceBra        "\v\{"
syntax match   ddcBraceKet        "\v\}"
syntax match   ddcDot             "\v\."
syntax match   ddcComma           "\v,"
syntax match   ddcSemiColon       "\v;"
syntax match   ddcColon           "\v:"
syntax match   ddcBackslash       "\v\\"
syntax match   ddcEquals          "\v\="
syntax match   ddcBar             "\v\|"

syntax match   ddcOperator          "\v[-~!@#$%&*+/<>^]"                            " w/o parens
syntax match   ddcOperator          "\v[-~!@#$%&*+/<>=:\|][-~!@#$%^&*+/<>=:\|?]+"   " w/o parens
syntax match   ddcOperator        "\v\([-~!@#$%&*+/<>=:\|][-~!@#$%^&*+/<>=:\|?]*\)" " w/  parens

syntax match   ddcLiteral         "\v-?[0-9][0-9boxwfi.#']*"
syntax match   ddcDeBruijn        "\v\^[0-9]+"

syntax match   ddcSquareColonBra  "\v\[:"
syntax match   ddcSquareColonKet  "\v:\]"
syntax match   ddcBraceColonBra   "\v\{:"
syntax match   ddcBraceColonKet   "\v:\}"
syntax match   ddcArrowTilde      "\v\~\>"
syntax match   ddcArrowDash       "\v-\>"
syntax match   ddcArrowDashLeft   "\v\<-"
syntax match   ddcArrowEquals     "\v\=\>"
syntax match   ddcBigLambda       "\v/\\"
syntax match   ddcDaConUnit       "\v\(\)"

"
" Identifiers
"

syntax match   ddcConstructor     "\v[A-Z][a-zA-Z0-9_]+'?"
syntax match   ddcPrimConstructor "\v[A-Z][a-zA-Z0-9_]+#"

syntax match   ddcVariable        "\v[a-z][a-zA-Z0-9_'$]+"
syntax match   ddcPrimVariable    "\v[a-z][a-zA-Z0-9_'$]+#"

syntax match   ddcDeclaration     "\v^[a-z][a-zA-Z0-9_'$]+"

"
" Comments
"

syntax keyword ddcTodo            TODO FIXME XXX HACK    contained
syntax match   ddcCommentLine     "\v--.*$"              contains=ddcTodo,@Spell
syntax region  ddcCommentBlock    start="{-"  end="-}"   contains=ddcCommentBlock,ddcTodo,@Spell
syntax region  ddcPragma          start="{-#" end="#-}"

"
" Highlighting
"

highlight def link ddcConstructor         Include
highlight def link ddcVariable            Normal
highlight def link ddcDeclaration         Identifier
highlight def link ddcPrimVariable        SpecialComment
highlight def link ddcPrimConstructor     SpecialComment
highlight def link ddcOperator            Operator
highlight def link ddcKeyword             Keyword
highlight def link ddcWitnessConstructor  SpecialComment

highlight def link ddcCommentLine         Comment
highlight def link ddcCommentBlock        Comment
highlight def link ddcPragma              SpecialComment
highlight def link ddcLiteral             Number
highlight def link ddcDeBruijn            SpecialComment

highlight def link ddcSquareBra           Delimiter
highlight def link ddcSquareKet           Delimiter
highlight def link ddcBraceBra            Delimiter
highlight def link ddcBraceKet            Delimiter
highlight def link ddcRoundBra            Delimiter
highlight def link ddcRoundKet            Delimiter
highlight def link ddcDot                 Delimiter
highlight def link ddcComma               Delimiter
highlight def link ddcSemiColon           Delimiter
highlight def link ddcColon               Delimiter
highlight def link ddcBackslash           Delimiter
highlight def link ddcEquals              Delimiter
highlight def link ddcBar                 Delimiter

highlight def link ddcSquareColonBra      Delimiter
highlight def link ddcSquareColonKet      Delimiter
highlight def link ddcBraceColonBra       Delimiter
highlight def link ddcBraceColonKet       Delimiter
highlight def link ddcArrowTilde          Delimiter
highlight def link ddcArrowDash           Delimiter
highlight def link ddcArrowDashLeft       Delimiter
highlight def link ddcArrowEquals         Delimiter
highlight def link ddcBigLambda           Delimiter
highlight def link ddcDaConUnit           Delimiter

highlight def link ddcTodo                Todo

let b:current_syntax = "ddc"
