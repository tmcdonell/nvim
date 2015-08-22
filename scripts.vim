"-----------------------------------------------------------------------
"-- Dynamic File Type Detection ----------------------------------------
"-----------------------------------------------------------------------


" File type already set?
if did_filetype()
  finish
endif


"-- Haskell ------------------------------------------------------------
if     getline(1) =~# '^#!.*/bin/env\s\+runghc\>'
  setfiletype haskell
elseif getline(1) =~# '^#!.*/bin/env\s\+runhaskell\>'
  setfiletype haskell
endif
