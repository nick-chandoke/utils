USING: kernel locals sequences io io.launcher io.encodings.utf8 fry accessors ;
IN: util

! returns slice of remainder of list that was unprocessed due to hitting exit condition or { } if whole list was processed.
:: loopseq ( ..a seq body: ( ..a tail head -- ..b tail cont? ) -- ..b ) seq [ [ { } f ] [ unclip-slice ] body compose if-empty ] loop ; inline

:: map-until ( seq g: ( elt -- y stop? ) -- seq )
  { } seq g [ [ 2drop f ] [ swap [ suffix ] dip t ] if ] compose loopseq ; inline
:: each-until ( ..a seq pred: ( elt -- stop? ) body: ( ..b -- ..a ) -- ..b )
  seq [ dup pred call [ 2drop f ] [ body call t ] if ] loopseq ; inline

: readproc ( cmdline -- output ) <process> swap >>command utf8 <process-reader> stream-contents ;
