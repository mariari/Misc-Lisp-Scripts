;; MDL is an interesting language. Seems that we have first class macros due to how <> and () interact

;; IDΚ how to variable so lambda write it
<DEFINE MY-TIME (FRM)
  <<FUNCTION (TIME-1)
    <EVAL .FRM>
    <<FUNCTION (TIME-2)
      <- .TIME-2 .TIME-1>>
     <TIME>>>
   <TIME>>>

<DEFINE FACT (X)
  <COND (<0? .X> 1)
        (T       <* .X <FACT <- .X 1>>>)>>

<MY-TIME <QUOTE <FACT 20>>>

;; OVERFLOWS at 21
<MY-TIME <QUOTE <FACT 10000>>>

;; No Let exists let us AUX at first
<DEFINE MY-TIME-FAST (FRM "AUX" TIME-1 TIME-2)
  <SET TIME-1 <TIME>>
  <EVAL .FRM>
  <SET TIME-2 <TIME>>
  <- .TIME-2 .TIME-1>>

<MY-TIME-FAST <QUOTE <FACT 20>>>

;; Lets remove our AUX with a macro
<DEFMAC LET1 ('VAR 'VAL 'BODY)
  <FORM <FORM FUNCTION (.VAR) .BODY> .VAL>>

;; <GUNASSIGN LET1>

<EXPAND '<LET1 X 3 .X>>

<DEFINE FOLDR (F INIT XS)
  <COND (<EMPTY? .XS>
           .INIT)
        (T <.F <NTH .XS 1> <FOLDR .F .INIT <REST .XS>>>)>>

<FOLDR ,+ 0 (1 2 3 4 5)>


;; Now with lets!
<DEFINE MY-TIME-LET (FRM)
  <LET1 TIME-1 <TIME>
     ;; Sadly didn't get progn working for LET-1. So we use PROG
     ;; PROG can bind values.... so our LET-1 is almost pointless!
     <PROG ()
       <EVAL .FRM>
       <LET1 TIME-2 <TIME>
          <- .TIME-2 .TIME-1>>>>>

<MY-TIME-LET <QUOTE <FACT 20>>>

;; We can now define let*, but first lets define let
<DEFMAC LET ('BIND "ARGS" 'BODY)
  <FORM <FORM FUNCTION <MAPF <FUNCTION (XS) <NTH .XS 1>> .BIND>
                       .BODY>
        ;; Need to apply or splice this out
        <MAPF ,REST .BIND>>>

;; LET does not work due to the apply

;; <GUNASSIGN LET>
