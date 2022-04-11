;; This file does not load, but rather holds definitions for a would be
;; type theory

;; The syntax around methods should be similar to type classes

;; I think this is enough to say the typing judgemeant
;; note for two items below ----, we mean both are
;; valid syntax

;; Γ ⊢ obj : Typeᵢ      Γ, obj : Typeᵢ ⊢ out : typeⱼ
;; -----------------------------------------------
;; (method-pi (obj typeᵢ) typeⱼ)
;; (method-pi typeᵢ typeⱼ)

;; (equivalent
;;   (method-pi (arg₁ typeᵢ) (method-pi (arg₂ typeₖ) ...))
;;   (method-pi (arg₁ typeᵢ) (arg₂ typeₖ) ...))


;; non dependent method instance

;; Γ ⊢ obj : Typeᵢ      Γ ⊢ out : typeⱼ
;; ----------------------------------
;; (method-fn typeᵢ typeⱼ)

;; I want to describe methods as having a set of instances,
;; this set can be calculated from the context Γ and all methods

;; Γ, method : Method ⊢ type-setᵢ : ∀ (s : type). method s
;; -----------------------------------------------------
;; (method-instances method type-setᵢ)

;; in
;; (defun <fn-name (:<param> …) …)
;; :<param> stands for implicit argument

(defsig has (fn typeᵢ (method-pi typeᵢ typeⱼ) typeᵢ))
(defun has (:obj _method)
  (type obj))

;; typeⱼ is most likely a keyword, string, or symbol
(defsig name-of (method-pi (obj typeᵢ) typeⱼ))
(defgeneric name-of (object)
  (:documentation "Grabs the name of the object handed to it"))


;; perfectly valid CL, works as expected it's an un-hygenic macro,
;; could do a lot better, but if it's within proper convention then it
;; is fine
(defmacro pi-type (pi-descriptions &rest body)
  "acts like dependent type pi, but for
(pi-type (<name₁> <type₁> …) …)
each <name> we also make <name>-type to quickly refer to the type of
the bound"
  `(pi ,pi-descriptions
       (let (,@(mapcar
                 (lambda (name)
                   `(,(lol:symb name '-type)
                     (type ,name)))
                 (mapcar #'car (lol:group pi-descriptions 2))))
         ,@body)))

(macroexpand-1 '(pi-type (obj typeᵢ name typeⱼ) name-type))
;; (PI (OBJ TYPEI NAME TYPEJ)
;;   (LET ((OBJ-TYPE (TYPE OBJ))
;;         (NAME-TYPE (TYPE NAME)))
;;     NAME-TYPE))

;; interesting note that this may not communicate
;; each slot in the hash table can have different object
;; types which dictate name for this relationship
;; It's likely that the name type we can fix
;; but the object and the corresponding
;; classification type change must stay
(sigstruct table
  (pi-type (obj  (has name-of)
            name (name-of obj))
     {trait  (hash-table name-type (classification name-type))
      symbol (hash-table name-type obj-type)}))
(defstruct table
  "the table is laid out such that the following relation is made

trait
(hash-table
  :bare-handed -> (#s(classification :yuki :ok))
  :cold        -> (#s(classification :yuki :intrinsic)))

symbol
(hash-table
  :yuki -> *yuki-object*)"
  (trait  (make-hash-table) :type hash-table)
  (symbol (make-hash-table) :type hash-table))


;; how do we determine if a is generic or we need to forall it?
(sigstruct classification
  (fn a
      {name      a
       qualifier (union keyword string)}))
(defstruct classification
  "a classification along with the name the classification belongs
to. This is usually a symbol, but could be a string in the case of flavor text"
  (name      (error "fill this in"))
  (qualifier (error "fill this in") :type (or keyword string)))



;; previous syntax is not ideal, it sets it up as a function
;; better to just work over a binder list, assume Π where appropriate
;; Besides that this is rather easy
(defmacro sigstruct (struct paramaters binder)
  "sigstruct defines the sig of a defstruct. namely it types all the
functions created by the struct and the struct creation itself"
  ;; time to parse!
  (let ((dependency-ordered-binders
          ;; We don't do it, but assume we actually did
          ,(maphash 'list (lambda (_slot_name type-spec) type-spec) binder)))
    ;; prog2 is like progn but it returns the second value
    `(prog2
       ,@(maphash (lambda (slot-name type-spec)
                    (flet ((pi-type-spec (body)
                             ;; this may be incorrect, we should collect the types from
                             ;; the other record slots, as the value may be dependent?
                             ;; Does this matter for the lookup function itself?
                             (let ((left (remove-if-not
                                          (lambda (param) (contains* type-spec param))
                                          paramaters)))
                               (if (null left)
                                   body
                                   `(pi ,left ,body)))))
                      ;; I could have sworn defstruct makes more than just a constructor
                      ;; for each slot
                      `(defsig ,(lol:symb struct '- slot-name)
                           ,(pi-type-spec (fn struct type-spec)))))
                  binder)
       ;; this is straight forward but we don't know what universe we end up in
       ;; so let us just imagine I have a calculation function off hand
       (deftype ,struct
           (pi ,(append ,parameters
                 ,dependency-order-binders)
            ,(calculate-universe binders)))
       (defsig ,(lol:symb 'make- struct)
           ;; is pi accurate to describe arguments which can be sent as keywords?
           (pi ,(append ,parameters
                        ,dependency-order-binders)
               ,struct))
       ;; All what's left is the struct-p, copy-struct, and (setf (struct ...) ...)
       ;; We will type those later!
       )))


(sigstruct classification (a)
  {name      a
   qualifier (union keyword string)})

;; I like this new syntax
;; though this does not compose well with my type macro ☹
(sigstruct table ((obj (has name-of))
                  (name (name-of obj)))
   {trait  (hash-table name-type (classification name-type))
    symbol (hash-table name-type obj-type)})

;; Maybe I can send in the binder function, like :bind-with
;; We would only do this for non standard binders, thus not fn or pi
;; Though this would require first class macros which we lack ☹
;; If the macro expects a macro maybe it can work as staging does!
(sigstruct table ((obj (has name-of))
                  (name (name-of obj)))
   {trait  (hash-table name-type (classification name-type))
    symbol (hash-table name-type obj-type)}
   :bind-with 'pi-type)

;; Other notes, we should call defsig, sigfun
;; have the duality between
;; def<name>
;; sig<name>
